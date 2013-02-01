require'parslet/convenience'
require'erb'
module LazyPP
  class Unit < Struct.new(:program, :file, :contents, :tree, :raw_tree, :time)
    attr_accessor :cpp
    def initialize prog, file, opts={}
      super prog, file, nil, nil, nil, nil
      @read,@scanned = false,false
      @opts={gen_header: false, build: true}.merge opts
    end
    def gen_header; @opts[:gen_header] end 
    def gen_header= val; @opts[:gen_header]= val end
    def debugger_required!
      @opts[:debugger_required?] = true
    end
    def debugger_required?
      @opts[:debugger_required?]
    end
    def build?; @opts[:build] end 
    def read
      return if @read
      warn "reading "+file
      #self.contents = File.read(file)
      self.contents = ERB.new(File.read(file)).result(binding)
      t = program.parse_str(self.contents)
      self.tree, self.raw_tree = t
      self.time = Time.now
      @read = true

      self.tree
    end
    def outname
      basename + '.cpp'
    end
    def scan
      return if @scanned
      warn "scanning "+file
      tree && tree.scan(program)
      @scanned = true
    end
    def basename
      File.basename(file, '.lpp')
    end
    def write
      if gen_header
        warn "writing "+(n= basename+'.hpp')
        open(n, 'w+') do |f|
          f.puts "#pragma once"
          # f.puts "#ifndef __HEADER_#{basename}"
          # f.puts "#define __HEADER_#{basename}"
          f.puts to_hpp(RenderState.new(program: program, gen_header: gen_header))
          # f.puts "#endif"
        end
      end
      warn 'writing '+outname
      open(outname, 'w+') do |f| 
        if gen_header
          f.puts "#include \"#{basename+'.hpp'}\""
        end
        f.puts to_cpp(RenderState.new(program: program, gen_header: gen_header))
      end
    end
    def to_cpp(*args)
      @cpp ||= (
        if gen_header ##filter out top-level declarations so they only appear in the header
          tree.to_cpp(*args) { |n| !(n.is_a?(VarDeclInitializer) || n.is_a?(VarDeclSimple)) }
        else
          tree.to_cpp(*args) 
        end
      )
    end
    def to_hpp(*args); @hpp ||= tree.to_hpp(*args) end
  end

  class Program
    attr_reader :raw_tree, :tree,:files, :parser, :build_dir
    def initialize opts={}
      opts = {dir: Dir.pwd}.merge opts
      @working_dir = opts[:dir]
      @parser = Parser.new
      @pkgs = Set.new
      @handlers, @settings, @files, @scheduled = {}, {}, {}, {}
      @build_dir = 'build'
      @handlers[ImportStmt] = proc { |stmt|
        warn "** Package added: #{stmt.p.name}" unless @pkgs.include?(stmt.p)
        @pkgs.add stmt.p
      }
      @handlers[IncludeStmt] = proc { |inc|
        if inc.type == :local 
          if inc.file =~ /\.lpp/
            warn "** lpp included #{inc.file}"
            u = add_file inc.file.to_s
            inc.file = u.outname
          elsif inc.file =~ /\.lhh/
            warn "** lpp will generate headers: #{inc.file}"
            u = add_file File.basename(inc.file.to_s,'.lhh')+'.lpp', gen_header: true, build: true
            inc.file = u.basename+'.hpp'
          end
        end
      }
    end

    def set_cpp0x; @settings['c++0x'] ||= (warn "** C++0x enabled"; true) end
    def cpp0x?; @settings['c++0x'] end

    def build_dir= val
      @build_dir = File.expand_path val
    end

    def add_file file, opts={}
      unless f =(@files[file] || @scheduled[file])
        @scheduled[file] = Unit.new(self, file, opts) 
      else; f
      end
    end

    def parse
      parse!
    end
    def rule r
      parser.send(r.to_s.downcase)
    end

    private
    def parse!
      @files.each do |f, u|
        if u.read
          u.scan
        end
      end
      if @scheduled.size > 0
        warn "#{@scheduled.size} scheduled files: "+ @scheduled.keys.join(', ')
        @files = @files.merge(@scheduled)
        @scheduled = {}
        parse!
      end
    end

    public
    def clear; @files = {} end

    def parse_str str
      raw_tree = @parser.parse_with_debug(str),#, reporter: Parslet::ErrorReporter::Deepest.new)
      [Transform.apply(raw_tree), raw_tree]
    rescue Parslet::ParseFailed => error
      puts error.cause.ascii_tree
    end

    def print_trees
      @files.each do |f, u|
        ap [f, u.raw_tree]
      end
    end

    def notify msg
      @handlers[msg.class] && @handlers[msg.class].(msg)
    end

    def [](key); @tree[key] end 

    def to_cpp; 
      @files.map { |f, u|
        #puts "#{f}:\n", u.to_cpp(RenderState.new program: self)
        "#{f}:\n#{u.to_cpp(RenderState.new program: self)}"
      }.join("\n")
      #@tree.to_cpp(RenderState.new(program: self)) end
    end
    
    def write build = false
      return if @files.any? {|f, u| u.tree.nil?}
      set_cpp0x if @pkgs.any? { |p| p.cpp0x? }
      FileUtils.mkdir_p build_dir
      workingdir = Dir.pwd
      Dir.chdir build_dir do
        warn 'Writing...'
        @files.each_value {|u| u.write }
        open(buildscript = "build.#{outname = @files[@files.keys.first].basename}.sh", 'w+') do |f|
          f.puts buildscripts
        end

        if system 'chmod +x '<< buildscript
          if build
            warn 'Building...'
            if system './' << buildscript
              FileUtils.mv outname, File.join(workingdir, outname) unless workingdir == build_dir
            end
          end
        end

        unless $?.success?
          puts ':-(' 
        else
          puts 'Great success!'
        end
      end
    end
    def buildscripts
      #objs = @files.map{|k,v| v.basename + '.o' }
      cpps = @files.map{|k,v| v.outname }
      linker_opts = @pkgs.map { |p| 
        p.linker or nil 
      }.compact.join' '
      compile_opts = @pkgs.map { |p| p.compile_opts }.compact.join(' ')
      compile_opts += ' -std=gnu++0x' if cpp0x?
      ( warn "debugger enabled (-g)"
        compile_opts.prepend '-g '
      ) if @files.values.any?(&:debugger_required?)

      # "#!/bin/sh \n" +
      # cpps.map{|f|"g++ -c #{f} #{compile_opts} &&\n"}.join +
      # "g++ #{objs.join ' '} -o #{@files.first[1].basename} #{linker_opts} \n"
      "#!/bin/sh\n" \
      "g++ -o #{@files.first[1].basename} #{compile_opts} #{cpps.join' '}  #{linker_opts}\n"
    end
  end
end