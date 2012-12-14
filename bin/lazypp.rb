#!/usr/bin/env ruby 
require'parslet'
require'pry'
require'open-uri'
require'yaml'
require'ap' ##awesome_print
require'set'
require'fileutils'
require'trollop'
require_relative'../lib/lazypp/common'
require_relative'../lib/lazypp/parser'
require_relative'../lib/lazypp/nodes'

begin
  require'rb-inotify'
rescue LoadError
  puts 'Recommended installing the `rb-inotify\' gems for directory watching abilities enhancements.'
end
$inotify = Object.const_defined? :INotify
require'find'


class Object; def p(o=nil) Kernel.p o||self end end
begin
  $:.unshift '~/projects/cparser/lib'
  require'cparser'
  CTransform = Parslet::Transform.new do
    rule(identifier: simple(:i)) {i.to_s.intern}
    rule(keyword: simple(:i)) {i.to_s.intern}
    rule(decimal: simple(:d)) {d.to_i}
  end
  CP = Class.new(CParser::Parser).new
  def CP.clean() @text = [] end
  def CP.parse(str) ##trap the text parsed for later :>
    super(@text << str)
  end
  CP.clean

  def cp(str) CTransform.apply CP.parse str end
rescue LoadError => e
  puts e
  def cp(str) :cparser_not_found_have_a_symbol_instead end
end

module LazyPP
  class Package
    PackageDir = File.expand_path('~/.config/cpptranny/pkg')
    @packages = {}
    attr_reader :includes, :linker, :name
    def self.new name
      name = name.map(&:downcase).join(?/)
      @packages[name] ||= super(name)
    end

    def initialize name
      if File.exists?(f = File.join(PackageDir, name)<<'.yml')
        dat = YAML.load_file f
        @includes = [*dat['include']]
        @linker   = dat['linker']
        @name = name
      else
        raise Errno::ENOENT.new f
      end
    end

    alias to_s name
  end
  class Program
    attr_reader :raw_tree, :tree,:files
    def initialize opts={}
      opts = {dir: Dir.pwd}.merge opts
      @working_dir = opts[:dir]
      puts @working_dir
      @p = Parser.new
      @pkgs = Set.new
      @handlers, @settings, @files = {}, {}, {}
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
      @scheduled = {}
    end

    def set_cpp0x; @settings['c++0x'] ||= (warn "** C++0x enabled"; true) end

    Unit = Class.new(Struct.new(:program, :file, :contents, :tree, :raw_tree, :time)) do
      attr_accessor :cpp
      def initialize prog, file, opts={}
        super prog, file, nil, nil, nil, nil
        @read,@scanned = false,false
        @opts={gen_header: false, build: true}.merge opts
      end
      def gen_header; @opts[:gen_header] end 
      def gen_header= val; @opts[:gen_header]= val end
      def build?; @opts[:build] end 
      def read
        return if @read
        warn "reading "+file
        self.contents = File.read(file)
        if t = program.parse_str(self.contents)
          self.tree = t[0]
          self.raw_tree = t[1]
        else
          self.tree, self.raw_tree=nil, nil
        end
        self.time = Time.now
        @read = true
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
          warn "writing "+basename+'.hpp'
          open(basename+'.hpp', 'w+') do |f|
            f.puts to_hpp(RenderState.new(program: program, gen_header: gen_header))
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
      def to_cpp(*args); @cpp ||= tree.to_cpp(*args) end
      def to_hpp(*args); @hpp ||= tree.to_hpp(*args) end
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

    private
    def parse!
      @files.each do |f, u|
        u.read
        u.scan
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
      [Transform.apply(raw_tree = @p.parse(str)), raw_tree]
    rescue Parslet::ParseFailed => _
      puts _.cause.ascii_tree
    end

    def print_trees
      @files.each do |f, u|
        ap [f, u.raw_tree]
      end
    end

    def notify msg
      @handlers[msg.class] ?
        @handlers[msg.class].(msg) :
        nil
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
      FileUtils.mkdir_p 'build'
      Dir.chdir 'build' do
        #binding.pry
        # open(@outname, 'w+') do |f| 
        #   f.puts to_cpp
        # end
        @files.each_value {|u| u.write }
        open(buildscript = "build.#{@files[@files.keys.first].basename}.sh", 'w+') do |f|
          f.puts buildscripts
        end

        if system 'chmod +x '<< buildscript
          system './' << buildscript if build
        end

        unless $?.success?
          puts ':-(' 
        else
          puts 'Great success!'
        end
      end
    end
    def buildscripts
      objs = @files.map{|k,v| v.basename + '.o' }
      cpps = @files.map{|k,v| v.outname }
      linker_opts = @pkgs.map { |p| 
        p.linker or nil 
      }.compact.join' '
      compile_opts = @settings['c++0x'] ? '-std=gnu++0x' : nil

      "#!/bin/sh \n" +
      cpps.map{|f|"g++ -c #{f} #{compile_opts} &&\n"}.join +
      #{}"g++ -c #{@outname} #{'-std=gnu++0x' if @settings['c++0x']} &&\n" \
      "g++ #{objs.join ' '} -o #{@files.first[1].basename} #{linker_opts} \n"
      #{}"g++ #{@basefn}.o -o #{@basefn} #{linker_opts} \n"
    end
  end
end

def check s
  open \
    'http://cdecl.org/query.php?q=' << URI.encode(s),
    &:read
end

opts = Trollop.options {
  banner <<-EOS
Usage: 
       lazypp.rb -f somefile.lpp

       Packages will be searched in #{LazyPP::Package::PackageDir}
  EOS
  opt :f, "file", type: :string
  opt :p, "print result", default: false
  opt :t, "print intermediate tree", default: false
  opt :w, "write", default: false
  opt :b, "build", default: false
  opt :r, "run", default: false
  opt :P, "start a pry session", default: false
  opt :S, "parse a string", type: :string
  opt :H, "force build headers", default: false
  opt(:W, "watch", default: false) if $inotify
}
opts[:W] ||= false

p = LazyPP::Program.new
res = nil

unless opts[:S].nil?
  res = p.parse_str(opts[:S])
  if res
    if opts[:p]; puts p.to_cpp; end
  end
else
  if not opts[:W]
    Trollop.die :f, "File argument (-f) is required" unless opts[:f] 
    Trollop.die :f, "File does not exist" unless File.exist?(opts[:f]) 
  end
end

if not File.exist?(LazyPP::Package::PackageDir) ||
  (realdir = File.realdirpath(LazyPP::Package::PackageDir)) &&
  !File.directory?(realdir)

  binding.pry
end

if opts[:W] #TODO something with this
  i = INotify::Notifier.new
  files = []
  Find.find(Dir.pwd) { |path|
    f = File.basename path
    if FileTest.directory? path
      if f == 'build'
        Find.prune
      end
    elsif f =~ /\.lpp/
      files << f
      i.watch path, :create, :delete, :moved_to, :modify {|event|}
    end
  }
  ( puts "Watching #{files.size} files (#{files.join', '})"
    i.run
  ) unless files.empty?
elsif opts[:f]
  p.add_file opts[:f]
  p.parse

  if opts[:H]
    p.files.each do |f,u| u.gen_header = true end
  end

  p.print_trees if opts[:t]
  puts p.to_cpp if opts[:p]

  p.write(opts[:b]) if opts[:w] || opts[:b]
end

binding.pry if opts[:P]