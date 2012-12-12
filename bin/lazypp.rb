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

class Object; def p() Kernel.p self end end
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
    attr_reader :raw_tree, :tree
    def initialize 
      @p = Parser.new
      @pkgs = Set.new
      @handlers, @settings = {}, {}
      @handlers[AutoDecl] = proc { |i|
        @settings['c++0x'] ||= (warn "** C++0x enabled"; true)
      }
      @handlers[ImportStmt] = proc { |stmt|
        warn "** Package added: #{stmt.p.name}"
        @pkgs.add stmt.p
      }
    end

    def parse_file file
      @basefn = ::File.basename(file, '.lpp')
      @outname = @basefn + '.cpp'
      parse_str File.read(file)
      #clean
      self
    end

    def parse_str str
      @raw_tree = @p.parse(str) 
      @tree = Transform.apply @raw_tree
      [@tree, @raw_tree]
    rescue Parslet::ParseFailed => _
      puts _.cause.ascii_tree
    end

    def notify msg
      @handlers[msg.class] ?
        @handlers[msg.class].(msg) :
        nil
    end

    def [](key); @tree[key] end 

    def to_cpp; @tree.to_cpp(RenderState.new(program: self)) end
    def write build = false
      FileUtils.mkdir_p 'build'
      Dir.chdir 'build' do
        open(@outname, 'w+') do |f| 
          f.puts to_cpp
        end
        open(buildscript = "build.#{@basefn}.sh", 'w+') do |f|
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
      linker_opts = @pkgs.map { |p| 
        p.linker or nil 
      }.compact.join' '
      "#!/bin/sh \n" \
      "g++ -c #{@outname} #{'-std=gnu++0x' if @settings['c++0x']} &&\n" \
      "g++ #{@basefn}.o -o #{@basefn} #{linker_opts} \n"
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
}

p = LazyPP::Program.new
res = nil

unless opts[:S].nil?
  res = p.parse_str(opts[:S])
  if res
    if opts[:p]; puts p.to_cpp; end
  end
else
  Trollop.die :f, "File argument (-f) is required" unless opts[:f] 
  Trollop.die :f, "File does not exist" unless File.exist?(opts[:f]) 
end

if not File.exist?(LazyPP::Package::PackageDir) ||
  (realdir = File.realdirpath(LazyPP::Package::PackageDir)) &&
  !File.directory?(realdir)

  binding.pry
end

if opts[:f]
  res ||= p.parse_file(opts[:f])

  unless res; abort; end
  ap p.tree if opts[:t]
  puts p.to_cpp,"\n\n" if opts[:p]

  p.write(opts[:b]) if opts[:w] || opts[:b]
end

binding.pry if opts[:P]