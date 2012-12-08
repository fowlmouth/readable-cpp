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
    PackageDir = File.expand_path(File.join __FILE__, '..', '..', 'pkg')
    @packages = {}
    attr_reader :includes, :linker
    def self.new name
      name = name.map(&:downcase).join(?/)
      @packages[name] ||= super(name)
    end

    def initialize name
      if File.exists?(f = File.join(PackageDir, name)<<'.yml')
        dat = YAML.load_file f
        @includes = [*dat['include']]
        @linker   = dat['linker']
      else
        raise Errno::ENOENT.new f
      end
    end
  end
  class Program
    attr_reader :file, :tree
    def initialize file
      @p = Parser.new
      @pkgs = Set.new
      @fn = file
      @basefn = ::File.basename(file, '.lpp')
      @outname = @basefn + '.cpp'
      parse
    end
    def parse
      @file = Transform.apply(@tree = @p.parse(File.read(@fn)))
      @file.each { |s| 
        if s.is_a? ImportStmt
          @pkgs.add s.p
        end
      }
    rescue Parslet::ParseFailed => _
      puts _.cause.ascii_tree
    end

    def [](key); @file[key] end 

    def to_cpp; @file.to_cpp end
    def write build = false
      open(@outname, 'w+') do |f| 
        f.puts to_cpp
      end
      open(buildscript = "build.#{@basefn}.sh", 'w+') do |f|
        f.puts buildscripts
      end
      system 'chmod +x '<< buildscript
      system './'<<buildscript if build
    end
    def buildscripts
      linker_opts = @pkgs.map { |p| p.linker or nil }.compact.join' '
      "#!/bin/sh \n" \
      "g++ -c #{@outname} &&\n" \
      "g++ #{@basefn}.o -o #{@basefn} #{linker_opts} \n"
    end
  end
end

def check s
  open \
    'http://cdecl.org/query.php?q=' << URI.encode(s),
    &:read
end

opts = Trollop::options {
  banner <<-EOS
Usage: 
       lazypp.rb -f somefile.lpp
  EOS
  opt :f, "file", type: :string
  opt :p, "print result", default: false
  opt :t, "print intermediate tree", default: false
  opt :w, "write", default: false
  opt :b, "build", default: false
}
Trollop.die :f, "File argument (-f) is required" unless opts[:f]
Trollop.die :f, "File does not exist" unless File.exist?(opts[:f])

p = LazyPP::Program.new opts[:f]

puts p.to_cpp,"\n\n" if opts[:p]
ap p.tree if opts[:t]

p.write(opts[:b]) if opts[:w] || opts[:b]

#binding.pry