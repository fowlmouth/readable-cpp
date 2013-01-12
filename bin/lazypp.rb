#!/usr/bin/env ruby 
require'parslet'
require'parslet/convenience'
require'pry'
require'open-uri'
require'yaml'
require'ap' ##awesome_print
require'set'
require'fileutils'
require'trollop'

%w[common parser nodes package program].
each do |f| require_relative '../lib/lazypp/'+f end

begin
  require'rb-inotify'
rescue LoadError
  puts 'Recommended installing the `rb-inotify\' gems for directory watching abilities enhancements.'
end
$inotify = Object.const_defined? :INotify
require'find'


class Object; def p(o=nil) 
  warn "Object#p call left-over at #{caller[0]}" if $VERBOSE || $DEBUG 
  Kernel.p o||self
end end
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


def check s
  open \
    'http://cdecl.org/query.php?q=' << URI.encode(s),
    &:read
end

opts = Trollop.options {
  banner <<-EOS
Usage: 
    lazypp.rb -f somefile.lpp

    Packages will be searched in
#{LazyPP::Package::PackageDir.map { |dir|
  "      #{dir}"
}.join(?\n)}

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
  opt :B, "set build directory", type: :string, default: './build'
  opt :I, "add package dir", type: :strings
  opt(:W, "watch", default: false) if $inotify
}
opts[:W] ||= false

p = LazyPP::Program.new
res = nil

p.build_dir = opts[:B] 
opts[:I].each(&LazyPP::Package::PackageDir.method(:<<)) \
  unless opts[:I].nil?

unless opts[:S].nil?
  puts p.parse_str(opts[:S])[0].to_cpp
else
  if not opts[:W]
    Trollop.die :f, "File argument (-f) is required" unless opts[:P] || opts[:f] 
    opts[:f] += '.lpp' unless opts[:f].nil? || opts[:f] =~ /\.lpp$/
    Trollop.die :f, "File does not exist #{opts[:f]}" if (opts[:f].nil? ? !opts[:P] : !File.exist?(opts[:f]))
  end
end

# if not File.exist?(LazyPP::Package::PackageDir) ||
#   (realdir = File.realdirpath(LazyPP::Package::PackageDir)) &&
#   !File.directory?(realdir)

#   binding.pry
# end

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
      i.watch(path, :create, :delete, :moved_to, :modify) {|event|

      }
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