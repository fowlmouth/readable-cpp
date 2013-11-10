
class Object
  def margs(m, *args)
    proc do |inp|
      self.send m, inp, *args
    end
  end
end
module LazyPP
  class Package
    PackageDir = [
      File.expand_path('~/.config/cpptranny/pkg'),
      Dir.pwd]
    @packages = {}
    attr_reader :includes, :linker, :name, :compile_opts
    def self.new name
      name = Array.wrap(name).map(&:downcase).join(?/)
      @packages[name] ||= super(name)
    end

    def initialize name
      if f = find_pkg( name)
        @name = name
        dat = YAML.load_file f
        @includes = dat['include'].nil? ? nil : Array.wrap(dat['include'])
        @linker   = parse_sh(dat['linker'] || '')
        @compile_opts = parse_sh(dat['compile_opts'] || '')
        @flags = Array.wrap(dat['flags'] || nil).compact
      else
        abort "Could not find package #{name}. Searched in #{PackageDir.join', '}"
      end
    end

    def parse_sh txt
      if txt.respond_to? :map then txt.map { |t| parse_sh t }
      elsif txt =~ /^`(.+)`/ then 
        warn "Executing sh commands from package #{name}: `#$1`"
        %x{#$1}.chomp##.gsub(/-l\\?;(?<foo>[^\\;]+)\\?;/, '-l\k<foo>') ##<- temporary fix for pkg-config returning -L;/path/; 
      else txt end
    end

    def cpp0x?() @flags.include?('c++0x') || @flags.include?('cpp0x') end

    def find_pkg name
      n = name + '.yml'
      PackageDir.map(&File.margs(:join, n)).
      #PackageDir.map{ |d| File.join(d, n) }.
        detect(&File.method(:exists?)) 
    end

    alias to_s name
  end

end