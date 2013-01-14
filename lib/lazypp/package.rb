
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
    attr_reader :includes, :linker, :name
    def self.new name
      name = name.map(&:downcase).join(?/)
      @packages[name] ||= super(name)
    end

    def initialize name
      if f = find_pkg( name)
        dat = YAML.load_file f
        @includes = dat['include'].nil? ? nil : Array.wrap(dat['include'])
        @linker   = dat['linker']
        @name     = name
        @flags = Array.wrap(dat['flags'] || nil).compact
      else
        raise Errno::ENOENT.new f
      end
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