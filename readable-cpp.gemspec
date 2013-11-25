Gem::Specification.new {|s|
	s.name = 'readable-cpp'
	s.version = '1.0'
	s.date = '2013-11-25'
	s.summary = ''
	s.description = ''
	s.authors = ['fowl']
	s.email = 'phowl.mouth@gmail.com'
	s.files = Dir['lib/*/*.rb']
	s.homepage = ''
	s.license = 'MIT'
	s.executables =['readable-cpp']

	s.add_dependency 'awesome_print', '~> 1.2'
	s.add_dependency 'parslet', '~> 1.5'
	s.add_dependency 'trollop', '~> 2.0'
	s.add_dependency 'pry', '>= 0.9.0'
}
