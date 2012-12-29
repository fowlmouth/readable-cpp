#!/usr/bin/env ruby

run = ARGV.include? 'run'
run &&= []
failed = []

Dir.glob '*.lpp' do |f|
  if system "lazypp.rb -f #{f} -b"
  	run << "./#{File.basename(f,'.lpp')}" if run
  else
  	failed << f
  end
end
run && run.each { |str| system str }

puts "#{failed.size} failed: #{failed.join', '}"
