#!/usr/bin/env ruby

Dir.glob '*.lpp' do |f|
  system "lazypp.rb -f #{f} -b -B ."
  system "./#{File.basename(f,'.lpp')}"
end