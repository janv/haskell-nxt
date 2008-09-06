
File.open '/dev/tty.NXT-DevB-1', 'a+' do |f|
  # f << "\x00\x00\x80\x03\x03\xE8\x03\xE8"
  f << "\x06\x00\x00\x03\xf4\x01\xf4\x01"
  f.flush
  
  puts f.getc
end