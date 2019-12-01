tests = [
  'aa bb cc dd ee', # valid
  'aa bb cc dd aa', # not valid
  'aa bb cc dd aaa' # valid
]

tests = File.read('4_input.txt').split("\n")

def passphrase_valid?(passphrase)
  arr = passphrase.split(' ').map { |word| word.split('').sort.join('') }
  arr == arr.uniq
end

count = 0
tests.each do |x|
  puts "#{x} => #{passphrase_valid?(x)}"
  count += 1 if passphrase_valid?(x)
end

puts "Total valid: #{count}"
