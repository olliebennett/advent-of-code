tests = [
  { in: '1122', out: 3 },
  { in: '1111', out: 4 },
  { in: '1234', out: 0 },
  { in: '91212129', out: 9 }
]

def calculate(input)
  arr = input.split('').map(&:to_i)
  cycle = arr.push(arr[0])
  
  cycle.each_cons(2).map { |c| c[1] if c[0] == c[1] }.compact.sum
end

tests.each do |t|
  res = calculate(t[:in])
  puts "#{t[:in]} => #{res} => #{res == t[:out]}"
end
