# bank = [0, 2, 7, 0]

bank = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]


def find_max_index(arr)
  arr.index(arr.max)
end

def reallocate(arr)
  idx = find_max_index(arr)
  remaining = arr[idx]
  arr[idx] = 0
  remaining.times do
    idx = (idx + 1) % arr.length
    arr[idx] = arr[idx] + 1
  end
  arr
end

puts "Bank (initial): #{bank}"

iter = 0
previous_states = []
until previous_states.include?(bank.join(','))
  previous_states << bank.join(',')
  bank = reallocate(bank)
  iter += 1
end
puts "Match identified after #{iter} iterations"
