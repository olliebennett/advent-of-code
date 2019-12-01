# input = %w[0
# 3
# 0
# 1
# -3]

input = File.read('5_input.txt').split("\n")

jumplist = input.map(&:to_i)
jumplist_len = jumplist.length

idx = 0

# puts "jumplist: #{jumplist.join(',')}"

1.upto(100000000).each do |step|
  # puts "step=#{step}; jumplist=[#{jumplist.join(',')}]; idx=#{idx}"

  step_size = jumplist[idx]

  if idx >= 0 && idx < jumplist_len
    if step_size >= 3
      jumplist[idx] = jumplist[idx] - 1
    else
      jumplist[idx] = jumplist[idx] + 1
    end
  else
    puts "We've exited the jumplist after #{step - 1} steps (idx=#{idx})"
    break
  end

  idx += step_size # take the step through jumplist
end
