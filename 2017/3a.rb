
tests = [
  [1, 0],
  [12, 3],
  [23, 2],
  [1024, 31],
  [361527, 0] # question!
]

def manhattan_distance(x, y)
  x.abs + y.abs
end

# Return vector for which direction to move after point 'n'
def vector(idx)
  # Dir'n:   right     up     left     down
  vectors = [[1, 0], [0, 1], [-1, 0], [0, -1]]
  vectors[idx % 4]
end

def calculate(x)
  loc = [0, 0]

  dir_idx = 0

  # We change direction after line_steps_remaining steps
  line_steps = 1
  line_steps_remaining = 1

  # We track how many straight lines we've move along
  line_count = 1

  1.upto(x - 1).each do |n|
    # puts "n=#{n}; loc=#{loc};"

    if line_steps_remaining == 0
      # we've reached end of the line!
      # reset remaining steps to the length of the line
      line_steps_remaining = line_steps
      line_count += 1
      if line_count % 2 == 0
        line_steps += 1 if dir_idx % 2 == 0
      end
      dir_idx += 1
    end

    line_steps_remaining -= 1

    step = vector(dir_idx)

    loc = [loc[0] + step[0], loc[1] + step[1]]
  end

  manhattan_distance(loc[0], loc[1])
end

tests.each do |test_in, test_out|
  puts "calculate(#{test_in}) => #{calculate(test_in)} (expected: #{test_out})"
end
