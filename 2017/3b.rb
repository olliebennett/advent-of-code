@matrix_centre = 10

@matrix = nil

tests = [
  [1, 1],
  [2, 1],
  [3, 2],
  [4, 4],
  [5, 5],
  [10, 26],
  [23, 806]
]

def print_matrix
  puts "Matrix (#{@matrix.length} x #{@matrix[0].length})"
  puts @matrix.map { |row| row.join(' ') }.join("\n")
end

def build_matrix
  @matrix = Array.new(@matrix_centre * 2 - 1) { |_| Array.new(@matrix_centre * 2 - 1) { |_| 0 }}
  # Initial value at [0][0]
  @matrix[@matrix_centre - 1][@matrix_centre - 1] = 1
  nil
end

def set_matrix_value(x, y, val)
  # puts "Setting: matrix[#{x - 1}, #{y - 1}] = #{val}"
  @matrix[@matrix_centre + x - 1][@matrix_centre + y - 1] = val
  nil
end

def get_matrix(x, y)
  @matrix[@matrix_centre + x - 1][@matrix_centre + y - 1]
end

def get_matrix_surrounding_sum(x, y)
  [
    get_matrix(x - 1, y + 1),
    get_matrix(x - 1, y),
    get_matrix(x - 1, y - 1),
    get_matrix(x, y + 1),
    get_matrix(x, y - 1),
    get_matrix(x + 1, y + 1),
    get_matrix(x + 1, y),
    get_matrix(x + 1, y - 1)
  ].reduce(:+)
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

    set_matrix_value(loc[0], loc[1], get_matrix_surrounding_sum(loc[0], loc[1]))

    # print_matrix
  end
  get_matrix(loc[0], loc[1])
end

tests.each do |test_in, test_out|
  build_matrix
  puts "TEST: calculate(#{test_in}) => #{calculate(test_in)} (expected: #{test_out})"
end

1.upto(1000).each do |x|
  build_matrix

  limit = 361527
  res = calculate(x)
  puts "calculate(#{x}) => #{calculate(x)}"
  if res > limit
    puts "Exceeded limit with calculate(#{x})=#{res}!"
    break
  end
end
