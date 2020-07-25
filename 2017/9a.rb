 
tests = [
  # str, groups, score
  ['{}', 1, 1],
  ['{{{}}}', 3, 6],
  ['{{},{}}', 3, 5],
  ['{{{},{},{{}}}}', 6, 16],
  ['{<{},{},{{}}>}', 1, 1],
  ['{<a>,<a>,<a>,<a>}', 1, 9],
  ['{{<a>},{<a>},{<a>},{<a>}}', 5, 9],
  ['{{<!>},{<!>},{<!>},{<a>}}', 2, 3],
  [File.read('9_input.txt'), 0, 0]
]

def delete_garbage(text)
  characters = text.split('')

  non_garbage = []

  allow_characters = true
  skip_character = false

  characters.each do |c|
    if skip_character == true
      skip_character = false
      next # skip this character (continue to the next one)
    end

    skip_character = true if c == '!'

    allow_characters = false if c == '<'

    if allow_characters == true
      non_garbage << c
    end

    allow_characters = true if c == '>'
  end

  return non_garbage.join('')
end

def count_groups(text)
  characters = text.split('')

  # count how many '{' characters we have
  return characters.count { |c| c == '{' }
end

def count_score(text)
  score = 0
  running_score = 0
  text.split('').each do |c|
    if c == '{'
      running_score += 1
      score += running_score
    elsif c == '}'
      running_score -= 1
    end
  end
  score
end

tests.each do |test_text, expected_count, expected_score|
  # puts "Test = #{test_text}"
  non_garbage_text = delete_garbage(test_text)
  # puts "non_garbage_text = #{non_garbage_text}"

  groups = count_groups(non_garbage_text)

  score = count_score(non_garbage_text)
  puts "#{test_text[0..10]}...: groups = #{groups} (vs #{expected_count}) / score = #{score} (vs #{expected_score})"
end
