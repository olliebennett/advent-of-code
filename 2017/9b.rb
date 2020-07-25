tests = [
  # str, garbage_count
  ['<>', 0],
  ['<random characters>', 17],
  ['<<<<>', 3],
  ['<{!>}>', 2],
  ['<!!>', 0],
  ['<!!!>>', 0],
  ['<{o"i!a,<{i<a>', 10],
  [File.read('9_input.txt'), 0]
]

def delete_garbage(text)
  characters = text.split('')

  non_garbage = []

  garbage = []

  allow_characters = true
  previously_allow_characters = true
  skip_character = false

  characters.each do |c|
    if skip_character == true
      skip_character = false
      next # skip this character (continue to the next one)
    end

    if c == '!'
      skip_character = true
      next
    end

    allow_characters = false if c == '<'

    if allow_characters
      non_garbage << c
    else
      garbage << c unless (c == '>' || (c == '<' && previously_allow_characters == true))
    end

    allow_characters = true if c == '>'

    previously_allow_characters = allow_characters
  end

  return non_garbage.join(''), garbage.length
end

tests.each do |test_text, expected_garbage_length|
  # puts "Test = #{test_text}"
  non_garbage_text, garbage_length = delete_garbage(test_text)
  # puts "non_garbage_text = #{non_garbage_text}"

  puts "#{test_text[0..50]}#{'...' if test_text.length > 50}: garbage_length = #{garbage_length} (vs #{expected_garbage_length})"
end
