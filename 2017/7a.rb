require 'tree' # gem install rubytree

input = %[pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)].split("\n")

programs = input.map do |line|
  prog_name, prog_weight, prog_children = line.split(%r{ \(|\) \-\>})

  {
    name: prog_name,
    weight: prog_weight.to_i,
    children: prog_children&.split(',')&.map(&:strip) || []
  }
end

puts "programs:"
puts programs.join("\n")

root_node = Tree::TreeNode.new("ROOT", "Root Content")
