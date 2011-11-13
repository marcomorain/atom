require 'erb'

template = File.open('atom.cpp', 'rb').read

cpp = ERB.new(template)

type = 'Cell*'
type_name = 'cell'
result = cpp.result()

File.open('atom_build.cpp', 'w') {|f| f.write(result) }
