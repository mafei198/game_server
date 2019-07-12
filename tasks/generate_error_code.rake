## The MIT License (MIT)
##
## Copyright (c) 2014-2024
## Savin Max <mafei.198@gmail.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.


desc "Generate Error Code"

task :generate_error_code => :environment do
  search_dirs = []
  search_dirs << File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/controllers")
  search_dirs << File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/helpers")
  search_dirs << File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/models")
  search_dirs << File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/models/lottery")
  search_dirs << File.expand_path("#{FRAMEWORK_ROOT_DIR}/game_server/src")

  error_code_file = File.expand_path("#{FRAMEWORK_ROOT_DIR}/app/config_data/gameconfig/error_code.xlsx")
  s = Roo::Excelx.new(error_code_file)

  old_error_atoms = []
  s.sheets.each do |sheet|
    s.default_sheet = sheet
    4.upto(s.last_row).each do |row| 
      atom = s.row(row)[0]
      if atom.present? then
        old_error_atoms << atom
      end
    end
  end

  new_error_atoms = []
  search_dirs.each do |dir|
    Dir.foreach(dir) do |file_path|
      if File.extname(file_path) != '.erl' 
        next
      end
      File.open(dir + "/" + file_path, "r") do |io|
        atoms = io.read.scan(/{fail, error.*\w}/)
        new_error_atoms += atoms
      end
    end
  end
  
  new_error_atoms = new_error_atoms.map{|atom| atom[7..-2] }
  new_error_atoms.uniq!.sort!

  new_added = []
  new_error_atoms.each do |new_error_atom|
    if not old_error_atoms.include?(new_error_atom)
      new_added << new_error_atom 
    end
  end

  puts "===============================New Added Error Code==============================="
  puts new_added
end
