#!/usr/bin/env ruby

require 'pathname'

# Don't automatically change directory because maybe people want to test one
# nixcrpkgs repository using the test script from another one.  But do give an
# early, friendly warning if they are running in the wrong directory.
def check_directory!
  return if File.directory?('pretend_stdenv')
  $stderr.puts "You should run this script from the nixcrpkgs directory."
  dir = Pathname(__FILE__).parent.parent
  $stderr.puts "Try running these commands:\n  cd #{dir}\n  test/test.rb"
  exit 1
end

def substitute_definitions(defs, str)
  str.gsub(/\$([\w-]+)/) do |x|
    defs.fetch($1).join(",")
  end
end

def expand_brackets_core(str, depth)
  finished_parts = []
  active_parts = [+'']
  while true
    if str.empty?
      raise "Unmatched opening brace" if depth > 0
      break
    elsif str.start_with?('}')
      str.slice!(0)
      raise "Unmatched closing brace" if depth == 0
      break
    elsif str.start_with?('{')
      # Recurse, which removes everything up to and
      # including the matching closing brace.
      str.slice!(0)
      options = expand_brackets_core(str, depth + 1)
      raise if options.empty?
      active_parts = active_parts.flat_map { |p1|
        options.map { |p2| p1 + p2 }
      }
    elsif str.start_with?(',')
      raise "Comma at top level" if depth == 0
      # Remove the comma, mark the parts we are working
      # on as finished, and start a new part.
      str.slice!(0)
      finished_parts += active_parts
      active_parts = ['']
    else
      part_length = str.index(/[{},]|$/)
      raise if part_length < 1
      part = str.slice!(0, part_length)
      active_parts.each do |s|
        s.insert(-1, part)
      end
    end
  end
  finished_parts + active_parts
end

# Expands something like "{a,b}{,.x}" to ["a", "a.x", "b", "b.x"]
def expand_brackets(str)
  expand_brackets_core(str.dup, 0)
end

def parse_derivation_list(str)
  defs = {}
  drvs = []
  str.each_line.with_index do |line, n|
    line.strip!
    next if line.empty?
    next if line.start_with?('#')

    if line.start_with?('define')
      md = line.match(/^define\s+([\w-]+)\s*=\s*(.*)$/)
      raise "Invalid definition syntax at line #{n}" if !md
      name, value = md[1], md[2]
      list = value.split(',').map(&:strip)
      defs[name] = list
      next
    end

    line = substitute_definitions(defs, line)
    drvs += expand_brackets(line)
  end

  drvs.each do |drv|
    if !drv.match?(/^[\w.-]+$/)
      raise "Invalid characters in derivation name: #{drv}"
    end
  end

  { defs: defs, drvs: drvs }
end

check_directory!

p parse_derivation_list(File.read('test/derivations.txt'))
