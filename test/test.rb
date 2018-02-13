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

# Expands something like "{a,b}{,.x}" to ["a", "a.x", "b", "b.x"]
def expand_brackets(str)
  # TODO
  raise
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
