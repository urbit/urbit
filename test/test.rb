#!/usr/bin/env ruby

require 'open3'
require 'pathname'
require 'set'
require 'sqlite3'  # gem install sqlite3
require_relative 'graph'
require_relative 'expand_brackets'

class AnticipatedError < RuntimeError
end

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

def parse_derivation_list(filename)
  defs = {}
  paths = []
  File.foreach(filename).with_index do |line, line_index|
    line_num = line_index + 1
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
    paths += expand_brackets(line)
  rescue AnticipatedError => e
    raise AnticipatedError, "#{filename}:#{line_num}: error: #{e}"
  end

  paths.each do |path|
    if !path.match?(/^[\w.-]+$/)
      raise "Invalid characters in path name: #{path}"
    end
  end

  { defs: defs, paths: paths }
end

def instantiate_drvs(paths)
  cmd = 'nix-instantiate ' + paths.map { |p| "-A #{p}" }.join(' ')
  stdout_str, stderr_str, status = Open3.capture3(cmd)
  if !status.success?
    $stderr.puts stderr_str
    raise AnticipatedError, "Failed to instantiate derivations."
  end

  paths.zip(stdout_str.split).to_h
end

def nix_db
  return $db if $db
  $db = SQLite3::Database.new '/nix/var/nix/db/db.sqlite', readonly: true
end

def get_build_status(drvs)
  drv_list_str = drvs.map { |d| "\"#{d}\"" }.join(", ")
  query = <<END
select d.path, v.id
from ValidPaths d
left join DerivationOutputs o on d.id == o.drv
left join ValidPaths v on o.path == v.path
where d.path in (#{drv_list_str});
END
  r = {}
  nix_db.execute(query) do |drv, output_id|
    output_built = !output_id.nil?
    r[drv] = r.fetch(drv, true) && output_built
  end
  r
end

# Returns a map that maps every derivation path to a list of derivation paths
# that it refers to, for all derivations in the Nix store.
def get_drv_graph
  map = {}
  query = <<END
select r1.path, r2.path
from Refs r
join ValidPaths r1 on r1.id == referrer
join ValidPaths r2 on r2.id == reference
where r1.path like '%.drv' and r2.path like '%.drv';
END
  nix_db.execute(query) do |drv1, drv2|
    (map[drv1] ||= []) << drv2
    map[drv2] ||= []
  end
  map
end

def graph_restrict_nodes(graph, allowed_nodes)
  check_graph!(graph)
  puts "Making transitive closure"
  graph = transitive_closure(graph)
  puts "Restricting graph"
  graph = restrict_graph(graph, Set.new(allowed_nodes))
  puts "Getting transitive reduction"
  transitive_reduction(graph)
end

def print_drv_stats(built_map)
  built_count = built_map.count { |drv, built| built }
  puts "Derivations built: #{built_count} out of #{built_map.size}"
end

begin
  check_directory!
  settings = parse_derivation_list('test/derivations.txt')
  path_drv_map = instantiate_drvs(settings.fetch(:paths))
  drvs = path_drv_map.values.uniq
  drv_built_map = get_build_status(drvs)
  global_drv_graph = get_drv_graph
  drv_graph = graph_restrict_nodes(global_drv_graph, drvs)
  print_graph(drv_graph)  # tmphax
  print_drv_stats(drv_built_map)
rescue AnticipatedError => e
  puts e
end
