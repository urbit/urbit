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
    defs.fetch($1)
  end
end

def parse_derivation_list(filename)
  defs = {}
  all_paths = Set.new
  all_attrs = {}
  File.foreach(filename).with_index do |line, line_index|
    line.strip!

    # Handle empty lines and comments.
    next if line.empty? || line.start_with?('#')

    # Handle variable definitions (e.g. "define windows = win32,win64").
    if line.start_with?('define')
      md = line.match(/^define\s+([\w-]+)\s*=\s*(.*)$/)
      if !md
        raise AnticipatedError, "Invalid definition syntax."
      end
      name, value = md[1], md[2]
      defs[name] = value
      next
    end

    # Expand variable definitions (e.g. $windows expands to "win32,win64").
    line = substitute_definitions(defs, line)

    # Figure out which parts of the line are attribute paths with brackets and
    # which are attributes.
    items = line.split(' ')
    attr_defs, path_items = items.partition { |p| p.include?('=') }

    # Expand any brackets in the attribute paths to get the complete list of
    # paths specified on this line.
    paths = path_items.flat_map { |p| expand_brackets(p) }

    # Process attribute definitions on the line, like "priority=1".
    attrs = {}
    attr_defs.each do |attr_def|
      md = attr_def.match(/^(\w+)=(\d+)$/)
      if !md
        raise AnticipatedError, "Invalid attribute definition: #{attr_def.inspect}."
      end
      name, value = md[1], md[2]
      case name
      when 'priority'
        attrs[:priority] = value.to_i
      else
        raise AnticipatedError, "Unrecognized attribute: #{name.inspect}."
      end
    end

    # Record the paths for this line and the attributes for those paths,
    # overriding previous attributes values if necessary.
    all_paths += paths
    if !attrs.empty?
      paths.each do |path|
        (all_attrs[path] ||= {}).merge!(attrs)
      end
    end
  rescue AnticipatedError => e
    raise AnticipatedError, "#{filename}:#{line_index + 1}: error: #{e}"
  end

  if all_paths.empty?
    raise AnticipatedError, "#{filename} specifies no paths"
  end

  all_paths.each do |path|
    if !path.match?(/^[\w.-]+$/)
      raise "Invalid characters in path name: #{path}"
    end
  end

  { defs: defs, paths: all_paths.to_a, attrs: all_attrs }
end

def instantiate_drvs(paths)
  cmd = 'nix-instantiate ' + paths.map { |p| "-A #{p}" }.join(' ')
  stdout_str, stderr_str, status = Open3.capture3(cmd)
  if !status.success?
    $stderr.puts stderr_str
    raise AnticipatedError, "Failed to instantiate derivations."
  end
  paths.zip(stdout_str.split.map(&:to_sym)).to_h
end

# We want there to be a one-to-one mapping between paths in the derivations.txt
# list and derivations, so we can make a graph of dependencies of the
# derivations and each derivation in the graph will have a unique path in the
# derivations.txt list.
def check_paths_are_unique!(path_drv_map)
  set = Set.new
  path_drv_map.each do |key, drv|
    if set.include?(drv)
      raise AnticipatedError, "The derivation #{key} is the same as " \
        "other derivations in the list.  Maybe use the 'omni' namespace."
    end
    set << drv
  end
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
    drv1 = drv1.to_sym
    drv2 = drv2.to_sym
    (map[drv1] ||= []) << drv2
    map[drv2] ||= []
  end
  map
end

def graph_restrict_nodes(graph, allowed_nodes)
  graph = restricted_transitive_closure(graph, Set.new(allowed_nodes))
  transitive_reduction(graph)
end

def graph_unmap(graph, map)
  rmap = {}
  map.each do |k, v|
    raise "Mapping is not one-to-one: multiple items map to #{v}" if rmap.key?(v)
    rmap[v] = k
  end
  gu = {}
  graph.each do |parent, children|
    gu[rmap.fetch(parent)] = children.map do |child|
      rmap.fetch(child)
    end
  end
  gu
end

def print_drv_stats(built_map)
  built_count = built_map.count { |drv, built| built }
  puts "Derivations built: #{built_count} out of #{built_map.size}"
end

def output_graphviz(path_graph, path_drv_map, drv_build_map)
  subgraph_names = Set.new
  path_graph.each_key do |path|
    path_components = path.split('.')
    name = path_components.first
    subgraph_names << name if name
  end

  File.open('paths.gv', 'w') do |f|
    f.puts "digraph {"
    subgraph_names.sort.each do |subgraph_name|
      f.puts "subgraph \"cluster_#{subgraph_name}\" {"
      f.puts "label=\"#{subgraph_name}\";"
      path_graph.each do |path, deps|
        next if !path.start_with?("#{subgraph_name}.")
        deps.each do |dep|
          f.puts "\"#{path}\" -> \"#{dep}\""
        end
      end
      f.puts "}"
    end
    f.puts "}"
  end
end

begin
  check_directory!
  settings = parse_derivation_list('test/derivations.txt')
  path_drv_map = instantiate_drvs(settings.fetch(:paths))
  check_paths_are_unique!(path_drv_map)
  drvs = path_drv_map.values.uniq
  drv_built_map = get_build_status(drvs)
  global_drv_graph = get_drv_graph
  drv_graph = graph_restrict_nodes(global_drv_graph, drvs)
  path_graph = graph_unmap(drv_graph, path_drv_map)
  output_graphviz(path_graph, path_drv_map, drv_built_map)
  # print_graph(path_graph)
  print_drv_stats(drv_built_map)
rescue AnticipatedError => e
  puts e
end
