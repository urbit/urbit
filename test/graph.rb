def print_graph(graph)
  graph.each do |parent, children|
    puts "#{parent} ->"
    children.each do |child|
      puts "  #{child}"
    end
  end
end

def check_graph!(graph)
  graph.each do |parent, children|
    children.each do |child|
      if !graph.key?(child)
        raise "Graph is missing an entry for #{child}"
      end
    end
  end
end

def depth_first_search_exclude_start(graph, start)
  stack = [graph.fetch(start).to_enum]
  visited = Set.new
  until stack.empty?
    begin
      node = stack.last.next
    rescue StopIteration
      stack.pop
      next
    end
    next if visited.include?(node)
    visited << node
    stack << graph.fetch(node).to_enum
    yield node
  end
end

def restricted_transitive_closure(graph, allowed_nodes)
  tc = {}
  allowed_nodes.each do |node|
    next if !allowed_nodes.include?(node)
    descendents = []
    depth_first_search_exclude_start(graph, node) do |descendent|
      next if !allowed_nodes.include?(descendent)
      descendents << descendent
    end
    tc[node] = descendents
  end
  tc
end

def transitive_reduction(graph)
  tr = {}
  graph.each do |start_node, nodes|
    nodes_with_max_distance_1 = Set.new(nodes)
    distance = 1
    until nodes.empty?
      nodes = Set.new nodes.flat_map &graph.method(:fetch)
      nodes_with_max_distance_1 -= nodes
      distance += 1
      if distance > graph.size
        raise "Cycle detected: this algorithm only works with DAGs."
      end
    end
    tr[start_node] = nodes_with_max_distance_1.to_a
  end
  tr
end
