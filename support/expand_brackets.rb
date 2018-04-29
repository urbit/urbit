def expand_brackets_core(str, depth)
  finished_parts = []
  active_parts = [+'']
  while true
    if str.empty?
      raise AnticipatedError, "Unmatched opening brace" if depth > 0
      break
    elsif str.start_with?('}')
      str.slice!(0)
      raise AnticipatedError, "Unmatched closing brace" if depth == 0
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
      raise AnticipatedError, "Comma at top level" if depth == 0
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
