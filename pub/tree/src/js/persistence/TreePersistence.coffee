module.exports =
  get: (path,query="no-query",cb) ->
    url = "#{window.tree.basepath(path)}.json?q=#{query}"
    $.get url, {}, (data) -> if cb then cb null,data
