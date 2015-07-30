module.exports =
  get: (path,type="body",cb) ->
    if path[0] isnt "/" then path = "/" + path
    url = "#{window.tree.basepath(type+path)}.json"
    $.get url, {}, (data) -> if cb then cb null,data
