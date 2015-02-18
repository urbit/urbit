TreeActions = require '../actions/TreeActions.coffee'

module.exports =
  get: (path,query,cb) ->
    url = "#{window.tree.basepath(path)}.json"
    if query then url += "?#{query}"
    $.get url, {}, (data) -> if cb then cb null,data