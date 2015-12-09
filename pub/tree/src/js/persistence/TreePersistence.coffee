dedup = {}  # XX wrong layer
if urb.send
  urb.appl = 'hood'
  urb.send.mark = 'write-tree'

module.exports =
  put: (sup,mime,cb)-> urb.send {sup,mime},cb
    
  get: (path,query="no-query",cb) ->
    url = "#{window.tree.basepath(path)}.json?q=#{@encode query}"
    return if dedup[url]
    dedup[url] = true
    $.get url, {}, (data) -> if cb then cb null,data
  encode: (obj)->
    delim = (n)-> Array(n+1).join('_') || '.'
    _encode = (obj)->
      if typeof obj isnt 'object'
        return [0,obj]
      dep = 0
      sub = for k,v of obj
        [_dep,res] = _encode v
        dep = _dep if _dep > dep
        k+(delim _dep)+res if res?
      dep++
      [dep, sub.join delim dep]
    (_encode obj)[1]
