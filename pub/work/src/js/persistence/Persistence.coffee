urb.appl = 'work'
module.exports =
  put: (update,cb) -> urb.send(update,{mark:'work-command'},cb)
  get: (id,cb) ->
    url = (urb.util.basepath "/pub/work/")+id+".json"
    $.get url, {}, (data) -> if cb then cb null,data
