window.urb = window.urb || {}
window.urb.appl = window.urb.appl || null

window.urb.req = function(method,url,params,json,cb) {
  var xhr = new XMLHttpRequest()
  method = method.toUpperCase()
  if(method == "PUT" || method == "DELETE")
    xhr.open("POST", url+"?"+method)
  else xhr.open(method, url)
  
  if(json)
    xhr.setRequestHeader("content-type", "text/json")

  if(!window.urb.oryx) throw "No CSRF token" // XX fetch auth.json
  _data = {oryx: window.urb.oryx}
  if(params.xyro) { _data.xyro = params.xyro; }
  if(params.ship) { _data.ship = params.ship; }
  if(params.path) { _data.path = params.path; }
  if(params.appl) { _data.appl = params.appl; }
  if(params.mark) { _data.mark = params.mark; }
  if(params.wire) { _data.wire = params.wire; }
  if(cb) {
    xhr.onload = function() {
      var err,res
      try {
        err = null
        res = {
          status:this.status,
          data: JSON.parse(this.responseText)
        }
        if(res.data.reload)
          res.reload = res.data.reload
      } catch(e) {
        // if(urb.wall !== false) document.write(this.responseText) // XX
        err = {
          message:"Failed to parse JSON",
          raw:this.responseText
        }
        res = null
      }
      finally {
       cb(err,res)
      }
    }
    xhr.onerror = function() {
      cb({
        status:this.status,
        data:this.responseText
      })
    }
  }
  xhr.send(JSON.stringify(_data))
}

// window.urb.getJSON = function(url,cb){ window.urb.reqJSON("GET",url, null, cb)}
// window.urb.reqJSON = function(method, url, data, cb){
//   var xhr = new XMLHttpRequest()
//   xhr.open(method, url)
//   xhr.onload = function(){
//     urb.fetchTag.call(xhr)
//     if(cb) cb(JSON.parse(xhr.responseText))
//   }
//   xhr.send(data === null ? null : JSON.stringify(data))
// }

window.urb.reqq = []
window.urb.qreq = function(method,url,params,json,cb) {
  walk = function() {
    qobj = {}
    qobj.oargs = window.urb.reqq[0]
    qobj.nargs = [].slice.call(qobj.oargs,0,4)
    qobj.nargs.push(function(){
      if(this.oargs[4])
        this.oargs[4].apply(window.urb,arguments)
      window.urb.reqq.shift()
      if(window.urb.reqq.length > 0)
        walk()
    }.bind(qobj))
    window.urb.req.apply(this,qobj.nargs)
  }
  l = window.urb.reqq.length
  window.urb.reqq.push(arguments);
  if(l == 0) { walk() }
}

window.urb.send = function(data,params,cb) { // or send(data, cb)
  if(!params || typeof params === "function")
    {cb = params; params = {}}

  var url, $send
  $send = this.send

  params.data = data
  params.ship = params.ship || this.ship
  params.appl = params.appl || this.appl
  params.mark = params.mark || $send.mark
  // params.seqn = params.seqn || $send.seqn
  params.wire = params.wire || "/"
  params.xyro = (typeof(params.data) === 'undefined') ? null : params.data

  
  if(!params.mark) throw new Error("You must specify a mark for urb.send.")
  if(!params.appl) throw new Error("You must specify an appl for urb.send.")

  url = ["to",params.appl,params.mark]
  url = "/~/"+url.join("/")

  // $send.seqn++

  this.qreq('post',url,params,true,function(err,data) {
    /* if(err) { $send.seqn--; }
    else */ if(data && data.data.fail && urb.wall !== false)
      document.write("<pre>"+JSON.stringify(params.xyro)+"\n"
                            +data.data.mess+"</pre>") // XX
    if(cb) { cb.apply(this,arguments); }
  })
}
// window.urb.send.seqn = 0
window.urb.send.mark = "json"


window.urb.gsig = function(params) {
  var path = params.path
  if(!path) path = ""
  if(path[0] !== "/") path = "/"+path
  return  "~"+params.ship+"/"+
          params.appl+
          path.replace(/[^\x00-\x7F]/g, "")
}

window.urb.puls = false
window.urb.cabs = {}
window.urb.poll = function(params) {
  if(!params) throw new Error("You must supply params to urb.poll.")

  var url, $this

  seqn = this.poll.seqn
  if(params.seqn) seqn = params.seqn()
  
  url = "/~/of/"+this.ixor+"?poll="+seqn

  this.puls = true

  $this = this
  this.req("get",url,params,true,function(err,res) {
    $this.poll.dely = params.dely || $this.poll.dely
    if(res){
      if(res.data.beat)
        return $this.poll(params)
      switch(res.data.type){
          case "news":
        return document.location.reload()  // XX check autoreload
          case "rush":
          case "mean":
        var err2 = err
        if(res.data.type == "mean")
          err2 = res.data.data
        var fn = $this.gsig(res.data.from)
        if($this.cabs[fn])
          $this.cabs[fn].call(this,err2,
            {status: res.status, data: res.data.data.json}) // XX non-json
        break;
          default:
        throw new Error("Lost event %"+res.data.type)
      }
      if(params.incs)
        params.incs()
      else 
        $this.poll.seqn++
      $this.poll.dely = 250
      return $this.poll(params)
    }
    
    else if(err){
      setTimeout(function() {
        $this.poll(params)
      }, $this.poll.dely)
      $this.poll.dely += Math.ceil($this.poll.dely*.2)
    }
    else throw "Neither error nor result on poll"
  })
}
window.urb.poll.seqn = 1
window.urb.poll.dely = 250

window.urb.bind = function(path, params, cb, nicecb){ // or bind(path, cb)
  if(!params || typeof params === "function")
    {cb = params; params = {}}
    
  params.path = path
  if(params.path[0] !== "/") params.path = "/"+params.path
  params.ship = params.ship || this.ship
  params.appl = params.appl || this.appl
  params.mark = params.mark || this.bind.mark
  params.wire = params.wire || params.path

  if(typeof path != "string")
    throw new Error("You must specify a string path for urb.bind.")
  if(!params.appl) throw new Error("You must specify an appl for urb.bind.")
  if(!cb) throw new Error("You must supply a callback to urb.bind.")
  
  var method, perm, url, $this

  if(params.mark !== "json")
    throw new Error("Non-json subscriptions unimplemented.")  //  XX
  url = "/~/is/"+this.gsig(params)+"."+params.mark
  
  params.path = params.wire
  this.cabs[this.gsig(params)] = cb

  $this = this
  this.qreq("put",url,params,true,function(err,res) {
    if(nicecb) { nicecb.apply(this,[err,{status: res.status, data: res.data}])}
    //  XX give raw data
    //
    if(!err && !$this.puls) $this.poll(params)
  })
}
urb.bind.mark = "json"

window.urb.drop = function(path, params, cb){  // or drop(path,cb)
  if(typeof params === "function")
    {cb = params; params = {}}
    
  params.path = path
  if(params.path[0] !== "/") params.path = "/"+params.path
  params.ship = params.ship || this.ship
  params.appl = params.appl || this.appl
  params.wire = params.wire || params.path

  if(typeof path != "string")
    throw new Error("You must specify a string path for urb.drop.")
  if(!params.appl) throw new Error("You must specify an appl for urb.drop.")
  
  url = "/~/is/"+this.gsig(params)+".json"
  method = "delete"
  this.req("delete",url,params,true,function(err,res) {
    if(cb) cb(err,res)
  })
}

window.urb.subscribe = function(params,cb) {   //  legacy interface
  if(!params) throw new Error("You must supply params to urb.subscribe")
  return window.urb.bind(params.path, params, cb, cb)
}

window.urb.unsubscribe = function(params,cb) { //  legacy intreface
  if(!params) throw new Error("You must supply params to urb.unsubscribe.")
  return window.urb.drop(params.path, params, cb)
}

window.urb.util = {
  isURL: function(s) {
     r = new RegExp('^(?!mailto:)(?:(?:http|https|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?:(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[0-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))|localhost)(?::\\d{2,5})?(?:(/|\\?|#)[^\\s]*)?$', 'i');
     return s.length < 2083 && r.test(s);
  },
  numDot: function(n) {
    _n = String(n)
    fun = function(s){
      if(s.length <= 3)
        return s
      return fun(s.slice(0,-3))+"."+s.slice(-3)
    }
    return fun((_n))
  },
  toDate: function (dat){
    var mils = Math.floor((0x10000 * dat.getUTCMilliseconds()) / 1000).toString(16)
    function pad(num, str){
      return ((new Array(num + 1)).join('0') + str).substr(-num,num)
    }
    return  '~' + dat.getUTCFullYear() + 
            '.' + (dat.getUTCMonth() + 1) + 
            '.' + dat.getUTCDate() + 
           '..' + pad(2, dat.getUTCHours()) + 
            '.' + pad(2, dat.getUTCMinutes()) + 
            '.' + pad(2, dat.getUTCSeconds()) + 
           '..' + pad(4, mils)
  },
  basepath: function(spur, pathname){
    spur = spur || ''
    if(spur === '/') spur = ''
    pathname = pathname || window.location.pathname
    if(pathname[0] == '/') pathname = pathname.slice(1)
    pathname = pathname.split("/")
    
    var pref, pred, prec, base = "" 
    while(base += "/"+(pref = pathname.shift()), pathname.length>0){
      if(pref[0] !== '~') break;
      if(pref === "~~") continue;
      base += "/"+(pred = pathname.shift())
      if(/[a-z\-]+/.test(pref.slice(1))){
        base += "/"+(prec = pathname.shift())
        if(prec == null) throw "Bad basepath."
        break;
      }
      if(pref !== "~") throw "Bad basepath /"+pref
      if(pred === "as"){
        base += "/"+(prec = pathname.shift())
        if(prec == null) throw "Bad basepath."        
        continue;
      }
      throw "Bad basepath /~/"+pred
    }
    return base+spur
  }
}
