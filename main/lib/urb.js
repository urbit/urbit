window.urb.seqn_u = 1
window.urb.dely = 0
window.urb.puls = false
window.urb.cabs = {}
if(!window.urb.appl) window.urb.appl = null

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
      try {
        err = null
        res = {
          status:this.status,
          data: JSON.parse(this.responseText)
        }
        if(res.data.reload)
          res.reload = res.data.reload
      } catch(e) {
        if(urb.wall || true) document.write(this.responseText) // XX
        err = {
          message:"Failed to parse JSON",
          raw:this.responseText
        }
        res = null
      }
      cb(err,res)
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

window.urb.reqq = []
window.urb.qreq = function(method,url,params,json,cb) {
  walk = function() {
    qobj = {}
    qobj.oargs = window.urb.reqq[0]
    qobj.nargs = [].slice.apply(qobj.oargs,[0,4])
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

window.urb.gsig = function(params) {
  var path = params.path
  if(!path) path = ""
  if(path[0] !== "/") path = "/"+path
  return  "~"+params.ship+"/"+
          params.appl+
          path.replace(/[^\x00-\x7F]/g, "")
}

window.urb.poll = function(params) {
  if(!params) throw new Error("You must supply params to urb.poll.")

  var url, $this

  seqn = this.seqn_u
  if(params.seqn) seqn = params.seqn()
  
  url = "/~/of/"+this.ixor+"?poll="+seqn

  this.puls = true

  $this = this
  this.req("get",url,params,true,function(err,res) {
    if(res){
      if(res.data.beat)
        return $this.poll(params)
      switch(res.data.type){
          case "news":
        return document.location.reload()  // XX check autoreload
          case "rush":
          case "mean":
        if(res.data.type == "mean")
          err = res.data.data
        var fn = $this.gsig(res.data.from)
        if($this.cabs[fn])
          $this.cabs[fn].call(this,err,
            {status: res.status, data: res.data.data.json}) // XX non-json
        break;
          default:
        throw new Error("Lost event %"+res.data.type)
      }
    }

    dely = params.dely ? params.dely : $this.dely

    if(err)
      dely = dely+Math.ceil(dely*.02)
    else {
      $this.dely = 0
      if(params.incs)
        params.incs()
      else 
        $this.seqn_u++
    }

    setTimeout(function() {
      $this.poll(params)
    },dely)
  })
}

// if (window.urb.auto) {  // need dependencies
//   var tries = 0
//   var cnt = 0
//   var param = {
//     type:"pol"
//   }
//   window.urb.poll(param)
// }

// window.urb.heartbeat = function() {
//   this.poll({
//     type:"heb",
//     ship:this.ship,
//     dely:30000,
//     seqn:function() {
//       return window.urb.seqn_h
//     },
//     incs:function() {
//       window.urb.seqn_h = window.urb.seqn_h+1
//     }
//   },function() {
//     console.log('heartbeat.')
//   })
// }
// window.urb.heartbeat()

//  //  /  //  /  //  //
// end old %eyre code //
//  //  /  //  /  //  //

window.urb.seqn_s = 0

// TODO urb.send(data, [params/params.appl]?, cb?)
window.urb.send = function(params,cb) { 
  if(!params)
    throw new Error("You must supply params to urb.send.")
  if(!params.appl && !this.appl){
    throw new Error("You must specify an appl for urb.send.")
  }

  var url, $this

  params.ship = params.ship ? params.ship : this.ship
  params.appl = params.appl ? params.appl : this.appl
  params.mark = params.mark ? params.mark : "json"
  params.xyro = params.data ? params.data : {}
  params.wire = params.wire ? params.wire : "/"


  url = ["to",params.appl,params.mark]
  url = "/~/"+url.join("/")

  this.seqn_s++

  $this = this
  this.qreq('post',url,params,true,function(err,data) {
    if(err) { $this.seqn_s--; }
    if(cb) { cb.apply(this,arguments); }
  })
}

window.urb.subscribe = function(params,cb) {   //  legacy interface
  if(!params) throw new Error("You must supply params to urb.subscribe")
  return window.urb.bind(params.path, params, cb, cb)
}
window.urb.bind = function(path, cb){ // or bind(path, params, cb, nicecb?)
  var params, nicecb
  if(arguments.length > 2)
    {params = cb; cb = arguments[2], nicecb = arguments[3]}
  else params = {}
    
  params.path = path
  if(params.path[0] !== "/") params.path = "/"+params.path
  params.ship = params.ship ? params.ship : this.ship
  params.appl = params.appl ? params.appl : this.appl
  params.mark = params.mark ? params.mark : "json"
  params.wire = params.wire ? params.wire : params.path

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

window.urb.unsubscribe = function(params,cb) {
  if(!params) throw new Error("You must supply params to urb.unsubscribe.")
  
  params.ship = params.ship ? params.ship : this.ship
  params.appl = params.appl ? params.appl : this.appl
  params.wire = params.wire ? params.wire : params.path

  
  if(!params.path) throw new Error("You must specify a path for urb.unsubscribe.")
  if(!params.appl) throw new Error("You must specify an appl for urb.unsubscribe.")
  if(!cb) throw new Error("You must supply a callback to urb.unsubscribe.")

  
  url = "/~/is/"+this.gsig(params)+"/"
  method = "delete"
  this.req("delete",url,params,true,function(err,res) {
    cb(err,res)
  })
}

window.urb.util = {
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
  }
}
