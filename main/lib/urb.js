window.urb.seqn_u = 0
window.urb.seqn_h = 0
window.urb.dely = 0
window.urb.puls = 0
window.urb.cabs = {}
if(!window.urb.appl) window.urb.appl = null

window.urb.req = function(method,url,params,json,cb) {
  var xhr = new XMLHttpRequest()
  xhr.open(method.toUpperCase(), url)
  
  if(json)
    xhr.setRequestHeader("content-type", "text/json")

  _data = {oryx: window.urb.oryx}
  if(params.xyro) { _data.xyro = params.xyro; }
  if(params.ship) { _data.ship = params.ship; }
  if(params.path) { _data.path = params.path; }
  if(params.appl) { _data.appl = params.appl; }
  if(params.mark) { _data.mark = params.mark; }
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
  if(path[0] != "/") path = "/"+path
  return  params.ship+"/"+
          params.appl+
          path.replace(/[^\x00-\x7F]/g, "")
}

window.urb.poll = function(params,cb) {
  if(!params)
    throw new Error("You must supply params to urb.poll.")

  var method, perm, url, $this

  seqn = this.seqn_u
  if(params.seqn)
    seqn = params.seqn()
  
  url = "/~/of/"+this.ixor

  this.puls = 1

  $this = this
  this.req(method,url,params,true,function(err,data) {
    if(data){
      if (data.reload) {
         return document.location.reload()
      } else {
        fn = $this.gsig(data.data)
        if($this.cabs[fn]) {
          $this.cabs[fn].call(this,err,
            {status: data.status, data: data.data.data})
        }
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
      $this.poll(params,cb)
    },dely)
  })
}

if (window.urb.auto) {
  var tries = 0
  var cnt = 0
  var param = {
    type:"pol"
  }
  window.urb.poll(param)
}

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

  url = ["to",params.appl,params.mark]
  url = "/~/"+url.join("/")

  this.seqn_s++

  $this = this
  this.qreq('post',url,params,true,function(err,data) {
    if(err) { $this.seqn_s--; }
    if(cb) { cb.apply(this,arguments); }
  })
}

window.urb.subscribe = function(params,cb) {
  if(!cb)
    throw new Error("You must supply a callback to urb.subscribe.")
  if(!params)
    throw new Error("You must supply params to urb.subscribe.")
  if(!params.appl) {
    if(!urb.appl)
      throw new Error("You must specify an appl for urb.subscribe.")
    params.appl = urb.appl
  }
  if(!params.path)
    throw new Error("You must specify a path for urb.subscribe.")
  params.ship = params.ship ? params.ship : this.ship

  var method, perm, url, $this

  params.type = "sub"

  this.cabs[this.gsig(params)] = cb

  url = [this.perms["sub"],this.user,this.port]
  url = "/"+url.join("/")
  method = "put"

  $this = this
  this.qreq(method,url,params,true,function(err,data) {
    if(cb) { cb.apply(this,[err,{status: data.status, data: data.data}])}
    if(!err && $this.puls == 0) {
      params.type = "pol"
      $this.poll(params)
    }
  })
}

window.urb.unsubscribe = function(params,cb) {
  if(!params)
    throw new Error("You must supply params to urb.unsubscribe.")
  if(!params.appl) {
    if(!urb.appl)
      throw new Error("You must specify an appl for urb.unsubscribe.")
    params.appl = urb.appl
  }
  if(!params.path)
    throw new Error("You must specify a path for urb.unsubscribe.")
  params.ship = params.ship ? params.ship : this.ship

  method = "put"
  type = "uns"
  url = [this.perms[type],this.user,this.port]
  url = "/"+url.join("/")

  var $this = this
  this.req(method,url,params,true,function(err,data) {
    cb(err,data)
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
