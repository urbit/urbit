window.urb = {
  ship: ship,
  port: port,
  auto: auto,
  oryx: oryx,
  user: user,
  seqn_h: 0,
  seqn_u: 0,
  seqn_s: 0,
  dely: 0,
  puls: 0,
  perms: {
    pol:"gie",
    sub:"tis",
    uns:"tiu",
    mes:"tim",
    heb:"tih"
  },
  cabs: {},

  req: function(method,url,params,json,cb) {
    var xhr = new XMLHttpRequest()
    xhr.open(method.toUpperCase(), url)
    if(json)
      xhr.setRequestHeader("content-type", "text/json")

    _data = {}
    if(params.data) { _data.data = params.data; }
    if(params.ship) { _data.ship = params.ship; }
    if(params.path) { _data.path = params.path; }
    if(params.appl) { _data.appl = params.appl; }
    __data = {oryx: oryx, xyro: _data}
    xhr.send(JSON.stringify(__data))

    if(cb) {
      xhr.onload = function() {
        cb(null,{
          "status":this.status,
          "data":JSON.parse(this.responseText)
        })
      }
      xhr.onerror = function() {
        cb({
          "status":this.status,
          "data":this.responseText
        })
      }
    }
  },

  send: function(params,cb) {
    if(!params)
      throw new Error("You must supply params to urb.send.")
    if(!params.appl)
      throw new Error("You must specify an appl for urb.send.")
    if(!params.data) { params.data = {}; }

    var method, perm, url, $this

    type = params.type ? params.type : "mes"
    perm = this.perms[type]

    params.ship = params.ship ? params.ship : this.ship

    method = "put"
    url = [perm,this.user,this.port,this.seqn_s]
    url = "/"+url.join("/")

    this.seqn_s++

    $this = this
    this.req(method,url,params,true,function(err,data) {
      if(err) { $this.seqn_s--; }
      if(cb) { cb.apply(this,arguments); }
    })
  },

  subscribe: function(params,cb) {
    if(!cb)
      throw new Error("You must supply a callback to urb.subscribe.")
    if(!params)
      throw new Error("You must supply params to urb.subscribe.")
    if(!params.appl)
      throw new Error("You must specify an appl for urb.subscribe.")
    if(!params.path)
      throw new Error("You must specify a path for urb.subscribe.")
    params.ship = params.ship ? params.ship : this.ship

    var method, perm, url, $this

    params.type = "sub"
    params.incs = function() {
        window.urb.seqn_u++
      }

    this.cabs[params.appl+","+params.path.replace(/[^\x00-\x7F]/g, "")+","+params.ship] = cb

    url = [this.perms["sub"],this.user,this.port]
    url = "/"+url.join("/")
    method = "put"

    $this = this
    this.req(method,url,params,true,function(err,data) {
      if(cb) { cb.call(this,err,{status: data.status, data: data.data.data})}
      if(!err && $this.puls == 0) {
        params.type = "pol"
        $this.poll(params)
      }
    })
  },

  unsubscribe: function(params,cb) {
    if(!params)
      throw new Error("You must supply params to urb.unsubscribe.")
    if(!params.appl)
      throw new Error("You must specify an appl for urb.unsubscribe.")
    if(!params.path)
      throw new Error("You must specify a path for urb.unsubscribe.")
    params.ship = params.ship ? params.ship : this.ship

    method = "put"
    type = "uns"
    url = [this.perms[type],this.user,this.port]
    url = "/"+url.join("/")

    var $this = this
    this.req(method,url,params,true,function(err,data) {
      fn = params.appl+","+params.path.replace(/[^\x00-\x7F]/g, "")+","+params.ship
      $this.cabs[fn]('subscription closed')
    })
  },

  heartbeat: function() {
    this.poll({
      type:"heb",
      ship:this.ship,
      incs:function() {
        window.urb.seqn_h++
      }
    },function() {
      console.log('heartbeat.')
    })
  },

  poll: function(params,cb) {
    if(!params)
      throw new Error("You must supply params to urb.poll.")

    var method, perm, url, $this

    method = "get"
    type = params.type ? params.type : "pol"
    perm = this.perms[type]
    url = [perm,this.user,this.port,this.seqn_u]
    url = "/"+url.join("/")

    this.puls = 1

    $this = this
    this.req(method,url,params,false,function(err,data) {
      console.log(data)
      if(cb) {
        cb.call(this,err,{status: data.status, data: data.data.data})
      } else if (data.data.reload) {
        document.location.reload()
      } else {
        fn = data.data.appl+","+data.data.path.replace(/[^\x00-\x7F]/g, "")
            +","+data.data.ship
        $this.cabs[fn].call(this,err,
          {status: data.status, data: data.data.data})
      }

      if(err)
        $this.dely += 1000
      else {
        $this.dely = 0
        params.incs()
      }

      setTimeout(function() {
        $this.poll(params,cb)
      },$this.dely)
    })
  }
}

auto = false
