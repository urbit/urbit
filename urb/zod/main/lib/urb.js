window.urb.seqn_s = 0
window.urb.cabs = {}

window.urb.send = function(params,cb) {
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
  this.qreq(method,url,params,true,function(err,data) {
    if(err) { $this.seqn_s--; }
    if(cb) { cb.apply(this,arguments); }
  })
}

window.urb.subscribe = function(params,cb) {
  console.log("Someone is subscribing");
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
    $this.cabs[$this.gsig(params)]('subscription closed')
    delete $this.cabs[$this.gsig(params)]
  })
}
