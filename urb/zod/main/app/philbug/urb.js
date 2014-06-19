window.urb = {
  ship: ship,
  port: port,
  auto: auto,
  oryx: oryx,
  user: user,
  appn: appn,
  seqn: 0,
  seqp: 1,
  dely: 0,

  req: function(method,url,data,json,cb) {
    var xhr = new XMLHttpRequest()
    xhr.open(method.toUpperCase(), url)
    if(json)
      xhr.setRequestHeader("content-type", "text/json")
    if(data)
      xhr.send(JSON.stringify(data))
    else
      xhr.send()
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

  subscribe: function(stream,path,cb) {
    if(!cb) 
      throw new Error("You must supply a callback to urb.subscribe.")

    var method, perm, url, $this

    method = "post"
    perm = "pis"
    url = [this.ship,perm,this.user,this.appn,this.port]
    if(stream) {
      url.push(stream)
      if(path)
        url.push(path)
    }
    url = "/"+url.join("/")


    $this = this
    this.req(method,url,{},true,function(err,data) {
      cb.apply(this,arguments)
      if(!err) { $this.poll(stream,cb); }
    })
  },

  send: function(data,cb) {
    if(!data) { data = {}; }
    if(!cb) { cb = function() {}; }

    var method, perm, url, $this

    method = "post"
    perm = "pim"
    url = [this.ship,perm,this.user,this.appn,this.port,this.seqn]
    url = "/"+url.join("/")

    this.seqn++

    $this = this
    this.req(method,url,data,true,function(err,data) {
      if(err) { $this.seqn--; }
      cb.apply(this,arguments)
    })
  },

  poll: function(stream,cb) {
    if(!stream)
      throw new Error("You must supply a stream to urb.poll.")
    if(!cb) 
      throw new Error("You must supply a callback to urb.poll.")

    var method, perm, url, $this

    method = "get"
    perm = "gie"
    if(!stream) { return false; }
    url = [this.ship,perm,this.user,this.appn,this.port,stream,this.seqp]
    url = "/"+url.join("/")

    $this = this
    this.req(method,url,null,false,function(err,data) {
      if(cb.apply(this,arguments) === false) { return; }
        
      if(err)
        $this.dely += 1000
      else {
        $this.dely = 0
        $this.seqp++
      }

      setTimeout(function() {
        $this.poll(stream,cb)
      },$this.dely)
    })
  }
}
