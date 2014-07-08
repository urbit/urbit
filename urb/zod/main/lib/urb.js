window.urb = {
  ship: ship,
  port: port,
  auto: auto,
  oryx: oryx,
  user: user,
  appl: appl,
  seqn: 0,
  seqp: 1,
  seqs: 2,
  dely: 0,

  req: function(method,url,data,json,cb) {
    var xhr = new XMLHttpRequest()
    xhr.open(method.toUpperCase(), url)
    if(json)
      xhr.setRequestHeader("content-type", "text/json")
    if(data)
      xhr.send(JSON.stringify({oryx: oryx, xyro: data}))
    else
      xhr.send({oryx: oryx})
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

  subscribe: function(path,cb) {
    if(!cb) 
      throw new Error("You must supply a callback to urb.subscribe.")

    var method, perm, url, $this

    method = "put"
    perm = "tis"
    url = [perm,this.user,this.appl,this.port]
    if(path) {
      url.push(this.seqs)
      url.push(path)
    }
    console.log(url)
    url = "/"+url.join("/")


    $this = this
    this.req(method,url,{},true,function(err,data) {
      cb.apply(this,arguments)
      if(!err) { $this.poll($this.seqs++,cb); }
    })
  },

  send: function(data,cb) {
    if(!data) { data = {}; }
    if(!cb) { cb = function() {}; }

    var method, perm, url, $this

    method = "put"
    perm = "tim"
    url = [perm,this.user,this.appl,this.port,this.seqn]
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
    url = [perm,this.user,this.appl,this.port,stream,this.seqp]
    url = "/"+url.join("/")
    console.log(url)

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
