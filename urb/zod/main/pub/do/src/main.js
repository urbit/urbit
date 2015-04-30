recl = React.createClass
div = React.DOM.div
a = React.DOM.a
b = React.DOM.button
input = React.DOM.input

Page = recl({
  handleClick: function(){
    if(window.authcode.length !== ''){
      urb.send({
        appl: "do",
        data: window.authcode,
        mark: "oauth2-code"})
    } else { console.log("nocode") }
  },

  sendSecret: function(){
    if($('#appsecret').val()) {
      urb.send({appl: "do",
                data: $('#appsecret').val(),
                mark: "client-secret" 
      })
    }
  },

  getList: function(){
    urb.send({appl: "do",
              data: {action:"list"},
              mark: "json"})
  },

  createDroplet: function(){
    urb.send({appl: "do",
              data: {
                    action:'create',
                    name:$('#name').val(),
                    region:$('#region').val(),
                    size:$('#size').val(),
                    image:$('#image').val(),
                    ssh:[$('#ssh').val()],
                    backups:null,//$('#backups').val(),
                    ipv6:null,//$('#ipv6').val(),
                    priv_networking:null,//$('#priv-networking').val(),
                    user_data:null//$('#user-data').val()
                    },
              mark: "json"})
  },

  render: function(){
    href = "https://cloud.digitalocean.com/v1/oauth/authorize?client_id=d8f46b95af38c1ab3d78ad34c2157a6959c23eb0eb5d8e393f650f08e6a75c6f&redirect_uri=http%3A%2F%2Flocalhost%3A8443%2Fmain%2Fpub%2Fdo%2Ffab&response_type=code&redirect_uri=http://localhost:8443/main/pub/do/fab&scope=read+write"
    return (div({}, [
        div({},
          a({href:href},[
            "get authcode",  
            b({onClick:this.handleClick}, "Send Authcode")
          ])
        ),
        div({}, [
          input({id:"appsecret"}, 
          b({onClick:this.sendSecret}, "Send Secret"))
        ]),
        b({onClick:this.getList}, "Get List"),
        div({}, [
          b({onClick:this.createDroplet}, "Create Droplet"),
          input({id:"name",placeholder:"Name of droplet"}), 
          input({id:"region",placeholder:"Region"}),
          input({id:"size",placeholder:"Size (str ending in mb"}),
          input({id:"image",placeholder:"Image"}),
          input({id:"ssh",placeholder:"ssh keys (optional)"}),
          input({id:"backups",placeholder:"backups (optional)"}),
          input({id:"ipv6",placeholder:"ipv6 (boolean, optional)"}),
          input({id:"user-data",placeholder:" user-data string (optional)"}),
          input({id:"priv-networking",placeholder:"Private Networking (boolean, optional)"})
        ])
      ])
    )
  }
})

mounted = React.render(Page({}), $("#container")[0])
urb.bind("/", function(err,d) {
return}) 
