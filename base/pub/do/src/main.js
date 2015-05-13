recl = React.createClass
div = React.DOM.div
a = React.DOM.a
b = React.DOM.button
hr = React.DOM.hr
table = React.DOM.table
th = React.DOM.th
tr = React.DOM.tr
td = React.DOM.td
input = React.DOM.input

Droplet = React.createClass({
  dropletAction:function(id, action){
    urb.send({
      appl:"do",
      data: {action: action,
            id: id}})
  },

  rebootDroplet: function() {
    urb.send({
      appl: "do",
      data: {action: 'reboot',
            id: this.props.id}})
  },

  render: function() {
    var $this = this    //local var, else it always points at second
    var acts = ["reboot","power_cycle","shutdown","power_off","power_on","password_reset",
                "enable_ipv6","enable_private_networking","snapshot","upgrade"]
    var buttons = [];
    var buttons = acts.map(function(act){
      console.log($this.props.id)
      return b({onClick:function(){
        $this.dropletAction($this.props.id, act)
      }}, act)
    })     
    kay = Object.keys(this.props)
    kay = kay.filter(function(b){return b!="children"}) //  XX individually adress props
  return div({},
    buttons,
    table({},
      tr({},kay.map(function(k){return th({},k)})),
      tr({},kay.map(function(k){return td({},JSON.stringify($this.props[k]))}))),
    hr())
  }
})

Page = recl({
  handleClick: function(){
    if(window.authcode.length !== ''){
      console.log(window.authcode);
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
                    ssh:[] // $('#ssh').val()],
                    backups:null,//$('#backups').val(),
                    ipv6:null,//$('#ipv6').val(),
                    priv_networking:null,//$('#priv-networking').val(),
                    user_data:null//$('#user-data').val()
                    },
              mark: "json"})
  },

  render: function(){
    href = "https://cloud.digitalocean.com/v1/oauth/authorize?client_id=d8f46b95af38c1ab3d78ad34c2157a6959c23eb0eb5d8e393f650f08e6a75c6f&redirect_uri=http%3A%2F%2Flocalhost%3A8443%2Fhome%2Fpub%2Fdo%2Ffab&response_type=code&scope=read+write"
      return (div({}, [
        div({},
          a({href:href},[
            "get authcode"
          ]),
          b({onClick:this.handleClick}, "Send Authcode")
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
        ]),
        div({},
          this.props.droplets.map(Droplet)
        )
      ])
    )
  }
})


mounted = React.render(Page({droplets:[]}), $("#container")[0])
urb.bind("/", function(err,d) {

  mounted.setProps({droplets:d.data})
return}) 
