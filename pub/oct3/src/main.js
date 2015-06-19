$(function() {
  $bord = $('#bord')
  $audi = $('#audi')
  $whom = $('#whom')

  lett = ["x","o"]
  symb = [" ","✕","◯"]
  drab = function(state) {
    space = function(_state,y,x) { 
      return "<div class='spac' data-index='"+y+"-"+x+
             "'>"+symb[_state]+"</div>" 
      }
    s = ""
    x = 0
    y = 0
    for(i=0;i<9;i++) {
      j = [0,3,6,1,4,7,2,5,8][i]  // XX math 
      s += space((state.box[j] ? 1 : 0)+(state.boo[j] ? 2 : 0),y,x)
      x++
      if((i+1)%3 == 0) {
        y++
        x=0
      }
    }
    $bord.html(s)
  }

  audi = function(state) {
    a = "<h1>audience</h1>"
    for(i=0;i<state.aud.length;i++) {
      a += "<div class='ship'>"+state.aud[i].slice(1)+"</div>"
    }
    $audi.html(a)
  }

  who = null
  turn = function(state) {
    if(state.plx.slice(1) == window.urb.user)
      who = "x"
    if(state.plo.slice(1) == window.urb.user)
      who = "o"
    if(who == null && (state.plx == "" || state.plo == ""))
      wurn = true
    else
      wurn = (state.who == who)
    $('body').toggleClass('turn',wurn)
    $('body').toggleClass('x',(state.who == 'x'))
    $('body').toggleClass('o',(state.who == 'o'))
  }

  assign = function(state) {
    if(!state.plo) 
      state.plo = "" 
    if(!state.plx) 
      state.plx = "" 
    $('#o .ship').toggleClass('anyone', (state.plo=="")).
      text(state.plo.slice(1))
    $('#x .ship').toggleClass('anyone', (state.plx=="")).
      text(state.plx.slice(1))
  }

  message = function(mess) {
    mess = mess.split('"')[1]
    mess = mess.split("=")
    mess = "<div class='ship'>"+mess[0].slice(1) + 
           "</div> ["+symb[lett.indexOf(mess[1].toLowerCase())+1]+"] WINS"
    $('body').append('<div id="message">'+mess+'</div>')
    setTimeout(function() { $('#message').fadeOut().remove(); }, 2000)
  }

  urb.appl = 'oct3'
  urb.bind('/oct3', function(err,res) {
    if(typeof(res.data) == 'string')
      return message(res.data)
    assign(res.data)
    drab(res.data)
    audi(res.data)
    turn(res.data)
  })

  $bord.on('click', function(e) {
    if(!$('body').hasClass('turn')) { return false }
    $t = $(e.target).closest('.spac')
    data = $.map(
      $t.attr('data-index').split('-'), 
      function(i) { return Number(i); })
    urb.send({mark:'oct3-move',data:data})
  })
})
