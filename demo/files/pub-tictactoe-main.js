$(function() {
  $bord = $('#bord')
  $whom = $('#whom')

  lett = ["x","o"]
  symb = [" ","✕","◯"]
  draw = function(state) {
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
   turn(state.who)
  }

  which = null
  turn = function(who) {
    $('body').toggleClass('turn',(who == which))
  }

  assign = function(who) {
    which = who
    turn(who)
    $('#ship .as').text(symb[Number(!lett.indexOf(who))+1])
    $('#user .as').text(symb[lett.indexOf(who)+1])
  }

  urb.appl = 'octo'
  urb.bind('/octo/web', function(err,res) {
    if(which == null) { assign(res.data.who) }
    draw(res.data)
  })

  // draw({
  //   box:[false,false,false,false,false,false,false,false,false],
  //   boo:[false,false,false,false,false,false,false,false,false]
  // })

  $bord.on('click', function(e) {
    if(!$('body').hasClass('turn')) { return false }
    $t = $(e.target).closest('.spac')
    data = $.map(
      $t.attr('data-index').split('-'), 
      function(i) { return Number(i); })
    urb.send({mark:'octo-move',data:data})
  })

  $('#ship .ship').text(window.urb.ship)
  $('#user .ship').text(window.urb.user)
})
