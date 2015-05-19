$(function() {
  $bord = $('#bord')
  $whom = $('#whom')

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
    $whom.html((state.who == 'x' ? symb[1] : symb[2]))
  }


  urb.appl = 'octo'
  urb.bind('/octo/o', function(err,res) {
    draw(res.data)
  })

  draw({
    box:[false,false,false,false,false,false,false,false,false],
    boo:[false,false,false,false,false,false,false,false,false]
  })

  $bord.on('click', function(e) {
    $t = $(e.target).closest('.spac')
    data = $.map(
      $t.attr('data-index').split('-'), 
      function(i) { return Number(i); })
    urb.send({mark:'octo-move',data:data})
  })

  $('#ship .ship').text(window.urb.ship)
  $('#user .ship').text(window.urb.user)
})
