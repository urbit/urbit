$(function() {
  $bord = $('#bord')
  $whom = $('#whom')

  symb = [" ","✕","◯"]
  draw = function(state) {
    space = function(_state,x,y) { 
      return "<div class='spac' data-index='"+x+"-"+y+
             "'>"+symb[_state]+"</div>" 
      }
    s = ""
    x = 0
    y = 0
    for(i=0;i<9;i++) {
      s += space((state.box[i] ? 1 : 0)+(state.boo[i] ? 2 : 0),x,y)
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
})