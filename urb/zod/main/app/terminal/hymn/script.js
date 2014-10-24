function jpok(a,b){
  var dat = {pax:urb.term.pax, act:{}}
  dat.act[a] = b
  urb.send({data:dat,mark:"term-in"}, function(e,dat){
    if(a === 'line' && dat.data.err){
      hist.unshift(prom.val())
      prom.val(b)
      hind = 0
    }
  })
}

var prom, prom_size, hist, hind, size, focus, pos, pax

$(function() {
  prom = $("#prom")
  prom_size = $("#prom-size")

  hist = []
  hind = 0

  var keys = ['l', 'x', 'r']
  var mod = /Mac|iPod|iPhone|iPad/.test(navigator.platform) ? 'ctrl' : 'alt'
  for (i in keys) (function(k){
    Mousetrap.bind(mod + '+' + k, function(){
      jpok('cmd', k)
    })
  })(keys[i])

  prom.keydown(function(e){
    switch(e.which){  
      default: return true
        break;
      case 13:  // %retn
        if(e.shiftKey) return true
        v = prom.val().replace(/\xa0/g, ' ')
        $(cont).append($('<b>').html(prem.innerHTML),
                       $('<div class="prom">').text(v))

        jpok('line', v)
        hist.unshift(v)
        prom.val('')
        return false
        break;
      case 38:  // %up
        if(hind == hist.length) return true
        if(pos().top === false) return true
        prom.val([hist[hind], hist[hind] = prom.val()][0]) // swap
        size()
        hind++
        return false
        break;
      case 40:  // %down
        if(hind == 0) return true
        if(pos().bot === false) return true
        size()
        hind--
        prom.val([hist[hind], hist[hind] = prom.val()][0]) // swap
        return false
    }
  })

  focus = function() { $(prom).focus(); }
  $('body').on('click', focus)
  focus()

  size = function() {
    prom_size.html(prom.val()+"<br />")
  }
  size()
  prom.on('input propertychange', size)

  pos = function() {
    v = prom.val()
    _top = v.slice(0,prom[0].selectionStart).indexOf("\n") === -1
    _bot = v.slice(prom[0].selectionStart).indexOf("\n") === -1
    return {top:_top,bot:_bot}
  }

  pax = '/lines'
  if(urb.term.pax != "/") pax += urb.term.pax
  urb.subscribe({path: pax}, function(e, dat){
    if(dat.data.ok) return;
    hist = dat.data.history.concat(hist)
    hind = 0
    // cont.innerHTML = ''
    for(var i in dat.data.lines){
      var lom = dat.data.lines[i]
      if(typeof lom == 'string')
        $(cont).append($('<div>').text(lom))
      else {
        $(cont).append($('<b>').text(lom.prompt),
                       $('<div class="prom">').text(lom.task))
      }
    }
    window.scrollTo(0,document.body.scrollHeight)
    prem.textContent = dat.data.prompt
  })
});
