
window.onload = function() {
  data = {}

  change = function(_data) {
    for(i in _data) {
      data[i] = _data[i]
    }
  }

  update = function() {
    for (var i in data) {
      if ($('#'+i).length < 1) {
        var e = document.createElement('tr')
        e.id = i
        $('#cont tbody').append(e)
      }
      $('#'+i).html("<td>~"+i+"</td><td>"+data[i]+"</td>")
    }
    $('#cont tbody').append([].sort.call($('#cont tr'), function (a, b) {
      return parseInt(b.childNodes[1].innerText) -
        parseInt(a.childNodes[1].innerText)
    }))
  }

  goof = function(e) {
    d = $.map($(".sel"), function(el) {return el.id})
    window.urb.send(d)
  }

  window.urb.subscribe("frog","goof", function(err,res) {
    if(err) 
      return console.log('cannot connect to frog/goof')
    change(res.data)
    update()

    return true
  })
  
  $('#cont').on('click', 'tr', function (e) {
    if (!e.ctrlKey) { $('.sel').removeClass('sel') }
    $(this).addClass('sel')
  })
}
