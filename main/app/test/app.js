$(function() {
  $tests = $("#tests")
  
  runtest = function(name) {
    test = $(name)
    test.attr('disabled', true)
    test.addClass('disabled')
    
    window.urb.send({
      appl:"test",
      data:{test:name}
    }, function(err,res) {
      test.attr('disabled', false)
      test.removeClass('disabled')
      
      _test = {
        name: name,
        result: res.data
      }
      
      console.log('set it')
      console.log(_test)
      
      $tests.prepend(renderTest(_test))
    })
  }
  
  renderTest = function(test) {
    css = "test"
    if(test.pending == true)
      css += " pending"
    $_test = $("<div class='"+css+"' id="+test.name+" onclick='runtest(\""+test.name+"\")'></div>")
    $_test.append("<div class='name'>"+test.name+"</div>")
    $_test.append("<div class='result'>"+JSON.stringify(test.result)+"</div>")
    return $_test
  }
  
  renderTests = function(testlist) {
    console.log("renderTests:  "+testlist)
    $tests.html("")
    for(i in testlist) {
      $tests.append(renderTest(testlist[i]))
    }
  }
  
  renderError = function(error) {
    $tests.html("<div class='error'>Sorry! There was an error fetching from Test: "+error+"</div>")
  }
  
  window.urb.subscribe({
    appl:"test",
    path:"/tests"
  }, function(err,res) {
    console.log('subscr')
    console.log(arguments)
    if (res.data.ok)
      return
    if(err)
      renderTests(err)
    else
    {
      if(res.data) {
        if(res.data) {
          renderTests(res.data)
        }
        else
        {
          renderTests("unknown error")
        }
      }
    }
  })
})
