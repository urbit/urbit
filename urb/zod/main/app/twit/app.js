$(function() {
  checkLength = function() {
    if($tweet.val().length > 140) {
      short = $tweet.val().slice(0,140)
      $tweet.val(short)
      return false
    }
  }
  setLength = function() {
    $("#length").val($("#tweetr").val().length+"/140")
  }
  
  twoDig = function(d) {
    return (d<10) ? "0"+d : d
  }
  setTime = function() {
    d = new Date()
    datestr = twoDig(d.getMonth()+1) + "-" + twoDig(d.getDate()) + "-" + d.getFullYear() + " " + twoDig(d.getHours()) + ":" + twoDig(d.getMinutes()) + ":" + twoDig(d.getSeconds())
    $("#twet .date").text(datestr)
  }
  setInterval(setTime,1000)
  setTime()
  
  $tweet = $("#tweetr")
  $time = $("#time")
  $submit = $('#submit')
  
  $tweet.focus()
  $tweet[0].selectionStart = $tweet[0].selectionEnd = $tweet.val().length
  
  $tweet.change(checkLength)
  $tweet.keydown(checkLength)
  $tweet.keyup(setLength)
  
  $submit.click(function() {
    tweet = $tweet.val()
    $tweet.attr('disabled', true)
    $submit.attr('disabled', true)
    $submit.addClass('disabled')
    
    window.urb.send({
      appl:"twit",
      data:{tweet:tweet}
    }, function(err,res) {
      $tweet.attr('disabled', false)
      $submit.attr('disabled', false)
      $submit.removeClass('disabled')
      
      _tweet = {
        created_at: String(new Date()),
        text: tweet,
        pending: true
      }
      
      console.log('set it')
      console.log(_tweet)
      
      $time.prepend(renderTweet(_tweet))
      
      $tweet.val('')
      setLength()
    })
  })
  
  renderTweet = function(tweet) {
    d = new Date(tweet.created_at)
    datestr = d.getMonth()+1 + "-" + d.getDate() + "-" + d.getFullYear() + " " + d.getHours() + ":" + d.getMinutes() + ":" + d.getSeconds()
    css = "tweet"
    if(tweet.pending == true)
      css += " pending"
    $_tweet = $("<div class='"+css+"'></div>")
    $_tweet.append("<div class='author'>@urbit_test</div>")
    $_tweet.append("<div class='date'>"+datestr+"</div>")
    $_tweet.append("<div class='text'>"+tweet.text+"</div>")
    return $_tweet
  }
  
  renderTimeline = function(timeline) {
    $time.html("")
    for(i in timeline) {
      $time.append(renderTweet(timeline[i]))
    }
  }
  
  renderError = function(error) {
    $time.html("<div class='error'>Sorry! There was an error fetching from Twitter: "+error+"</div>")
  }
  
  window.urb.subscribe({
    appl:"twit",
    path:"/line"
  }, function(err,res) {
    console.log('subscr')
    console.log(arguments)
    console.log((res.data && res.data[0]))
    if(err) 
      return
    if(res.data) {
      if(res.data[0]) {
        renderTimeline(res.data)
      }
      if(res.errors) {
        renderError(res.errors[0])
      }
    }
  })
  
})
