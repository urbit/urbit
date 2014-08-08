$(function() {
  checkLength = function() {
    if($tweet.val().length > 140) {
      e.stopPropagation()
      e.preventDefault()
      return false
    }
  }
  setLength = function() {
    $("#length").val($("#tweetr").val().length+"/140")
  }
  
  $tweet = $("#tweetr")
  $time = $("#time")
  
  $tweet.focus()
  $tweet[0].selectionStart = $tweet[0].selectionEnd = $tweet.val().length
  
  $tweet.change(checkLength)
  $tweet.keydown(checkLength)
  $tweet.keyup(setLength)
  
  $('#submit').click(function() {
    tweet = $tweet.val()
    $tweet.attr('disabled', true)
    window.urb.send({
      appl:"twit",
      data:{tweet:tweet}
    }, function(err,res) {
      console.log(arguments)
      $tweet.attr('disabled', true)
      $tweet.val('')
    })
  })
  
  timeline = JSON.parse($('#jime')[0].innerHTML)
  for(i in timeline) {
    tweets = timeline[i]
    d = new Date(tweets.created_at)
    datestr = d.getMonth()+1 + "-" + d.getDate() + "-" + d.getFullYear() + " " + d.getHours() + ":" + d.getMinutes() + ":" + d.getSeconds()
    $tweets = $("<div class='tweet'></div>")
    $tweets.append("<div class='text'>"+tweets.text+"</div>")
    $tweets.append("<div class='date'>"+datestr+"</div>")
    $time.append($tweets)
  }
})
