module.exports =
  uuid32: ->
    vals = for i in [0..5]
      str = Math.ceil(Math.random()*10000000).toString(32)
      ("00000"+str).substr(-5,5)
    vals.unshift Math.ceil(Math.random()*8)
    "0v" + vals.join '.'

  getScroll: ->
    @writingPosition = $('#c').outerHeight(true)+$('#c').offset().top-$(window).height()

  setScroll: ->
    window.util.getScroll()
    $(window).scrollTop($("#c").height())

  isScrolling: ->
    if not window.util.writingPosition
      window.util.getScroll()
    return ($(window).scrollTop()+$('#writing').outerHeight() < window.util.writingPosition)

  checkScroll: ->
    if window.util.isScrolling()
      $('body').addClass 'scrolling'
    else
      $('body').removeClass 'scrolling'
  talk:
    mainStations: ["court","floor","porch"]
    mainStationPath: (user) -> "~#{user}/#{window.util.talk.mainStation(user)}"
    mainStation: (user) ->
      if not user then user = window.urb.user
      switch user.length
        when 3
          return "court"
        when 6
          return "floor"
        when 13
          return "porch"

