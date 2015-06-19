so = {}
so.ls = $(window).scrollTop()
so.cs = $(window).scrollTop()
so.w = null
so.$d = $('#nav > div')
setSo = -> 
  so.$n = $('#station-container')
  so.w = $(window).width()
  so.h = $(window).height()
  so.dh = $("#c").height()
  so.nh = so.$n.outerHeight(true)
setSo()
setInterval setSo,200

$(window).on 'resize', (e) ->
  if so.w > 1170
    so.$n.removeClass 'm-up m-down m-fixed'

ldy = 0

$(window).on 'scroll', (e) -> 
  so.cs = $(window).scrollTop()

  if so.w > 1170
    so.$n.removeClass 'm-up m-down m-fixed'
  if so.w < 1170
    dy = so.ls-so.cs

    if so.cs <= 0
      so.$n.removeClass 'm-up'
      so.$n.addClass 'm-down m-fixed'
      return

    if so.cs+so.h > so.dh then return

    if so.$n.hasClass 'm-fixed' and
    so.w < 1024
      so.$n.css left:-1*$(window).scrollLeft()

    if dy > 0 and ldy > 0
      if not so.$n.hasClass 'm-down'
        so.$n.removeClass('m-up').addClass 'm-down'
        top = so.cs-so.nh
        if top < 0 then top = 0
        so.$n.offset top:top
      if so.$n.hasClass('m-down') and 
      not so.$n.hasClass('m-fixed') and 
      so.$n.offset().top >= so.cs
        so.$n.addClass 'm-fixed'
        so.$n.attr {style:''}

    if dy < 0 and ldy < 0
      if not so.$n.hasClass 'm-up'
        so.$n.removeClass 'open'
        so.$n.removeClass('m-down m-fixed').addClass 'm-up'
        so.$n.attr {style:''}
        top = so.cs
        sto = so.$n.offset().top
        if top < 0 then top = 0
        if top > sto and top < sto+so.nh then top = sto
        so.$n.offset top:top

  ldy = dy
  so.ls = so.cs

$(window).on 'scroll', window.util.checkScroll