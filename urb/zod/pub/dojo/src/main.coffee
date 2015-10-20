[DOM,recl,rend] = [React.DOM, React.createClass, React.renderComponent]
[div, pre, span] = [DOM.div, DOM.pre, DOM.span]
str = JSON.stringify

Prompt = recl render: ->
  [pro,cur,buf] = [@props.prompt[@props.appl] ? "X", @props.cursor, @props.input + " "]
  pre {}, @props.appl+pro,
    span {style: background: 'lightgray'}, buf.slice(0,cur), "\u0332", buf.slice(cur)

Matr = recl render: ->
  lines = @props.rows.map (lin)-> pre {}, lin, " "
  lines.push Prompt
    appl:   @props.appl, 
    prompt: @props.prompt, 
    input:  @props.input, 
    cursor: @props.cursor
  div {}, lines

$ ->

  met = $('<pre>').text('m').css(display: 'none').appendTo(term).width()
  subs = ""
  # $(window).resize -> 
  #   window.termWif = ($(term).width() / met).toFixed()
  #   path =  "/new/#{termWif}"
  #   if path is subs
  #     return
  #   if subs
  #     urb.unsubscribe path: subs
  #   subs = path
  #   urb.subscribe {path}, (err,dat)->
  #       if err or dat.data.ok
  #         return;
  #       syncRev = dat.data.rev
  #       unless termRev > syncRev
  #         termRev = syncRev
  #         matr.setProps rows: dat.data.stak
  #         document.title = "Matrix"  # XX  debug
  # $(window).resize()

  flash = ($el, background)->
    $el.css {background}
    if background
      setTimeout (-> flash $el,''), 50
  bell = -> flash ($ 'body'), 'black'

  matr = rend (Matr
    rows:[]
    appl:""
    prompt:{"": "# "}
    input:""
    cursor:0
    history:[]
    offset:0  ), term
  window.matr = matr
  update = (a) -> matr.setProps a
  buffer = "": new Share ""
  window.buffer = buffer
  choose = (appl)->
    urb.appl = appl
    buffer[appl] ?= new Share ""
    updPrompt '', null
    update {appl, cursor: 0, input: buffer[appl].buf}
  print = (txt)-> update rows: [matr.props.rows..., txt]
  sync = (ted,app)->
    app ?= matr.props.appl
    if app isnt matr.props.appl then return
    b = buffer[app]
    update input: b.buf, cursor: b.transpose ted, matr.props.cursor
  updPrompt = (app,pro) ->
    prompt = $.extend {}, matr.props.prompt
    if pro? then prompt[app] = pro else delete prompt[app]
    update {prompt}
  sysStatus = ()-> updPrompt '', (
    [app,pro] = [matr.props.appl, (k for k,v of matr.props.prompt when k isnt '')]
    if app is '' then (pro.join ', ')+'# ' else null
  )
 
  peer = (ruh,app) ->
    app ?= urb.appl
    if ruh.map then return ruh.map (rul)-> peer rul, app
    mapr = matr.props
    switch Object.keys(ruh)[0]
      when 'txt' then print ruh.txt
      when 'tan' then ruh.tan.split("\n").map print
      when 'pro' then updPrompt app, ruh.pro.cad
      when 'hop' then update cursor: ruh.hop; bell() # XX buffer.transpose?
      when 'blk' then console.log "Stub #{str ruh}"
      when 'det' then buffer[app].receive ruh.det; sync ruh.det.ted, app
      when 'act' then switch ruh.act
        when 'clr' then update rows:[]
        when 'bel' then bell()
        when 'nex' then update
          input: ""
          cursor: 0
          history: 
            if !mapr.input then mapr.history
            else [mapr.input, mapr.history...]
          offset: 0
      #   else throw "Unknown "+(JSON.stringify ruh)
      else v = Object.keys(ruh); console.log v, ruh[v[0]]

  join = (app)->
    if matr.props.prompt[app]?
      return print '# already-joined: '+app
    choose app
    urb.bind "/sole", {wire:"/"}, (err,d)->
      if err then console.log err
      else if d.data then peer d.data, app
  cycle = ()->
    apps = Object.keys matr.props.prompt
    if apps.length < 2 then return
    choose apps[1 + apps.indexOf urb.appl] ? apps[0]
  part = (appl)->
    mapr = matr.props
    unless mapr.prompt[appl]?
      return print '# not-joined: '+appl
    urb.drop "/sole", {appl, wire: "/"}
    if appl is mapr.appl then cycle()
    updPrompt appl, null
    sysStatus()
  join urb.appl
  window.join = join; window.part = part
  
  pressed = []
  deltim = null
  #later = (data)->
  #  if data
  #    pressed.push data
  #  clearTimeout deltim
  #  setTimeout (->
  #    if urb.reqq.length > 0 
  #      return deltim = later()
  #    urb.send data: pressed
  #    pressed = []
  #  ), 500

  urb.send.mark = 'sole-action'
  sendAction = (data)->
    if matr.props.appl then urb.send data, (e,res)->
      if res.status isnt 200 then $('#err')[0].innerText = res.data.mess
    else if data is 'ret'
      app = /^[a-z-]+$/.exec(buffer[""].buf.slice(1))
      unless app? and app[0]?
        return bell()
      else switch buffer[""].buf[0]
        when '+' then doEdit set: ""; join app[0]
        when '-' then doEdit set: ""; part app[0]
        else bell()
  
  doEdit = (ted)->
    det = buffer[matr.props.appl].transmit ted
    sync ted
    sendAction {det}

  yank = ''
  eatKyev= (mod, key)->
    mapr = matr.props
    switch mod.sort().join '-'
      when '', 'shift'
        if key.str
          doEdit ins: cha: key.str, at: mapr.cursor
          update cursor: mapr.cursor+1
        switch key.act
          when 'entr' then sendAction 'ret'
          when 'up'
            history = mapr.history.slice(); offset = mapr.offset
            if history[offset] == undefined
              return
            [input, history[offset]] = [history[offset], mapr.input]
            offset++
            doEdit set: input
            update {offset, history, cursor: input.length}
          when 'down'
            history = mapr.history.slice(); offset = mapr.offset
            offset--
            if history[offset] == undefined
              return
            [input, history[offset]] = [history[offset], mapr.input]
            doEdit set: input
            update {offset, history, cursor: input.length}
          when 'left' then if mapr.cursor > 0 
            update cursor: mapr.cursor-1
          when 'right' then if mapr.cursor < mapr.input.length
            update cursor: mapr.cursor+1
          when 'baxp' then if mapr.cursor > 0
            doEdit del: mapr.cursor-1
          #else (if key.act then console.log key.act)
      when 'ctrl' then switch key.str || key.act
        when 'a','left'  then update cursor: 0
        when 'e','right' then update cursor: mapr.input.length
        when 'l' then update rows: []
        when 'entr' then bell()
        when 'w' then eatKyev ['alt'], act:'baxp'
        when 'p' then eatKyev [], act: 'up'
        when 'n' then eatKyev [], act: 'down'
        when 'b' then eatKyev [], act: 'left'
        when 'f' then eatKyev [], act: 'right'
        when 'g' then bell()
        when 'x' then cycle()
        when 'v'
          appl = if mapr.appl isnt '' then '' else urb.appl
          update {appl, cursor:0, input:buffer[appl].buf}
          sysStatus()
        when 't'
          if mapr.cursor is 0 or mapr.input.length < 2
            return bell()
          cursor = mapr.cursor
          if cursor < mapr.input.length
            cursor++
          doEdit [{del:cursor-1},ins:{at:cursor-2,cha:mapr.input[cursor-1]}]
          update {cursor}
        when 'u' 
          yank = mapr.input.slice(0,mapr.cursor)
          doEdit (del:mapr.cursor - n for n in [1..mapr.cursor])
        when 'k'
          yank = mapr.input.slice(mapr.cursor)
          doEdit (del:mapr.cursor for _ in [mapr.cursor...mapr.input.length])
        when 'y'
          doEdit (ins: {cha, at: mapr.cursor + n} for cha,n in yank)
        else console.log mod, str key
      when 'alt' then switch key.str || key.act
        when 'f','right'
          rest = mapr.input.slice(mapr.cursor)
          rest = rest.match(/\W*\w*/)[0] # XX unicode
          update cursor: mapr.cursor + rest.length
        when 'b','left'
          prev = mapr.input.slice(0,mapr.cursor)
          prev = prev.split('').reverse().join('')  # XX
          prev = prev.match(/\W*\w*/)[0] # XX unicode
          update cursor: mapr.cursor - prev.length
        when 'baxp'
          prev = mapr.input.slice(0,mapr.cursor)
          prev = prev.split('').reverse().join('')  # XX
          prev = prev.match(/\W*\w*/)[0] # XX unicode
          yank = prev
          doEdit (del: mapr.cursor-1 - n for _,n in prev)
      else console.log mod, str key

  Mousetrap.handleKey = (char, mod, e)->
    norm = {
      capslock:  'caps'
      pageup:    'pgup'
      pagedown:  'pgdn'
      backspace: 'baxp'
      enter:     'entr'
    }

    key = switch
      when char.length is 1
        if e.type is 'keypress'
          chac = char.charCodeAt(0)
          if chac < 32          # normalize ctrl keys
            char = String.fromCharCode chac | 96
          str: char
      when e.type is 'keydown'
        if char isnt 'space'
          act: norm[char] ? char
      when e.type is 'keyup' and norm[key] is 'caps'
        act: 'uncap'
    if !key then return
    if key.act and key.act in mod
      return
    e.preventDefault()
    #[fore, aft] = (
    #  [sli,cur] = [mapr.input.slice, mapr.cursor]
    #  [sli(0, cur), sli(cur)]
    #)
    eatKyev mod, key

    #amod = (arr)->
    #  for i in arr
    #    unless mod.indexOf(i) < 0
    #      return yes
    #  no
    # if key.str or key.act is 'baxp' or key.act is 'entr'
    #   termRev++
    #   [bot, rest...] = old = matr.props.rows
    #   matr.setProps rows:(
    #     switch  key.act 
    #       when 'baxp'
    #         if amod ['ctrl', 'meta']
    #           ['', rest...]
    #         else if amod ['alt']
    #           [(bot.replace /\ *[^ ]*$/, ''), rest...]
    #         else if bot and bot.length 
    #           [bot.slice(0, -1), rest...]
    #         else if rest[0] and rest[0].length
    #           res = rest.slice()
    #           res[0] = res[0].slice(0, -1)
    #           res
    #         else rest
    #       when 'entr'
    #         ['', old...]
    #       when undefined
    #         if mod.length > 1 or (mod.length and !amod ['shift'])
    #           old
    #         else unless old and bot isnt null
    #           [key.str]
    #         #else if bot.length is termWif
    #         #  [key.str, old...]
    #         else [bot + key.str, rest...]
    #   )
    #   document.title = "Matri"  # XX  debug
    # later {mod, key}

