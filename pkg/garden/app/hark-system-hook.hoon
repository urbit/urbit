/-  hark=hark-store, hood, docket
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
+$  state-1  [%1 lagging=_|]
::
++  lag-interval  ~m10
--
%+  verb  |
%-  agent:dbug
^-  agent:gall
=|  state-1
=*  state  -
=<
|_  =bowl:gall
+*   this  .
     def  ~(. (default-agent this %|) bowl)
     io   ~(. agentio bowl)
     pass  pass:io
     cc   ~(. +> bowl)
++  on-init
  ^-  (quip card _this)
  :_  this
  [onboard tire:cy check:lag ~]:cc
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  |^
  =+  !<(old=app-states vase)
  =^  cards-1  old
    ?.  ?=(%0 -.old)  `old
    [[tire:cy:cc]~ old(- %1)]
  ?>  ?=(%1 -.old)
  =/  cards-tire  [tire:cy:cc ~]
  [(weld cards-1 cards-tire) this(state old)]
  ::
  +$  app-states  $%(state-0 state-1)
  +$  state-0  [%0 lagging=_|]
  --
::
++  on-save  !>(state)
++  on-poke  on-poke:def
++  on-peek  on-peek:def
++  on-watch  on-watch:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  wire  (on-agent:def wire sign)
    [%kiln %vats ~]    `this
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  |^
  ?+  wire  (on-arvo:def wire sign)
    [%clay %tire ~]    take-clay-tire
    [%clay %warp * ~]  (take-clay-warp i.t.t.wire)
    [%check-lag ~]     take-check-lag
  ==
  ::
  ++  take-check-lag
    ^-  (quip card _this)
    ?>  ?=([%behn %wake *] sign)
    =+  .^(lag=? %$ (scry:io %$ /zen/lag))
    ?:  =(lagging lag)  :_(this ~[check:lag:cc])
    :_  this(lagging lag)
    :_  ~[check:lag:cc]
    ?:(lagging start:lag:cc stop:lag:cc)
  ::
  ++  take-clay-tire
    ^-  (quip card _this)
    ?>  ?=(%tire +<.sign)
    ?-    -.p.sign
        %&  [(turn ~(tap in ~(key by p.p.sign)) warp:cy:cc) this]
        %|
      ?-    -.p.p.sign
          %zest  `this
          %warp  `this
          %wait
        =/  =action:hark  (~(blocked de:cc desk.p.p.sign) weft.p.p.sign)
        :_  this
        ~[(poke:ha:cc action)]
      ==
    ==
  ::
  ++  take-clay-warp
    |=  =desk
    ^-  (quip card _this)
    ?>  ?=(%writ +<.sign)
    =/  cards
      ?.  |(=(desk %base) ~(has-docket de:cc desk))  ~
      =/  =action:hark  ~(commit de:cc desk)
      ~[(poke:ha:cc action)]
    [[(warp:cy:cc desk) cards] this]
  --
::
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
+*  io   ~(. agentio bowl)
    pass  pass:io
::
++  onboard
  ^-  card
  %-  poke:ha
  :+  %add-note  [/ [q.byk.bowl /onboard]]
  ::  We special case this in the grid UI, but should include something
  ::  for third parties
  [~[text+'Welcome to urbit'] ~ now.bowl / /]
::
++  lag
  |%
  ++  check  (~(wait pass /check-lag) (add now.bowl lag-interval))
  ++  place  [q.byk.bowl /lag]
  ++  body   `body:hark`[~[text/'Runtime lagging'] ~ now.bowl / /]
  ++  start  (poke:ha %add-note [/ place] body)
  ++  stop   (poke:ha %del-place place)
  --
++  ha
  |%
  ++  pass  ~(. ^pass /hark)
  ++  poke
    |=(=action:hark (poke-our:pass %hark-store hark-action+!>(action)))
  --
::
++  cy
  |%
  ++  tire  ~(tire pass /clay/tire)
  ++  warp
    |=  =desk
    (~(warp-our pass /clay/warp/[desk]) desk ~ %next %z da+now.bowl /)
  --
::
++  de
  |_  =desk
  ++  scry-path   (scry:io desk /desk/docket-0)
  ++  has-docket  .^(? %cu scry-path)
  ++  docket      .^(docket:^docket %cx scry-path)
  ++  hash        .^(@uv %cz (scry:io desk ~))
  ++  place       `place:hark`[q.byk.bowl /desk/[desk]]
  ++  version     ud:.^(cass:clay %cw (scry:io desk /))
  ++  body
    |=  [=path title=cord content=(unit cord)]
    ^-  body:hark
    [~[text+title] ?~(content ~ ~[text/u.content]) now.bowl ~ path]
  ::
  ::
  ++  title-prefix
    |=  =cord
    %+  rap  3
    ?:  =(desk %base)
      ['System software' cord ~]
    ?:  has-docket
      ['App: "' title:docket '"' cord ~]
    ['Desk: ' desk cord ~]
  ::
  ++  get-version
    ?:  has-docket
      (rap 3 'version: ' (ver version:docket) ~)
    (rap 3 'hash: ' (scot %uv hash) ~)
  ::
  ++  commit
    ^-  action:hark
    ?:(=(1 version) created updated)
  ::
  ++  created
    ^-  action:hark
    :+  %add-note  [/created place]
    (body /desk/[desk] (title-prefix ' has been installed') ~)
  ::
  ++  updated
    ^-  action:hark
    :+  %add-note  [/update place]
    (body /desk/[desk] (title-prefix (rap 3 ' has been updated to ' get-version ~)) ~)
  ::
  ++  blocked
    |=  =weft
    ^-  action:hark
    :+  %add-note  [/blocked place]
    %^  body  /blocked  (title-prefix ' is blocked from upgrading')
    `(rap 3 'Blocked waiting for system version: ' (scot %ud num.weft) 'K' ~)
  ::
  ++  ver
    |=  =version:^docket
    =,  version
    `@t`(rap 3 (num major) '.' (num minor) '.' (num patch) ~)
  ::
  ++  num
    |=  a=@ud
    `@t`(rsh 4 (scot %ui a))
  --
++  note
  |%
  ++  merge
    |=  [=desk hash=@uv]
    ^-  (list body:hark)
    :_  ~
    :*  ~[text+'Desk Updated']
        ~[text+(crip "Desk {(trip desk)} has been updated to hash {(scow %uv hash)}")]
        now.bowl
        /update/[desk]
        /
    ==
  --
--
