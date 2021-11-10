/-  hark=hark-store, hood, docket
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
+$  state-0  [%0 lagging=_|]
::
++  lag-interval  ~m10
--
%+  verb  |
%-  agent:dbug
^-  agent:gall
=|  state-0
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
  [onboard watch:kiln check:lag ~]:cc
::
++  on-load
  |=  =vase
  =+  !<(old=state-0 vase)
  `this(state old)
::
++  on-save  !>(state)
++  on-poke  on-poke:def
++  on-peek  on-peek:def
++  on-watch  on-watch:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  
  ?+  wire  (on-agent:def wire sign)
    [%kiln %vats ~]  take-kiln-vats
  ==
  ++  take-kiln-vats
    ?-  -.sign
      ?(%poke-ack %watch-ack)  (on-agent:def wire sign)
      %kick                    :_(this (drop safe-watch:kiln:cc))
    ::
        %fact
      ?.  ?=(%kiln-vats-diff-0 p.cage.sign)  `this
      =+  !<(=diff:hood q.cage.sign)
      ?+  -.diff  `this
      ::
          %commit
        ?.  |(=(desk.diff %base) ~(has-docket de:cc desk.diff))  `this
        =/  =action:hark  ~(commit de:cc desk.diff)
        :_  this
        ~[(poke:ha:cc action)]
      ::
          %block
        =/  =action:hark  (~(blocked de:cc desk.diff) blockers.diff)
        :_  this 
        ~[(poke:ha:cc action)]
      ==
    ==
  --
::
++  on-arvo  
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?.  ?=([%check-lag ~] wire)  (on-arvo:def wire sign)
  ?>  ?=([%behn %wake *] sign)
  =+  .^(lag=? %$ (scry:io %$ /zen/lag))
  ?:  =(lagging lag)  :_(this ~[check:lag:cc])
  :_  this(lagging lag)
  :_  ~[check:lag:cc]
  ?:(lagging start:lag:cc stop:lag:cc)
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
++  kiln
  |%
  ++  path   /kiln/vats
  ++  pass  ~(. ^pass path)
  ++  watch  (watch-our:pass %hood path)
  ++  watching  (~(has by wex.bowl) [path our.bowl %hood])
  ++  safe-watch  `(unit card)`?:(watching ~ `watch)
  --
::
++  de
  |_  =desk
  ++  scry-path  (scry:io desk /desk/docket-0)
  ++  has-docket  .^(? %cu scry-path)
  ++  docket  .^(docket:^docket %cx scry-path)
  ++  hash    .^(@uv %cz (scry:io desk ~))
  ++  place    `place:hark`[q.byk.bowl /desk/[desk]]
  ++  vat
    .^(vat:hood %gx (scry:io %hood /kiln/vat/[desk]/noun))
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
    ?:(=(1 ud.cass:vat) created updated)
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
    |=  blockers=(set ^desk)
    ^-  action:hark
    :+  %add-note  [/blocked place]
    %^  body  /blocked  (title-prefix ' is blocked from upgrading')
    `(rap 3 'Blocking desks: ' (join ', ' ~(tap in blockers)))
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
