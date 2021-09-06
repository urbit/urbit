/-  hark=hark-store, hood, docket
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
::
::
--
=<  
%+  verb  &
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*   this  .
     def  ~(. (default-agent this %|) bowl)
     io   ~(. agentio bowl)
     pass  pass:io
     cc   ~(. +> bowl)
++  on-init  on-init:def 
::
::
++  on-load
  |=  =vase
  :_  this :: TODO: move to on-init
  :-  onboard:cc
  (drop safe-watch:kiln:cc)
++  on-save  !>(~)
::
++  on-poke
  |=  [=mark =vase]
  !!
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
      ?.  ?=(%kiln-vats-diff p.cage.sign)  `this
      =+  !<(=diff:hood q.cage.sign)
      ?+  -.diff  `this
      ::
          %merge
        =/  =action:hark  ~(updated de:cc desk.diff)
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
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
++  on-leave  on-leave:def
--
|_  =bowl:gall
+*  io   ~(. agentio bowl)
    pass  pass:io
++  onboard
  ^-  card
  %-  poke:ha
  :+  %add-note  [/ [q.byk.bowl /onboard]]
  ::  We special case this in the grid UI, but should include something
  ::  for third parties
  [~[text+'Welcome to urbit'] ~ now.bowl / /]

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
  ++  scry-path  (scry:io desk /desk/docket)
  ++  has-docket  .^(? %cu scry-path)
  ++  docket  .^(docket:^docket %cx scry-path)
  ++  hash    .^(@uv %cz (scry:io desk ~))
  ++  place    `place:hark`[q.byk.bowl /desk/[desk]]
  ++  body
    |=  [=path title=cord content=cord]
    ^-  body:hark
    [~[text+title] ~[text+content] now.bowl path ~]
  ::
  ::
  ++  title-prefix
    |=  =cord
    ?:  =(desk %base)
      'System software'
    ?:  has-docket  
      (rap 3 'App: ' title:docket cord ~)
    (rap 3 'Desk: ' desk cord ~)
  ::
  ++  updated
    ^-  action:hark
    :+  %add-note  [/update place]
    %^  body  /updated  (title-prefix ' has been updated')
    ?:  has-docket
      (rap 3 'Version: ' (ver version:docket) ~)
    (rap 3 'Hash: ' (scot %uv hash) ~)
  ::
  ++  blocked
    |=  blockers=(set ^desk)
    ^-  action:hark
    :+  %add-note  [/blocked place]
    %^  body  /blocked  (title-prefix ' is blocked from upgrading')
    (rap 3 'Blocking desks: ' (join ', ' ~(tap in blockers)))
  ::
  ++  ver
    |=  =version:^docket
    =,  version
    `@t`(rap 3 (num major) '.' (num minor) '.' (num patch) ~)
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
