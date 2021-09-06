/-  hark=hark-store, invite=invite-store
/+  verb, dbug, default-agent, agentio
|%
+$  card  card:agent:gall
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
++  on-load
  |=  =vase
  :_  this
  (drop safe-watch:sub:cc)
++  on-save  !>(~)
::
++  on-poke  on-poke:def
++  on-peek  on-peek:def
++  on-watch  on-watch:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  
  ?+  wire  (on-agent:def wire sign)
    [%invites ~]  take-invites
  ==
  ++  take-invites
    ?-  -.sign
      ?(%poke-ack %watch-ack)  (on-agent:def wire sign)
      %kick                    :_(this (drop safe-watch:sub:cc))
    ::
        %fact
      ?.  ?=(%invite-update p.cage.sign)  `this
      =+  !<(=update:invite q.cage.sign)
      ?+  -.update  `this
      ::
          %invite
        :_  this
        (~(created inv:cc [term uid]:update) invite.update)^~
      ::
          %accepted
        :_  this
        ~(accepted inv:cc [term uid]:update)^~
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
++  ha
  |%
  ++  pass  ~(. ^pass /hark)
  ++  poke
    |=(=action:hark (poke-our:pass %hark-store hark-action+!>(action)))
  --
++  sub
  |%
  ++  path   /updates
  ++  wire   /invites
  ++  pass  ~(. ^pass wire)
  ++  watch  (watch-our:pass %invite-store path)
  ++  watching  (~(has by wex.bowl) [wire our.bowl %invite-store])
  ++  safe-watch  `(unit card)`?:(watching ~ `watch)
  --
::
++  inv
  |_  [=term uid=serial:invite]
  ++  bin      [/ place]
  ++  path     /[term]/(scot %uv uid)
  ++  place    `place:hark`[q.byk.bowl path]
  ++  accepted
    (poke:ha %del-place place)
  ++  created 
    |=  =invite:invite
    =;  =body:hark
      (poke:ha %add-note bin body)
    =,  invite
    =/  title=@t
      %:  rap  3 
        'You have been invited to ' 
        (scot %p entity.resource)
        '/'
        name.resource
        ' by '
        ~
      ==
    :*  ~[text+title ship+ship]
        ~[text+text]
        now.bowl
        /
        path
    ==
  --
--
