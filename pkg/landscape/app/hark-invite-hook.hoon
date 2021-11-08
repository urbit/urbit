/-  hark=hark-store, invite=invite-store
/+  verb, dbug, default-agent, agentio, resource, metadata=metadata-store
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  blocked=(jug resource [uid=serial:invite =invite:invite])
     ~
  ==
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
  :_  this
  ~[watch:invite-store:cc]
::
++  on-load
  |=  =vase
  :_  this
  (drop safe-watch:invite-store:cc)
++  on-save  !>(state)
::
++  on-poke  on-poke:def
++  on-peek  on-peek:def
++  on-watch  on-watch:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  |^  
  ?+  wire  (on-agent:def wire sign)
    [%invites ~]  take-invites
  ::
      [%groups @ @ @ ~]
    =/  rid=resource  (de-path:resource t.wire)
    (take-groups rid)
  ==
  ++  take-groups
    |=  rid=resource
    =/  blocking  (~(get ju blocked) rid)
    |^
    ?+  -.sign  (on-agent:def wire sign)
    ::
        %kick
      ?:  =(~ blocking)  `this
      (flush-blocking (rap 3 (scot %p entity.rid) '/' name.rid ~))
    ::
        %fact
      ?.  ?=(%metadata-hook-update p.cage.sign)  `this
      =+  !<(=hook-update:metadata q.cage.sign)
      ?.  ?=(%preview -.hook-update)  `this
      (flush-blocking title.metadatum.hook-update)
    ==
    ::
    ++  flush-blocking
      |=  to=cord
      :_  this(blocked (~(del by blocked) rid))
      %+  turn  ~(tap in blocking)
      |=  [uid=serial:invite =invite:invite]
      (~(created inv:cc %groups uid) invite to)
    --
  ::
  ++  take-invites
    ?-  -.sign
      ?(%poke-ack %watch-ack)  (on-agent:def wire sign)
      %kick                    :_(this (drop safe-watch:invite-store:cc))
    ::
        %fact
      ?.  ?=(%invite-update p.cage.sign)  `this
      =+  !<(=update:invite q.cage.sign)
      ?+  -.update  `this
      ::
          %accepted
        :_  this
        ~(dismissed inv:cc [term uid]:update)^~
      ::
          %decline
        ~&  decline/uid.update
        :_  this
        ~(dismissed inv:cc [term uid]:update)^~
      ::
          %invite
        =*  rid  resource.invite.update
        ?+  term.update  
        ::  default
          =/  fallback  (rap 3 (scot %p entity.rid) '/' name.rid ~)
          :_  this
          (~(created inv:cc [term uid]:update) invite.update fallback)^~
        ::
            %groups
          =.  blocked  (~(put ju blocked) rid [uid invite]:update)
          :_  this
          (drop ~(safe-watch preview:cc rid))
        ::
            %graph
          :_  this
          (~(created inv:cc [term uid]:update) invite.update 'a group chat')^~
          
        ==
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
++  invite-store
  |%
  ++  path   /updates
  ++  wire   /invites
  ++  pass  ~(. ^pass wire)
  ++  watch  (watch-our:pass %invite-store path)
  ++  watching  (~(has by wex.bowl) [wire our.bowl %invite-store])
  ++  safe-watch  `(unit card)`?:(watching ~ `watch)
  --
::
++  preview
  |_  rid=resource
  ++  path   preview+(en-path:resource rid)
  ++  wire   groups+(en-path:resource rid)
  ++  pass  ~(. ^pass wire)
  ++  watching  (~(has by wex.bowl) [wire our.bowl %metadata-pull-hook])
  ++  watch  (watch-our:pass %metadata-pull-hook path)
  ++  safe-watch  `(unit card)`?:(watching ~ `watch)
  --
    

::
++  inv
  |_  [=term uid=serial:invite]
  ++  bin      [/ place]
  ++  path     /[term]/(scot %uv uid)
  ++  place    `place:hark`[q.byk.bowl path]
  ++  dismissed
    (poke:ha %del-place place)
  ++  created 
    |=  [=invite:invite to=cord]
    =;  =body:hark
      (poke:ha %add-note bin body)
    =,  invite
    =/  title=@t
      (rap 3 'You have been invited to ' to ' by ' ~)
    :*  ~[text+title ship+ship]
        ~[text+text]
        now.bowl
        /
        invite+path
    ==
  --
--
