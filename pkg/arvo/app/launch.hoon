/-  launch
/+  *server, default-agent, dbug
|%
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 *]
      [%3 *]
      [%4 state-zero]
  ==
::
+$  state-zero
  $:  tiles=(map term tile:launch)
      tile-ordering=(list term)
      first-time=?
  ==
::
+$  card  card:agent:gall
++  launch-who
  |=  =desk
  [%pass /who %arvo %e %serve [~ /who] desk /gen/who/hoon ~]
--
::
=|  [%4 state-zero]
=*  state  -
%-  agent:dbug
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bol)
::
++  on-init
  ^-  (quip card _this)
  :_  this(state *[%4 state-zero])
  [(launch-who q.byk.bol)]~
::
++  on-save  !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  old-state  !<(versioned-state old)
  ?:  ?=(%4 -.old-state)
    [~ this(state old-state)]
  :_  this
  ::%+  weld
  ::  []~  ::  TODO: kill all subscriptions
  :~  (launch-who q.byk.bol)
      [%pass / %arvo %e %disconnect [~ /]]
  ==
::
++  on-poke
  |=  [mar=mark vas=vase]
  ^-  (quip card _this)
  ?+    mar  (on-poke:def mar vas)
      %json
    ?>  (team:title our.bol src.bol)
    =/  jon  !<(json vas)
    :-  ~
    ?.  =(jon [%s 'disable welcome message'])
      this
    this(first-time %.n)
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  (on-watch:def path)
::
::  |^
::  ?>  (team:title our.bowl src.bowl)
::  =/  cards=(list card)
::    ?+  path  (on-watch:def path)
::        [%keys ~]  (give %chat-update !>([%keys ~(key by inbox)]))
::        [%all ~]   (give %chat-initial !>(inbox))
::    ==
::  [cards this]
::  ::
::  ++  give
::    |=  =cage
::    ^-  (list card)
::    [%give %fact ~ cage]~
::  --
++  on-peek   on-peek:def
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?:  ?=(%bound +<.sin)  [~ this]
  (on-arvo:def wir sin)
::
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
