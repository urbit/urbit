::
::  Soto: A Dojo relay for Urbit's Landscape interface
::  Relays sole-effects to subscribers and forwards sole-action pokes
::
/-  sole
/+  *soto, default-agent
|%
+$  card  card:agent:gall
+$  state-zero  ~
::
--
=|  state-zero
=*  state  -
^-  agent:gall
|_  bol=bowl:gall
+*  this      .
    soto-core  +>
    sc        ~(. soto-core bol)
    def       ~(. (default-agent this %|) bol)
::
++  on-init  [~ this]
++  on-save  !>(state)
::
++  on-load
  |=  old=vase
  :_  this(state !<(state-zero old))
  [%pass /bind/soto %arvo %e %disconnect [~ /'~dojo']]~
::
++  on-poke  on-poke:def
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?+  pax  (on-watch:def pax)
      [%sototile ~]
    :_  this
    [%give %fact ~ %json !>(~)]~
  ==
::
++  on-agent  on-agent:def
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card _this)
  ?:  ?=(%bound +<.sin)
    [~ this]
  (on-arvo:def wir sin)
::
++  on-fail   on-fail:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
::
--
