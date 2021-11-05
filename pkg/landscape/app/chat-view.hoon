::  chat-view [landscape]: deprecated
::
/+  default-agent
|%
+$  card  card:agent:gall
--
::
^-  agent:gall
|_  bol=bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bol)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  :*  %pass  /srv  %agent  [our.bol %file-server]
          %poke  %file-server-action
          !>([%serve-dir /'~chat' /app/landscape %.n %.y])
      ==
  ==
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-save   !>(~)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
