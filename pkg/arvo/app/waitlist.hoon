::  waitlist: tlon.io hosting waiting list invite codes
::
::    to receive your multiple-use tlon.io invite code:
::    |link ~host %waitlist
::
/-  sole
/+  default-agent
::
|%
+$  state-0   [%0 secret=@]
::
+$  card      card:agent:gall
++  base-url  'https://link.tlon.io/w/'
--
::
=|  state-0
=*  state  -
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  [~ this(secret eny.bowl)]
::
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  loaded  !<(state-0 vase)
  [~ this(state loaded)]
::.
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?.  ?=([%sole *] path)
    (on-watch:def path)
  :_  this
  =/  url=@t
    %^  cat  3
      base-url
    %-  crip
    %-  (x-co:co 8)
    (end [3 4] (mix src.bowl secret))
  :_  ~
  :^  %give  %fact  ~
  :-  %sole-effect
  !>  ^-  sole-effect:sole
  :~  %mor
      [%txt ""]
      [%txt "your unique referral link:"]
      [%klr [[`%un ~ ~] [url]~]~]
      [%txt ""]
      [%bye ~]
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =-  [~ this]
  =/  code  !<(@t vase)
  ~&  ^-  ship
      %+  mix  (rash code hex)
      (end [3 4] secret)
  ~
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?.  ?=([%x %secret ~] path)  ~
  ``atom+!>(secret)
::
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
