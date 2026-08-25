::  domains-manager: manage domains & subdomains
::
/+  dbug, verb
::
|%
+$  state-0
  $:  %0
      tops=(set turf)
      subs=(set desk)
      next=(unit [wen=@da wat=(set desk)])
  ==
::
+$  action
  $%  [%put =turf]
      [%del =turf]
  ==
::
++  tell-acme
  |=  [our=ship tops=(set turf) subs=(set desk)]
  ^-  card
  =;  tez=(set (set turf))
    [%pass /acme/set-known %agent [our %acme] %poke %noun !>([%set-known tez])]
  %-  ~(run in tops)
  |=  top=turf
  ::NOTE  letsencrypt prohibits certs with more than 100 subjects.
  ::      perf gets worse the more subjects are on a cert.
  ::      here we limit domain group size to a conservative 50 max.
  ::TODO  limit sets to max 50 domains
  =;  s=(set turf)  (~(put in s) top)       ::  retain top-level domain
  (~(run in subs) |=(d=desk (snoc top d)))  ::  append subdomains
::
++  tell-eyre
  |=  =action
  ^-  card
  [%pass /eyre/turf %arvo %e %rule %turf action]
::
++  debounce-time  ~m5  ::REVIEW
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  &
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
::
++  on-init
  ^-  (quip card _this)
  :_  this
  [%pass /clay/tire %arvo %c %tire `~]~
::
++  on-save  !>(state)
++  on-load
  |=  ole=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 ole))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  ?=(%noun mark)  ~|([dap.bowl %strange-mark mark=mark] !!)
  =+  ;;(act=action q.vase)
  ?-  -.act
      %put
    ?:  (~(has in tops) turf.act)  [~ this]
    =.  tops  (~(put in tops) turf.act)
    [~[(tell-eyre act) (tell-acme our.bowl tops subs)] this]
  ::
      %del
    ?.  (~(has in tops) turf.act)  [~ this]
    =.  tops  (~(del in tops) turf.act)
    [~[(tell-eyre act) (tell-acme our.bowl tops subs)] this]
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ~|  wire=wire
  ?+  wire  ~|(%bad-wide !!)
      [%clay %tire ~]
    ~|  [- +<]:sign
    ?>  ?=([%clay %tire *] sign)
    ::  %tire is a notification and may only have a diff.
    ::  for convenience, we always scry for the full rock instead
    ::
    =/  =rock:tire:clay
      ?:  ?=(%& -.p.sign)  p.p.sign
      .^(rock:tire:clay %cx /(scot %p our.bowl)//(scot %da now.bowl)/tire)
    =/  suz=(set desk)
      %+  roll  ~(tap by rock)
      |=  [[=desk =zest:clay *] suz=(set desk)]
      ?.  ?=(%live zest)  suz  ::REVIEW
      (~(put in suz) desk)
    ::  if the set of subdomains is the currently-known set, do nothing
    ::
    ?:  =(suz subs)
      [~ this]
    ::  if no subs would get removed, apply the change right away
    ::
    ?:  =(~ (~(dif in subs) suz))
      =.  subs  suz
      =.  next  ~
      [[(tell-acme our.bowl tops subs)]~ this]
    ::  some subs would get removed, we must debounce this change
    ::
    =/  until=@da  (add now.bowl debounce-time)
    =.  next  `[until suz]
    [[%pass /debounce %arvo %b %wait until]~ this]
  ::
      [%debounce ~]
    ?>  ?=([%behn %wake *] sign)
    ?~  next  [~ this]
    ?:  (gth wen.u.next now.bowl)  [~ this]
    =+  del=(~(dif in subs) wat.u.next)
    =.  subs  wat.u.next
    :_  this(next ~)
    :-  (tell-acme our.bowl tops subs)
    %-  zing
    %+  turn  ~(tap in del)
    |=  =desk
    %+  turn  ~(tap in tops)
    |=  =turf
    ^-  card
    [%pass /eyre/cert %arvo %e %rule %cert (snoc turf desk) ~]
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ~  ::TODO
::
++  on-watch  |=(* [~ this])
++  on-leave  |=(* [~ this])
++  on-agent  |=(* [~ this])
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card _this)
  %-  (slog (rap 3 dap.bowl ': +on-fail: ' term ~) tang)
  [~ this]
--
