::  invite-view: provide a json interface to invite-store
::
::    accepts subscriptions at the empty path.
::    passes through all invites and their updates.
::    only accepts subcriptions from the host's team.
::
::TODO  could maybe use /lib/proxy-hook, be renamed invite-proxy-hook
::
/+  *invite-json, default-agent, verb
::
|%
+$  state-0  [%0 ~]
::
+$  card  card:agent:gall
--
::
::
=|  state-0
=*  state  -
::
=>  |%
    ++  watch-updates
      |=  our=ship
      ^-  card
      [%pass /store %agent [our %invite-store] %watch /updates]
    --
::
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  [[(watch-updates our.bowl)]~ this]
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  =(~ path)
  ?>  (team:title our.bowl src.bowl)
  :_  this
  =/  =invites
    .^(invites %gx /=invite-store/(scot %da now.bowl)/all/noun)
  [%give %fact ~ %json !>((invites-to-json invites))]~
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?.  =(/store wire)  (on-agent:def wire sign)
  :_  this
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack] !!)
    %watch-ack  ~
    %kick       [(watch-updates our.bowl)]~
  ::
      %fact
    ~|  [dap.bowl %unexpected-fact-mark p.cage.sign]
    ?>  ?=(%invite-update p.cage.sign)
    [%give %fact `/ %json !>((update-to-json !<(invite-update q.cage.sign)))]~
  ==
::
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
