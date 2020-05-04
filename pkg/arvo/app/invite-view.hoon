::  invite-view: provide a json interface to invite-store
::
::    accepts subscriptions at the /primary path.
::    passes through all invites and their updates.
::    only accepts subcriptions from the host's team.
::
::TODO  could maybe use /lib/proxy-hook, be renamed invite-proxy-hook
::
/+  *invite-json, default-agent, dbug
::
|%
+$  card  card:agent:gall
--
::
=>
  |%
  ++  watch-updates
    |=  our=ship
    ^-  card
    [%pass /store %agent [our %invite-store] %watch /updates]
  --
::
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  [[(watch-updates our.bowl)]~ this]
::
++  on-save  on-save:def
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?.  =(/primary path)
    (on-watch:def path)
  :_  this
  =/  =invites
    .^(invites %gx /=invite-store/(scot %da now.bowl)/all/noun)
  [%give %fact ~ %json !>((invites-to-json invites))]~
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  :_  this
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack] !!)
    %watch-ack  ~
    %kick       [(watch-updates our.bowl)]~
  ::
      %fact
    ~|  [dap.bowl %unexpected-fact-mark p.cage.sign]
    ?>  ?=(%invite-update p.cage.sign)
    :~  :*
      %give   %fact
      ~[/primary]  %json
      !>((update-to-json !<(invite-update q.cage.sign)))
    ==  ==
  ==
::
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
