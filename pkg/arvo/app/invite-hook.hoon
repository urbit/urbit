::  invite-hook: receive invites from any source
::
::    only handles %invite actions. accepts json, but only from the host team.
::    can be poked by the host team to send an invite out to someone.
::    can be poked by foreign ships to send an invite to us.
::
/+  *invite-json, default-agent, verb, dbug
::
|%
+$  state-0  [%0 ~]
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    [~ this]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    :_  this
    ?+  mark  (on-poke:def mark vase)
        %json
      ::  only accept json from ourselves.
      ::
      ?>  (team:title our.bowl src.bowl)
      =/  act  (json-to-action !<(json vase))
      ?>  ?=(%invite -.act)
      [(invite-hook-poke:do recipient.invite.act act)]~
    ::
        %invite-action
      =/  act=invite-action  !<(invite-action vase)
      ?.  ?=(%invite -.act)  ~
      ?:  (team:title our.bowl src.bowl)
        ::  outgoing. we must be inviting another ship. send them the invite.
        ::
        ?<  (team:title our.bowl recipient.invite.act)
        [(invite-hook-poke:do recipient.invite.act act)]~
      ::  else incoming. ensure invitatory exists and invite is not a duplicate.
      ::
      ?>  ?=(^ (invitatory-scry:do path.act))
      ?>  ?=(~ (invite-scry:do path.act uid.act))
      [(invite-poke:do path.act act)]~
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
++  invite-hook-poke
  |=  [=ship action=invite-action]
  ^-  card
  :*  %pass
      /invite-hook
      %agent
      [ship %invite-hook]
      %poke
      %invite-action
      !>(action)
  ==
::
++  invite-poke
  |=  [=path action=invite-action]
  ^-  card
  :*  %pass
      path
      %agent
      [our.bowl %invite-store]
      %poke
      %invite-action
      !>(action)
  ==
::
++  invitatory-scry
  |=  pax=path
  ^-  (unit invitatory)
  =.  pax
    ;:(weld /=invite-store/(scot %da now.bowl)/invitatory pax /noun)
  .^((unit invitatory) %gx pax)
::
++  invite-scry
  |=  [pax=path uid=serial]
  ^-  (unit invite)
  =.  pax
    ;:(weld /=invite-store/(scot %da now.bowl)/invite pax /(scot %uv uid)/noun)
  .^((unit invite) %gx pax)
--

