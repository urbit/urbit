::  invite-hook [landscape]: receive invites from any source
::
::    only handles %invite actions:
::    - can be poked by the host team to send an invite out to someone.
::    - can be poked by foreign ships to send an invite to us.
::
/-  *invite-store
/+  default-agent, dbug
::
|%
+$  state-0  [%0 ~]
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  :_  this
  ?+  mark  (on-poke:def mark vase)
      %invite-action
    =/  act=action  !<(action vase)
    ?+  -.act     ~
        %invites
      ?.  =,(bowl =(our src))  ~
      ::  outgoing. we must be inviting other ships. send them each an invite
      ::
      %+  turn  ~(tap in recipients.invites.act)
      |=  recipient=ship
      ^-  card
      ?<  =,(bowl =(our recipient))
      %+  invite-hook-poke  recipient
      :^  %invite  term.act  uid.act
      ^-  invite
      :*  ship.invites.act
          app.invites.act
          resource.invites.act
          recipient
          text.invites.act
      ==
    ::
        %invite
      ?:  =,(bowl =(our src))
        ::  outgoing. we must be inviting another ship. send them the invite.
        ::
        ?<  =(our.bowl recipient.invite.act)
        [(invite-hook-poke recipient.invite.act act)]~
      ::  else incoming. ensure invitatory exists and invite is not a duplicate.
      ::
      ?>  ?=(^ (invitatory-scry term.act))
      ?>  ?=(~ (invite-scry term.act uid.act))
      [(invite-poke term.act act)]~
    ==
  ==
  ::
  ++  invite-hook-poke
    |=  [=ship =action]
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
    |=  [=term =action]
    ^-  card
    :*  %pass
        /[term]
        %agent
        [our.bowl %invite-store]
        %poke
        %invite-action
        !>(action)
    ==
  ::
  ++  invitatory-scry
    |=  =term
    .^  (unit invitatory)
        %gx
        %+  weld
          /(scot %p our.bowl)/invite-store/(scot %da now.bowl)/invitatory
        /[term]/noun
    ==
  ::
  ++  invite-scry
    |=  [=term uid=serial]
    .^  (unit invite)
        %gx
        %+  weld
          /(scot %p our.bowl)/invite-store/(scot %da now.bowl)/invite
        /[term]/(scot %uv uid)/noun
    ==
  --
::
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
