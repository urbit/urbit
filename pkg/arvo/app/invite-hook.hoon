::  invite-hook: receive invites from any source
::
/+  *invite-json
|%
+$  move  [bone [%poke wire dock [%invite-action invite-action]]]
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
++  poke-json
  |=  =json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  =/  act  (json-to-action json)
  ?>  ?=(%invite -.act)
  :_  this
  [(invite-hook-poke recipient.invite.act act)]~
::
++  poke-invite-action
  |=  act=invite-action
  ^-  (quip move _this)
  :_  this
  ?+  -.act
    ~
  ::
      %invite
    ?:  (team:title our.bol src.bol)
      ?>  !(team:title our.bol ship.invite.act)
      [(invite-hook-poke recipient.invite.act act)]~
    ?>  ?=(^ (invitatory-scry path.act))
    ?>  ?=(~ (invite-scry path.act uid.act))
    [(invite-poke path.act act)]~
  ==
::
++  invite-hook-poke
  |=  [=ship action=invite-action]
  ^-  move
  [ost.bol %poke /invite-hook [ship %invite-hook] [%invite-action action]]
::
++  invite-poke
  |=  [pax=path action=invite-action]
  ^-  move
  [ost.bol %poke pax [our.bol %invite-store] [%invite-action action]]
::
++  invitatory-scry
  |=  pax=path
  ^-  (unit invitatory)
  =.  pax
    ;:(weld /=invite-store/(scot %da now.bol)/invitatory pax /noun)
  .^((unit invitatory) %gx pax)
::
++  invite-scry
  |=  [pax=path uid=serial]
  ^-  (unit invite)
  =.  pax
    ;:(weld /=invite-store/(scot %da now.bol)/invite pax /(scot %uv uid)/noun)
  .^((unit invite) %gx pax)
--

