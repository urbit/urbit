/-  *invite-store
/+  resource
|%
++  slan  |=(mod=@tas |=(txt=@ta (need (slaw mod txt))))
::
++  seri                                              :::  serial
  =,  dejs:format
  ^-  $-(json serial)
  (cu (slan %uv) so)
::
++  invites-to-json
  |=  inv=invites
  ^-  json
  %-  pairs:enjs:format
  %+  turn  ~(tap by inv)
  |=  [=term =invitatory]
  ^-  [cord json]
  [term (invitatory-to-json invitatory)]
::
++  invitatory-to-json
  |=  =invitatory
  ^-  json
  =,  enjs:format
  %-  pairs
  %+  turn  ~(tap by invitatory)
  |=  [=serial =invite]
  ^-  [cord json]
  [(scot %uv serial) (invite-to-json invite)]
::
++  invite-to-json
  |=  =invite
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%ship (ship ship.invite)]
      [%app [%s app.invite]]
      [%resource (enjs:resource resource.invite)]
      [%recipient (ship recipient.invite)]
      [%text [%s text.invite]]
  ==
::
++  update-to-json
  |=  upd=update
  =,  enjs:format
  ^-  json
  %+  frond  %invite-update
  %-  pairs
  :~
    ?:  =(%initial -.upd)
      ?>  ?=(%initial -.upd)
      [%initial (invites-to-json invites.upd)]
    ?:  =(%create -.upd)
      ?>  ?=(%create -.upd)
      [%create (pairs [%term s+term.upd]~)]
    ?:  =(%delete -.upd)
      ?>  ?=(%delete -.upd)
      [%delete (pairs [%term s+term.upd]~)]
    ?:  =(%accepted -.upd)
      ?>  ?=(%accepted -.upd)
      :-  %accepted
      %-  pairs
      :~  [%term s+term.upd]
          [%uid s+(scot %uv uid.upd)]
          [%invite (invite-to-json invite.upd)]
      ==
    ?:  =(%decline -.upd)
      ?>  ?=(%decline -.upd)
      :-  %decline
      %-  pairs
      :~  [%term s+term.upd]
          [%uid s+(scot %uv uid.upd)]
      ==
    ?:  =(%invite -.upd)
      ?>  ?=(%invite -.upd)
      :-  %invite
      %-  pairs
      :~  [%term s+term.upd]
          [%uid s+(scot %uv uid.upd)]
          [%invite (invite-to-json invite.upd)]
      ==
    ?:  =(%invitatory -.upd)
      ?>  ?=(%invitatory -.upd)
      :-  %invitatory
      (invitatory-to-json invitatory.upd)
    ::
    ::  %noop
    [*@t *json]
  ==
::
++  json-to-action
  |=  jon=json
  ^-  action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create so]
        [%delete so]
        [%invite invite]
        [%accept accept]
        [%decline decline]
    ==
  ::
  ++  invite
    %-  ot
    :~  [%term so]
        [%uid seri]
        [%invite invi]
    ==
  ::
  ++  accept
    %-  ot
    :~  [%term so]
        [%uid seri]
    ==
  ::
  ++  decline
    %-  ot
    :~  [%term so]
        [%uid seri]
    ==
  ::
  ++  invi
    %-  ot
    :~  [%ship (su ;~(pfix sig fed:ag))]
        [%app so]
        [%resource dejs:resource]
        [%recipient (su ;~(pfix sig fed:ag))]
        [%text so]
    ==
  --
--
