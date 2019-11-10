/-  *invite-store
|%
++  slan  |=(mod/@tas |=(txt/@ta (need (slaw mod txt))))
::
++  seri                                              :::  serial
  =,  dejs:format
  ^-  $-(json serial)
  (cu (slan %uv) so)
::
++  invites-to-json
  |=  inv=invites
  ^-  json
  %+  frond:enjs:format  %invite-initial
  %-  pairs:enjs:format
  %+  turn  ~(tap by inv)
  |=  [=path =invitatory]
  ^-  [cord json]
  [(spat path) (invitatory-to-json invitatory)]
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
      [%path (path path.invite)]
      [%recipient (ship recipient.invite)]
      [%text [%s text.invite]]
  ==
::
++  update-to-json
  |=  upd=invite-update
  =,  enjs:format
  ^-  json
  %+  frond  %invite-update
  %-  pairs
  :~
    ?:  =(%create -.upd)
      ?>  ?=(%create -.upd)
      [%create (pairs [%path (path path.upd)]~)]
    ?:  =(%delete -.upd)
      ?>  ?=(%delete -.upd)
      [%delete (pairs [%path (path path.upd)]~)]
    ?:  =(%accepted -.upd)
      ?>  ?=(%accepted -.upd)
      :-  %accepted
      %-  pairs
      :~  [%path (path path.upd)]
          [%uid s+(scot %uv uid.upd)]
          [%invite (invite-to-json invite.upd)]
      ==
    ?:  =(%decline -.upd)
      ?>  ?=(%decline -.upd)
      :-  %decline
      %-  pairs
      :~  [%path (path path.upd)]
          [%uid s+(scot %uv uid.upd)]
      ==
    ?:  =(%invite -.upd)
      ?>  ?=(%invite -.upd)
      :-  %invite
      %-  pairs
      :~  [%path (path path.upd)]
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
  ^-  invite-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create create]
        [%delete delete]
        [%invite invite]
        [%accept accept]
        [%decline decline]
    ==
  ::
  ++  create
    (ot [%path pa]~)
  ::
  ++  delete
    (ot [%path pa]~)
  ::

  ++  invite
    %-  ot
    :~  [%path pa]
        [%uid seri]
        [%invite invi]
    ==
  ::
  ++  accept
    %-  ot
    :~  [%path pa]
        [%uid seri]
    ==
  ::
  ++  decline
    %-  ot
    :~  [%path pa]
        [%uid seri]
    ==
  ::
  ++  invi
    %-  ot
    :~  [%ship (su ;~(pfix sig fed:ag))]
        [%app (se %tas)]
        [%path pa]
        [%recipient (su ;~(pfix sig fed:ag))]
        [%text so]
    ==
  --
--

