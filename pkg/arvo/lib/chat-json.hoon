/-  *chat-store, *chat-view
|%
::
++  slan  |=(mod/@tas |=(txt/@ta (need (slaw mod txt))))
::
++  seri                                              :::  serial
  =,  dejs:format
  ^-  $-(json serial)
  (cu (slan %uv) so)
::
++  re                                                ::  recursive reparsers
  |*  {gar/* sef/_|.(fist:dejs-soft:format)}
  |=  jon/json
  ^-  (unit _gar)
  =-  ~!  gar  ~!  (need -)  -
  ((sef) jon)
::
++  dank                                              ::  tank
  ^-  $-(json (unit tank))
  =,  ^?  dejs-soft:format
  %+  re  *tank  |.  ~+
  %-  of  :~
    leaf+sa
    palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
    rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
  ==
::
++  eval                                              :::  %exp speech
  :::  extract contents of an %exp speech, evaluating
  :::  the {exp} if there is no {res} yet.
  |=  a=json
  ^-  [cord (list tank)]
  =,  ^?  dejs-soft:format
  =/  exp  ((ot expression+so ~) a)
  %-  need
  ?~  exp
    [~ '' ~]
  :+  ~  u.exp
  =/  res  ((ot output+(ar dank) ~) a)
  ?^  res
    u.res
  p:(mule |.([(sell (slap !>(..^zuse) (ream u.exp)))]~))  ::TODO  oldz
::
++  lett
  |=  =letter
  ^-  json
  =,  enjs:format
  ?-  -.letter
      %text
    (frond %text s+text.letter)
  ::
      %url
    (frond %url s+url.letter)
  ::
      %code
    %+  frond  %code
    %-  pairs
    :~  [%expression s+expression.letter]
        [%output a+(turn output.letter tank)]
    ==
  ::
      %me
    (frond %me s+narrative.letter)
  ::
  ==
::
++  enve
  |=  =envelope
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%uid s+(scot %uv uid.envelope)]
      [%number (numb number.envelope)]
      [%author (ship author.envelope)]
      [%when (time when.envelope)]
      [%letter (lett letter.envelope)]
  ==
::
++  conf
  |=  =config
  ^-  json
  =,  enjs:format
  %-  pairs
  :~  [%length (numb length.config)]
      [%read (numb read.config)]
  ==
::
++  inbox-to-configs
  |=  =inbox
  ^-  chat-configs
  %-  ~(run by inbox)
  |=  =mailbox
  ^-  config
  config.mailbox
::
++  configs-to-json
  |=  cfg=chat-configs
  =,  enjs:format
  ^-  json
  %+  frond  %chat-configs
  %-  pairs
  %+  turn  ~(tap by cfg)
  |=  [pax=^path =config]
  ^-  [cord json]
  [(spat pax) (conf config)]
::
++  inbox-to-json
  |=  box=inbox
  =,  enjs:format
  ^-  json
  %+  frond  %chat-initial
  %-  pairs
  %+  turn  ~(tap by box)
  |=  [pax=^path =mailbox]
  ^-  [cord json]
  :-  (spat pax)
  %-  pairs
  :~  [%envelopes [%a (turn envelopes.mailbox enve)]]
      [%config (conf config.mailbox)]
  ==
::
++  update-to-json
  |=  upd=chat-update
  =,  enjs:format
  ^-  json
  %+  frond  %chat-update
  %-  pairs
  :~
    ::
    ::  %message
    ?:  =(%message -.upd)
      ?>  ?=(%message -.upd)
      :-  %message
      %-  pairs
      :~  [%path (path path.upd)]
          [%envelope (enve envelope.upd)]
      ==
    ::
    ::  %read
    ?:  =(%read -.upd)
      ?>  ?=(%read -.upd)
      [%read (pairs [%path (path path.upd)]~)]
    ::
    ::  %create
    ?:  =(%create -.upd)
      ?>  ?=(%create -.upd)
      :-  %create
      %-  pairs
      :~  [%ship (ship ship.upd)]
          [%path (path path.upd)]
      ==
    ::
    ::  %delete
    ?:  =(%delete -.upd)
      ?>  ?=(%delete -.upd)
      [%delete (pairs [%path (path path.upd)]~)]
    ::
    ::  %config
    ?:  =(%config -.upd)
      ?>  ?=(%config -.upd)
      :-  %config
      %-  pairs
      :~  [%path (path path.upd)]
          [%config (conf config.upd)]
      ==
    ::
    ::  %noop
    [*@t *^json]
  ==
::
++  json-to-action
  |=  jon=json
  ^-  chat-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create create]
        [%delete delete]
        [%message message]
        [%read read]
    ==
  ::
  ++  create
    %-  ot
    :~  [%ship (su ;~(pfix sig fed:ag))]
        [%path pa]
    ==
  ::
  ++  delete
    (ot [%path pa]~)
  ::
  ++  message
    %-  ot
    :~  [%path pa]
        [%envelope envelope]
    ==
  ::
  ++  read
    (ot [%path pa] ~)
  ::
  ++  envelope
    %-  ot
    :~  [%uid seri]
        [%number ni]
        [%author (su ;~(pfix sig fed:ag))]
        [%when di]
        [%letter letter]
    ==
  ::
  ++  letter
    %-  of
    :~  [%text so]
        [%url so]
        [%code eval]
        [%me so]
    ==
  ::
  --
::
++  json-to-view-action
  |=  jon=json
  ^-  chat-view-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%create create]
        [%delete delete]
    ==
  ::
  ++  create
    %-  ot
    :~  [%path pa]
        [%security sec]
        [%read (as (su ;~(pfix sig fed:ag)))]
        [%write (as (su ;~(pfix sig fed:ag)))]
    ==
  ::
  ++  delete
    (ot [%path pa]~)
  ::
  ++  sec
    =,  dejs:format
    ^-  $-(json chat-security)
    (su (perk %channel %village %journal %mailbox ~))
  --
--

