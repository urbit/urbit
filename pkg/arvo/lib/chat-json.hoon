/-  *chat-store, *chat-hook, *chat-view
/+  chat-eval
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
  ::
  |=  a=json
  ^-  [cord (list tank)]
  =,  ^?  dejs-soft:format
  =+  exp=((ot expression+so ~) a)
  %-  need
  ?~  exp  [~ '' ~]
  :+  ~  u.exp
  ::NOTE  when sending, if output is an empty list, chat-store will evaluate
  (fall ((ot output+(ar dank) ~) a) ~)
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
++  hook-update-to-json
  |=  upd=chat-hook-update
  =,  enjs:format
  ^-  json
  %+  frond  %chat-hook-update
  %-  pairs
  %+  turn  ~(tap by synced.upd)
  |=  [pax=^path shp=^ship]
  ^-  [cord json]
  [(spat pax) s+(scot %p shp)]
::
++  update-to-json
  |=  upd=chat-update
  =,  enjs:format
  ^-  json
  %+  frond  %chat-update
  %-  pairs
  :~
    ?:  ?=(%message -.upd)
      :-  %message
      %-  pairs
      :~  [%path (path path.upd)]
          [%envelope (enve envelope.upd)]
      ==
    ?:  ?=(%messages -.upd)
      :-  %messages
      %-  pairs
      :~  [%path (path path.upd)]
          [%start (numb start.upd)]
          [%end (numb end.upd)]
          [%envelopes [%a (turn envelopes.upd enve)]]
      ==
    ?:  ?=(%read -.upd)
      [%read (pairs [%path (path path.upd)]~)]
    ?:  ?=(%create -.upd)
      [%create (pairs [%path (path path.upd)]~)]
    ?:  ?=(%delete -.upd)
      [%delete (pairs [%path (path path.upd)]~)]
    ?:  ?=(%config -.upd)
      :-  %config
      %-  pairs
      :~  [%path (path path.upd)]
          [%config (conf config.upd)]
      ==
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
        [%messages messages]
        [%read read]
    ==
  ::
  ++  create
    (ot [%path pa]~)
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
  ++  messages
    %-  ot
    :~  [%path pa]
        [%envelopes (ar envelope)]
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
++  json-to-hook-action
  |=  jon=json
  ^-  chat-hook-action
  =,  dejs:format
  =<  (parse-json jon)
  |%
  ++  parse-json
    %-  of
    :~  [%add-owned add-owned]
        [%add-synced add-synced]
        [%remove pa]
    ==
  ::
  ++  add-owned
    %-  ot
    :~  [%path pa]
        [%allow-history bo]
    ==
  ::
  ++  add-synced
    %-  ot
    :~  [%ship (su ;~(pfix sig fed:ag))]
        [%path pa]
        [%ask-history bo]
    ==
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
        [%join join]
        [%groupify groupify]
    ==
  ::
  ++  create
    %-  ot
    :~  [%title so]
        [%description so]
        [%app-path pa]
        [%group-path pa]
        [%security sec]
        [%members (as (su ;~(pfix sig fed:ag)))]
        [%allow-history bo]
    ==
  ::
  ++  delete
    (ot [%app-path pa]~)
  ::
  ++  join
    %-  ot
    :~  [%ship (su ;~(pfix sig fed:ag))]
        [%app-path pa]
        [%ask-history bo]
    ==
  ::
  ++  groupify
    =-  (ot [%app-path pa] [%existing -] ~)
    (mu (ot [%group-path pa] [%inclusive bo] ~))
  ::
  ++  sec
    =,  dejs:format
    ^-  $-(json rw-security)
    (su (perk %channel %village %journal %mailbox ~))
  --
--
