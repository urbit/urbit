/-  sur=chat-store
^?
=<  [sur .]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ::
  ++  letter
    |=  =^letter
    ^-  json
    ?-  -.letter
        %text
      (frond %text s+text.letter)
    ::
        %me
      (frond %me s+narrative.letter)
    ::
        %url
      (frond %url s+url.letter)
    ::
        %code
      %+  frond  %code
      %-  pairs
      :-  [%expression s+expression.letter]
      :_  ~
      :-  %output
      ::  virtualize output rendering, +tank:enjs:format might crash
      ::
      =/  result=(each (list json) tang)
        (mule |.((turn output.letter tank)))
      ?-  -.result
        %&  a+p.result
        %|  a+[a+[%s '[[output rendering error]]']~]~
      ==
    ==
  ::
  ++  envelope
    |=  =^envelope
    ^-  json
    %-  pairs
    :~  [%uid s+(scot %uv uid.envelope)]
        [%number (numb number.envelope)]
        [%author (ship author.envelope)]
        [%when (time when.envelope)]
        [%letter (letter letter.envelope)]
    ==
  ::
  ++  config
    |=  =^config
    ^-  json
    %-  pairs
    :~  [%length (numb length.config)]
        [%read (numb read.config)]
    ==
  ::
  ++  configs
    |=  cfg=^configs
    ^-  json
    %+  frond  %chat-configs
    %-  pairs
    %+  turn  ~(tap by cfg)
    |=  [pax=^path =^config]
    ^-  [cord json]
    [(spat pax) (^config config)]
  ::
  ++  inbox
    |=  box=^inbox
    ^-  json
    %+  frond  %chat-initial
    %-  pairs
    %+  turn  ~(tap by box)
    |=  [pax=^path =mailbox]
    ^-  [cord json]
    :-  (spat pax)
    %-  pairs
    :~  [%envelopes [%a (turn envelopes.mailbox envelope)]]
        [%config (config config.mailbox)]
    ==
  ::
  ++  update
    |=  upd=^update
    ^-  json
    %+  frond  %chat-update
    %-  pairs
    :~
    ?:  ?=(%message -.upd)
        :-  %message
        %-  pairs
        :~  [%path (path path.upd)]
            [%envelope (envelope envelope.upd)]
        ==
    ?:  ?=(%messages -.upd)
        :-  %messages
        %-  pairs
        :~  [%path (path path.upd)]
            [%start (numb start.upd)]
            [%end (numb end.upd)]
            [%envelopes [%a (turn envelopes.upd envelope)]]
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
            [%config (config config.upd)]
        ==
    [*@t *json]
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ::
  ++  action
    |=  jon=json
    ^-  ^action
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
      :~  [%uid serial]
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
    ++  serial
      ^-  $-(json ^serial)
      (cu (cury slav %uv) so)
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
    ::  %exp speech
    ++  eval
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
    --
  --
::
++  inbox-to-configs
  |=  =inbox
  ^-  configs
  %-  ~(run by inbox)
  |=  =mailbox
  ^-  config
  config.mailbox
::
++  eval
  |=  [=bowl:gall =hoon]
  ^-  (list tank)
  =/  fowl=[our=@p now=@da eny=@uvJ]
    :+  our.bowl
      now.bowl
    (shaz (cat 3 (mix [now eny]:bowl) %eny))
  ::
  =/  subject  [fowl ..zuse]
  =/  minted=(each [=type =nock] (list tank))
    %-  mule  |.
    (~(mint ut -:!>(subject)) %noun hoon)
  ?:  ?=(%| -.minted)  p.minted
  =/  =toon
    (mock [subject nock.p.minted] |=(^ ~))
  ?-  -.toon
    %0  [(sell type.p.minted p.toon) ~]
    %1  :-  leaf+".^ unsupported in chat eval"
        (turn ;;((list path) p.toon) smyt)
    %2  [leaf+"crash!" p.toon]
  ==
--
