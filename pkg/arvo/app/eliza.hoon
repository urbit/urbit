::  eliza: questionnaire bot
::
::    creates dms with users and asks them survey questions,
::    but not before asking them if they want to participate.
::
::    usage:
::    - ask ~zod and ~bus the questions from the feedback-2021-01 survey:
::      :eliza [%initiate %feedback-2021-01 ~[~zod ~bus]]
::    - write results to csv files in /app/eliza/:
::      :eliza [%export ~]
::
::NOTE  implementation details:
::  - if it exists in afairs, the chat has been created
::
/-  graph=graph-store, graph-view
/+  *eliza, re=resource, dbug, verb, default-agent
/~  chains  chain  /app/eliza
::
|%
+$  state-1
  $:  %1
      afairs=(map ship afair)
      record=(jar study [when=@da =ship data=(map place datum)])
  ==
::
+$  study  @tasstudy
::  $afair: current relation to ship
::
+$  afair
  $%  [%quiet ~]
      [%block ~]
      [%convo =convo]
  ==
::  $convo: active conversation with ship
::
+$  convo
  $:  what=study
      back=(lest place)
      data=(map place datum)
  ==
::  $reply: user response
::
+$  reply
  $%  [%help ~]                                         ::  ask for usage help
      [%stop ~]                                         ::  ask to block bot
      [%back ~]                                         ::  to previous point
      [%wild ~]                                         ::  unparsable response
      [%nice d=datum]                                   ::  response to query
  ==
::  $voice: internal effect (on convo) caused by reply
::
+$  voice
  $:  tell=(list @t)
  $=  know
  $%  [%same ~]
      [%stop ~]
      [%back ~]
      [%move p=place]
      [%done ~]
  ==  ==
::
+$  action
  $%  [?(%initiate %resend) =study who=(list ship)]
      [%export ~]
      [%nuke ~]
  ==
::
+$  card  card:agent:gall
--
::
=|  state-1
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
     [[listen:talk:do]~ this]
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    |^  ^-  (quip card _this)
        =/  any-state   !<($%(state-0 state-1) old)
        =.  state
          ?-  -.any-state
            %1  any-state
            %0  [%1 afairs record]:any-state
          ==
        sanity-check
    ::
    ++  sanity-check
      =/  studies=(set study)
        %-  ~(gas in ~(key by record))
        %+  murn  ~(tap by afairs)
        |=  [* a=afair]
        ?:(?=(%convo -.a) (some what.convo.a) ~)
      ~|  [%need-studies studies]
      ?>  (levy ~(tap in studies) ~(has by chains))
      [~ this]
    ::
    +$  state-0
      $:  %0
          afairs=(map ship afair)
          record=(jar study [when=@da =ship data=(map place datum)])
          chains=(map study chain)
      ==
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
      %noun  $(mark %eliza-action)
    ::
        %eliza-action
      =/  =action  !<(action vase)
      ?-  -.action
        %export  [[export:do]~ this]
      ::
          ?(%initiate %resend)
        ?.  (~(has by chains) study.action)
          ~&  [%no-such-study study.action]
          [~ this]
        =/  [skip=(list ship) send=(list ship)]
          ?:  ?=(%resend -.action)  [who.action ~]
          %+  skid  who.action
          %~  has  in
          %-  ~(gas in *(set ship))
          %+  turn  (~(get ja record) study.action)
          |=([* s=ship *] s)
        ~?  !=(~ skip)
          ['skipping, already answered previously' skip]
        =|  cards=(list card)
        |-
        ?~  send
          [cards this]
        =^  caz  state
          (initiate:do i.send study.action)
        $(cards (weld caz cards), send t.send)
      ::
          %nuke
        =-  [(zing -) this(afairs ~)]
        (turn ~(tap in ~(key by afairs)) delete:talk:do)
      ==
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  wire  (on-agent:def wire sign)
        [%create @ @ ~]
      =*  ship   (slav %p i.t.wire)
      =*  study  i.t.t.wire
      ?-  -.sign
          %poke-ack
        ?~  p.sign  [~ this]
        =/  msg=tape
          "{(trip dap.bowl)} failed to create chat with {(scow %p ship)}"
        %-  (slog leaf+msg u.p.sign)
        ::TODO  can we figure out why? can we retry or something?
        [~ this]
      ::
          %watch-ack
        ?~  p.sign  [~ this]
        =/  msg=tape
          "{(trip dap.bowl)} failed to watch thread for {(scow %p ship)}"
        %-  (slog leaf+msg u.p.sign)
        ::TODO  this _shouldn't_ happen, right?
        [~ this]
      ::
          %kick
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          =/  msg=tape
            "{(trip dap.bowl)} failed to create chat with {(scow %p ship)}"
          %-  (slog leaf+msg tang)
          ::TODO  should we retry? can we figure out why it failed?
          [~ this]
        ::
            %thread-done
          =^  cards  state
            (initiate-part-2:do & ship study)
          [cards this]
        ==
      ==
    ::
        [%delete @ ~]
      =*  ship   (slav %p i.t.wire)
      ~|  [%deletion ship]
      (on-agent:def wire sign)
    ::
        [%listen ~]
      ?-  -.sign
        %poke-ack  (on-agent:def wire sign)
        %kick      [[listen:talk:do]~ this]
      ::
          %watch-ack
        ?~  p.sign  [~ this]
        =/  msg=tape
          "{(trip dap.bowl)} failed to watch graph-store"
        %-  (slog leaf+msg u.p.sign)
        ::TODO  this _shouldn't_ happen, right?
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %graph-update-1
          =/  upd  !<(update:graph q.cage.sign)
          =^  cards  state
            (process-graph-update:do q.upd)
          [cards this]
        ==
      ==
    ==
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  chat-name         'Tlon survey bot'
++  chat-description  'Send "STOP" to opt out.'
++  chat-id
  |=  =ship
  ^-  resource:re
  [our.bowl (cat 3 %tlon-survey-bot- (scot %p ship))]
::  +talk: graph card creation
::
++  talk
  |%
  ++  listen
    ^-  card
    [%pass /listen %agent [our.bowl %graph-store] %watch /updates]
  ::
  ++  create  ::NOTE  creates and watches a thread because, uh...
    |=  [=ship =study]
    ^-  (list card)
    =/  tid=@ta  (cat 3 'eliza-create-' (scot %p ship))
    =;  =action:graph-view
      =/  args   [~ `tid %graph-create !>([~ action])]
      =/  =wire  /create/(scot %p ship)/[study]
      :~  [%pass wire %agent [our.bowl %spider] %watch /thread-result/[tid]]
          [%pass wire %agent [our.bowl %spider] %poke %spider-start !>(args)]
      ==
    :*  %create
        (chat-id ship)
        chat-name
        chat-description
        `%graph-validator-chat
        [%policy %invite (sy ship ~)]
        'chat'
    ==
  ::
  ++  delete
    |=  =ship
    ^-  (list card)
    =/  tid=@ta  (cat 3 'eliza-delete-' (scot %p ship))
    =/  =action:graph-view
      [%delete (chat-id ship)]
    =/  args   [~ `tid %graph-delete !>([~ action])]
    =/  =wire  /delete/(scot %p ship)
    :~  [%pass wire %agent [our.bowl %spider] %watch /thread-result/[tid]]
        [%pass wire %agent [our.bowl %spider] %poke %spider-start !>(args)]
    ==
  ::
  ++  send
    |=  [=ship msgs=(list @t)]
    ^-  card
    =;  upd=update:graph
      [%pass / %agent [our.bowl %graph-store] %poke %graph-update-1 !>(upd)]
    ::TODO  this is api, man... move into lib or w/e
    =|  nodes=(list [index:graph node:graph])
    |-  ^-  upd=update:graph
    ?~  msgs
      :-  now.bowl
      :+  %add-nodes  (chat-id ship)
      (~(gas by *(map index:graph node:graph)) nodes)
    =.  now.bowl  +(now.bowl)  ::NOTE  so we don't re-use indices
    =-  $(nodes [- nodes], msgs t.msgs)
    :-  [now.bowl]~
    :_  [%empty ~]
    ^-  post:graph
    :*  our.bowl
        [now.bowl]~
        now.bowl
        [%text i.msgs]~
        ~
        ~
    ==
  --
::  +export: export all records to per-study files
::
++  export
  ^-  card
  =-  [%pass /export %arvo %c %info %home %& -]
  ^-  soba:clay
  %+  turn  ~(tap by record)
  |=  [=study res=(list [@da ship (map place datum)])]
  ^-  [path miso:clay]
  =/  =path   /app/[dap.bowl]/[study]/txt
  =/  =chain  (~(got by chains) study)
  =/  queso   %+  skim  `(list place)`flow.chain
              |=(p=place ?=(^ (~(got by bits.chain) p)))
  :-  path
  %+  feel:space:userlib
    [(scot %p our.bowl) %home (scot %da now.bowl) path]
  :-  %txt
  !>  ^-  (list @t)
  :-  (rap 3 (join ',' `(list @t)`['when' 'ship' queso]))
  %+  turn  res
  |=  [when=@da =ship data=(map place datum)]
  ^-  @t
  %+  rap   3
  %+  join  ','
  ^-  (list @t)
  :+  (scot %da when)
    (scot %p ship)
  %+  turn  queso
  |=  =place
  ?~  datum=(~(get by data) place)  ''
  |^
  ?-  -.u.datum
    %loose  (safe d.u.datum)
    %yesno  ?:(d.u.datum 'yes' 'no')
    %multi  =+  q=(~(got by bits.chain) place)
            |-
            ?:  ?=([%drift *] q)  $(q q.q)
            ?>  ?=([%multi *] q)
            (safe (snag d.u.datum a.q))
  ==
  ::
  ++  safe
    |=  t=@t
    :((cury cat 3) '"' t '"')  ::TODO  escape quotes
  --
::  +initiate: open chat & start survey with ship
::
++  initiate
  |=  [=ship =study]
  ^-  (quip card _state)
  ?~  far=(~(get by afairs) ship)
    ~&  [%opening-chat-with ship study]
    [(create:talk ship study) state]
  ?-  -.u.far
    %quiet  (initiate-part-2 | ship study)
    %block  ~&  [ship 'doesn\'t want to talk to the bot']  [~ state]
    %convo  ~&  [ship 'is still talking to the bot']       [~ state]
  ==
::  +initiate-part-2: start survey with ship
::
++  initiate-part-2
  |=  [new=? =ship =study]
  ^-  (quip card _state)
  ~&  [%starting-convo-with ship study]
  =/  =chain  (~(got by chains) study)
  =/  =convo  [study [i.flow.chain]~ ~]
  =.  afairs  (~(put by afairs) ship %convo convo)
  :_  state
  ::NOTE  we don't care about skipping past @t points here,
  ::      because we inject a consent query at the start of every chain.
  :_  ~
  %+  send:talk  ship
  =-  ?.(new - [hiya:phrases -])
  [(point:render (~(got by bits.chain) i.flow.chain))]~
::  +process-graph-update: per-response logic
::
++  process-graph-update
  |=  upd=action:graph
  ^-  (quip card _state)
  ::  relevancy and sanity checks
  ::
  ?.  ?=(%add-nodes -.upd)
    [~ state]
  =/  msgs=(list [=index:graph node:graph])  ~(tap by nodes.upd)
  =|  cards=(list card)
  |-
  ?~  msgs  [cards state]
  =;  [caz=(list card) nas=_state]
    =.  state  nas
    =.  cards  (weld caz cards)
    $(msgs t.msgs)
  =*  post  post.i.msgs
  =*  ship  author.post
  ?:  =(our.bowl ship)
    [~ state]
  ?.  (~(has by afairs) ship)
    ~&  [dap.bowl %no-convo-with ship]
    [~ state]
  ?.  =((chat-id ship) resource.upd)
    ~&  [dap.bowl %weird-resource resource.upd]
    [~ state]
  ::  conversational logic
  ::
  =/  =afair   (~(got by afairs) ship)
  ?.  ?=(%convo -.afair)  [~ state]
  =/  =convo   convo.afair
  =/  text=@t  (fold-post-content contents.post)
  =/  =reply   (parse-response convo text)
  =.  convo    (store-response convo reply)
  =/  =voice   (converse ship convo reply)
  ::  effects on state
  ::
  :-  [(send:talk ship tell.voice)]~
  ?:  ?=(%stop -.know.voice)
    =.  afairs  (~(put by afairs) ship %block ~)
    ::TODO  should delete chat?
    state
  ::
  ?:  ?=(%done -.know.voice)
    =.  afairs  (~(put by afairs) ship %quiet ~)
    =.  record  (~(add ja record) what.convo [now.bowl ship data.convo])
    state
  ::
  =;  =_convo
    =.  afairs  (~(put by afairs) ship %convo convo)
    state
  ?-  -.know.voice
    %same  convo
    %back  ?>(?=(^ t.back.convo) convo(back t.back.convo))
    %move  convo(back [p.know.voice back.convo])
  ==
::  +fold-post-content: collapse into @t post body
::
++  fold-post-content
  |=  contents=(list content:graph)
  ^-  @t
  %+  rap   3
  %+  join  ' '
  %+  turn  contents
  |=  c=content:graph
  ?+  -.c  ''
    %text     text.c
    %mention  (scot %p ship.c)
    %url      url.c
  ==
::  +parse-response: reply from post body
::
++  parse-response
  |=  [=convo text=@t]
  ^-  reply
  =;  r=(unit reply)
    (fall r [%wild ~])
  ?:  =('STOP' text)  `[%stop ~]
  ?:  =('HELP' text)  `[%help ~]
  ?:  =('BACK' text)  `[%back ~]
  ::
  =/  =query
    =+  (~(got by chains) what.convo)
    =/  =point  (~(got by bits) i.back.convo)
    ?<(?=(@t point) point)
  ::
  =-  (bind - (lead %nice))
  |^  ^-  (unit datum)
  ?-  -.query
    %loose  `[%loose text]
    %yesno  (rest (stag %yesno yeanay))
    %multi  (rest (stag %multi (picky (lent a.query))))
    %drift  $(query q.query)
  ==
  ::
  ++  rest    |*(r=rule (rust (cass (trip text)) r))
  ::
  ++  punc
    |*  r=rule
    =-  (ifix [- -] r)
    (punt ;~(pose (stun [1 3] dot) zap wut))
  ::
  ++  yeanay  (punc ;~(pose (cold & yea) (cold | nay)))
  ++  yea     (perk 'yes' 'yeah' 'yea' 'sure' ~)
  ++  nay     (perk 'nope' 'no' 'nah' ~)
  ::
  ++  picky
    |=  num=@ud
    %-  punc
    %+  ifix  [(punt pal) (punt par)]
    %+  cook  (curr sub 'a')
    (shim 'a' (add 'a' (dec num)))
  --
::  +store-response: if it's a datum, store it
::
++  store-response
  |=  [=convo =reply]
  ^+  convo
  ?.  ?=(%nice -.reply)  convo
  convo(data (~(put by data.convo) i.back.convo d.reply))
::  +converse: process response into conversation update
::
++  converse
  |=  [=ship =convo =reply]
  ^-  voice
  =/  =chain  (~(got by chains) what.convo)
  =/  =point  (~(got by bits.chain) i.back.convo)
  ?-  -.reply
      %help
    :_  [%same ~]
    ['Say BACK to go to the previous question. Say STOP to be left alone.']~
  ::
      %stop
    [[stop:phrases]~ %stop ~]
  ::
      %back
    ?~  t.back.convo
      :_  [%same ~]
      ~[buck:phrases (point:render point)]
    :_  [%back ~]
    ~[back:phrases (point:render (~(got by bits.chain) i.t.back.convo))]
  ::
      %wild
    :_  [%same ~]
    :~  dumb:phrases
      ::
        ?<  ?=(@ point)
        %^  cat  3
          (point:render point)
        :((cury cat 3) '\0a(' (expectation:render point) ')')
    ==
  ::
      %nice
    =|  out=(list @t)
    =/  =place  i.back.convo
    |-
    =/  nap=(unit ^place)
      (next-place place d.reply chain)
    ?~  nap  [out %done ~]  ::TODO  done msg
    =.  place  u.nap
    =/  =^point  (~(got by bits.chain) place)
    ?@  point  $(out (snoc out point))
    :_  [%move place]
    %+  snoc  out
    (point:render point)
  ==
::  +next-place: determine next sequence based on response, ~ if end
::
++  next-place
  |=  [here=place hear=datum =chain]
  ^-  (unit place)
  =/  spot=point  (~(got by bits.chain) here)
  ?:  ?=([%drift *] spot)
    ~|  [here hear]
    `(next.spot hear)
  |-
  ?~  t.flow.chain  ~
  ?:  =(here i.flow.chain)  `i.t.flow.chain
  $(flow.chain t.flow.chain)
::  +render: to @t
::
++  render
  |%
  ::  +point: string of query
  ::
  ++  point
    |=  =^point
    ^-  @t
    ?@  point  (unbreak point)
    ?+  -.point  (unbreak q.point)
      %drift  $(point q.point)
    ::
        %multi
      %+  rap  3
      %+  join  '\0a'
      :-  q.point
      =+  i='A'
      |-
      ?~  a.point  ~
      :_  $(i +(i), a.point t.a.point)
      :((cury cat 3) i ') ' i.a.point)
    ==
  ::  +expectation: description of expected response for query
  ::
  ++  expectation
    |=  =query
    ^-  @t
    ?-  -.query
      %loose  ''
      %yesno  'yes or no'
      %multi  (cat 3 'choose one of A through ' (add 'A' (lent a.query)))
      %drift  $(query q.query)
    ==
  ::  +unbreak: replace newlines with spaces
  ::
  ++  unbreak
    |=  t=@t
    (crip (turn (trip t) |=(c=@ ?:(=('\0a' c) ' ' c))))
  --
::  +phrases: built-in personality
::
++  phrases
  |%
  ++  hiya
    '''
    Hello! I'm Eliza, a questionairre bot working for Tlon. Say BACK to undo
    an answer. Say STOP, and I will never bother you again.
    '''
  ++  dumb
    '''
    I'm not very smart yet. Could you repeat that in a way I can understand?
    '''
  ++  back  'Alright, let\'s take a step back.'
  ++  buck  'This is as far back as we can go.'
  ++  stop  'Alright, sorry for bothering you. Goodbye forever.'
  --
--
