/-  hall
/+  *server, chat, hall-json
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/index
  /|  /html/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/css/index
  /|  /css/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/css/index
  /|  /css/
      /~  ~
  ==
/=  chat-png
  /^  (map knot @)
  /:  /===/app/chat/img  /_  /png/
::
=,  chat
::
|_  [bol=bowl:gall sta=state]
::
++  this  .
::
::  +prep: set up the app, migrate the state once started
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    =/  inboxpat  /circle/inbox/config/peers
    =/  circlespat  /circles/[(scot %p our.bol)]
    =/  inboxi/poke 
      :-  %hall-action
          [%source %inbox %.y (silt [[our.bol %i] ~]~)]
    :_  this
    :~  [ost.bol %peer inboxpat [our.bol %hall] inboxpat]
        [ost.bol %peer circlespat [our.bol %hall] circlespat]
        [ost.bol %connect / [~ /'~chat'] %chat]
        [ost.bol %poke /chat [our.bol %hall] inboxi]
    ==
  [~ this(sta u.old)]
::
::  +peer-primary: subscribe to our data and updates
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %chat-streams str.sta]~
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  sta
  [~ this]
::
::  +poke-chat: send us an action
::
++  poke-chat-action
  |=  act=action:chat
  ^-  (quip move _this)
  :_  this
  %+  turn  lis.act
    |=  hac=action:hall
    ^-  move
    :*  ost.bol
        %poke
        /p/[(scot %da now.bol)]
        [our.bol %hall]
        [%hall-action hac]
    ==
::
::  +send-chat-update: utility func for sending updates to all our subscribers
::
++  send-chat-update
  |=  upd=update
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [=bone *]
  [bone %diff %chat-update upd]
::
::
::  +hall arms
::
::
::  +diff-hall-prize: handle full state initially handed to us by hall
::
++  diff-hall-prize
  |=  [wir=wire piz=prize:hall]
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
    ::
    ::  %circles wire
    ::
      %circles
    ?>  ?=(%circles -.piz)
      :-  (send-chat-update [%circles cis.piz])
      %=  this
          circles.str.sta  cis.piz
      ==
    ::
    ::  %circle wire
    ::
      %circle
::    ?+  -.piz
::      ::
::      ::  %peers prize
::      ::
::::        %peers
::::      ?>  ?=(%peers -.piz)
::::      [~ this]
::      ::
::      ::  %circle prize
::      ::
::        %circle
      ?>  ?=(%circle -.piz)
      =/  circle/circle:hall  [our.bol &2:wir]
      ?:  =(circle [our.bol %inbox])
    ::
    ::  fill inbox config and remote configs with prize data
    ::
        =/  configs
          %-  ~(uni in configs.str.sta)
          ^-  (map circle:hall (unit config:hall))
          (~(run by rem.cos.piz) |=(a=config:hall `a))
        ~&  pes.piz
        =/  circles/(list circle:hall)
          %+  turn  ~(tap in src.loc.cos.piz)
            |=  src=source:hall
            ^-  circle:hall
            cir.src
        =/  meslis/(list [circle:hall (list envelope:hall)])
          %+  turn  circles
          |=  cir=circle:hall
          ^-  [circle:hall (list envelope:hall)]
          [cir ~]
        :-
          %+  turn  ~(tap in (~(del in (silt circles)) [our.bol %inbox]))
            |=  cir=circle:hall
            ^-  move
            =/  pat/path  /circle/[nom.cir]/config/grams
            [ost.bol %peer pat [our.bol %hall] pat]
          %=  this
              inbox.str.sta  loc.cos.piz
              configs.str.sta  configs
              messages.str.sta  (molt meslis)
          ==
    ::
    ::  fill remote configs with message data
    ::
      =*  messages  messages.str.sta
      =/  circle/circle:hall  [our.bol &2:wir]
      :-  ~
      %=  this
        messages.str.sta  (~(put by messages) circle nes.piz)
      ==
  ==
::
::  +diff-hall-rumor: handle updates to hall state
::
++  diff-hall-rumor
  |=  [wir=wire rum=rumor:hall]
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
  ::  %circles
      %circles
    ?>  ?=(%circles -.rum)
      ?:  add.rum
        =/  cis  (~(put in circles.str.sta) cir.rum)
        :-  (send-chat-update [%circles cis])
        %=  this
            circles.str.sta  cis
        ==
      =/  cis  (~(del in circles.str.sta) cir.rum)
      :-  (send-chat-update [%circles cis])
      %=  this
          circles.str.sta  cis
      ==
  ::
  ::
  ::  %circle: fill remote configs with message data
  ::
      %circle
    ?>  ?=(%circle -.rum)
    =*  sto  rum.rum
    ?+  -.sto
      [~ this]
    ::
    ::  %gram:
    ::
        %gram
    ?>  ?=(%gram -.sto)
    =*  messages  messages.str.sta
    =/  circle/circle:hall  [our.bol &2:wir]
    =/  nes/(list envelope:hall)  (~(got by messages) circle)
    :-  (send-chat-update [%message circle nev.sto])
    %=  this
      messages.str.sta  (~(put by messages) circle (snoc nes nev.sto))
    ==
    ::
    ::  %peer:
    ::
        %peer
    ?>  ?=(%peer -.sto)
    ~&  add.sto
    ~&  who.sto
    ~&  qer.sto
    [~ this]
    ::
    ::  %config: config has changed
    ::
        %config
      =*  circ  cir.sto
      ::
      ?+  -.dif.sto
        [~ this]
      ::
      ::  %full: set all of config without side effects 
      ::
          %full
        =*  conf  cof.dif.sto
        :-  (send-chat-update [%config circ conf])
        %=  this
          configs.str.sta  (~(put by configs.str.sta) circ `conf)
        ==
      ::
      ::  %read: the read count of one of our configs has changed
      ::
          %read
        ?:  =(circ [our.bol %inbox])
          ::  ignore when circ is inbox
          [~ this]
        =/  uconf/(unit config:hall)  (~(got by configs.str.sta) circ)
        ?~  uconf
          ::  should we crash?
          [~ this]
        =/  conf/config:hall
          %=  u.uconf
            red  red.dif.sto
          ==
        :-  (send-chat-update [%config circ conf])
        %=  this
          configs.str.sta  (~(put by configs.str.sta) circ `conf)
        ==
      ::
      ::  %source: the sources of our inbox have changed
      ::
          %source
        ?.  =(circ [our.bol %inbox])
          :: ignore when circ is not inbox
          [~ this]
        =*  affectedcir  cir.src.dif.sto
        =/  pat/path  /circle/[nom.affectedcir]/grams/config
        ::  we've added a source to our inbox
        ::
        ?:  add.dif.sto
          =/  newinbox  %=  inbox.str.sta
            src  (~(put in src.inbox.str.sta) src.dif.sto)
          ==
          :-  
          %+  weld
            [ost.bol %peer pat [our.bol %hall] pat]~
            (send-chat-update [%inbox newinbox])
          %=  this
            inbox.str.sta  newinbox
          ::src.inbox.str.sta  (~(put in src.inbox.str.sta) src.dif.sto)
          ::
            configs.str.sta
              ?:  (~(has by configs.str.sta) affectedcir)
                configs.str.sta
              (~(put by configs.str.sta) affectedcir ~)
          ==
        =/  newinbox  %=  inbox.str.sta
          src  (~(del in src.inbox.str.sta) src.dif.sto)
        ==
        ::  we've removed a source from our inbox
        ::
        :-  
        %+  weld
          [ost.bol %pull pat [our.bol %hall] ~]~
          (send-chat-update [%inbox newinbox])
        %=  this
          src.inbox.str.sta  (~(del in src.inbox.str.sta) src.dif.sto)
        ::
          configs.str.sta  (~(del by configs.str.sta) affectedcir)
        ==
      ==
      ::  end of branching on dif.sto type
    ==
    ::  end of branching on sto type
  ==
  ::  end of i.wir branching
::
::  +lient arms
::
::
::  +bound: lient tells us we successfully bound our server to the ~chat url
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: serve pages from file system based on URl path
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ?+  site.request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::
  ::  styling
  ::
      [%'~chat' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~chat' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~chat' %img *]
    =/  img  (as-octs:mimes:html (~(got by chat-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::  inbox page
  ::
     [%'~chat' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
::
::  +subscription-retry arms
::
::
::  +reap: recieve acknowledgement for peer, retry on failure
::
++  reap
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circle
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ::
      %circles
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ==
::
::  +quit: subscription failed/quit at some point, retry
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ?~  wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ?+  i.wir
    (mean [leaf+"invalid wire for diff: {(spud wir)}"]~)
  ::
      %circle
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ::
      %circles
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ==
::
--
