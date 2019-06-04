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
    =/  inboxpat  /circle/inbox/config/group
    =/  circlespat  /circles/[(scot %p our.bol)]
    =/  inboxwir  /circle/[(scot %p our.bol)]/inbox/config/group
    =/  inboxi/poke 
      :-  %hall-action
          [%source %inbox %.y (silt [[our.bol %i] ~]~)]
    :_  this
    :~  [ost.bol %peer inboxwir [our.bol %hall] inboxpat]
        [ost.bol %peer circlespat [our.bol %hall] circlespat]
        [ost.bol %connect / [~ /'~chat'] %chat]
        [ost.bol %poke /chat [our.bol %hall] inboxi]
    ==
  [~ this(sta u.old)]
::
::  +peer-messages: subscribe to subset of messages and updates
::
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  =/  indices  (generate-circle-indices wir)
  ?~  indices
    :_  this
    [ost.bol %diff %chat-initial str.sta]~
  =*  messages  messages.str.sta
  =/  lisunitmov/(list (unit move)) 
    %+  turn  indices
      |=  [cir=circle:hall start=@ud]
      ^-  (unit move)
      =/  wholelist/(unit (list envelope:hall))  (~(get by messages) cir)
      ?~  wholelist
        ~
      =/  end/@  (lent u.wholelist)
      ?:  (gte start end)
        ~
      :-  ~
      :*  ost.bol
          %diff
          %chat-update
          [%messages cir start end (swag [start end] u.wholelist)]
      ==
  =/  lismov/(list move)
    %+  turn
    %+  skim  lisunitmov
    |=  umov=(unit move)
    ^-  ?
    ?~  umov
      %.n
    %.y
    need
  :_  this
  %+  weld
  [ost.bol %diff %chat-config str.sta]~
  lismov
::
++  peer-updates
  |=  wir=wire
  ^-  (quip move _this)
  [~ this]
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
  %+  turn  (prey:pubsub:userlib /updates bol)
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
::      ::
::      ::  %circle prize
::      ::
::        %circle
      ?>  ?=(%circle -.piz)
      =/  circle/circle:hall  [our.bol &3:wir]
      ?:  =(circle [our.bol %inbox])
    ::
    ::  fill inbox config and remote configs with prize data
    ::
        =/  configs
          %-  ~(uni in configs.str.sta)
          ^-  (map circle:hall (unit config:hall))
          (~(run by rem.cos.piz) |=(a=config:hall `a))
        ::
        =/  circles/(list circle:hall)
          %+  turn  ~(tap in src.loc.cos.piz)
            |=  src=source:hall
            ^-  circle:hall
            cir.src
        ::
        =/  meslis/(list [circle:hall (list envelope:hall)])
          %+  turn  circles
          |=  cir=circle:hall
          ^-  [circle:hall (list envelope:hall)]
          [cir ~]
        ::
        =/  localpeers/(set @p)  
          %-  silt  %+  turn  ~(tap by loc.pes.piz)
          |=  [shp=@p stat=status:hall]
          shp
        ::
        =/  peers/(map circle:hall (set @p))
          %-  ~(rep by rem.pes.piz)
          |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
          ^-  (map circle:hall (set @p))
          =/  newset
            %-  silt  %+  turn  ~(tap by grp)
            |=  [shp=@p stat=status:hall]
            shp
          (~(put by acc) cir newset)
        ::
        :-
          %+  turn  ~(tap in (~(del in (silt circles)) [our.bol %inbox]))
            |=  cir=circle:hall
            ^-  move
            =/  wir/wire  /circle/[(scot %p our.bol)]/[nom.cir]/config/group
            =/  pat/path  /circle/[nom.cir]/config/group
            [ost.bol %peer wir [our.bol %hall] pat]
          ::
          %=  this
              inbox.str.sta  loc.cos.piz
              configs.str.sta  configs
              messages.str.sta  (molt meslis)
              peers.str.sta  (~(put by peers) [our.bol %inbox] localpeers)
          ==
    ::
    ::  fill remote configs with message data
    ::
      =*  messages  messages.str.sta
      =/  circle/circle:hall  [`@p`(slav %p &2:wir) &3:wir]
      =/  localpeers/(set @p)  
        %-  silt  %+  turn  ~(tap by loc.pes.piz)
        |=  [shp=@p stat=status:hall]
        shp
      ::
      =/  peers/(map circle:hall (set @p))
        %-  ~(rep by rem.pes.piz)
        |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
        ^-  (map circle:hall (set @p))
        =/  newset
          %-  silt  %+  turn  ~(tap by grp)
          |=  [shp=@p stat=status:hall]
          shp
        (~(put by acc) cir newset)
      :-  ~
      %=  this
        messages.str.sta  (~(put by messages) circle nes.piz)
        peers.str.sta  (~(uni by peers.str.sta) (~(put by peers) circle localpeers))
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
            peers.str.sta  (~(put by peers.str.sta) [our.bol cir.rum] ~)
        ==
      =/  cis  (~(del in circles.str.sta) cir.rum)
      :-  (send-chat-update [%circles cis])
      %=  this
          circles.str.sta  cis
          peers.str.sta  (~(del by peers.str.sta) [our.bol cir.rum])
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
    =/  circle/circle:hall  [`@p`(slav %p &2:wir) &3:wir]
    =/  nes/(list envelope:hall)  (~(got by messages) circle)
    :-  (send-chat-update [%message circle nev.sto])
    %=  this
      messages.str.sta  (~(put by messages) circle (snoc nes nev.sto))
    ==
    ::
    ::  %status:
    ::
      %status
    ?>  ?=(%status -.sto)
    =/  upeers/(unit (set @p))  (~(get by peers.str.sta) cir.sto)
    ?~  upeers
      [~ this]
    =/  peers/(set @p)
      ?:  =(%remove -.dif.sto)
        (~(del in u.upeers) who.sto)
      (~(put in u.upeers) who.sto)
    :-  (send-chat-update [%peers cir.sto peers])
    %=  this 
      peers.str.sta  (~(put by peers.str.sta) cir.sto peers)
    ==

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
        =/  newwir/wire
          /circle/[(scot %p hos.affectedcir)]/[nom.affectedcir]/grams/config/group
        =/  pat/path  /circle/[nom.affectedcir]/grams/config/group
        ::  we've added a source to our inbox
        ::
        ?:  add.dif.sto
          =/  newinbox  %=  inbox.str.sta
            src  (~(put in src.inbox.str.sta) src.dif.sto)
          ==
          :-  
          %+  weld
            [ost.bol %peer newwir [hos.affectedcir %hall] pat]~
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
          [ost.bol %pull newwir [hos.affectedcir %hall] ~]~
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
      =/  shp/@p  (slav %p &2:wir)
      =/  pat  /circle/[&3:wir]/config/group
      ?:  =(&3:wir 'inbox')
        :_  this
        [ost.bol %peer wir [shp %hall] pat]~
      ?:  (~(has in src.inbox.str.sta) [[shp &3:wir] ~])
        :_  this
        [ost.bol %peer wir [shp %hall] pat]~
      [~ this]
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
      =/  shp/@p  (slav %p &2:wir)
      =/  pat  /circle/[&3:wir]/config/group
      ?:  =(&3:wir 'inbox')
        :_  this
        [ost.bol %peer wir [shp %hall] pat]~
      ?:  (~(has in src.inbox.str.sta) [[shp &3:wir] ~])
        :_  this
        [ost.bol %peer wir [shp %hall] pat]~
      [~ this]
  ::
      %circles
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ==
::
--
