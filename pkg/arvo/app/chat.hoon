/-  hall
/+  *server, chat, hall-json
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/js/tile
  /|  /js/
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
/=  chat-png
  /^  (map knot @)
  /:  /===/app/chat/img  /_  /png/
::
=,  chat
::
|%
+$  state
  $%  [%0 str=streams]
  ==
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:eyre term]
      [%peer wire dock path]
      [%quit ~]
      [%poke wire dock poke]
      [%peer wire dock path]
      [%pull wire dock ~]
      [%diff diff]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
::  +prep: set up the app, migrate the state
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?^  old
    :_  this(+<+ u.old)
    [(launch-poke [/chattile '/~chat/js/tile.js'])]~
  ::
  =/  inbox-path  /circle/inbox/config/group
  ::
  :_  this
  :*  [ost.bol %connect / [~ /'~chat'] %chat]
      (launch-poke [/chattile '/~chat/js/tile.js'])
      (hall-peer /circle/(scot %p our.bol)/inbox/config/group inbox-path)
      (hall-peer /circles/(scot %p our.bol) /circles/(scot %p our.bol))
      (hall-source [our.bol %i])
      ?:  =((clan:title our.bol) %czar)
        ~
      ?:  =(our.bol ~marzod)
        :-  (hall-create %announcements 'Announcements from Tlon' %journal)  
        [(hall-source [~marzod %announcements])]~
      ?:  =(our.bol ~dopzod)
        :-  (hall-create %urbit-dev 'Chat about developing on Urbit' %channel)
        [(hall-create %urbit-help 'Help about Urbit' %channel)]~
      :~  (hall-create %hall-internal-announcements '' %village)
          (hall-source [our.bol %hall-internal-announcements])
          (hall-source [~marzod %announcements])
      ==
  ==
::
::  +peer-chattile: subscribe to data necessary for chat tile
::
++  peer-chattile
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json (construct-tile-json str)]~
::
::  +peer-messages: subscribe to subset of messages and updates
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  =*  messages  messages.str
  :_  this
  :-  [ost.bol %diff %chat-config str]
  %+  murn  ~(tap by messages)
  |=  [cir=circle:hall envelopes=(list envelope:hall)]
  ^-  (unit move)
  =/  length=@  (lent envelopes)
  =/  start=@
    ?:  (gte length 100)
      (sub length 100)
    0
  =/  end=@  length
  =/  offset=@  (sub end start)
  :-  ~
  :*  ost.bol
      %diff
      %chat-update
      [%messages cir start end (swag [start offset] envelopes)]
  ==
::
::  +poke-chat: send a list of actions to hall
::
++  poke-chat-action
  |=  act=action:chat
  ^-  (quip move _this)
  :_  this
  %+  turn  lis.act
  |=  hac=action:hall
  ^-  move
  [ost.bol %poke /p/(scot %da now.bol) [our.bol %hall] [%hall-action hac]]
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
      %circles
    ?>  ?=(%circles -.piz)
    =.  str  str(circles cis.piz)
    :_  this(str str)
    (send-chat-update [[%circles cis.piz] str])
  ::
      %circle
    ?>  ?=(%circle -.piz)
    ?.  =([our.bol &3:wir] [our.bol %inbox])
    ::
    ::  fill remote configs with message data
    ::
      =*  messages  messages.str
      =/  circle=circle:hall  [`@p`(slav %p &2:wir) &3:wir]
      =/  peers=(map circle:hall (set @p))
        %-  ~(rep by rem.pes.piz)
        |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
        ^+  acc
        (~(put by acc) cir (silt (turn ~(tap by grp) head)))
      ::
      =.  str
        %=  str
          messages  (~(put by messages) circle nes.piz)
          peers
            %-  ~(uni by peers.str)
            (~(put by peers) circle ~(key by loc.pes.piz))
        ==
      :_  this(str str)
      (send-chat-update [[%messages circle 0 (lent messages) nes.piz] str])
    ::
    ::  fill inbox config and remote configs with prize data
    ::
    =/  circles=(list circle:hall)  (turn ~(tap in src.loc.cos.piz) head)
    ::
    =/  peers=(map circle:hall (set @p))
      %-  ~(rep by rem.pes.piz)
      |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
      ^+  acc
      (~(put by acc) cir (silt (turn ~(tap by grp) head)))
    ::
    :-
      %+  turn  ~(tap in (~(del in (silt circles)) [our.bol %inbox]))
      |=  cir=circle:hall
      %+  hall-peer
        /circle/(scot %p our.bol)/[nom.cir]/config/group
      /circle/[nom.cir]/config/group
    %=  this
        inbox.str  loc.cos.piz
        peers.str  (~(put by peers) [our.bol %inbox] ~(key by loc.pes.piz))
    ::
        configs.str
      %-  ~(uni in configs.str)
      ^-  (map circle:hall (unit config:hall))
      (~(run by rem.cos.piz) some)
    ::
        messages.str  
      %-  molt
      %+  turn  circles
      |=  cir=circle:hall
      ^-  [circle:hall (list envelope:hall)]
      [cir ~]
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
      %circles
    (handle-rumor-circles rum)
  ::
      %circle
    (handle-rumor-circle wir rum)
  ::
  ==
::
::  +handle-rumor-circles
::
++  handle-rumor-circles
  |=  rum=rumor:hall
  ^-  (quip move _this)
  ?>  ?=(%circles -.rum)
  =/  cis
    ?:  add.rum
      (~(put in circles.str) cir.rum)
    (~(del in circles.str) cir.rum)
  =.  str
    %=  str
      circles  cis
      peers
        ?:  add.rum
          (~(put by peers.str) [our.bol cir.rum] ~)
        (~(del by peers.str) [our.bol cir.rum])
    ==
  :_  this(str str)
  (send-chat-update [[%circles cis] str])
::
++  handle-rumor-circle
  |=  [wir=wire rum=rumor:hall]
  ^-  (quip move _this)
  ?>  ?=(%circle -.rum)
  ?+  -.rum.rum
    [~ this]
  ::
  ::  %gram: new message
  ::
      %gram
    (handle-rumor-circle-gram wir rum.rum)
  ::
  ::  status: status update
  ::
      %status
    (handle-rumor-circle-status rum.rum)
  ::
  ::  %config: config has changed
  ::
      %config
    ?+  -.dif.rum.rum
      [~ this]
    ::
    ::  %full: set all of config without side effects
    ::
        %full
      =*  conf  cof.dif.rum.rum
      =.  configs.str  (~(put by configs.str) cir.rum.rum `conf)
      :_  this(str str)
      (send-chat-update [[%config cir.rum.rum conf] str])
    ::
    ::  %read: the read count of one of our configs has changed
    ::
        %read
      (handle-rumor-circle-config-read rum.rum)
    ::
    ::  %source: the sources of our inbox have changed
    ::
        %source
      (handle-rumor-circle-config-source rum.rum)
    ::
    ::  %remove: remove a circle
    ::
        %remove
      =.  str
        %=  str
          configs   (~(del by configs.str) cir.rum.rum)
          messages  (~(del by messages.str) cir.rum.rum)
          peers     (~(del by peers.str) cir.rum.rum)
        ==
      :_  this(str str)
      (send-chat-update [[%delete cir.rum.rum] str])
      ::
    ==
  ==
::
++  handle-rumor-circle-gram
  |=  [wir=wire sto=rumor-story:hall]
  ^-  (quip move _this)
  ?>  ?=(%gram -.sto)
  =*  messages  messages.str
  =/  circle=circle:hall  [`@p`(slav %p &2:wir) &3:wir]
  =/  nes=(unit (list envelope:hall))  (~(get by messages) circle)
  ?~  nes
    [~ this]
  =.  messages.str  (~(put by messages) circle (snoc u.nes nev.sto))
  :_  this(str str)
  (send-chat-update [[%message circle nev.sto] str])
::
++  handle-rumor-circle-status
  |=  sto=rumor-story:hall
  ^-  (quip move _this)
  ?>  ?=(%status -.sto)
  =/  upeers=(unit (set @p))  (~(get by peers.str) cir.sto)
  ?~  upeers
    [~ this]
  =/  peers=(set @p)
    ?:  =(%remove -.dif.sto)
      (~(del in u.upeers) who.sto)
    (~(put in u.upeers) who.sto)
  =.  peers.str  (~(put by peers.str) cir.sto peers)
  :_  this(str str)
  (send-chat-update [[%peers cir.sto peers] str])
::
++  handle-rumor-circle-config-read
  |=  sto=rumor-story:hall
  ^-  (quip move _this)
  ?>  ?=(%config -.sto)
  ?>  ?=(%read -.dif.sto)
  ?:  =(cir.sto [our.bol %inbox])
    ::  ignore when cir.sto is inbox
    [~ this]
  =/  conf=(unit config:hall)  (~(got by configs.str) cir.sto)
  ?~  conf
    [~ this]
  =.  red.u.conf  red.dif.sto
  =.  configs.str  (~(put by configs.str) cir.sto conf)
  :_  this(str str)
  (send-chat-update [[%config cir.sto u.conf] str])
::
::  +handle-rumor-circle-config-source: on source, subscribe and add to inbox
::  on remove source, send delete and remove data from state
::
++  handle-rumor-circle-config-source
  |=  sto=rumor-story:hall
  ^-  (quip move _this)
  ?>  ?=(%config -.sto)
  ?>  ?=(%source -.dif.sto)
  ?.  =(cir.sto [our.bol %inbox])
    :: ignore when cir.sto is not inbox
    [~ this]
  =*  circ  cir.src.dif.sto
  =/  wir  /circle/(scot %p hos.circ)/[nom.circ]/grams/0/config/group
  ::  we've added a source to our inbox
  ::
  ?:  add.dif.sto
    =.  str
      %_  str
        src.inbox  (~(put in src.inbox.str) src.dif.sto)
        ::
        configs
          ?:  (~(has by configs.str) circ)
            configs.str
          (~(put by configs.str) circ ~)
      ==
    ::
    =/  pax  /circle/[nom.circ]/grams/0/config/group
    :_  this(str str)
    :-  [ost.bol %peer wir [hos.circ %hall] pax]
    (send-chat-update [[%inbox inbox.str] str])
  ::
  =.  src.inbox.str  (~(del in src.inbox.str) src.dif.sto)
  ::  we've removed a source from our inbox
  ::
  =.  str
    %=  str
      inbox  inbox.str
    ::
      configs   (~(del by configs.str) circ)
      messages  (~(del by messages.str) circ)
      peers     (~(del by peers.str) circ)
    ==
  =/  fake=circle:hall
    [our.bol (crip (weld (trip 'hall-internal-') (trip nom.circ)))]
  ::
  :_  this(str str)
  ;:  weld
    ^-  (list move)
    ::  just forward the delete to our clients
    ::
    ?~  (~(get by configs.str) fake)
      [ost.bol %pull wir [hos.circ %hall] ~]~
    ::  if we get a delete from another ship, delete our fake circle copy
    ::
    :-  [ost.bol %pull wir [hos.circ %hall] ~]
    [ost.bol %poke /f [our.bol %hall] [%hall-action [%delete nom.fake ~]]]~
  ::
    (send-chat-update [[%inbox inbox.str] str])
    (send-chat-update [[%delete circ] str])
  ==
::
::  +bound: lient tells us we successfully bound our server to the ~chat url
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: serve pages from file system based on URl path
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ?:  =(name 'tile')
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
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
  ::  paginated message data
  ::
      [%'~chat' %scroll @t @t @t @t ~]
    =/  cir=circle:hall  [(slav %p &3:site.request-line) &4:site.request-line]
    =/  start=@ud  (need (rush &5:site.request-line dem))
    =/  parsedend=@ud  (need (rush &6:site.request-line dem))
    =*  messages  messages.str
    =/  envs/(unit (list envelope:hall))  (~(get by messages) cir)
    ?~  envs
      [~ this]
    ?:  (gte start (lent u.envs))
      [~ this]
    =/  end=@
      ?:  (gte parsedend (lent u.envs))
        (dec (lent u.envs))
      parsedend
    =/  offset  (sub end start)
    =/  jon=json  %-  msg-to-json
    :*  %messages
        cir
        start
        end
        (swag [start offset] u.envs)
    ==
    :_  this
    [ost.bol %http-response (json-response:app (json-to-octs jon))]~
  ::
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
    =/  shp=@p  (slav %p &2:wir)
    =/  pat  /circle/[&3:wir]/config/group
    ?:  =(&3:wir 'inbox')
      :_  this
      [ost.bol %peer wir [shp %hall] pat]~
    ?:  (~(has in src.inbox.str) [[shp &3:wir] ~])
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
    =/  shp=@p  (slav %p &2:wir)
    =/  pat  /circle/[&3:wir]/config/group
    ?:  =(&3:wir 'inbox')
      :_  this
      [ost.bol %peer wir [shp %hall] pat]~
    ?:  (~(has in src.inbox.str) [[shp &3:wir] ~])
      :_  this
      [ost.bol %peer wir [shp %hall] pat]~
    [~ this]
  ::
      %circles
    :_  this
    [ost.bol %peer wir [our.bol %hall] wir]~
  ==
::
::  +utilities
::
::
::  +send-chat-update: utility func for sending updates to all our subscribers
::
++  send-chat-update
  |=  [upd=update str=streams]
  ^-  (list move)
  =/  jon  (construct-tile-json str)
  ::
  %+  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %chat-update upd]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /chattile bol)
  |=  [=bone *]
  [bone %diff %json jon]
::
++  construct-tile-json
  |=  str=streams
  ^-  json
  :-  %o
  %-  my
  :~  ['config' (config-to-json str)]
    ::
      :-  'numbers'
      %-  numbers-to-json
      ^-  (list [circle:hall @ud])
      %+  turn  ~(tap by messages.str)
      |=  [cir=circle:hall lis=(list envelope:hall)]
      ^-  [circle:hall @ud]
      ?~  lis
        [cir 0]
      =/  last  (snag (dec (lent lis)) `(list envelope:hall)`lis)
      [cir (add num.last 1)]
  ==
::
++  launch-poke
  |=  [=path =cord]
  ^-  move
  [ost.bol %poke /chat [our.bol %launch] [%launch-action %chat path cord]]
::
++  hall-peer
  |=  [wir=wire pat=path]
  ^-  move
  [ost.bol %peer wir [our.bol %hall] pat]
::
++  hall-create
  |=  [name=@tas description=@t =security:hall]
  ^-  move
  =/  poke  [%hall-action [%create name description security]]
  [ost.bol %poke /chat [our.bol %hall] poke]
::
++  hall-source
  |=  cir=circle:hall
  ^-  move
  =/  poke  [%hall-action [%source %inbox %.y (silt [cir ~]~)]]
  [ost.bol %poke /chat [our.bol %hall] poke]
::
--
