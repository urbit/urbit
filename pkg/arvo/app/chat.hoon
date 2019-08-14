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
::
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
::
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
  =/  launcha  (launch-poke [/chattile '/~chat/js/tile.js'])
  ?~  old
    =/  peer-inbox
      %+  hall-peer
        /circle/[(scot %p our.bol)]/inbox/config/group
      /circle/inbox/config/group
    =/  circlespat  /circles/[(scot %p our.bol)]
    =/  sourceannounce  (hall-source [~marzod %announcements])
    ::
    =/  hall-actions=(list move)
      ?:  =((clan:title our.bol) %czar)
        ~
      ?:  =(our.bol ~marzod)
        :-  (hall-create %announcements 'Announcements from Tlon' %journal)  
        [sourceannounce]~
      ?:  =(our.bol ~dopzod)
        :-  (hall-create %urbit-dev 'Chat about developing on Urbit' %channel)
        [(hall-create %urbit-help 'Help about Urbit' %channel)]~
      :~  (hall-create %hall-internal-announcements '' %village)
          (hall-source [our.bol %hall-internal-announcements])
          sourceannounce
      ==
    ::
    =/  actions=(list move)
      :~  [ost.bol %connect / [~ /'~chat'] %chat]
          launcha
          peer-inbox
          (hall-peer circlespat circlespat)
          (hall-source [our.bol %i])
      ==
    :_  this
    (weld actions hall-actions)
  ::
  :_  this(+<+ u.old)
  [launcha]~
::
++  peer-chattile
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json (construct-tile-json str)]~
::
::  +peer-messages: subscribe to subset of messages and updates
::
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  =*  messages  messages.str
  =/  lismov=(list move)
    %+  murn  ~(tap by messages)
    |=  [cir=circle:hall lis=(list envelope:hall)]
    ^-  (unit move)
    =/  envs=(unit (list envelope:hall))  (~(get by messages) cir)
    ?~  envs
      ~
    =/  length/@  (lent u.envs)
    =/  start/@
      ?:  (gte length 100)
        (sub length 100)
      0
    =/  end/@  length
    =/  offset/@  (sub end start)
    :-  ~
    :*  ost.bol
        %diff
        %chat-update
        [%messages cir start end (swag [start offset] u.envs)]
    ==
  :_  this
  [[ost.bol %diff %chat-config str] lismov]
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
  :_  this(circles.str cis.piz)
  (send-chat-update [[%circles cis.piz] sta])
  ::
  ::  %circle wire
  ::
    %circle
  ?>  ?=(%circle -.piz)
  =/  circle/circle:hall  [our.bol &3:wir]
  ::
  ::  fill inbox config and remote configs with prize data
  ::
  ?:  =(circle [our.bol %inbox])
    =/  configs
      %-  ~(uni in configs.str)
      ^-  (map circle:hall (unit config:hall))
      (~(run by rem.cos.piz) |=(a=config:hall `a))
    ::
    =/  circles=(list circle:hall)
      (turn ~(tap in src.loc.cos.piz) head)
    ::
    =/  meslis=(list [circle:hall (list envelope:hall)])
      %+  turn  circles
      |=  cir=circle:hall
      ^-  [circle:hall (list envelope:hall)]
      [cir ~]
    ::
    =/  localpeers=(set @p)
      (silt (turn ~(tap by loc.pes.piz) head))
    ::
    =/  peers=(map circle:hall (set @p))
      %-  ~(rep by rem.pes.piz)
      |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
      ^-  (map circle:hall (set @p))
      (~(put by acc) cir (silt (turn ~(tap by grp) head)))
    ::
    :-
      %+  turn  ~(tap in (~(del in (silt circles)) [our.bol %inbox]))
      |=  cir=circle:hall
      ^-  move
      =/  wir/wire  /circle/[(scot %p our.bol)]/[nom.cir]/config/group
      =/  pat/path  /circle/[nom.cir]/config/group
      [ost.bol %peer wir [our.bol %hall] pat]
    %=  this
        inbox.str  loc.cos.piz
        configs.str  configs
        messages.str  (molt meslis)
        peers.str  (~(put by peers) [our.bol %inbox] localpeers)
    ==
  ::
  ::  fill remote configs with message data
  ::
    =*  messages  messages.str
    =/  circle=circle:hall  [`@p`(slav %p &2:wir) &3:wir]
    =/  localpeers=(set @p)  (silt (turn ~(tap by loc.pes.piz) head))
    ::
    =/  peers=(map circle:hall (set @p))
      %-  ~(rep by rem.pes.piz)
      |=  [[cir=circle:hall grp=group:hall] acc=(map circle:hall (set @p))]
      ^-  (map circle:hall (set @p))
      (~(put by acc) cir (silt (turn ~(tap by grp) head)))
    =/  sta
      %=  str
        messages  (~(put by messages) circle nes.piz)
        peers  (~(uni by peers.str) (~(put by peers) circle localpeers))
      ==
    =/  messageupdate=update  [%messages circle 0 (lent messages) nes.piz]
    :-  (send-chat-update [messageupdate sta])
    this(str sta)
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
  ::
    %circles
  ?>  ?=(%circles -.rum)
  =/  cis
    ?:  add.rum
      (~(put in circles.str) cir.rum)
    (~(del in circles.str) cir.rum)
  =/  sta
    %=  str
      circles  cis
      peers
        ?:  add.rum
          (~(put by peers.str) [our.bol cir.rum] ~)
        (~(del by peers.str) [our.bol cir.rum])
    ==
  :_  this(str sta)
  (send-chat-update [[%circles cis] sta])
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
  =*  messages  messages.str
  =/  circle=circle:hall  [`@p`(slav %p &2:wir) &3:wir]
  =/  nes=(unit (list envelope:hall))  (~(get by messages) circle)
  ?~  nes
    [~ this]
  =/  sta
    %=  str
      messages  (~(put by messages) circle (snoc u.nes nev.sto))
    ==
  :_  this(str sta)
  (send-chat-update [[%message circle nev.sto] sta])
  ::
  ::  status:
  ::
    %status
  ?>  ?=(%status -.sto)
  =/  upeers=(unit (set @p))  (~(get by peers.str) cir.sto)
  ?~  upeers
    [~ this]
  =/  peers=(set @p)
    ?:  =(%remove -.dif.sto)
      (~(del in u.upeers) who.sto)
    (~(put in u.upeers) who.sto)
  =/  sta
    %=  str
      peers  (~(put by peers.str) cir.sto peers)
    ==
  :_  this(str sta)
  (send-chat-update [[%peers cir.sto peers] sta])
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
    =/  sta
      %=  str
        configs  (~(put by configs.str) circ `conf)
      ==
    :_  this(str sta)
    (send-chat-update [[%config circ conf] sta])
  ::
  ::  %read: the read count of one of our configs has changed
  ::
      %read
    ?:  =(circ [our.bol %inbox])
      ::  ignore when circ is inbox
      [~ this]
    =/  uconf/(unit config:hall)  (~(got by configs.str) circ)
    ?~  uconf
      [~ this]
    =/  conf/config:hall
      %=  u.uconf
        red  red.dif.sto
      ==
    =/  sta
      %=  str
        configs  (~(put by configs.str) circ `conf)
      ==
    :_  this(str sta)
    (send-chat-update [[%config circ conf] sta])
  ::
  ::  %source: the sources of our inbox have changed
  ::
      %source
    ?.  =(circ [our.bol %inbox])
      :: ignore when circ is not inbox
      [~ this]
    =*  affectedcir  cir.src.dif.sto
    =/  newwir/wire
      /circle/(scot %p hos.affectedcir)/[nom.affectedcir]/grams/0/config/group
    =/  pat/path  /circle/[nom.affectedcir]/grams/0/config/group
    ::  we've added a source to our inbox
    ::
    ?:  add.dif.sto
      =/  newinbox  %=  inbox.str
        src  (~(put in src.inbox.str) src.dif.sto)
      ==
      =/  sta
        %=  str
          inbox  newinbox
          ::
          configs
            ?:  (~(has by configs.str) affectedcir)
              configs.str
            (~(put by configs.str) affectedcir ~)
        ==
      ::
      :_  this(str sta)
      %+  weld
        [ost.bol %peer newwir [hos.affectedcir %hall] pat]~
      (send-chat-update [[%inbox newinbox] sta])
    ::
    =/  newinbox  %=  inbox.str
      src  (~(del in src.inbox.str) src.dif.sto)
    ==
    ::  we've removed a source from our inbox
    ::
    =/  sta
      %=  str
        inbox  newinbox
      ::
        configs   (~(del by configs.str) affectedcir)
        messages  (~(del by messages.str) affectedcir)
        peers     (~(del by peers.str) affectedcir)
      ==
    =/  fakecir=circle:hall
      :-  our.bol
      (crip (weld (trip 'hall-internal-') (trip nom.affectedcir)))
    ::
    ?~  (~(get by configs.str) fakecir)
      ::  just forward the delete to our clients
      ::
      :_  this(str sta)
      :-  [ost.bol %pull newwir [hos.affectedcir %hall] ~]
      %+  weld
        (send-chat-update [[%inbox newinbox] sta])
      (send-chat-update [[%delete affectedcir] sta])
    ::  if we get a delete from another ship, delete our fake circle copy
    ::
    =/  deletefake  [%hall-action [%delete nom.fakecir ~]]
    :_  this(str sta)
    :-  [ost.bol %pull newwir [hos.affectedcir %hall] ~]
    :-  [ost.bol %poke /fake [our.bol %hall] deletefake]
    %+  weld
      (send-chat-update [[%inbox newinbox] sta])
    (send-chat-update [[%delete affectedcir] sta])
  ::
  ::  %remove: remove a circle
  ::
      %remove
    =/  sta
      %=  str
        configs   (~(del by configs.str) circ)
        messages  (~(del by messages.str) circ)
        peers     (~(del by peers.str) circ)
      ==
    :-  (send-chat-update [[%delete circ] sta])
    this(str sta)
    ::
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
  =/  updates=(list move)
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %chat-update upd]
  ::
  =/  jon  (construct-tile-json str)
  =/  tile-updates=(list move)
    %+  turn  (prey:pubsub:userlib /chattile bol)
    |=  [=bone *]
    [bone %diff %json jon]
  ::
  (weld updates tile-updates)
::
++  construct-tile-json
  |=  str=streams
  ^-  json
  =/  numbers/(list [circle:hall @ud])
    %+  turn  ~(tap by messages.str)
      |=  [cir=circle:hall lis=(list envelope:hall)]
      ^-  [circle:hall @ud]
      ?~  lis
        [cir 0]
      =/  last  (snag (dec (lent lis)) `(list envelope:hall)`lis)
      [cir (add num.last 1)]
  =/  maptjson=(map @t json)
    %-  my
    :~  ['config' (config-to-json str)]
        ['numbers' (numbers-to-json numbers)]
    ==
  [%o maptjson]
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
