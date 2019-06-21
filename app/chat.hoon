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
        [ost.bol %poke /chat [our.bol %launch] [%noun [%chat /chattile '/~chat/js/tile.js']]]
    ==
  :-  [ost.bol %poke /chat [our.bol %launch] [%noun [%chat /chattile '/~chat/js/tile.js']]]~
  this(sta u.old)
::
::
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
      [cir num.last]
  =/  maptjson  *(map @t json)
  =.  maptjson
    (~(put by maptjson) 'config' (config-to-json str))
  =.  maptjson
    (~(put by maptjson) 'numbers' (numbers-to-json numbers))
  [%o maptjson]
::
++  peer-chattile
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json (construct-tile-json str.sta)]~
::
::  +peer-messages: subscribe to subset of messages and updates
::
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  ~&  (lent (prey:pubsub:userlib /primary bol))
  =*  messages  messages.str.sta
  =/  lisunitmov=(list (unit move))
    %+  turn  ~(tap by messages)
    |=  [cir=circle:hall lis=(list envelope:hall)]
    ^-  (unit move)
    =/  envs/(unit (list envelope:hall))  (~(get by messages) cir)
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
  %+  weld
    [ost.bol %diff %chat-config str.sta]~
  %+  turn  %+  skim  lisunitmov
    |=  umov=(unit move)
    ^-  ?
    ?~  umov
      %.n
    %.y
    need
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
  |=  [upd=update str=streams]
  ^-  (list move)
  =/  updates/(list move)
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %chat-update upd]
  ::
  =/  tile-updates/(list move)
    %+  turn  (prey:pubsub:userlib /chattile bol)
    |=  [=bone *]
    [bone %diff %json (construct-tile-json str)]
  ::
  %+  weld
    updates
    tile-updates
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
      =/  str  %=  str.sta
        circles  cis.piz
      ==
      :-  (send-chat-update [[%circles cis.piz] str])
      this(str.sta str)
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
      =/  cis
        ?:  add.rum
          (~(put in circles.str.sta) cir.rum)
        (~(del in circles.str.sta) cir.rum)
      =/  str  %=  str.sta
          circles  cis
          peers
            ?:  add.rum
              (~(put by peers.str.sta) [our.bol cir.rum] ~)
            (~(del by peers.str.sta) [our.bol cir.rum])
      ==
      :-  (send-chat-update [[%circles cis] str])
      this(str.sta str)
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
    =/  str  %=  str.sta
      messages  (~(put by messages) circle (snoc nes nev.sto))
    ==
    :-  (send-chat-update [[%message circle nev.sto] str])
    this(str.sta str)
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
    =/  str
      %=  str.sta
        peers  (~(put by peers.str.sta) cir.sto peers)
      ==
    :-  (send-chat-update [[%peers cir.sto peers] str])
    this(str.sta str)
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
        =/  str
          %=  str.sta
            configs  (~(put by configs.str.sta) circ `conf)
          ==
        :-  (send-chat-update [[%config circ conf] str])
        this(str.sta str)
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
        =/  str
          %=  str.sta
            configs  (~(put by configs.str.sta) circ `conf)
          ==
        :-  (send-chat-update [[%config circ conf] str])
        this(str.sta str)
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
          =/  str
            %=  str.sta
              inbox  newinbox
              ::
              configs
                ?:  (~(has by configs.str.sta) affectedcir)
                  configs.str.sta
                (~(put by configs.str.sta) affectedcir ~)
            ==
          ::
          :_  this(str.sta str)
          %+  weld
            [ost.bol %peer newwir [hos.affectedcir %hall] pat]~
            (send-chat-update [[%inbox newinbox] str])
        ::
        =/  newinbox  %=  inbox.str.sta
          src  (~(del in src.inbox.str.sta) src.dif.sto)
        ==
        ::  we've removed a source from our inbox
        ::
        =/  str
          %=  str.sta
            inbox  newinbox
          ::
            configs  (~(del by configs.str.sta) affectedcir)
          ==
        ::
        :_  this(str.sta str)
        %+  weld
          [ost.bol %pull newwir [hos.affectedcir %hall] ~]~
          (send-chat-update [[%inbox newinbox] str])
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
    =/  cir/circle:hall  [(slav %p &3:site.request-line) &4:site.request-line]
    =/  start/@ud  (need (rush &5:site.request-line dem))
    =/  parsedend/@ud  (need (rush &6:site.request-line dem))
    =*  messages  messages.str.sta
    =/  envs/(unit (list envelope:hall))  (~(get by messages) cir)
    ?~  envs
      [~ this]
    ?:  (gte start (lent u.envs))
      [~ this]
    =/  end/@
      ?:  (gte parsedend (lent u.envs))
        (dec (lent u.envs))
      parsedend
    =/  offset  (sub end start)
    =/  jon/json  %-  msg-to-json
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
