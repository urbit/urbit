/-  *inbox
/+  *server, *inbox-json
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
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:eyre term]
      [%poke wire dock [%launch-action [@tas path @t]]]
      [%diff %inbox-initial inbox]
      [%quit ~]
  ==
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
::  +prep: set up the app, migrate the state
::
++  prep
  |=  old=*
  ^-  (quip move _this)
  :_  this
  :~  [ost.bol %connect / [~ /'~chat'] %inbox-view]
      (launch-poke [/chattile '/~chat/js/tile.js'])
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
  =+  url=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.url)
    ?~  back-path
      ''
    i.back-path
  ?:  =(name 'tile')
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
  ?+  site.url
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
    [%'~chat' %scroll @t @t *]
    =*  start  i.t.t.site.url
    =*  end  i.t.t.t.site.url
    =/  pax  t.t.t.site.url
    =/  envelopes  (envelope-scry [start end pax])
    :_  this
    :~
    :*  ost.bol
        %http-response
        %-  json-response:app
        %-  json-to-octs 
        %+  envelopes-update
          envelopes
        [start end pax]
    ==
    ==
  ::
  ::
  ::  inbox page
  ::
     [%'~chat' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
++  peer-inbox
  |=  pax=path
  ^-  (quip move _this)
  ::  create inbox with 100 messages max per mailbox and send that along
  ::  then quit the subscription
  ~&  pax
  :_  this
  :~
  :*  ost.bol
      %diff
      %inbox-initial
      %-  ~(run by all-scry)
      |=  mail=mailbox
      ^-  mailbox
      ~&  mail
      :+  (truncate-envelopes envelopes.mail)
        read.mail
      owner.mail
  ==
  ==
  ::[ost.bol %quit ~]~

::  
++  launch-poke
  |=  [=path =cord]
  ^-  move
  [ost.bol %poke /chat [our.bol %launch] [%launch-action %chat path cord]]
::
++  envelope-scry
  |=  pax=path
  ^-  (list envelope)
  =.  pax  ;:  weld
    `path`/=inbox/(scot %da now.bol)/envelopes
    pax
    `path`/noun
  ==
  .^((list envelope) %gx pax)
::
++  all-scry
  ^-  inbox
  =/  pax=path  /=inbox/(scot %da now.bol)/all/noun
  .^(inbox %gx pax)
::
++  envelopes-update
  |=  [envelopes=(list envelope) start=@t end=@t pax=path]
  ^-  json
  %+  frond:enjs:format  %inbox-update
  %-  pairs:enjs:format
  :~
    :-  %messages
    %-  pairs:enjs:format
    :~  [%path (pa pax)]
        [%start (numb:enjs:format (need (rush start dem)))]
        [%end (numb:enjs:format (need (rush end dem)))]
        [%envelopes [%a (turn envelopes enve)]]
    ==
  ==
::
++  truncate-envelopes
  |=  envelopes=(list envelope)
  ^-  (list envelope)
  =/  length  (lent envelopes)
  ?:  (lth length 100)
    envelopes
  (swag [(sub length 100) 100] envelopes)
::
--
