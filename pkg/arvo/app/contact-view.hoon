::  contact-view: sets up contact JS client and combines commands
::  into semantic actions for the UI
::
/-  *group-store
/+  *server, *contact-json, default-agent
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/css/index
  /|  /css/
      /~  ~
  ==
/=  contact-png
  /^  (map knot @)
  /:  /===/app/contacts/img  /_  /png/
::
|%
+$  card  card:agent:gall
--
::
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this       .
      contact-core  +>
      cc         ~(. contact-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    :~  [%pass /updates %agent [our.bowl %contact-store] %watch /updates]
        [%pass / %arvo %e %connect [~ /'~contacts'] %contact-view]
        (launch-poke:cc [%contact-view /primary '/~contacts/js/tile.js'])
        (contact-poke:cc [%create /~/default])
        (group-poke:cc [%bundle /~/default])
        (contact-poke:cc [%add /~/default our.bowl *contact])
        (group-poke:cc [%add [our.bowl ~ ~] /~/default])
    ==
  ::
  ++  on-save   on-save:def
  ++  on-load   on-load:def
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?+  mark  (on-poke:def mark vase)
        %json                 [(poke-json:cc !<(json vase)) this]
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?:  ?=([%http-response *] path)  [~ this]
    ?.  =(/primary path)  (on-watch:def path)
    [[%give %fact ~ %json !>((rolodex-to-json all-scry:cc))]~ this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick
      [[%pass / %agent [our.bol %contact-store] %watch /updates]~ this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %contact-update
        =/  update=json  (update-to-json !<(contact-update q.cage.sign))
        [[%give %fact `/primary %json !>(update)]~ this]
      ==
    ==
  ::
  ++  on-arvo   
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
++  poke-json
  |=  jon=json
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  (poke-contact-view-action (json-to-view-action jon))
::
++  poke-contact-view-action
  |=  act=contact-view-action
  ^-  (list card)
  ?-  -.act
      %create
    :~  (group-poke [%bundle path.act])
        (contact-poke [%create path.act])
    ==
  ::
      %delete
    :~  (group-poke [%unbundle path.act])
        (contact-poke [%delete path.act])
    ==
  ::
      %add
    :~  (group-poke [%add [ship.act ~ ~] path.act])
        (contact-poke [%add path.act ship.act contact.act])
    ==
  ::
      %remove
    :~  (group-poke [%remove [ship.act ~ ~] path.act])
        (contact-poke [%remove path.act ship.act])
    ==
  ==
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.url)
    ?~  back-path
      ''
    i.back-path
  ?+  site.url  not-found:gen
      [%'~contacts' %css %index ~]  (css-response:gen style)
      [%'~contacts' %js %index ~]   (js-response:gen script)
      [%'~contacts' %js %tile ~]    (js-response:gen tile-js)
      [%'~contacts' %img *]
    (png-response:gen (as-octs:mimes:html (~(got by contact-png) `@ta`name)))
  ::
      [%'~contacts' *]  (html-response:gen index)
  ==
::
::  +utilities
::
++  contact-poke
  |=  act=contact-action
  ^-  card
   [%pass / %agent [our.bol %contact-store] %poke %contact-action !>(act)]
::
++  launch-poke
  |=  act=[@tas path @t]
  ^-  card
   [%pass / %agent [our.bol %launch] %poke %launch-action !>(act)]
::
++  group-poke
  |=  act=group-action
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(act)]
::
++  all-scry
  ^-  rolodex
  .^(rolodex %gx /=contact-store/(scot %da now.bol)/all/noun)
--
