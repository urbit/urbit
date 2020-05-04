/-  launch
/+  *server, default-agent, dbug
::
/=  index
  /^  $-([json marl] manx)
  /:  /===/app/launch/index  /!noun/
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/js/index
  /|  /js/
      /~  ~
  ==
/=  channel-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/js/channel
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/css/index
  /|  /css/
      /~  ~
  ==
/=  launch-png
  /^  (map knot @)
  /:  /===/app/launch/img  /_  /png/
::
|%
+$  versioned-state
  $%  [%0 state-zero]
      [%1 state-two]
      [%2 state-two]
      [%3 state-two]
  ==
+$  state-zero
  $:  tiles=(set tile:launch)
      data=tile-data:launch
      path-to-tile=(map path @tas)
  ==
+$  state-two
  $:  tiles=(set tile:launch)
      data=tile-data:launch
      path-to-tile=(map path @tas)
      first-time=?
  ==
::
+$  card  card:agent:gall
++  launch-who
  |=  =desk
  [%pass /who %arvo %e %serve [~ /who] desk /gen/who/hoon ~]
--
::
=|  [%3 state-two]
=*  state  -
%-  agent:dbug
^-  agent:gall
|_  bol=bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bol)
++  on-init
  ^-  (quip card _this)
  :_  this(state *[%3 state-two])
  :~  (launch-who q.byk.bol)
      [%pass / %arvo %e %connect [~ /] %launch]
  ==
::
++  on-save  !>(state)
::
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  old-state  !<(versioned-state old)
  =|  cards=(list card)
  |-
  ?-    -.old-state
      %0
    $(old-state [%1 tiles data path-to-tile %.n]:old-state)
  ::
      %1
    =/  new-state=state-two
      =,  old-state
      :*  (~(del in tiles) [%contact-view /primary])
          (~(del by data) %contact-view)
          (~(del by path-to-tile) /primary)
          first-time
      ==
    $(old-state [%2 new-state])
  ::
      %2
    $(old-state [%3 +.old-state], cards [(launch-who q.byk.bol) cards])
  ::
      %3
    [(flop cards) this(state old-state)]
  ==
::
++  on-poke
  |=  [mar=mark vas=vase]
  ^-  (quip card _this)
  ?+    mar  (on-poke:def mar vas)
      %json
    ?>  (team:title our.bol src.bol)
    =/  jon  !<(json vas)
    :-  ~
    ?.  =(jon [%s 'disable welcome message'])
      this
    this(first-time %.n)
  ::
      %launch-action
    =/  act  !<(action:launch vas)
    ?-  -.act
        %add
      =/  beforedata  (~(get by data) name.act)
      =/  newdata
        ?~  beforedata
          (~(put by data) name.act [*json url.act])
        (~(put by data) name.act [jon.u.beforedata url.act])
      =/  new-tile  `tile:launch`[`@tas`name.act `path`subscribe.act]
      :-  [%pass subscribe.act %agent [our.bol name.act] %watch subscribe.act]~
      %=  this
        tiles         (~(put in tiles) new-tile)
        data          newdata
        path-to-tile  (~(put by path-to-tile) subscribe.act name.act)
      ==
::
        %remove
      :-  [%pass subscribe.act %agent [our.bol name.act] %leave ~]~
      %=  this
        tiles         (~(del in tiles) [name.act subscribe.act])
        data          (~(del by data) name.act)
        path-to-tile  (~(del by path-to-tile) subscribe.act)
      ==
    ==
  ::
      %handle-http-request
    =+  !<([eyre-id=@ta =inbound-request:eyre] vas)
    :_  this
    %+  give-simple-payload:app    eyre-id
    %+  require-authorization:app  inbound-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    =/  request-line  (parse-request-line url.request.inbound-request)
    =/  name=@t
      =/  back-path  (flop site.request-line)
      ?~  back-path
        ''
      i.back-path
    ?+  site.request-line
      not-found:gen
    ::
        [~ ~]
      =/  hym=manx
        %+  index
          [%b first-time]
        ^-  marl
        %+  turn  ~(tap by data)
        |=  [key=@tas [jon=json url=@t]]
        ^-  manx
        ;script@"{(trip url)}";
      (manx-response:gen hym)
    ::
        [%'~launch' %css %index ~]       :: styling
      (css-response:gen style)
    ::
        [%'~launch' %js %index ~]        :: javascript
      (js-response:gen script)
    ::
        [%'~launch' %img *]              :: images
      =/  img=(unit @)  (~(get by launch-png) `@ta`name)
      ?~  img
        not-found:gen
      (png-response:gen (as-octs:mimes:html u.img))
    ::
        [%'~modulo' %session ~]
      =/  session-js
        %-  as-octt:mimes:html
        ;:  weld
            "window.ship = '{+:(scow %p our.bol)}';"
            "window.urb = new Channel();"
        ==
      (js-response:gen session-js)
    ::
        [%'~channel' %channel ~]
      (js-response:gen channel-js)
    ==
  ==
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?:  ?=([%http-response *] pax)
    [~ this]
  ?.  ?=([%main *] pax)
    (on-watch:def pax)
  =/  data=json
    %-  pairs:enjs:format
    %+  turn  ~(tap by data)
    |=  [key=@tas [jon=json url=@t]]
    [key jon]
  :_  this
  [%give %fact ~ %json !>(data)]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
++  on-agent
  |=  [wir=wire sin=sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=(%fact -.sin)
    (on-agent:def wir sin)
  ?.  ?=(%json p.cage.sin)
    (on-agent:def wir sin)
  ::
  =/  jon=json   !<(json q.cage.sin)
  =/  name=@tas  (~(got by path-to-tile) wir)
  =/  dat=(unit [json url=@t])  (~(get by data) name)
  ?~  dat  [~ this]
  :_  this(data (~(put by data) name [jon url.u.dat]))
  [%give %fact ~[/main] %json !>((frond:enjs:format name jon))]~
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%bound +<.sin)
    (on-arvo:def wir sin)
  [~ this]
--
