/+  *server, default-agent, verb, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero  [%0 =configuration]
+$  configuration
  $:  url-prefix=(unit @t)
  ==
--
%+  verb  |
%-  agent:dbug
=|  state-zero
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  [%pass /chat-bind %arvo %e %connect [~ /'~chat'] %landscape]
      [%pass /group-bind %arvo %e %connect [~ /'~groups'] %landscape]
      [%pass /link-bind %arvo %e %connect [~ /'~link'] %landscape]
      [%pass /dojo-bind %arvo %e %connect [~ /'~dojo'] %landscape]
      [%pass /modulo-bind %arvo %e %connect [~ /'~modulo'] %landscape]
      [%pass /channel-bind %arvo %e %connect [~ /'~channel'] %landscape]
      [%pass /launch-bind %arvo %e %connect [~ /'~launch'] %landscape]
      [%pass /index-bind %arvo %e %connect [~ /] %landscape]
  ==
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  [~ this(state !<(state-zero old-vase))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+  mark  (on-poke:def mark vase)
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vase)
    :_  this
    %+  give-simple-payload:app    id
    %+  require-authorization:app  req
    handle-http-request
  ==
  ::
  ++  handle-http-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    ?.  =(src.bowl our.bowl)  [[403 ~] ~]
    =*  req  request.inbound-request
    =/  req-line  (parse-request-line url.req)
    ?+  method.req  not-found:gen
        %'GET'  (handle-get header-list.req req-line)
    ==
  ::
  ++  handle-get
    |=  [headers=header-list:http req-line=request-line]
    ^-  simple-payload:http
    ?~  ext.req-line
      $(req-line [[[~ %html] (snoc site.req-line 'index')] args.req-line])
    ?~  site.req-line  not-found:gen
    =*  url-prefix  url-prefix.configuration
    ?.  ?|  ?&(?=(^ url-prefix) =((need url-prefix) i.site.req-line))
            =(url-prefix ~)
        ==
      not-found:gen
    =/  file=(unit octs)
      ?+  site.req-line  ~
          [%'' %'index' ~]  (get-file-at /app/launch [t.site u.ext]:req-line)
          [%'~launch' *]    (get-file-at /app/launch [t.site u.ext]:req-line)
          [%'~link' *]      (get-file-at /app/link [t.site u.ext]:req-line)
          [%'~chat' *]      (get-file-at /app/chat [t.site u.ext]:req-line)
          [%'~dojo' *]      (get-file-at /app/soto [t.site u.ext]:req-line)
          [%'~groups' *]    (get-file-at /app/groups [t.site u.ext]:req-line)
          [%'~modulo' %session ~]
        %-  some
        %-  as-octt:mimes:html
        %+  weld
          "window.ship = '{+:(scow %p our.bowl)}';"
        "window.urb = new Channel();"
      ::
          [%'~channel' %channel ~]
        (get-file-at /app/launch/js /channel u.ext.req-line)
      ==
    ?~  file  not-found:gen
    ?+  u.ext.req-line  not-found:gen
        %html  (html-response:gen u.file)
        %js    (js-response:gen u.file)
        %css   (css-response:gen u.file)
        %png   (png-response:gen u.file)
    ==
  ::
  ++  get-file-at
    |=  [base=path file=path ext=@ta]
    ^-  (unit octs)
    ?.  ?=(?(%html %css %js %png) ext)  ~
    =/  =path
      :*  (scot %p our.bowl)
          q.byk.bowl
          (scot %da now.bowl)
          (snoc (weld base file) ext)
      ==
    ?.  .^(? %cu path)  ~
    (some (as-octs:mimes:html .^(@ %cx path)))
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+  path  (on-watch:def path)
      [%http-response *]  [~ this]
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+  +<.sign  (on-arvo:def wire sign)
      %bound   [~ this]
  ==
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
