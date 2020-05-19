/-  *landscape
/+  *server, default-agent, verb, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =configuration
      serving=(map url-base=path clay-base=path)
  ==
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
      [%pass /publish-bind %arvo %e %connect [~ /'~publish'] %landscape]
      [%pass /landscape-bind %arvo %e %connect [~ /'~landscape'] %landscape]
      [%pass /index-bind %arvo %e %connect [~ /] %landscape]
  ==
::
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
      %landscape-action
    (landscape-action !<(action vase))
  ::
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vase)
    :_  this
    %+  give-simple-payload:app  id
    (handle-http-request req)
  ==
  ::
  ++  landscape-action
    |=  act=action
    ^-  (quip card _this)
    ?-  -.act
        %serve-dir
      :-  ~
      this(serving (~(put by serving) url-base.act clay-base.act))
        %unserve-dir
      :-  ~
      this(serving (~(del by serving) url-base.act))
    ==
  ::
  ++  handle-http-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    =*  req       request.inbound-request
    =*  headers   header-list.req
    =/  req-line  (parse-request-line url.req)
    ?.  =(method.req %'GET')
      not-found:gen
    =?  req-line  ?=(~ ext.req-line)
      [[[~ %html] (snoc site.req-line 'index')] args.req-line]
    ?>  ?=(^ ext.req-line)
    ?~  site.req-line
      not-found:gen
    =*  url-prefix  url-prefix.configuration
    ?.  ?|  ?&(?=(^ url-prefix) =((need url-prefix) i.site.req-line))
            =(url-prefix ~)
        ==
      not-found:gen
    ?+  site.req-line  (get-custom req-line)
    ::
        [%'' %'index' ~]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ::
        [%'~link' *]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ::
        [%'~chat' *]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ::
        [%'~dojo' *]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ::
        [%'~groups' *]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ::
        [%'~landscape' %js %session ~]
      %+  require-authorization-simple:app
        inbound-request
      %-  js-response:gen
      (as-octt:mimes:html "window.ship = '{+:(scow %p our.bowl)}';")
    ::
        [%'~landscape' *]
      %+  require-authorization-simple:app
        inbound-request
      (get-file-at /app/landscape [t.site u.ext]:req-line)
    ==
  ::
  ::
  ++  get-full-clay-path
    |=  pax=path
    ^-  (unit path)
    %-  ~(rep by serving)
    |=  [[url-base=path clay-base=path] out=(unit path)]
    ?^  out
      out
    =/  suf  (get-suffix url-base pax)
    ?~  suf  ~
    `(weld clay-base u.suf)
  ::
  ++  get-suffix
    |=  [a=path b=path]
    ^-  (unit path)
    ?:  (gth (lent a) (lent b))
      ~
    |-
    ?~  a
      `b
    ?~  b
      ~
    ?.  =(i.a i.b)
      ~
    %=  $
      a  t.a
      b  t.b
    ==
  ::
  ++  get-custom
    |=  req-line=request-line
    ^-  simple-payload:http
    =/  =path  (snoc site.req-line (need ext.req-line))
    =/  clay-path  (get-full-clay-path path)
    ?~  clay-path
      not-found:gen
    =/  scry-path
      :*  (scot %p our.bowl)
          q.byk.bowl
          (scot %da now.bowl)
          u.clay-path
      ==
    ?.  .^(? %cu scry-path)
      not-found:gen
    =/  file  (as-octs:mimes:html .^(@ %cx scry-path))
    ?+  ext.req-line  not-found:gen
        [~ %html]  (html-response:gen file)
        [~ %js]    (js-response:gen file)
        [~ %css]   (css-response:gen file)
        [~ %png]   (png-response:gen file)
    ==
  ::
  ++  get-file-at
    |=  [base=path file=path ext=@ta]
    ^-  simple-payload:http
    =/  =path
      :*  (scot %p our.bowl)
          q.byk.bowl
          (scot %da now.bowl)
          (snoc (weld base file) ext)
      ==
    ?.  .^(? %cu path)
      not-found:gen
    =/  file  (as-octs:mimes:html .^(@ %cx path))
    ?+  ext  not-found:gen
        %html  (html-response:gen file)
        %js    (js-response:gen file)
        %css   (css-response:gen file)
        %png   (png-response:gen file)
    ==
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
