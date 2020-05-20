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
      serving=(map url-base=path [clay-base=path public=?])
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
  :_  this(serving (~(put by serving) /'~landscape' [/app/landscape %.n]))
  [%pass /index-bind %arvo %e %connect [~ /] %landscape]~
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
      ?:  (~(has by serving) url-base.act)
        ~|("url already bound to {<(~(got by serving) url-base.act)>}" !!)
      :-  ~
      this(serving (~(put by serving) url-base.act [clay-base.act public.act]))
    ::
        %unserve-dir
      [~ this(serving (~(del by serving) url-base.act))]
    ::
        %toggle-permission
      ?.  (~(has by serving) url-base.act)
        ~|("url is not bound" !!)
      =/  [clay-base=path public=?]  (~(got by serving) url-base.act)
      :-  ~
      this(serving (~(put by serving) url-base.act [clay-base !public]))
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
    ::
    ?:  ?=([%'~landscape' %js %session ~] site.req-line)
      %+  require-authorization-simple:app
        inbound-request
      %-  js-response:gen
      (as-octt:mimes:html "window.ship = '{+:(scow %p our.bowl)}';")
    ::
    =/  [payload=simple-payload:http public=?]  (get-file req-line)
    ?:  public
      payload
    (require-authorization-simple:app inbound-request payload)
  ::
  ::
  ++  get-clay-path
    |=  pax=path
    ^-  (unit [path ?])
    %-  ~(rep by serving)
    |=  [[url-base=path clay-base=path public=?] out=(unit [path ?])]
    ?^  out
      out
    =/  suf  (get-suffix url-base pax)
    ?~  suf  ~
    `[(weld clay-base u.suf) public]
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
  ++  get-file
    |=  req-line=request-line
    ^-  [simple-payload:http ?]
    =/  pax=path  (snoc site.req-line (need ext.req-line))
    =/  clay-path=(unit [path ?])  (get-clay-path pax)
    ?~  clay-path
      [not-found:gen %.n]
    =/  scry-path
      :*  (scot %p our.bowl)
          q.byk.bowl
          (scot %da now.bowl)
          -.u.clay-path
      ==
    ?.  .^(? %cu scry-path)
      [not-found:gen %.n]
    =/  file  (as-octs:mimes:html .^(@ %cx scry-path))
    :_  +.u.clay-path
    ?+  ext.req-line  not-found:gen
        [~ %html]  (html-response:gen file)
        [~ %js]    (js-response:gen file)
        [~ %css]   (css-response:gen file)
        [~ %png]   (png-response:gen file)
    ==
  ::
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
