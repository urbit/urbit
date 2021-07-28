::  file-server [landscape]:
::
::  mounts HTTP endpoints for Landscape (and third-party) user applications
::
/-  srv=file-server, glob
/+  *server, default-agent, verb, dbug
|%
+$  card  card:agent:gall
+$  serving    (map path [=content public=? single-page=?])
+$  content
  $%  [%clay =path]
      [%glob =glob:glob]
  ==
::
+$  state-3
  $:  %3
      =configuration:srv
      =serving
  ==
--
::
%+  verb  &
%-  agent:dbug
::
=|  state-3
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  |^
  :_  this
  (connect /)^~
  ::
  ++  connect
    |=  =path
    ^-  card
    [%pass path %arvo %e %connect [~ path] %file-server]
  --
::
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  |^
  =+  !<(old-state=versioned-state old-vase)
  =?  old-state  ?=(%0 -.old-state)
    %=    old-state
        -  %1
        serving-0
      %-  ~(run by serving-0.old-state)
      |=  [=clay=path public=?]
      ^-  [content ?]
      [[%clay clay-path] public]
    ==
  =?  old-state  ?=(%1 -.old-state)
    %=  old-state
       -  %2
       serving  (~(del by serving.old-state) /'~landscape'/js/index)
    ==
  =?  old-state  ?=(%2 -.old-state)
    %=    old-state
        -  %3
        serving
      %-  ~(run by serving.old-state)
      |=  [=content public=?]
      ^-  [^content ? ?]
      [content public %.y]
    ==
  ?>  ?=(%3 -.old-state)
  =.  serving.old-state
    (~(del by serving.old-state) /)
  =.  serving.old-state
    (~(del by serving.old-state) /[''])
  [~ this(state old-state)]
  ::
  +$  serving-0  (map url-base=path [=clay=path public=?])
  +$  serving-1  (map url-base=path [=content public=?])
  +$  versioned-state
    $%  state-0
        [%1 state-1]
        [%2 state-1]
        state-3
    ==
  ::
  +$  state-0
    $:  %0
        =configuration:srv
        =serving-0
    ==
  +$  state-1
    $:  =configuration:srv
        serving=serving-1
    ==
  --
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  ?+  mark  (on-poke:def mark vase)
      %file-server-action  (file-server-action !<(action:srv vase))
      %handle-http-request
    =+  !<([id=@ta req=inbound-request:eyre] vase)
    :_  this
    %+  give-simple-payload:app  id
    (handle-http-request req)
  ==
  ::
  ++  file-server-action
    |=  act=action:srv
    ^-  (quip card _this)
    |^
    ?-  -.act
        %serve-dir
      =*  url-base  url-base.act
      ?:  (~(has by serving) url-base)
        ~|("url already bound to {<(~(got by serving) url-base.act)>}" !!)
      :-  [%pass url-base %arvo %e %connect [~ url-base] %file-server]~
      %_  this
          serving
        (~(put by serving) url-base clay+clay-base.act public.act spa.act)
      ==
    ::
        %serve-glob
      =*  url-base  url-base.act
      =/  new-bind=?
        (~(has by serving) url-base)
      =.  serving
        (~(put by serving) url-base glob+glob.act public.act %.y)
      :_  this
      ?.  new-bind  ~
      [%pass url-base %arvo %e %connect [~ url-base] %file-server]~
    ::
        %unserve-dir
      :-  [%pass url-base.act %arvo %e %disconnect [~ url-base.act]]~
      this(serving (~(del by serving) url-base.act))
    ::
        %toggle-permission
      ?.  (~(has by serving) url-base.act)
        ~|("url is not bound" !!)
      =/  [=content public=? spa=?]  (~(got by serving) url-base.act)
      :-  ~
      this(serving (~(put by serving) url-base.act [content !public spa]))
    ::
        %set-landscape-homepage-prefix
      =.  landscape-homepage-prefix.configuration  prefix.act
      :_  this
      (give [%configuration configuration])
    ==
    ::
    ++  give
      |=  =update:srv
      ^-  (list card)
      [%give %fact [/all]~ [%file-server-update !>(update)]]~
    --
  ::
  ++  handle-http-request
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    =*  req       request.inbound-request
    =*  headers   header-list.req
    =/  req-line  (parse-request-line url.req)
    ?.  =(method.req %'GET')  not-found:gen
    ?:  ?=([%'~landscape' %js %session ~] site.req-line)
      %+  require-authorization-simple:app
        inbound-request
      %.  %-  as-octs:mimes:html
          (rap 3 'window.ship = "' (rsh 3 (scot %p our.bowl)) '";' ~)
      %*  .  js-response:gen
        cache  %.n
      ==
    ::
    =/  =path
      ?~  site.req-line
        /
      /[i.site.req-line]
    ?.  (~(has by serving) path)
      (redirect:gen '/~grid')
    =/  is-file   ?=(^ ext.req-line)
    =/  =content  content:(~(got by serving) path)
    ?>  ?=(%glob -.content)
    =/  suffix=^path
      (weld (slag (lent path) site.req-line) (drop ext.req-line))
    =/  data=mime
      (~(gut by glob.content) suffix (~(got by glob.content) /index/html))
    =/  mime-type=@t  (rsh 3 (crip <p.data>))
    =/  headers
      :~  content-type+mime-type 
          max-1-wk:gen 
          'service-worker-allowed'^'/'
      ==
    %+  require-authorization-simple:app  inbound-request
    [[200 headers] `q.data]
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-watch:def path)
      [%http-response *]  [~ this]
      [%all ~]            [(give [%configuration configuration]) this]
  ==
  ::
   ++  give
    |=  =update:srv
    ^-  (list card)
    [%give %fact ~ [%file-server-update !>(update)]]~
  --
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+  +<.sign  (on-arvo:def wire sign)
      %bound
    ?:  accepted.sign  [~ this]
    ~&  [dap.bowl %failed-to-bind path.binding.sign]
    [~ this(serving (~(del by serving) path.binding.sign))]
  ==
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
      [%x %clay %base %hash ~]
    ::  TODO: have web client ask kiln directly
    =/  ego  (scot %p our.bowl)
    =/  wen  (scot %da now.bowl)
    ``hash+!>(.^(@ %gx /[ego]/hood/[wen]/kiln/base-hash/noun))
  ==
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
