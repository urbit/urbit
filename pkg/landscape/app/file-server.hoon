::  file-server [landscape]:
::
::  mounts HTTP endpoints for Landscape (and third-party) user applications
::
/-  srv=file-server, glob
/+  *server, default-agent, verb, dbug
|%
+$  card  card:agent:gall
+$  serving    (map url-base=path [=content public=? single-page=?])
+$  content
  $%  [%clay =path]
      [%glob =glob:glob]
  ==
::
+$  state-4
  $:  %4
      =configuration:srv
      =serving
  ==
--
::
%+  verb  |
%-  agent:dbug
::
=|  state-4
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  |^
  :_  %_  this
          serving
        %-  ~(gas by *^serving)
        %+  turn
          ^-  (list path)
          [/ /'~landscape' ~]
        |=(pax=path [pax [clay+/app/landscape %.n %.y]])
      ==
  :~  (connect /)
      (connect /'~landscape')
      [%pass /serve-who %arvo %e %serve [~ /who] %home /gen/who/hoon ~]
  ==
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
  =|  cards=(list card)
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
  =?  cards  ?=(%3 -.old-state)
    :_  cards
    [%pass /serve-who %arvo %e %serve [~ /who] %home /gen/who/hoon ~]
  =?  old-state  ?=(%3 -.old-state)
    old-state(- %4)
  ?>  ?=(%4 -.old-state)
  [cards this(state old-state)]
  ::
  +$  serving-0  (map url-base=path [=clay=path public=?])
  +$  serving-1  (map url-base=path [=content public=?])
  +$  serving-3  (map url-base=path [=content public=? single-page=?])
  +$  versioned-state
    $%  state-0
        [%1 state-1]
        [%2 state-1]
        state-3
        state-4
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
  +$  state-3
    $:  %3
        =configuration:srv
        serving=serving-3
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
      ?:  (~(has by serving) url-base)
        ~|("url already bound to {<(~(got by serving) url-base.act)>}" !!)
      :-  [%pass url-base %arvo %e %connect [~ url-base] %file-server]~
      this(serving (~(put by serving) url-base glob+glob.act public.act %.y))
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
    |^
    =*  req       request.inbound-request
    =*  headers   header-list.req
    =/  req-line  (parse-request-line url.req)
    ?.  =(method.req %'GET')  not-found:gen
    =.  site.req-line
      %+  murn  site.req-line
      |=  =cord
      ^-  (unit ^cord)
      ?:(=(cord '') ~ `cord)
    =/  is-file   ?=(^ ext.req-line)
    =?  req-line  ?=(~ ext.req-line)
      [[[~ %html] (snoc site.req-line 'index')] args.req-line]
    ?~  site.req-line
      not-found:gen
    =*  url-prefix  landscape-homepage-prefix.configuration
    ?.  ?|  ?=(~ url-prefix)
            =(u.url-prefix i.site.req-line)
        ==
      not-found:gen
    ::
    ?:  ?=([%'~landscape' %js %session ~] site.req-line)
      %+  require-authorization-simple:app
        inbound-request
      %.  %-  as-octs:mimes:html
          (rap 3 'window.ship = "' (rsh 3 (scot %p our.bowl)) '";' ~)
      %*  .  js-response:gen
        cache  %.n
      ==
    ::
    =/  [payload=simple-payload:http public=?]  (get-file req-line is-file)
    ?:  public  payload
    (require-authorization-simple:app inbound-request payload)
    ::
    ++  get-file
      |=  [req-line=request-line is-file=?]
      ^-  [simple-payload:http ?]
      =/  pax=path
        ?~  ext.req-line  site.req-line
        (snoc site.req-line u.ext.req-line)
      =/  content=(unit [=content suffix=path public=?])
        (match-content-path pax is-file)
      ?~  content  [not-found:gen %.n]
      ?-  -.content.u.content
          %clay
        =/  scry-start=path
          :*  (scot %p our.bowl)
              q.byk.bowl
              (scot %da now.bowl)
              path.content.u.content
          ==
        =/  scry-path=path
          (weld scry-start (lowercase suffix.u.content))
        =?  scry-path  !.^(? %cu scry-path)
          (weld scry-start /index/html)
        ?.  .^(? %cu scry-path)  [not-found:gen %.n]
        ?:  ?=([~ %woff2] ext.req-line)
          :_  public.u.content
          [[200 [['content-type' '/font/woff2'] ~]] `.^(octs %cx scry-path)]
        =/  file  (as-octs:mimes:html .^(@ %cx scry-path))
        :_  public.u.content
        =/  ext  (rear scry-path)
        ?+  ext  not-found:gen
            %js    (js-response:gen file)
            %css   (css-response:gen file)
            %png   (png-response:gen file)
            %svg   (svg-response:gen file)
            %ico   (ico-response:gen file)
          ::
              %html
            %.  file
            %*    .   html-response:gen
                cache
              !=(/app/landscape/index/html (slag 3 scry-path))
            ==
        ==
      ::
          %glob
        =/  data=(unit mime)
          (~(get by glob.content.u.content) suffix.u.content)
        ?~  data
          [not-found:gen %.n]
        :_  public.u.content
        =/  mime-type=@t  (rsh 3 (crip <p.u.data>))
        =/  headers
          :~  content-type+mime-type 
              max-1-wk:gen 
              'service-worker-allowed'^'/'
          ==
        [[200 headers] `q.u.data]
      ==
    ::
    ++  lowercase
      |=  upper=(list @t)
      %+  turn  upper
      |=  word=@t
      %-  crip
      %+  turn  (rip 3 word)
      |=  char=@t
      ?.  &((gte char 'A') (lte char 'Z'))
        char
      (add char ^~((sub 'a' 'A')))
    ::
    ++  match-content-path
      |=  [pax=path is-file=?]
      ^-  (unit [content path ?])
      %+  roll
        %+  sort  ~(tap by serving)
        |=  [[a=path *] [b=path *]]
        (gth (lent a) (lent b))
      |=  $:  [url-base=path =content public=? spa=?]
              out=(unit [content path ?])
          ==
      ?^  out  out
      =/  suf  (get-suffix url-base pax)
      ?~  suf  ~
      =-  `[content - public]
      ?:  ?&(spa !is-file)
        /index/html
      u.suf
    ::
    ++  get-suffix
      |=  [a=path b=path]
      ^-  (unit path)
      ?:  (gth (lent a) (lent b))  ~
      |-
      ?~  a  `b
      ?~  b  ~
      ?.  =(i.a i.b)  ~
      %=  $
        a  t.a
        b  t.b
      ==
    --
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
  ::
      [%x %our ~]
    ``json+!>(s+(scot %p our.bowl))
  ::
      [%x %url *]
    =/  url  t.t.path
    ``noun+!>((~(has by serving) url))
  ==
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
