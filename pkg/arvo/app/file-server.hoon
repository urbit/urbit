::  file-server [landscape]:
::
::  mounts HTTP endpoints for Landscape (and third-party) user applications
::
/-  srv=file-server, glob
/+  *server, default-agent, verb, dbug, version
|%
+$  card  card:agent:gall
+$  serving    (map url-base=path [=content public=? single-page=?])
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
%+  verb  |
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
      %-  js-response:gen
      (as-octt:mimes:html "window.ship = '{+:(scow %p our.bowl)}';")
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
        (get-content pax is-file)
      ?~  content  [not-found:gen %.n]
      ?-  -.content.u.content
          %clay
        =/  scry-path=path
          :*  (scot %p our.bowl)
              q.byk.bowl
              (scot %da now.bowl)
              (lowercase (weld path.content.u.content suffix.u.content))
          ==
        ?.  .^(? %cu scry-path)  [not-found:gen %.n]
        =/  file  (as-octs:mimes:html .^(@ %cx scry-path))
        :_  public.u.content
        ?+  ext.req-line  not-found:gen
            [~ %js]    (js-response:gen file)
            [~ %css]   (css-response:gen file)
            [~ %png]   (png-response:gen file)
          ::
              [~ %html]
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
        =/  mime-type=@t  (rsh 3 1 (crip <p.u.data>))
        ::  Should maybe inspect to see how long cache should hold
        ::
        [[200 ['content-type' mime-type] max-1-da:gen ~] `q.u.data]
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
    ++  get-content
      |=  [pax=path is-file=?]
      ^-  (unit [content path ?])
      =/  first-try  (match-content-path pax (~(del by serving) /) is-file)
      ?^  first-try  first-try
      =/  root  (~(get by serving) /)
      ?~  root  ~
      (match-content-path pax (~(gas by *^serving) [[/ u.root] ~]) is-file)
    ::
    ++  match-content-path
      |=  [pax=path =^serving is-file=?]
      ^-  (unit [content path ?])
      %-  ~(rep by serving)
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
    =/  versions  (base-hash:version [our now]:bowl)
    ``hash+!>(?~(versions 0v0 (end 0 25 i.versions)))
  ==
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
