/@  eyre-reqs
/@  blue
/@  renderer
/-  serv=sky-server
/-  srv=server
/-  b=blue
::  BLUE FALCON - a prototypical alternative to hawk
^-  kook:neo
=<
|%
++  state  pro/%blue
++  poke   (sy %eyre-task %gift ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%renderer (sy %manx ~)]
  ==
++  deps
  %-  ~(gas by *band:neo)
  ~
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  state  !<(blue q.pail)
    ?+    stud  ~|(bad-stud/stud !!)
        %eyre-task
      ~&  >  'blue got an HTTP request'
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      =/  inner=pith:neo
        (pave:neo pax:(parse-url:serv request.req))
      ~&  >  inner   :: /~met/home/diary
      ?.  authenticated.req
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        :_  pail
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request.req)
      ::
      ?+    method.request.req  
          ~|(%unsupported-http-method !!)
          ::  GET: make a renderer at a new session
          ::       and wait for its manx as a %gift
          %'GET'
        =/  sesh=road:neo  #/[uv/(end 3^4 eny.bowl)]
        :_  :-  %blue
            !>  
            :-  renderers.state
            (~(put by sessions.state) sesh task)
        :~  :*  (welp here.bowl sesh) 
                %make
                %diary-ui  ::  XX (~(got by renderers.state) inner)
                `[%renderer !>([sesh ~])]
                (~(gas by *crew:neo) src/inner ~)
            ==
        ==
      ::
          ::  POST: forward poke as manx to session specified by URL
          ::        and update the top-level session's task
          ::        so we know who to respond to when %gift comes in
          %'POST'
        =/  sesh  
          ^-  road:neo
          [(snag 0 inner) ~]
        =/  body  (parse-body:serv request.req)
        :_  :-  %blue
            !>  
            :-  renderers.state
            (~(put by sessions.state) sesh task)
        :~  :-  (welp here.bowl inner) 
            [%poke [%manx !>(body)]]
        ==
      ==
    ::
        ::  gift: A renderer's manx has updated after
        ::        an http request, and now we must respond
        %gift
      ~&  >  '%gift case of blue'
      :_  pail
      =/  sesh  (gift-session:b !<(gift:neo vax))
      =/  ui  (session-ui:b [bowl sesh])
      =/  [eyre-id=@ta req=inbound-request:eyre]
        (~(got by sessions.state) sesh)  :: XX sesh needs to be a unit
      ^-  (list card:neo)
      %:  eyre-cards
        eyre-id
        bowl
        200
        ['content-type' 'text/html']~
        ui
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    ::=/  renderers  
    ::  (malt (limo [[#/[p/our.bowl]/home/diary %diary-ui] ~]))
    =/  renderers  ~
    :_  [%blue !>([renderers ~])]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %blue]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  --
--
::
|%
++  manx-to-octs
  |=  man=manx
  (as-octt:mimes:html (en-xml:html man))
::
++  eyre-cards
  |=  [eyre-id=@ta =bowl:neo status=@ud =header-list:http =manx]
  ^-  (list card:neo)
  =/  =pith:neo  #/[p/our.bowl]/$/eyre
  =/  head=sign:eyre:neo  [eyre-id %head [status header-list]]
  =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
  =/  done=sign:eyre:neo  [eyre-id %done ~]
  :~  [pith %poke eyre-sign/!>(head)]
      [pith %poke eyre-sign/!>(data)]
      [pith %poke eyre-sign/!>(done)]
  ==
--