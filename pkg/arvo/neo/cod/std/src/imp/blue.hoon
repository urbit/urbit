/@  eyre-reqs
/@  blue
/@  renderer
/-  serv=sky-server
/-  srv=server
/-  b=blue
^-  kook:neo
|%
++  state  pro/%blue
++  poke   (sy %eyre-task %gift ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%renderer (sy %http-request ~)]
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
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      =/  inner=pith:neo
        (pave:neo pax:(parse-url:serv request.req))
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
            (~(put by sessions.state) [sesh eyre-id])
        :~  :*  (welp here.bowl sesh) 
                %make
                %diary-ui  ::  XX (~(got by renderers.state) inner)
                `[%renderer !>([sesh ~])]
                (~(gas by *crew:neo) src/inner ~)
            ==
        ==
      ::
          ::  POST: forward post to session specified by URL
          %'POST'
        :_  pail
        :~  :-  (welp here.bowl inner) 
            [%poke [%http-request !>(request.req)]]
        ==
      ==
    ::
        ::  gift: A renderer's state has updated.
        ::        Forward to corresponding eyre-id.
        %gift
      =/  gift  !<(gift:neo vax)
      =/  sesh  (gift-session:b gift)
      =/  =mode:neo  mode:(~(got of:neo gift) sesh)
      =/  eyre-id  (~(got by sessions.state) sesh)
      ::
      =/  ui  (session-ui:b [bowl sesh])
      =/  data=sign:eyre:neo  
        :*  eyre-id 
            %data
            `(as-octt:mimes:html (en-xml:html ui))
        ==
      ::
      =/  head=sign:eyre:neo  
        :*  eyre-id 
            %head 
            200
            :~  ['Content-Type' 'text/event-stream']
                ['Cache-Control' 'no-cache']
                ['Connection' 'keep-alive']
            ==
        ==
      ::
      :_  pail
      ^-  (list card:neo)
      =+  #/[p/our.bowl]/$/eyre
      ?-    mode
          %del  
        ~|('%blue got a %del gift' !!)
          %dif  
        :~  [- %poke eyre-sign/!>(data)]
        ==
          %add
        :~  [- %poke eyre-sign/!>(head)]
            [- %poke eyre-sign/!>(data)]
        ==
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    :_  [%blue !>([~ ~])]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %blue]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  --
--