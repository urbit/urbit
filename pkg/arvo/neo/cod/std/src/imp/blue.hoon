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
  ++  init
    |=  pal=(unit pail:neo)
    [~ [%blue !>([~ ~])]]
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
        :_  blue/!>([renderers.state `eyre-id])
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
        ::        If top-level and in response to a request, 
        ::        forward to corresponding eyre-id.
        %gift
      ?~  open-eyre-id.state  [~ pail]
      =/  id  u.open-eyre-id.state
      =/  gift  !<(gift:neo vax)
      =/  sesh  (get-session:b gift)
      ?~  sesh  [~ pail]
      :_  blue/!>([renderers.state ~])
      ^-  (list card:neo)
      =+  #/[p/our.bowl]/$/eyre
      :~  :*  - 
              %poke 
              %eyre-sign
              !>
              :^    id 
                  %head 
                200
              ['content-type' 'text/html']~
          ==
      ::
          :*  - 
              %poke 
              %eyre-sign
              !>
              :+  id 
                %data
              :-  ~
              %-  as-octt:mimes:html
              %-  en-xml:html
              (get-ui:b [bowl u.sesh])
          ==
      ::
          [- %poke eyre-sign/!>([id %done ~])]
      ==
    ==
  --
--