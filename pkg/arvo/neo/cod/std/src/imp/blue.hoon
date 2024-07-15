/@  eyre-reqs
/@  blue
/-  serv=sky-server
/-  srv=server
::  BLUE FALCON - a prototypical alternative to hawk
^-  kook:neo
|%
++  state  pro/%blue
++  poke   (sy %eyre-task ~)
++  kids
  :+  ~  %y
  ^-  (map pish:neo lash:neo)
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%eyre-task ~]
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
    ?+    stud  ~|(bad-stud/stud !!)
        %eyre-task
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      ?.  authenticated.req
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        :_  pail
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request.req)
      =/  purl  (parse-url:serv request.req)
      =/  inner=pith:neo  (pave:neo pax.purl)
      ~&  >  inner
      =/  =crew:neo  (~(gas by *crew:neo) src/inner ~)
      =/  state  !<(blue q.pail)
      ::=/  rend=(unit stud:neo)
      ::  (~(get by renderers.state) inner)
      =/  rend  [~ %diary-ui]
      ~&  >  rend
      ::  XX handle case where there's no UI saved in shadow
      ::  XX allow user to specify a UI via url params
      ::  XX handle wizard and tree cases via URL params
      ::  XX handle case where nothing exists at this pith
      ::     in here rather than in the renderer
      ?~  rend  !!
      =/  =made:neo  [(need rend) `[stud vax] crew]
      ~&  >  made
      :_  pail
      :~  [(welp here.bowl #/[uv/(end 3^4 eny.bowl)]) %make made]
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    =/  renderers  
      (malt (limo [[#/[p/our.bowl]/home/diary %diary-ui] ~]))
    :_  [%blue !>(renderers)]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %blue]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  --
--
