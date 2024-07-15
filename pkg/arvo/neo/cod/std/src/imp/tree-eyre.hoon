/@  eyre-reqs
/-  serv=sky-server
/-  srv=server
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %eyre-task ~)
++  kids  *kids:neo
++  deps   *deps:neo
++  form 
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ?+  stud  ~|(bad-stud/stud !!)
        %eyre-task
      =+  !<(=task:eyre:neo vase)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      ?.  authenticated.req
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        :_  pail
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request.req)
      =/  purl  (parse-url:serv request.req)
      =/  inner=pith:neo  (pave:neo pax.purl)
      =/  =crew:neo  (~(gas by *crew:neo) src/inner ~)
      =/  =made:neo  [%tree-eyre-handler `[stud vase] crew]
      :_  sig/!>(~)
      :~  [(welp here.bowl #/[uv/(end 3^4 eny.bowl)]) %make made]
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %tree]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  --
--