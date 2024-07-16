/@  eyre-reqs
/-  serv=sky-server
/-  srv=server
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %eyre-task ~)
++  kids  *kids:neo
++  deps   
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=&
      ^-  quay:neo
      :-  [pro/%root ~]
      ^-  (unit port:neo)
      :+  ~  %z
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[any/~ ~]
      ==
  ==
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
      ::~&  headers/header-list.request.req
      ~&  >>>  p:(need (~(get by deps.bowl) %src))
      =/  inner=pith:neo  p:(need (~(get by deps.bowl) %src))  ::(pave:neo pax.purl)
      ~&  >  (tail inner)
      :: ~&  ?~  (~(get by deps.bowl) %src)  1/~
      :: ?~  (~(get by ~(tar of:neo q:(need (~(get by deps.bowl) %src)))) (tail inner))  2/~
      :: q.saga:(need (~(get by ~(tar of:neo q:(need (~(get by deps.bowl) %src)))) (tail inner)))
      :: ~&   ~(key by (~(kid of:neo q:(need (~(get by deps.bowl) %src))) (tail inner)))
      =/  =crew:neo  (~(gas by *crew:neo) src/inner ~)
      ~&  >>>  crew/crew
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