/@  eyre-reqs
/@  blue
/@  renderer
/-  serv=sky-server
/-  srv=server
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %eyre-task ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%sig (sy %http-request ~)]
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
    ?>  =(%eyre-task stud)  
    =+  !<(=task:eyre:neo vax)
    =/  [eyre-id=@ta req=inbound-request:eyre]  task
    =/  inner=pith:neo
      (pave:neo pax:(parse-url:serv request.req))
    ?.  authenticated.req
      =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
      :_  pail
      %+  ~(respond neo:srv eyre)   eyre-id
      (login-redirect:gen:srv request.req)
    =/  color  #/blue  :: XX derive this from URL param
    :_  pail
    :~  :-  (welp here.bowl color) 
        [%poke [stud vax]]
    ==
  ::
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %prism]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
        [(welp here.bowl #/blue) %make %blue ~ ~]
    ==
  --
--