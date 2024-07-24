/@  eyre-reqs
/-  serv=server
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %eyre-task ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  ~
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
      :_  sig/!>(~)
      =/  doc
        %-  en-xml:html
        ;html
          ;body
            ;style
              ;+  ;/  %-  trip
              '''
              body {
                color: red;
              }
              '''
            ==
            ;h1: tree stub: {(trip url.request.req)}
          ==
        ==
      =/  head=sign:eyre:neo  [eyre-id %head [200 ~]]
      =/  data=sign:eyre:neo  [eyre-id %data `(as-octt:mimes:html doc)]
      =/  done=sign:eyre:neo  [eyre-id %done ~]
      :~  [#/[p/our.bowl]/$/eyre %poke eyre-sign/!>(head)]
          [#/[p/our.bowl]/$/eyre %poke eyre-sign/!>(data)]
          [#/[p/our.bowl]/$/eyre %poke eyre-sign/!>(done)]
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
