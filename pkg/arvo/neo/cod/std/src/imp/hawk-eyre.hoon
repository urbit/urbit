/@  eyre-reqs
/-  serv=server
^-  kook:neo
|%
++  state  pro/%eyre-reqs
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
      =/  lin  (parse-request-line:serv url.request.req)
      =/  inner=pith:neo  (pave:neo (slag 2 site.lin))
      ?.  ?=([[%p @] *] inner)
        =/  =manx
          ;div: 400
        =/  =pith:neo  #/[p/our.bowl]/$/eyre
        =/  head=sign:eyre:neo  [eyre-id %head [200 [['content-type' 'text/html'] ~]]]
        =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs:serv manx)]
        =/  done=sign:eyre:neo  [eyre-id %done ~]
        :_  sig/!>(~)
        :~  [pith %poke eyre-sign/!>(head)]
            [pith %poke eyre-sign/!>(data)]
            [pith %poke eyre-sign/!>(done)]
            [here.bowl %cull ~]
        ==
      =/  args  (~(gas by *(map @t @t)) args.lin)
      =/  =crew:neo  (~(gas by *crew:neo) src/inner ~)
      :_  sig/!>(~)
      :~  [(welp here.bowl #/[uv/eny.bowl]) %make %hawk-eyre-handler `[stud vax] crew]
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %hawk]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  --
--

