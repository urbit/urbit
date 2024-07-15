/@  htmx-type=htmx
/-  serv=sky-server
/-  render
/>  htmx
/<  node
/<  http-request
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   *(set stud:neo)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=&
      ^-  quay:neo
      :-  [pro/%diary (sy %diary-diff ~)]
      ^-  (unit port:neo)
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  [[%.n %da] %.n]
          `lash:neo`[[%only %txt] ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    !!
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    ~&  >  "*** diary-ui on-init ***"
    =/  [=stud:neo =vase]  (need pal)
    :_  [stud vase]
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    =/  purl  (parse-url:serv request.req)
    =/  id=@da  (slav %da (~(gut by pam.purl) 'hawk-id' '~2000.1.1'))
    =/  slot=@ud  (slav %ud (~(gut by pam.purl) 'slot' '999'))
    =/  meta  [id slot]
    =/  src  (~(got by deps.bowl) %src)
    =/  here  p.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      =/  root=idea:neo  (~(got of:neo q.src) /)
      =;  ui
        %:  eyre-cards:render
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          ui
        ==
      ^-  manx
      ;div.fc.jc.ac.wf.hf
        ; hello world
      ==
::        %'POST'
::      =/  purl  (parse-url:serv request.req)
::      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
::      =/  body  (parse-body:serv request.req)
::      =/  poke-stud
::        ^-  stud:neo
::        ~|  %no-stud-specified
::        (~(got by pam.purl) 'stud')
::      =/  mul
::        %-  mule
::        |.
::        ?:  =(content-type 'application/x-www-form-urlencoded')
::          (http-request [poke-stud `request:http`request.req])
::        (node [poke-stud body])
::      ?-    -.mul
::          %.n
::        %:  eyre-cards
::            eyre-id
::            bowl
::            500
::            :~
::              ['content-type' 'text/html']
::              ['HX-Reswap' 'outerHTML']
::            ==
::            ;div.b0.p-page.wf.hf.fc.g2.as
::              ;a.p2.br1.bd1.b1.hover.loader.block
::                =href  "/neo/hawk{(spud pax.purl)}"
::                ;span.loaded: reload
::                ;span.loading
::                  ;+  loading.feather-icons
::                ==
::              ==
::              ;+  (print-tang (tang p.mul))
::            ==
::        ==
::      ::
::          %.y
::        =/  =pail:neo  [poke-stud p.mul]
::        =/  bol  *bowl:neo
::        =.  here.bol  here
::        =.  our.bol  our.bowl
::        =.  now.bol  now.bowl
::        =.  eny.bol  eny.bowl
::        =/  =manx
::          ?~  converter=(mole |.((htmx pail)))
::            (default-refresher here)
::          =/  mul
::            %-  mule
::            |.((u.converter bol))
::          ?-  -.mul
::            %.y  p.mul
::            %.n  ;div: error
::          ==
::        :-  [here %poke pail]
::        %:  eyre-cards
::            eyre-id
::            bowl
::            200
::            ['content-type' 'text/html']~
::            manx
::        ==
::      ==
    ==
  --
--