/@  htmx-type=htmx
/-  feather-icons
/-  sky-server
/>  htmx
/<  node
/<  http-request
=<
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   *(set stud:neo)
++  kids  *kids:neo
++  deps  
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=|
      ^-  quay:neo
      :-  [[%or pro/%htmx any/~ ~] ~]
      ^-  (unit port:neo)
      :+  ~  %z
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[[%or pro/%htmx any/~ ~] ~]
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
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    =/  purl  (parse-url:sky-server request.req)
    =/  id=@da  (slav %da (~(gut by pam.purl) 'hawk-id' '~2000.1.1'))
    =/  slot=@ud  (slav %ud (~(gut by pam.purl) 'slot' '999'))
    =/  meta  [id slot]
    ?~  src=(~(get by deps.bowl) %src)
      =/  stub
        ;div.wf.hf.fc.jc.ac.g2
          ;h1: nothing here
          ;a.b1.br1.bd1.hover.loader.p2
            =href  "/neo/hawk/{(scow %p our.bowl)}/home"
            ;span.loaded: go home
            ;span.loading
              ;+  loading.feather-icons
            ==
          ==
        ==
      =/  pit=pith:neo  (pave:neo pax:(parse-url:sky-server request.req))
      %:  eyre-cards
          eyre-id
          bowl
          404
          ['content-type' 'text/html']~
          %-  doc
          ~(lift hawk pit stub stub & meta bowl)
      ==
    =/  here  p.u.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      ?~  reet=(~(get of:neo q.u.src) /)
        =/  bol  *bowl:neo
        =.  here.bol  here
        =/  stub
          ;div.fc.jc.ac.wf.hf
            ; nothing here
          ==
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            %-  doc
            ~(lift hawk here.bol stub stub & meta bowl)
        ==
      =/  root=idea:neo  u.reet
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  now.bol  now.bowl
      =.  eny.bol  eny.bowl
      =.  kids.bol  (~(del of:neo q.u.src) /)
      =/  has-conversion  =(%htmx p.pail.root)
      =/  main
        ?:  has-conversion
          =/  mul
            %-  mule
            |.
            (!<(htmx-type q.pail.root) bol)
          ?-  -.mul
            %.y  p.mul
            %.n
              ;div.b0.p-page.wf.hf.fc.g2.as
                ;+  (print-tang p.mul)
              ==
          ==
        ;div.fc.jc.ac.wf.hf
          ; no renderer for {<p.pail.root>}
        ==
      =;  c
        ?:  (~(has by pam.purl) 'no-save')  c
        c
        :: [(sky-move-tab bol slot) c]
      %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          %-  doc
          ~(lift hawk here.bol main (raw-view bol(kids q.u.src)) has-conversion meta bowl)
      ==
    ::
        %'POST'
      =/  purl  (parse-url:sky-server request.req)
      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
      =/  body  (parse-body:sky-server request.req)
      =/  poke-stud
        ^-  stud:neo
        ~|  %no-stud-specified
        (~(got by pam.purl) 'stud')
      =/  mul
        %-  mule
        |.
        ?:  =(content-type 'application/x-www-form-urlencoded')
          (http-request [poke-stud `request:http`request.req])
        (node [poke-stud body])
      ?-    -.mul
          %.n
        %:  eyre-cards
            eyre-id
            bowl
            500
            :~
              ['content-type' 'text/html']
              ['HX-Reswap' 'outerHTML']
            ==
            ;div.b0.p-page.wf.hf.fc.g2.as
              ;a.p2.br1.bd1.b1.hover.loader.block
                =href  "/neo/hawk{(spud pax.purl)}"
                ;span.loaded: reload
                ;span.loading
                  ;+  loading.feather-icons
                ==
              ==
              ;+  (print-tang (tang p.mul))
            ==
        ==
      ::
          %.y
        =/  =pail:neo  [poke-stud p.mul]
        =/  bol  *bowl:neo
        =.  here.bol  here
        =.  our.bol  our.bowl
        =.  now.bol  now.bowl
        =.  eny.bol  eny.bowl
        =/  =manx
          ?~  converter=(mole |.((htmx pail)))
            (default-refresher here)
          =/  mul
            %-  mule
            |.((u.converter bol))
          ?-  -.mul
            %.y  p.mul
            %.n  ;div: error
          ==
        :-  [here %poke pail]
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            manx
        ==
      ==
    ::
        %'PUT'
      =/  purl  (parse-url:sky-server request.req)
      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
      =/  body  (parse-body:sky-server request.req)
      =/  poke-stud
        ^-  stud:neo
        ~|  %no-stud-specified
        (~(got by pam.purl) 'stud')
      =/  mul
        %-  mule
        |.
        ?:  =(content-type 'application/x-www-form-urlencoded')
          (http-request [poke-stud `request:http`request.req])
        (node [poke-stud body])
      ?-    -.mul
          %.n
        %:  eyre-cards
            eyre-id
            bowl
            500
            :~
              ['content-type' 'text/html']
              ['HX-Reswap' 'outerHTML']
            ==
            ;div.b0.p-page.wf.hf.fc.g2.as
              ;a.p2.br1.bd1.b1.hover.loader.block
                =href  "/neo/hawk{(spud pax.purl)}"
                ;span.loaded: reload
                ;span.loading
                  ;+  loading.feather-icons
                ==
              ==
              ;+  (print-tang (tang p.mul))
            ==
        ==
      ::
          %.y
        =/  =pail:neo  [poke-stud p.mul]
        =/  bol  *bowl:neo
        =.  here.bol  here
        =.  our.bol  our.bowl
        =.  now.bol  now.bowl
        =.  eny.bol  eny.bowl
        =/  =manx
          ?~  converter=(mole |.((htmx pail)))
            (default-refresher here)
          =/  mul
            %-  mule
            |.((u.converter bol))
          ?-  -.mul
            %.y  p.mul
            %.n  ;div: error
          ==
        :-  [here %make poke-stud `pail ~]
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            manx
        ==
      ==
    ==
  --
--
::
|%
++  manx-to-octs
  |=  man=manx
  %-  as-octt:mimes:html
  %+  welp  "<!DOCTYPE html>"
  (en-xml:html man)
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
      [here.bowl %cull ~]
      [here.bowl %tomb ~]
  ==
++  print-tang
  |=  =tang
  ;div.prose
    ;h1: crash!
    ;pre
      ;code
        ;*
        %+  turn  tang
        |=  =tank
        ;/((of-wall:format (~(win re tank) 0 55)))
      ==
    ==
  ==
++  sky-move-tab
  |=  [=bowl:neo slot=@ud]
  ::  assumes location of sky is /sky
  [#/[p/our.bowl]/sky %poke %sky-diff !>([%move-tab slot here.bowl])]
++  default-refresher
  |=  =pith
  =/  tath  (en-tape:pith:neo pith)
  ;div.loading
    =hx-get  "/neo/hawk{tath}"
    =hx-target  "closest .rendered"
    =hx-select  ".rendered"
    =hx-trigger  "load once"
    =hx-indicator  "closest .loader"
    =hx-swap  "outerHTML"
    ;+  loading.feather-icons
  ==
::  XX deprecate in favor of Tree
++  raw-view
  |=  =bowl:neo
  ;div.fc.g1.p-page
    ;details.bd1.br1.b0
      ;summary.p2.bold: state
      ;+
      ?~  node=(~(get of:neo kids.bowl) /)
        ;div.p2: none
      =/  =pail:neo  q.saga.u.node
      ;div.fc.g2.as.p2
        ;a.p1.mono.f2.b1.br1.bd1.hover.loader
          =href  "/neo/hawk/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(p.pail p.pail mark.p.pail))}"
          ;span.loaded: {<p.pail>}
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;div.pre.mono.scroll-x.p2
          ;+  ;/
          =/  size  (met 3 (jam q.q.pail))
          ?:  (gth size 750)  "vase too large to print: {<size>}"
          (of-wall:format (~(win re (sell q.pail)) 0 80))
        ==
      ==
    ==
    ;h2.bold.mt1: children
    ;*
    %+  murn  (flop (sort ~(tap by (~(kid of:neo kids.bowl) /)) aor))
    |=  [=pith:neo *]
    =/  heer  (welp here.bowl pith)
    ?:  ?|
          ::  ignored shrubs
          ?=([[%p @] %cod %grab *] heer)
          ?=([[%p @] %cod %grow *] heer)
          :: ?=([[%p @] %cod %std %out *] heer)
          ?=([[%p @] %cod %std %pre *] heer)
          ?=([[%p @] %out *] heer)
          ?=([[%p @] %srv *] heer)
          ?=([[%p @] %sky *] heer)
        ==
      ~
    :-  ~
    =/  tape  (en-tape:pith:neo pith)
    ;a.p2.b1.br1.bd1.hover.loader
      =href  "/neo/hawk{(en-tape:pith:neo here.bowl)}{tape}"
      ;span.loaded: {tape}
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
++  hawk
  |_  [here=pith main=manx raw=manx has-app=? meta=[@da @ud] =bowl:neo]
  ++  our-tape
    =/  f  (snag 0 here)
    ?@(f (trip f) (scow f))
  ++  id  -.meta
  ++  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
  ++  lift
    ^-  manx
    ;div.hawk.fc.wf.hf.br1
      =id  "hawk-{idt}"
      =hawk-id  (scow %da id)
      =hx-target  "closest .hawk"
      =hx-target-x  "closest .rendered"
      =hx-target-404  "this"
      ;div
        =class  "raw wf hf b0 scroll-y scroll-x {(trip ?:(has-app 'hidden' ''))}"
        ;+  raw
      ==
      ;div
        =class  "rendered wf hf b0 scroll-y scroll-x {(trip ?:(has-app '' 'hidden'))}"
        =id  "hawk-rendered-{idt}"
        =morph-retain  "class"
        ;+  main
      ==
    ==
  --
++  doc
  |=  in=manx
  ^-  manx
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: hawk
      ;*  standard-head-tags.sky-server
      ;meta
        =name  "htmx-config"
        =content  (trip '{"ignoreTitle":"true"}')
        ;
      ==
    ==
    ;body
      =hx-ext  "dom-enc,response-targets,morph"
      =hx-swap  "outerHTML"
      =hx-boost  "true"
      ;+  in
    ==
  ==
--
