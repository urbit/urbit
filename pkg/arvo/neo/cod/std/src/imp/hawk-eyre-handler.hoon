/@  htmx-type=htmx
/-  feather-icons
/-  serv=sky-server
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
    =/  purl  (parse-url:serv request.req)
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
      =/  pit=pith:neo  (pave:neo pax:(parse-url:serv request.req))
      %:  eyre-cards
          eyre-id
          bowl
          404
          ['content-type' 'text/html']~
          ~(lift hawk pit stub stub & meta)
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
            ~(lift hawk here.bol stub stub & meta)
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
        [(sky-move-tab bol slot) c]
      %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          ~(lift hawk here.bol main (raw-view bol(kids q.u.src)) has-conversion meta)
      ==
    ::
        %'POST'
      =/  purl  (parse-url:serv request.req)
      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
      =/  body  (parse-body:serv request.req)
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
      =/  purl  (parse-url:serv request.req)
      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
      =/  body  (parse-body:serv request.req)
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
  (as-octt:mimes:html (en-xml:html man))
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
    =hx-target  "closest .hawk"
    =hx-select  ".hawk"
    =hx-trigger  "load once"
    =hx-indicator  "closest .loader"
    =hx-swap  "outerHTML"
    ;+  loading.feather-icons
  ==
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
  |_  [here=pith main=manx raw=manx has-app=? meta=[@da @ud]]
  ++  our-tape
    =/  f  (snag 0 here)
    ?@(f (trip f) (scow f))
  ++  id  -.meta
  ++  slot  +.meta
  ++  slot-tag
    ::  XX  oh boy this is hacky.
    ::  working with slots in ssr is tough
    ?:  =(slot 999)  "s-1"
    "s{<slot>}"
  ++  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
  ++  vals
    %^  cat  3  'js:{'
    %^  cat  3  '"hawk-id": $(event?.target)?.closest("[slot]").attr("hawk-id") || "~2001.1.1"'
    %^  cat  3  ', '
    %^  cat  3  'slot: $(event?.target)?.closest("[slot]").attr("slot")?.slice(1) || "0"'
    '}'
  ++  lift
    ^-  manx
    ;div.hawk.fc.wf.hf
      =id  "hawk-{idt}"
      =hawk-id  (scow %da id)
      =slot  slot-tag
      =hx-params  "hawk-id,slot"
      =hx-vals  (trip vals)
      =hx-target  "closest .hawk"
      =hx-target-x  "closest .rendered"
      =hx-target-404  "this"
      ;+  header
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
  ++  header
    ;header.b2.p1.frw.g1.ac
      =id  "hawk-header-{idt}"
      =style  "border: 2px solid var(--b2);"
      ;button
        =class  "p1 hover b2 br1 bd0 {(trip ?:(has-app '' 'toggled'))}"
        =onclick
          """
          $(this).toggleClass('toggled');
          $(this).closest('.hawk').find('.raw').toggleClass('hidden');
          $(this).closest('.hawk').find('.rendered').toggleClass('hidden');
          $(this).closest('header').children('.hawk-tog').toggleClass('hidden');
          """
        ;+  outline:feather-icons
      ==
      ;div
        =class  "hawk-tog frw g1 ac grow {(trip ?:(has-app '' 'hidden'))}"
        ;*
          =<  p
          %^  spin  here
                0
              |=  [=iota a=@]
            :_  +(a)
          ;div.fr.ac.g1
            =style  "height: 2rem;"
            ;div.f4.s-1
              ;+  chevron-right.feather-icons
            ==
            ;a.hover.b2.br1.p-1.s0.loader.fc.ac.jc
              =style  "height: 2rem;"
              =hx-vals  "\{\"id\": \"{<id>}\", \"slot\": \"{<slot>}\"}"
              =href  "/neo/hawk{(en-tape:pith:neo (scag +(a) here))}"
              ;span.loaded
                ;+  ;/
                ?:  =(a 0)  "/"
                (trip ?@(iota iota (scot iota)))
              ==
              ;span.loading
                ;+  loading.feather-icons
              ==
            ==
          ==
        ;div.grow;
      ==
      ;form
        =class  "hawk-tog grow fr m0 relative {(trip ?:(has-app 'hidden' ''))}"
        =style  "height: 2rem;"
        =hx-get  "/neo/hawk"
        =hx-target  "closest .hawk"
        ;div.absolute
          =style  "top: 0.5rem; right: 0.5rem;"
          ;div.loader
            ;div.loaded(style "opacity: 0"): ---
            ;div.loading
              ;+  loading:feather-icons
            ==
          ==
        ==
        ;input.p-1.br1.b1.wf.s0.loaded.grow.bd0
          =style  "margin-left: 5px;"
          =type  "text"
          =value  (en-tape:pith:neo here)
          =oninput
            """
            $(this).attr('value', this.value);
            $(this).parent().attr('hx-get', '/neo/hawk'+this.value);
            htmx.process(document.body);
            """
          ;
        ==
      ==
      ;div.fr.ac.jc.g1.hawk-actions
        =id  "hawk-actions-{idt}"
        =hx-ext  "ignore:html-enc"
        ;button.p1.hover.b2.br1.loader.s-1
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff&head=slide-up"
          =hx-on-htmx-after-request
            """
            let swaper = $(this).closest('[slot]');
            let slot = parseInt(swaper.attr('slot').slice(1));
            let swapee = $(this).closest('a-i-r').find(`[slot='s$\{slot-1}']`);
            if (swaper.length && swapee.length) \{
              let ee = swapee.attr('slot');
              let er = swaper.attr('slot');
              swapee.attr('slot', er);
              swaper.attr('slot', ee);
            }
            $(this).emit('hawks-moved');
            """
          =type  "button"
          =hx-swap  "none"
          ;span.loaded
            ;+  chevron-left:feather-icons
          ==
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;button.p1.hover.b2.br1.loader.s-1
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff&head=slide-down"
          =hx-on-htmx-after-request
            """
            let swaper = $(this).closest('[slot]');
            let slot = parseInt(swaper.attr('slot').slice(1));
            let swapee = $(this).closest('a-i-r').find(`[slot='s$\{slot+1}']`);
            if (swaper.length && swapee.length) \{
              let ee = swapee.attr('slot');
              let er = swaper.attr('slot');
              swapee.attr('slot', er);
              swaper.attr('slot', ee);
            }
            $(this).emit('hawks-moved');
            """
          =hx-swap  "none"
          =type  "button"
          ;span.loaded
            ;+  chevron-right:feather-icons
          ==
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;button.p1.hover.b2.br1.loader.s-1
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff&head=minimize"
          =hx-swap  "none"
          =type  "button"
          =hx-on-htmx-after-request
            """
            let air = $(this).closest('a-i-r');
            let now = parseInt(air.attr('hawks')) - 1;
            air.attr('hawks', now);
            $(this).closest('[slot]')[0].removeAttribute('slot');
            $(this).emit('hawks-moved');
            """
          ;span.loaded
            ;+  minimize:feather-icons
          ==
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;style
          ;+  ;/  %-  trip
          '''
          @media(max-width: 900px) {
            .hawk-actions {
              display: none !important;
            }
          }
          '''
        ==
      ==
    ==
  --
--
