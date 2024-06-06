/@  htmx-type=htmx
/-  feather-icons
/-  serv=sky-server
/>  htmx
/<  node
=>
|%
++  manx-to-octs
  |=  man=manx
  (as-octt:mimes:html (en-xml:html man))
++  eyre-cards
  |=  [eyre-id=@ta =bowl:neo status=@ud =manx]
  ^-  (list card:neo)
  =/  =pith:neo  #/[p/our.bowl]/$/eyre
  =/  head=sign:eyre:neo  [eyre-id %head [status [['content-type' 'text/html'] ~]]]
  =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
  =/  done=sign:eyre:neo  [eyre-id %done ~]
  :~  [pith %poke eyre-sign/!>(head)]
      [pith %poke eyre-sign/!>(data)]
      [pith %poke eyre-sign/!>(done)]
      [here.bowl %cull ~]
  ==
++  default-refresher
  |=  =pith
  =/  tath  (en-tape:pith:neo pith)
  ;div
    =hx-get  "/neo/hawk{tath}"
    =hx-target  "closest .hawk"
    =hx-select  ".hawk"
    =hx-trigger  "load once"
    =hx-swap  "outerHTML"
    ;
  ==
++  hawk
  |_  [here=pith main=manx raw=manx has-app=?]
  ++  id  *@da
  ++  our-tape
    =/  f  (snag 0 here)
    ?@(f (trip f) (scow f))
  ++  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
  ++  slot  0  :: XX fix sky positional saving
  ++  lift
    ^-  manx
    ;div.hawk.fc.wf.hf
      =id  "hawk-{idt}"
      =hx-params  "id,slot"
      =hx-vals  "\{\"id\": \"{<id>}\", \"slot\": \"{<slot>}\"}"
      ;+  header
      ;div
        =class  "raw p-page wf hf b0 scroll-y scroll-x {(trip ?:(has-app 'hidden' ''))}"
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
            ;div.f4.s-1: >
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
        ;button.p1.hover.b2.br1.loader.s-1
          =id  "hawk-slide-up-{idt}"
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff"
          =hx-target  "find .loading"
          =hx-swap  "outerHTML"
          =head  "slide-up"
          =hawk-slot  "{<slot>}"
          ;span.loaded
            ;+  chevron-left:feather-icons
          ==
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;button.p1.hover.b2.br1.loader.s-1
          =id  "hawk-slide-down-{idt}"
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff"
          =hx-target  "find .loading"
          =hx-swap  "outerHTML"
          =head  "slide-down"
          =hawk-slot  "{<slot>}"
          ;span.loaded
            ;+  chevron-right:feather-icons
          ==
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
        ;button.p1.hover.b2.br1.loader.s-1
          =id  "hawk-close-{idt}"
          =hx-post  "/neo/hawk/{our-tape}/sky?stud=sky-diff"
          =hx-target  "find .loading"
          =hx-swap  "outerHTML"
          =head  "minimize"
          =hawk-slot  "{<slot>}"
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
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   *(set stud:neo)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  ~
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=|
      ^-  quay:neo
      :-  [pro/%htmx ~]
      :::-  [[%or rol/[%ui-main pro/%htmx] pro/%htmx ~] ~]
      ^-  (unit port:neo)
      :+  ~  %z
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[any/~ ~]
          ::`lash:neo`[[%or rol/[%link pro/%htmx] pro/%htmx any/~ ~] ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `pail
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    ::XX revive when auth
    ::?.  authenticated.req
    ::  %:  eyre-cards
    ::      eyre-id
    ::      bowl
    ::      403
    ::      ;div: 403
    ::  ==
    ?~  src=(~(get by deps.bowl) %src)
      =/  main=manx
        ;div.wf.hf.fc.jc.ac: nothing here
      =/  raw
        ;div.wf.hf.fc.jc.ac: raw view
      %:  eyre-cards
          eyre-id
          bowl
          200
          ~(lift hawk #/[p/our.bowl] main raw |)
      ==
    =/  here  p.u.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      =/  root=idea:neo  (~(got of:neo q.u.src) /)
      ?>  =(%htmx p.pail.root)
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  now.bol  now.bowl
      =.  eny.bol  eny.bowl
      =.  kids.bol  q.u.src
      ::  XX src.bowl
      =/  main  (!<(htmx-type q.pail.root) bol)
      =/  raw
        ;div.wf.hf.fc.jc.ac: raw view
      %:  eyre-cards
          eyre-id
          bowl
          200
          ~(lift hawk here.bol main raw &)
      ==
    ::
        %'POST'
      =/  purl  (parse-url:serv request.req)
      =/  body  (parse-body:serv request.req)
      =/  poke-stud
        ^-  stud:neo
        ~|  %no-stud-specified
        (~(got by pam.purl) 'stud')
      =/  mul  (mule |.((node [poke-stud body])))
      ?-    -.mul
          %.n
        %:  eyre-cards
            eyre-id
            bowl
            400
            ;div
              ;*
              %+  turn  (tang p.mul)
              |=  =tank
              ;div: {(of-wall:format (~(win re tank) 0 55))}
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
            manx
        ==
      ==
    ==
  --
--
