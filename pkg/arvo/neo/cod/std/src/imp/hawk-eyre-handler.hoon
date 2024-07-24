/@  htmx-type=htmx
/-  feather-icons
/-  serv=sky-server
/-  sky-wrapper
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
          ~(lift hawk here.bol main (raw-view bol(kids q.u.src)) has-conversion meta bowl)
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
    =hx-target  "closest .rendered"
    =hx-select  ".rendered"
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
  |_  [here=pith main=manx raw=manx has-app=? meta=[@da @ud] =bowl:neo]
  ++  our-tape
    =/  f  (snag 0 here)
    ?@(f (trip f) (scow f))
  ++  id  -.meta
  ++  slot  +.meta
  ++  slot-tag
    ::  XX  oh boy this is hacky.
    ::  working with slots in ssr is tough
    ?:  =(slot 999)  "s0"
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
    %-
      ~(lift sky-wrapper bowl)
    ;div.hawk.fc.wf.hf.br1.iframe-switch
      =id  "hawk-{idt}"
      =hawk-id  (scow %da id)
      =slot  slot-tag
      =hx-params  "hawk-id,slot"
      =hx-vals  (trip vals)
      =hx-target  "closest .hawk"
      =hx-target-x  "closest .rendered"
      =hx-target-404  "this"
      ::  ;+  header
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
    ;header.b2.frw.g1.ac
      =id  "hawk-header-{idt}"
      =style  "padding: 0 4px;"
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
          =/  pad  ?:(=(a 0) "p-1" "p1")
          ;div.fr.ac
            =style  "height: 2rem;"
            ;div.f4.s-2
              ;+  chevron-right.feather-icons
            ==
            ;a
              =class  "hover b2 {pad} s0 loader fc ac jc"
              =style  "height: 2rem;"
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
        =class  "hawk-tog grow fr ac m0 relative {(trip ?:(has-app 'hidden' ''))}"
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
        ;input.br1.b1.wf.s0.loaded.grow.bd0
          =style  "margin-left: 5px; padding: 2px 4px;"
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
++  feather
'''
/**    feather.css
  *      ~2024.4.6
  *
  *    styling resets
  *    and
  *    utility classes
  *
  **/

:root {

  /* --font: 'Urbit Sans';
     --font-mono: 'Monaco';
     --mono-scale: 0.8;
     --letter-spacing: 0.024em;
     --line-height: 1.4;
   */
  --0in: calc(0 * var(--1in));
  --1in: 4px;
  --font-size: calc(4 * var(--1in));
  --2in: calc(2 * var(--1in));
  --3in: calc(3 * var(--1in));
  --4in: calc(4 * var(--1in));
  --5in: calc(5 * var(--1in));
  --6in: calc(6 * var(--1in));
  --7in: calc(7 * var(--1in));
  --8in: calc(8 * var(--1in));
  --9in: calc(9 * var(--1in));

  --10in: calc(10 * var(--1in));
  --11in: calc(11 * var(--1in));
  --12in: calc(12 * var(--1in));
  --13in: calc(13 * var(--1in));
  --14in: calc(14 * var(--1in));
  --15in: calc(15 * var(--1in));
  --16in: calc(16 * var(--1in));
  --17in: calc(17 * var(--1in));
  --18in: calc(18 * var(--1in));
  --19in: calc(19 * var(--1in));

  --20in: calc(20 * var(--1in));
  --21in: calc(21 * var(--1in));
  --22in: calc(22 * var(--1in));
  --23in: calc(23 * var(--1in));
  --24in: calc(24 * var(--1in));
  --25in: calc(25 * var(--1in));
  --26in: calc(26 * var(--1in));
  --27in: calc(27 * var(--1in));
  --28in: calc(28 * var(--1in));
  --29in: calc(29 * var(--1in));

  --30in: calc(30 * var(--1in));
  --31in: calc(31 * var(--1in));
  --32in: calc(32 * var(--1in));
  --33in: calc(33 * var(--1in));
  --34in: calc(34 * var(--1in));
  --35in: calc(35 * var(--1in));
  --36in: calc(36 * var(--1in));
  --37in: calc(37 * var(--1in));
  --38in: calc(38 * var(--1in));
  --39in: calc(39 * var(--1in));
  --40in: calc(40 * var(--1in));


  /* --light-b-3: #dd5522;
     --light-b-2: #ddaa33;
     --light-b-1: #55dd33;
     --light-b0: #dddddd;
     --light-b1: #cccccc;
     --light-b2: #bbbbbb;
     --light-b3: #aaaaaa;
     --light-b4: #999999;
     --light-f-3: #993311;
     --light-f-2: #aaaa22;
     --light-f-1: #339911;
     --light-f0: #111111;
     --light-f1: #333333;
     --light-f2: #444444;
     --light-f3: #555555;
     --light-f4: #777777;

     --dark-b-3: #551111;
     --dark-b-2: #555511;
     --dark-b-1: #225511;
     --dark-b0: #222222;
     --dark-b1: #333333;
     --dark-b2: #444444;
     --dark-b3: #555555;
     --dark-b4: #666666;
     --dark-f-3: #ee7755;
     --dark-f-2: #ccbb33;
     --dark-f-1: #55cc33;
     --dark-f0: #eeeeee;
     --dark-f1: #cccccc;
     --dark-f2: #bbbbbb;
     --dark-f3: #aaaaaa;
     --dark-f4: #888888;
   */
  --b-3: var(--light-b-3);
  --b-2: var(--light-b-2);
  --b-1: var(--light-b-1);
  --b0:  var(--light-b0);
  --b1:  var(--light-b1);
  --b2:  var(--light-b2);
  --b3:  var(--light-b3);
  --b4:  var(--light-b4);
  --f-3: var(--light-f-3);
  --f-2: var(--light-f-2);
  --f-1: var(--light-f-1);
  --f0:  var(--light-f0);
  --f1:  var(--light-f1);
  --f2:  var(--light-f2);
  --f3:  var(--light-f3);
  --f4:  var(--light-f4);
}
@media (prefers-color-scheme: dark) {
  :root {
    --b-3: var(--dark-b-3);
    --b-2: var(--dark-b-2);
    --b-1: var(--dark-b-1);
    --b0:  var(--dark-b0);
    --b1:  var(--dark-b1);
    --b2:  var(--dark-b2);
    --b3:  var(--dark-b3);
    --b4:  var(--dark-b4);
    --f-3: var(--dark-f-3);
    --f-2: var(--dark-f-2);
    --f-1: var(--dark-f-1);
    --f0:  var(--dark-f0);
    --f1:  var(--dark-f1);
    --f2:  var(--dark-f2);
    --f3:  var(--dark-f3);
    --f4:  var(--dark-f4);
  }
}
* {
  font-size: var(--font-size);
}
/*
@media (max-width: 900px) {
  :root {
    --font-size: calc(1.3 * var(--font-size));
  }
}
*/
body {
  font-family: var(--font);
  letter-spacing: var(--letter-spacing);
  line-height: 1;
  height: 100%;
}
button {
  border: inherit;
  background: inherit;
  color: inherit;
}

.break {
  word-break: break-word;
}
.action {
  touch-action: manipulation;
}
.hidden,
.folded {
  display: none !important;
}
.jc {
  justify-content: center;
}
.jb {
  justify-content: space-between;
}
.ja {
  justify-content: space-around;
}
.js {
  justify-content: start;
}
.je {
  justify-content: end;
}
.js {
  justify-content: start;
}
.as {
  align-items: start;
}
.af {
  align-items: stretch;
}
.ae {
  align-items: end;
}
.ac {
  align-items: center;
}
.wfc {
  width: fit-content;
}
.wf {
  width: 100%;
}
.mw-page {
  max-width: 650px;
}
.hf {
  height: 100%;
}
.hfc {
  height: fit-content;
}
.mono {
  font-family: var(--font-mono), monospace;
  font-size: calc(1em * var(--mono-scale));
}
.italic {
  font-style: italic;
}
.underline {
  text-decoration: underline;
}
.bold {
  font-weight: bold;
}
.strike {
  text-decoration: line-through;
}

.pre {
  white-space: pre;
}
.pre-line {
  white-space: pre-line;
}

.tl {
  text-align: left;
}
.tc {
  text-align: center;
}
.tr {
  text-align: right;
}

.block {
  display: block;
}
.inline {
  display: inline-block;
}

.fc {
  display: flex;
  flex-direction: column;
}
.fcr {
  display: flex;
  flex-direction: column-reverse;
}
.fr {
  display: flex;
  flex-direction: row;
}
.frw {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}
.basis-full {
  flex-basis: 100%;
}
.basis-half {
  flex-basis: 50%;
  flex-shrink: 0;
}
.basis-none {
  flex-basis: 0%;
  flex-shrink: 1;
}
.shrink-0 {
  flex-shrink: 0;
}
.relative {
  position: relative;
}
.absolute {
  position: absolute;
}
.fixed {
  position: fixed;
}
.sticky {
  position: sticky;
}

.z-2 {
  z-index: -20;
}
.z-1 {
  z-index: -10;
}
.z0 {
  z-index: 0;
}
.z1 {
  z-index: 10;
}
.z2 {
  z-index: 20;
}

.grow {
  flex-grow: 1;
}

.g0 {
  gap: 0;
}
.g1 {
  gap: 4px;
}
.g2 {
  gap: 8px;
}
.g3 {
  gap: 12px;
}
.g4 {
  gap: 16px;
}
.g5 {
  gap: 20px;
}
.g6 {
  gap: 24px;
}
.g7 {
  gap: 32px;
}
.g8 {
  gap: 40px;
}



.p-8 {
  padding: 32px 64px;
}
.p-7 {
  padding: 28px 56px;
}
.p-6 {
  padding: 24px 48px;
}
.p-5 {
  padding: 20px 40px;
}
.p-4 {
  padding: 16px 32px;
}
.p-3 {
  padding: 12px 24px;
}
.p-2 {
  padding: 8px 16px;
}
.p-1 {
  padding: 4px 8px;
}
.p0 {
  padding: 0;
}
.p1 {
  padding: 4px;
}
.p2 {
  padding: 8px;
}
.p3 {
  padding: 12px;
}
.p4 {
  padding: 16px;
}
.p5 {
  padding: 24px;
}
.p6 {
  padding: 30px;
}
.p7 {
  padding: 34px;
}
.p8 {
  padding: 38px;
}
.p-page {
  padding-top: 30px;
  padding-bottom: 200px;
  padding-left: 12px;
  padding-right: 12px;
}


.ma {
  margin: auto;
}

.mt1 {
  margin-top: 1rem;
}
.mt2 {
  margin-top: 2rem;
}
.mt3 {
  margin-top: 3rem;
}

.m0 {
  margin: 0;
}




.o0 {
  opacity: 0%;
}
.o1 {
  opacity: 10%;
}
.o2 {
  opacity: 20%;
}
.o3 {
  opacity: 30%;
}
.o4 {
  opacity: 40%;
}
.o5 {
  opacity: 50%;
}
.o6 {
  opacity: 60%;
}
.o7 {
  opacity: 70%;
}
.o8 {
  opacity: 80%;
}
.o9 {
  opacity: 90%;
}
.o10 {
  opacity: 100%;
}




.scroll-y {
  overflow-y: auto;
}
.scroll-x {
  overflow-x: auto;
}
.scroll-hidden {
  overflow: hidden;
}
.loader {
  position: relative;
}
.loading {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  pointer-events: none;
}
.loader .loading {
  opacity: 0;
  transition: opacity 300ms;
}
.htmx-request .loader .loading,
.loader.htmx-request .loading {
  opacity: 1;
}
.loader .loaded {
  opacity: 1;
  transition: opacity 300ms;
}
.htmx-request .loader .loaded,
.loader.htmx-request .loaded {
  opacity: 0;
}

.f-3 {
  color: var(--f-3);
}
.f-2 {
  color: var(--f-2);
}
.f-1 {
  color: var(--f-1);
}
.f0 {
  color: var(--f0);
}
.f1 {
  color: var(--f1);
}
.f2 {
  color: var(--f2);
}
.f3 {
  color: var(--f3);
}
.f4 {
  color: var(--f4);
}

.b-3 {
  background-color: var(--b-3);
}
.b-2 {
  background-color: var(--b-2);
}
.b-1 {
  background-color: var(--b-1);
}
.b0 {
  background-color: var(--b0);
}
.b1 {
  background-color: var(--b1);
}
.b2 {
  background-color: var(--b2);
}
.b3 {
  background-color: var(--b3);
}
.b4 {
  background-color: var(--b4);
}

.s-2 {
  font-size: 0.7rem;
  }
.s-1 {
  font-size: 0.85rem;
}
.s0 {
  font-size: 1rem;
}
.s1 {
  font-size: 1.15rem;
}
.s2 {
  font-size: 1.3rem;
}
.s3 {
  font-size: 1.45rem;
}
.s4 {
  font-size: 1.6rem;
}
.s5 {
  font-size: 2rem;
}
.s6 {
  font-size: 2.4rem;
}
.bd0 {
  border: none;
}
.bd1 {
  border: 0.8px solid var(--b3);
}
.bd2 {
  border: 0.8px solid var(--f3);
}
.bd3 {
  border: 2px solid var(--f1);
}
/* deprecated */
.border {
  border: 0.8px solid var(--f3);
}

.br1 {
  border-radius: 3px;
}
.br2 {
  border-radius: 6px;
}
.br3 {
  border-radius: 12px;
}

.hover:hover {
  filter: invert(20%);
}
.toggled {
  filter: invert(100%);
}
.hover.toggled:hover {
  filter: invert(100%);
}
.active,
.selected {
  filter: invert(10%);
}
.hover.active:hover,
.hover.selected:hover {
  filter: invert(25%);
}
.numbered > *:before {
  content: counter(line);
  display: inline-block;
  /* border-right: 1px solid var(--f3); */
  /* padding: 0 .5em; */
  /* margin-right: .5em; */
  color: var(--f3);
  /* min-width: 34px; */
  text-align: right;
}
.numbered > * {
    display: block;
    counter-increment: line;
}
.prose h1 {
  font-size: 1.45rem;
  margin: 1rem 0;
}
.prose h2 {
  font-size: 1.3rem;
  margin: 1rem 0;
}
.prose h3 {
  font-size: 1.15rem;
  margin: 1rem 0;
}
.prose h1, .prose h2, .prose h3 {
  font-weight: bold;
}
.prose p {
  margin-bottom: 1rem;
  line-height: var(--line-height);
}
.prose img {
  max-width: 100%;
  display: block;
  max-height: 350px;
}
.prose details {
  margin-bottom: 1rem;
}
.prose a {
  text-decoration: underline;
}
.prose blockquote {
  margin-left: 10px;
  border-left: 2px solid var(--b3);
  padding: 4px;
  padding-left: 12px;
  color: var(--f2);
}
.prose pre {
  font-family: var(--font-mono);
  font-size: calc(1em * var(--mono-scale));
  overflow-x: auto;
  width: 100%;
  display: block;
  padding: 8px;
}
.prose code {
  font-family: var(--font-mono);
  font-size: calc(1em * var(--mono-scale));
  color: var(--f2);
}
.prose ul,
.prose ol {
  padding-left: 29px;
  line-height: calc(calc(1 + var(--line-height)) / 2);
  margin-bottom: 0.6rem;
}
.prose ul p,
.prose ol p {
  margin-bottom: 0;
  line-height: var(--line-height);
}
.prose ul ul,
.prose ol ul,
.prose ul ol,
.prose ol ol {
  margin-bottom: 0;
}
.prose summary {
  user-select: none;
  cursor: pointer;
}
'''
--
