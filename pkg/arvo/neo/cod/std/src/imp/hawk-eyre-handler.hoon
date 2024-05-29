/@  htmx
/-  feather-icons
/-  serv=server
=>
  |%
  ++  main
    ^-  curb:neo
    [%or rol/[%ui-main pro/%htmx] pro/%htmx ~]
    :: rol/[%ui-main pro/%htmx]
  ++  kids-curb
    ^-  curb:neo
    any/~
  :: rol/[%ui-list pro/%htmx]
  ++  manx-to-octs
    |=  man=manx
    (as-octt:mimes:html (en-xml:html man))
  ::
  ++  render
    |=  [main=manx kids=marl]
    ;div
      ;+  main
      ;div
        ;*  kids
      ==
    ==
  ++  hawk
    |_  [here=pith main=manx raw=manx]
    ++  id  *@da
    ++  idt  `tape`(zing (scan +:(scow %da id) (most dot (star ;~(less dot prn)))))
    ++  has-app  %.y  ::  XX : switch on it. make it real. etc
    ++  lift
      ;div.hawk.fc.wf.hf
        =id  "hawk-{idt}"
        =hx-params  "id,slot"
        =hx-vals  "\{\"id\": \"{<id>}\", \"slot\": \"{<0>}\"}"
        ;+  header
        ::;+  raw
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
            =hx-post  "/neo/hawk/sky?stud=sky-diff"
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
            =hx-post  "/neo/hawk/sky?stud=sky-diff"
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
            =hx-post  "/neo/hawk/sky?stud=sky-diff"
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
      :-  [main ~]
      ^-  (unit port:neo)
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[kids-curb ~]
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
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =;  =manx
      =/  head=sign:eyre:neo  [eyre-id %head [200 [['content-type' 'text/html'] ~]]]
      =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
      =/  done=sign:eyre:neo  [eyre-id %done ~]
      :~  [pith %poke eyre-sign/!>(head)]
          [pith %poke eyre-sign/!>(data)]
          [pith %poke eyre-sign/!>(done)]
          [here.bowl %cull ~]
      ==
    ?~  src=(~(get by deps.bowl) %src)
      ;div: 404
    =/  root=idea:neo  (~(got of:neo q.u.src) /)
    ?>  =(%htmx p.pail.root)
    =/  bol  *bowl:neo
    =.  here.bol  p.u.src
    =.  our.bol  our.bowl
    =.  now.bol  now.bowl
    =.  eny.bol  eny.bowl
    =.  kids.bol  q.u.src
    ::  XX src.bowl
    =/  main  (!<(htmx q.pail.root) bol)
    =/  raw  *manx
    ~(lift hawk here.bol main raw)
  --
--

