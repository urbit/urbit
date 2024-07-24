/@  htmx-type=htmx
/@  tree-diff
/-  serv=sky-server
/*  feather
/*  reset
/*  jquery
/*  htmx-js
/*  htmx-response-targets
/*  htmx-idiomorph
/>  htmx
/<  node
/<  http-request
=<
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   (sy %tree-diff %ack ~)
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
++  form 
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke  
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ~&  >  stud/stud
    :_  eyre-task/q.pail
    ?~  src=(~(get by deps.bowl) %src)  ~
    ?~  (~(get of:neo q.u.src) /)  ~
    =/  here  p.u.src
    ?+  stud  !!
    ::
        %ack
      ?~  !<((unit quit:neo) vase)  
        ?:  =(*pail:neo pail)  ~
        =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
        =/  location
          %-  crip 
            %+  weld  "/neo/tree" 
              (en-tape:pith:neo here)
        %:  eyre-cards
        eyre-id.this
        bowl
        200
        :~  
          'content-type'^'text/html'
          'HX-Refresh'^'true'
          ::'HX-Redirect'^location
          ::'HX-Reswap'^'outerHTML'
        ==
        ;div
        ;  success 
        ==
        ==
      =/  =quit:neo  (need !<((unit quit:neo) vase))
      ?+  -.quit  ~
          %goof
        =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
        %:  eyre-cards
        eyre-id.this
        bowl
        200
        ['content-type' 'text/html']~
        (err-trace-manx +.quit)
        ==
      ==
      ::
        %tree-diff
      =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
      =/  eyre-id  eyre-id.this
      =/  diff  !<(tree-diff vase)
      ~&  >>>  diff-tree-imp/diff
      =,  diff
      ?-  -.diff  
        ::
          %send-make
        :~  
            [pith %make stud init conf]
        ==
        ::
          %send-poke
        =/  vax  vase.diff
        :~  
            [here %poke [stud vax]]
        ==
        ::
          %send-cull
        ~&  >>>  pith-cull/here
        :~  
            [here %cull ~]
            :: [here %tomb ~]
        ==
        ::  
          %req-parsing-err  ~
      ==
    ==
  ++  init 
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    ?~  src=(~(get by deps.bowl) %src)  
      %:(empty-view-cards eyre-id bowl 404)
    ?~  (~(get of:neo q.u.src) /)
      %:(empty-view-cards eyre-id bowl 200)
    =/  here  p.u.src
    =/  bol  *bowl:neo
    =.  here.bol  here
    =.  our.bol  our.bowl
    =.  kids.bol  q.u.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
      ::
        %'GET'
      %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          (view bol)
      ==
      ::
        %'POST'
      =/  purl  (parse-url:serv request.req)
      =/  poke-stud
        ^-  stud:neo
        ~|  %no-stud-specified
        (~(got by pam.purl) 'stud')
      =/  diff-vase  
        (http-request [poke-stud `request:http`request.req])
      =/  diff-type   !<(tree-diff diff-vase)
      ~&  >>  diff-type/diff-type
      ?-  -.diff-type
        ::
          %send-make 
        ::  %make cards don't have error(%goof) %acks yet 
        ::  sending eyre response here for now
        %+  welp 
        :~  (poke-tree-card here.bowl diff-vase)
        ==
        %:  eyre-cards
          eyre-id
          bowl
          200
          :~
            'content-type'^'text/html'
            'HX-Refresh'^'true'
          ==
          *manx
          ==
        ::
          %send-poke 
        :~  (poke-tree-card here.bowl diff-vase)
        ==
        ::
          %send-cull
        =/  poke-card=(list card:neo)  ~[(poke-tree-card here.bowl diff-vase)]
        =/  location  
          %-  crip 
            %+  weld  "/neo/tree" 
              (en-tape:pith:neo (snip here))
        ;:  welp
          poke-card
          %:  eyre-cards
          eyre-id
          bowl
          200
          :~
            'content-type'^'text/html'
            'HX-Redirect'^location
          ==
          *manx
          ==
        ==
        ::
          %req-parsing-err
        %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          (err-trace-manx tang.diff-type)
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
  =/  headers  
  %+  welp   
    header-list
  ['last-tree-location'^(en-cord:pith:neo /home) ~]
  =/  head=sign:eyre:neo  [eyre-id %head [status headers]]
  =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
  =/  done=sign:eyre:neo  [eyre-id %done ~]
  :~  [pith %poke eyre-sign/!>(head)]
      [pith %poke eyre-sign/!>(data)]
      [pith %poke eyre-sign/!>(done)]
      [here.bowl %cull ~]
      [here.bowl %tomb ~]
  ==
::
++  empty-view-cards
  |=  [eyre-id=@ta =bowl:neo status=@ud]
  =/  empty-view=manx
    ;div
      ;h1: nothing here
    ==
  %:  eyre-cards
    eyre-id
    bowl
    status
    ['content-type' 'text/html']~
    empty-view
  ==
::
++  poke-tree-card
  |=  [here=pith:neo vax=vase]
  ^-  card:neo
  [here %poke %tree-diff vax]
::
++  err-trace-manx
|=  =tang
^-  manx
  ;div.p2
    ;*  %+  turn  tang
      |=  =tank
      ^-  manx
      ;div.wf.fr.js.p1.error.monospace
        ;  {~(ram re tank)}
      ==
  ==
::
++  del-manx
  ^-  manx
  ;div
  =style  "grid-column: 3 ; grid-row: 1; justify-self: end;"
    ;p:  deleted
  ==
::
++  view 
  |=  =bowl:neo
  ^-  manx
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: tree
      ;script: {(trip jquery)}
      ;script: {(trip htmx-js)}
      ;script: {(trip htmx-response-targets)}
      ;script: {(trip htmx-idiomorph)}
      ;link
        =rel   "stylesheet"
        =href  "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/themes/light.css"
        ;
      ==
      ;script
        =type  "module"
        =src   "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/shoelace.js"
        ;
      ==
    ;style: {(trip reset)}
    ;style: {(trip feather)}
    ;style: {tree-style}
    ==
    ;body 
      ;+  (body-view bowl)
    ==
  ==
::
++  tree-style 
  ^~
  %-  trip 
  '''
  @font-face {
  font-family: 'Urbit Sans';
  src: url("https://media.urbit.org/fonts/UrbitSans/UrbitSansVFWeb-Regular.woff2") format("woff2");
  font-style: normal;
  font-weight: 600;
  }
  body{
  font-family: 'Urbit Sans';
  font-size: 15px;
  }
  input{
  font-family: 'monospace', monospace;
  font-size: 13px;
  }
  .monospace{
  font-family: 'monospace', monospace;
  font-size: 13px;
  }
  .red-hover:hover{
  background-color: #FF0000; 
  color: white;
  border-radius: 6px;
  }
  .pointer{
  cursor: pointer;
  }
  .bd.bd2{
  border: 0.8px solid black;
  }
  .error{
  color: #FF0000; 
  }
  .bg-white{
  background: white;
  }
  .hover-grey:hover{
  background: #dbdbdb;
  }
  '''
::
++  body-view
  |=  =bowl:neo
  ^-  manx
  ;div.wf
    ;+
      ?~  node=(~(get of:neo kids.bowl) /)
        ;div: no kids
    =/  =pail:neo  q.saga.u.node
    ;div.fc.js.p2.g2
      ;+  (shrub-view bowl pail)
      ;+  (kids-view bowl)
    ==
  ==
::
++  shrub-view 
  |=  [=bowl:neo =pail:neo]
  ^-  manx
  ;div
    ;div.fc.g2.bd.bd2.br2.p2
      ;div.top.fr.jb.g2
        ;p.p2.hfc.wf.grow:  {(en-tape:pith:neo here.bowl)}
        ;+  buttons
      ==
      ;div.fr.jb.g2
        ;+  ?.  =(p.pail %hoon)
          (state-print pail)
        ;div.hidden
        ;
        ==
        ;div.fr.je.grow.g2
          ;+  (pro-files pail bowl)
          ;a.loader.p2.bd.bd2.br2.hover-grey
          =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(p.pail p.pail mark.p.pail))}"
            ;span.loaded.hf: {<p.pail>}
            ;span.loading.hf:  loading
          ==
        ==
      ==
      ;+  (forms bowl)
      ;+  ?:  =(p.pail %hoon)
        (state-print pail)
      ;div.hidden
      ;
      ==
    ==
  ==
++  kids-view
  |=  =bowl:neo
  ^-  manx
  ;div.fc.g2
    ;*
    %+  turn  
        %+  sort  ~(tap of:neo kids.bowl) 
        aor
    |=  [=pith:neo =idea:neo]
    ^-  manx
    ?~  pith  
      ;div.hidden
        ;  nothing
      ==
    ;a.fr.jb.g1.bd.bd2.br2.hover-grey
    =href  "/neo/tree{(en-tape:pith:neo (welp here.bowl pith))}"
      ;div.p2.hfc.p2.hover
        ;  {(en-tape:pith:neo pith)}
      ==
      ::for some reason broken when trying to jam vase that's too large 
      ::
      ::(preview-state q.saga.idea)
    ==
  ==
::
++  state-print
  |=  =pail:neo
  ^-  manx
  ?:  =(p.pail %hoon)
    =/  wain=(list @t)  (to-wain:format !<(@t q.pail))
    ;div.fc.g1.p2.grow.monospace
      ;* 
      %+  turn  wain
      |=  lin=@t 
      ;p.monospace:  {(trip lin)}\0a
    ==
  ;div.fr.js.p2.monospace
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 750)  "vase too large to print: {<size>}"
    ~(ram re (sell q.pail))
  ==
::
:: ++  preview-state
::   |=  =pail:neo
::   ^-  manx
::   ;div.fr.js.p2
::     ;+  ;/
::     =/  size  (met 3 (jam q.q.pail))
::     ?:  (gth size 300)  "too-large"
::     =/  state-tape  ~(ram re (sell q.pail))
::     ::=/  length  (lent state-tape)
::     ::?:  (gth length 140)  
::       ::(weld (oust [140 (sub length 140)] state-tape) "...")
::     state-tape
::   ==
::
++  pro-files 
  |=  [=pail:neo =bowl:neo]
  ^-  manx
  ?.  =(p.pail %hoon)
    ;div.hidden
    ;
    ==
  =/  =name:neo  [our here]:bowl
  =/  fool=(each file:ford:neo tang)
    %-  mule
    |.
    (scan (trip !<(@t q.pail)) (rein:ford:neo name))
  ?:  ?=(%| -.fool)  
    ;div.hidden
    ;
    ==
  ;div.fr.g2
    ;*  %+  turn  pro.p.fool
    |=  =pro:ford:neo
    ~&  >>>  -:!>(stud.pro)
    ^-  manx
    ;a.loader.bd.bd2.br2.p2.hover-grey
      =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(stud.pro stud.pro mark.stud.pro))}"
        ;span.loaded.hfc: {<stud.pro>}
        ;span.loading.hfc:  loading
    ==
  ==
::
++  buttons
  ^-  manx
  ;div.buttons.fr.g2
    ;button.make.p2.bd.bd2.br2.hover-grey.b-3.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.poke').removeClass('toggled');
      $(this).parent().find('.cull').removeClass('toggled');
      $(this).parent().parent().parent().find('.make-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.cull-form').addClass('hidden');
      $(this).parent().parent().parent().find('.poke-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  make
    ==
    ;button.poke.p2.bd.bd2.br2.hover-grey.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.make').removeClass('toggled');
      $(this).parent().find('.cull').removeClass('toggled');
      $(this).parent().parent().parent().find('.poke-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.make-form').addClass('hidden');
      $(this).parent().parent().parent().find('.cull-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  poke
    ==
    ;button.cull.p2.bd.bd2.br2.hover-grey.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.make').removeClass('toggled');
      $(this).parent().find('.poke').removeClass('toggled');
      $(this).parent().parent().parent().find('.cull-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.make-form').addClass('hidden');
      $(this).parent().parent().parent().find('.poke-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  cull
    ==
  ==
::
++  forms
  |=  =bowl:neo
  ^-  manx
    ;div.forms.fc.as.g2.wf
      ;div.error-box
      ;
      ==
      ;+  (make-form bowl)
      ;+  (poke-form bowl)
      ;+  (cull-form bowl)
    ==
::
++  make-form
  |=  =bowl:neo
  ^-  manx
  ;form.make-form.hidden.bd.bd2.br2.fc.g2.p2.wf.hfc  ::  .bg-white
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-make"
  =hx-swap    "innerHTML"
  =hx-target  ".error-box"
    ;div.fr.g2
      ;input.hidden
      =type   "text"
      =name   "here"
      =value  (en-tape:pith:neo here.bowl)
      ;
      ==
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "pith"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "/some/pith"
      =required      ""
      ;
      ==
      ;input.bd.bd2.br2.p2 
      =type          "text"
      =name          "stud"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "%shrub-type"
      =required      ""
      ;
      ==
      ;input.bd.bd2.br2.p2 
      =type          "text"
      =name          "head-pail"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "%state-type"
      =required      ""
      ;
      ==
    ==
    ;div.fr.g2
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "vase"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "state value"
      =required      ""
      ;
      ==
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "conf"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "(map term pith:neo)"
      =required      ""
      ;
      ==
      ;button.bd.bd2.br2.p2.loader
    :: =onclick  
    :: """
    :: $(this).parent().toggleClass('hidden');
    :: $(this).parent().next().toggleClass('bg-white');
    :: $(this).parent().next().find('.open').toggleClass('hidden');
    :: $(this).parent().next().find('.close').toggleClass('hidden');
    :: """
        ;span.loaded:  make
        ;span.loading:  loading
      ==
    ==
  ==
::
++  poke-form
  |=  =bowl:neo
  ^-  manx
  ;form.poke-form.hidden.bd.bd2.br2.fr.jb.g2.p2.wf
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-poke"
  =hx-reswap  "true"
  ::=hx-swap    "innerHTML"
  =hx-target  ".error-box"
    ;input.p2.bd.bd2.br2
    =type          "text"
    =name          "stud"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "%diff-type"
    =required      ""
    ;
    ==
    ;input.p2.bd.bd2.br2.grow
    =type          "text"
    =name          "vase"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "diff value"
    =required      ""
    ;
    ==
    ;button.loader.bd.bd2.br2
      ;span.loaded:  poke
      ;span.loading:  loading
    ==
  ==
::
++  cull-form
  |=  =bowl:neo
  =/  warning  "Are you sure you want to delete this shrub and all their kids?"
  ^-  manx
  ;form.cull-form.hidden.bd.bd2.br2.p2.wf
  =style    "border: 2px solid #FF0000; border-radius: 6px;"
  =hx-post  "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-cull"
    ;div.fc.ac
      ;p:  {warning}
    ==
    ;div.fr.jc.g8.p2
      ;button.cull-trigger.hfc.p2.red-hover
        ;span:  yes
      ==
      ;span.hfc.p2.red-hover.pointer
      =onclick  
      """
      $(this).parent().parent().addClass('hidden');
      $(this).parent().parent().parent().parent().find('.top').find('.buttons').find('.cull').toggleClass('toggled');
      """
        ;span:  no
      ==
    ==
  ==
::
--