/@  eyre-reqs
/@  htmx-type=htmx
/@  tree-diff
/-  serv=sky-server
/-  srv=server
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
++  state  pro/%sig
++  poke   (sy %tree-diff %ack %eyre-task ~)
++  kids  *kids:neo
++  deps   
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=&
      ^-  quay:neo
      :-  [pro/%root ~]
      ^-  (unit port:neo)
      :+  ~  %z
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[any/~ ~]
      ==
  ==
++  form 
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %tree]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  ::
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    :_  sig/!>(~)
    ?+  stud  ~|(bad-stud/stud !!)
        %tree-diff  
      =/  diff  !<(tree-diff vase)
      ~&  >>>  diff-tree-imp/diff
      =,  diff
      ?-  -.diff  
        ::
          %send-make
        :~  
            [pith %make stud.diff init conf]
        ==
        ::
          %send-poke
        =/  vax  vase.diff
        :~  
            [pith %poke [stud.diff vax]]
        ==
        ::
          %send-cull
        ~&  >>>  pith-cull/pith
        :~  
            [pith %cull ~]
        ==
        ::  
          %req-parsing-err  ~
      ==
        %ack  ~
      :: ?~  !<((unit quit:neo) vase)  
      ::   ?:  =(*pail:neo pail)  ~
      ::   =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
      ::   %:  eyre-cards
      ::   eyre-id.this
      ::   bowl
      ::   200
      ::   :~  
      ::     'content-type'^'text/html'
      ::     'HX-Refresh'^'true'
      ::   ==
      ::   *manx
      ::   ==
      :: =/  =quit:neo  (need !<((unit quit:neo) vase))
      :: ?+  -.quit  ~
      ::     %goof
      ::   =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
      ::   %:  eyre-cards
      ::   eyre-id.this
      ::   bowl
      ::   200
      ::   ['content-type' 'text/html']~
      ::   (err-trace-manx +.quit)
      ::   ==
      :: ==
        %eyre-task
      =+  !<(=task:eyre:neo vase)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      ?.  authenticated.req
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request.req)
      =/  purl  (parse-url:serv request.req)
      =/  inner=pith:neo  (pave:neo pax.purl)
      =/  src  (~(got by deps.bowl) %src)
      =/  here  (tail inner)
      ~&  >>  inner/inner
      ?+    method.request.req  ~|(%unsupported-http-method !!)
        ::
          %'GET'
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            ?~  idea=(~(get of:neo q.src) here)
              ;div
                ;h1: nothing here
              ==
            =/  local=pail:neo  q.saga:(need idea)
            =/  kids=(list pith:neo)  (get-kids here q.src)
            (view inner local kids bowl)
          ==
        ::
          %'POST'
        =/  poke-stud
          ^-  stud:neo
          ~|  %no-stud-specified
          (~(got by pam.purl) 'stud')
        =/  diff-vase  
          (http-request [poke-stud `request:http`request.req])
        =/  diff-type   !<(tree-diff diff-vase)
        ?-  -.diff-type
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
          %send-poke 
        :~  (poke-tree-card here.bowl diff-vase)
        ==
          %send-cull
        =/  poke-card=(list card:neo)  ~[(poke-tree-card here.bowl diff-vase)]
        =/  location  
          %-  crip 
            %+  weld  "/neo/tree" 
              (en-tape:pith:neo (snip inner))
        %+  welp
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
    ==
  --
--
::
|%
::
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
  ==
::
++  poke-tree-card
  |=  [here=pith:neo vax=vase]
  ^-  card:neo
  [here %poke %tree-diff vax]
::
++  get-kids
  |=  [=pith:neo =lore:neo]
  =|  i=@
  ^-  (list pith:neo)
  =+  piths=(full-pith pith lore)
  |-
  ?:  =(i (lent piths))  piths
    =/  p=pith:neo  (snag i piths)
    ?~  (~(kid of:neo lore) p)  
      $(i +(i))
    =/  grand-kids  (full-pith p lore)
    $(i +(i), piths (welp piths grand-kids))
::
++  full-pith
  |=  [parent=pith:neo =lore:neo]
  ^-  (list pith:neo)
  ?~  (~(kid of:neo lore) parent)  ~
  %+  turn  ~(tap in ~(key by (~(kid of:neo lore) parent)))  
      |=  p=pith:neo 
      %+  welp  parent
      p
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
++  view 
  |=  [here=pith:neo =pail:neo kids=(list pith:neo) =bowl:neo]
  ;html
    ;head
      ;meta(charset "UTF-8");
      ;title: tree
      ;script: {(trip jquery)}
      ;script: {(trip htmx-js)}
      ;script: {(trip htmx-response-targets)}
      ;script: {(trip htmx-idiomorph)}
      ;link
        =rel  "stylesheet"
        =href  "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/themes/light.css"
        ;
      ==
      ;script
        =type  "module"
        =src  "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/shoelace.js"
        ;
      ==
    ;style: {(trip reset)}
    ;style: {(trip feather)}
    ;style: {tree-style}
    ==
    ;body 
      ;+  (body-view here pail kids bowl)
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
  |=  [here=pith:neo =pail:neo kids=(list pith:neo) =bowl:neo]
  ^-  manx
  ;div.wf
    :: ;+
    ::   ?~  kids
    ::     ;div: no kids
    ;div.fc.js.p2.g2
      ;+  (shrub-view here pail bowl)
      ;+  (kids-view pail kids bowl)
    ==
  ==
::
++  shrub-view 
  |=  [here=pith:neo =pail:neo =bowl:neo] 
  ^-  manx
  ;div
    ;div.fc.g2.bd.bd2.br2.p2
      ;div.top.fr.jb.g2
        ;p.p2.hfc.wf.grow:  {(en-tape:pith:neo here)}
        ;+  buttons
      ==
      ;div.fr.jb.g2
        ;+  ?.  =(p.pail %hoon)
          (state-print pail)
        ;div.hidden
        ;
        ==
        ;div.fr.je.grow.g2
          ;+  (pro-files pail here bowl)
          ;a.loader.p2.bd.bd2.br2.hover-grey
          =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(p.pail p.pail mark.p.pail))}"
            ;span.loaded.hf: {<p.pail>}
            ;span.loading.hf:  loading
          ==
        ==
      ==
      ;+  (forms here)
      ;+  ?:  =(p.pail %hoon)
        (state-print pail)
      ;div.hidden
      ;
      ==
    ==
  ==
++  kids-view
  |=  [=pail:neo kids=(list pith:neo) =bowl:neo]
  ^-  manx
  ;div.fc.g2
    ;*
    %+  turn  
        %+  sort  kids
        aor
    |=  =pith:neo
    ^-  manx
    ?~  pith  
      ;div.hidden
        ;  nothing
      ==
    ;a.fr.jb.g1.bd.bd2.br2.hover-grey
    =href  "/neo/tree/{(scow %p our.bowl)}{(en-tape:pith:neo pith)}"
      ;div.p2.hfc.p2.hover
        ;  {(en-tape:pith:neo pith)}
      ==
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
++  pro-files 
  |=  [=pail:neo here=pith:neo =bowl:neo]
  ^-  manx
  ?.  =(p.pail %hoon)
    ;div.hidden
    ;
    ==
  =/  =name:neo  [our.bowl here]
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
  |=  here=pith:neo
  ^-  manx
    ;div.forms.fc.as.g2.wf
      ;div.error-box
      ;
      ==
      ;+  (make-form here)
      ;+  (poke-form here)
      ;+  (cull-form here)
    ==
::
++  make-form
  |=  here=pith:neo
  ^-  manx
  ;form.make-form.hidden.bd.bd2.br2.fc.g2.p2.wf.hfc  ::  .bg-white
  =hx-post    "/neo/tree{(en-tape:pith:neo here)}?stud=tree-diff&head=send-make"
  =hx-swap    "innerHTML"
  =hx-target  ".error-box"
    ;div.fr.g2
      ;input.hidden
      =type   "text"
      =name   "here"
      =value  (en-tape:pith:neo here)
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
        ;span.loaded:  make
        ;span.loading:  loading
      ==
    ==
  ==
::
++  poke-form
  |=  here=pith:neo
  ^-  manx
  ;form.poke-form.hidden.bd.bd2.br2.fr.jb.g2.p2.wf
  =hx-post    "/neo/tree{(en-tape:pith:neo here)}?stud=tree-diff&head=send-poke"
  =hx-swap    "outterHTML"
  =hx-target  ".error-box"
    ;input.hidden
    =type   "text"
    =name   "here"
    =value  (en-tape:pith:neo here)
    ;
    ==
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
  |=  here=pith:neo
  =/  warning  "Are you sure you want to delete this shrub and all their kids?"
  ^-  manx
  ;form.cull-form.hidden.bd.bd2.br2.p2.wf
  =style    "border: 2px solid #FF0000; border-radius: 6px;"
  =hx-post  "/neo/tree{(en-tape:pith:neo here)}?stud=tree-diff&head=send-cull"
    ;div.fc.ac
      ;p:  {warning}
    ==
    ;div.fr.jc.g8.p2
      ;button.cull-trigger.hfc.p2.red-hover
      =name     "pith"
      =value    (en-tape:pith:neo here)
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
--