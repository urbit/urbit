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
    ~&  ?:  =(stud %ack)  !<((unit quit:neo) vase)  'not an %ack'
    =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
    =/  eyre-id  eyre-id.this
    :_  eyre-task/q.pail
    ?+  stud  !!
    ::
        %ack
      ?~  !<((unit quit:neo) vase)
      ~
      ::  on recieving empty ack we do nothing till we get %ack for %tomb cards fixed
      ::  would be nice to have one %ack or even one card for tomb-stoning shrub and all their kids 
        :: %:  eyre-cards
        :: eyre-id
        :: bowl
        :: 200
        :: ['content-type' 'text/html']~
        :: success-manx
        :: ==
      =/  =quit:neo  (need !<((unit quit:neo) vase))
      ~&  >>  got-quit/quit
      ::  XX: ADD 404 MANX
      ::  if not goof crush 404
      ?+  -.quit  
        %:  eyre-cards
        eyre-id
        bowl
        404
        ['content-type' 'text/html']~
        *manx
        ==
          %goof
        %:  eyre-cards
        eyre-id
        bowl
        200
        ['content-type' 'text/html']~
        (err-trace-manx +.quit)
        ==
      ==
      ::
        %tree-diff
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
            [pith %poke [stud vax]]
        ==
        ::
          %send-tomb
        ~&  >>>  pith-tomb/pith
        :~  
            [pith %cull ~]
            [pith %tomb ~]
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
      =/  empty-view=manx
        ;div
          ;h1: nothing here
        ==
      %:  eyre-cards
        eyre-id
        bowl
        404
        ['content-type' 'text/html']~
        empty-view
      ==
    =/  here  p.u.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
      ::
        %'GET'
      ?~  (~(get of:neo q.u.src) /)
        =/  empty-view=manx
          ;div
            ;h1: nothing here
          ==
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            empty-view
        ==
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  kids.bol  q.u.src
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
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  kids.bol  q.u.src
      ~&  >>  diff-type/diff-type
      ?-  -.diff-type
        ::
          %send-make 
        ::  %make cards don't have error(%goof) %acks yet 
        ::  sending eyre response here for now 
        ::  XX:  should wait a few seconds and update whole page 
        %+  welp 
        :~  (poke-tree-card here.bowl diff-vase)
        ==
        %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          success-manx
          ==
        ::
          %send-poke 
        :~  (poke-tree-card here.bowl diff-vase)
        ==
        ::
          %send-tomb
        =/  pax=pith:neo  (tail (tail +.diff-type))
        =/  poke-card=(list card:neo)  ~[(poke-tree-card here.bowl diff-vase)]
        =/  kids  (kids-to-card pax q.u.src here.bowl here)
        ;:  welp
          (flop kids)
          poke-card
        ::   %:  eyre-cards
        ::   eyre-id
        ::   bowl
        ::   200
        ::   ['content-type' 'text/html']~
        ::   (view bol)
        ::   ==
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
++  poke-tree-card
  |=  [here=pith:neo vax=vase]
  ^-  card:neo
  [here %poke %tree-diff vax]
::
++  kids-to-card
  |=  [=pith:neo =lore:neo here=pith:neo head-pith=pith:neo]
  ^-  (list card:neo)
  =/  cards  *(list card:neo)
  =/  kids  ~(tap in ~(key by ~(tar of:neo lore)))
  ?~  kids  ~
  %+  turn  kids
    |=  p=pith:neo 
    (poke-tree-card here !>([%send-tomb (welp head-pith p)]))
::
++  err-trace-manx
|=  =tang
  ;div.p2
    ;*  %+  turn  tang
      |=  =tank
      ^-  manx
      (err-manx ~(ram re tank))
  ==
::
++  err-manx       
  |=  =tape
  =/  msg  
    ?~  tape  "failed to parse entered value to hoon"
  tape
  ^-  manx
  ;div.wf.fr.js.p1.error
    ;span:  {msg}
  ==
::
++  del-manx
  ^-  manx
  ;div
  =style  "grid-column: 3 ; grid-row: 1; justify-self: end;"
    ;p:  deleted
  ==
::
++  success-manx
  ^-  manx
  ;div.p2
    ;p:  success
  ==
::
++  view 
  |=  =bowl:neo
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
      ;+  (body-view bowl)
    ==
  ==
::
++  tree-style 
  ^~
  %-  trip 
  '''
  .red-hover:hover{
  background-color: #FF0000; 
  color: white;
  border-radius: 6px;
  }
  .pointer{
  cursor: pointer;
  }
  .bd{
  border: 0.8px solid black;
  }
  .mt05{
  margin-top: 0.5rem;
  }
  .mr05{
  margin-right: 0.5rem;
  }
  .ml05{
  margin-left: 0.5rem;
  }
  .error{
  color: #FF0000; 
  }
  .opened{
  background: black;
  color: white;
  }
  .bg-white{
  background: white;
  }
  .hover-gray:hover{
  background: #dbdbdb;
  }
  '''
::
++  body-view
  |=  =bowl:neo
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
  ;div
    ;div.fc.g2.bd.br2.p2
      ;div.top.fr.jb.g2
        ;p.p2.hfc.wf.grow:  {(en-tape:pith:neo here.bowl)}
        ;div.wf.fr.je
          ::  XX: move this down underneath buttons
          ;a.loader.p2.bd.br2
          =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(p.pail p.pail mark.p.pail))}"
            ;span.loaded.hf: {<p.pail>}
            ;span.loading.hf:  loading
          ==
        ==
        ;+  buttons
      ==
      ;+  (state-print pail)
      ;+  (forms bowl)
    ==
  ==
++  kids-view
  |=  =bowl:neo
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
    ;a.fr.jb.g1.bd.br2.hover-gray
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
  ::  XX:  check if has imports display next to pro %hoon file a-tag 
  ?:  =(p.pail %hoon)
    =/  wain=(list @t)  (to-wain:format !<(@t q.pail))
    ;div.fc.g1.p2
      ;*  %+  turn  wain
      |=  lin=@t 
      ;p:  {(trip lin)}\0a
    ==
  ;div.fr.js.p2
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 750)  "vase too large to print: {<size>}"
    ~(ram re (sell q.pail))
  ==
::
++  preview-state
  |=  =pail:neo
  ^-  manx
  ;div.fr.js.p2
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 300)  "too-large"
    =/  state-tape  ~(ram re (sell q.pail))
    ::=/  length  (lent state-tape)
    ::?:  (gth length 140)  
      ::(weld (oust [140 (sub length 140)] state-tape) "...")
    state-tape
  ==
::
++  buttons
  ;div.buttons.fr.g2
    ;button.make.p2.bd.br2.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.poke').removeClass('toggled');
      $(this).parent().find('.tomb').removeClass('toggled');
      $(this).parent().parent().parent().find('.make-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.tomb-form').addClass('hidden');
      $(this).parent().parent().parent().find('.poke-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  make
    ==
    ;button.poke.p2.bd.br2.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.make').removeClass('toggled');
      $(this).parent().find('.tomb').removeClass('toggled');
      $(this).parent().parent().parent().find('.poke-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.make-form').addClass('hidden');
      $(this).parent().parent().parent().find('.tomb-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  poke
    ==
    ;button.tomb.p2.bd.br2.bg-white
    =onclick  
      """
      $(this).toggleClass('toggled');
      $(this).parent().find('.make').removeClass('toggled');
      $(this).parent().find('.poke').removeClass('toggled');
      $(this).parent().parent().parent().find('.tomb-form').toggleClass('hidden');
      $(this).parent().parent().parent().find('.make-form').addClass('hidden');
      $(this).parent().parent().parent().find('.poke-form').addClass('hidden');
      $(this).parent().parent().parent().find('.error-box').html('')
      """
      ;span:  tomb
    ==
  ==
::
++  forms
  |=  =bowl:neo
  ;div.forms.fc.as.g2.wf.p1
    ;div.error-box
    ;
    ==
    ;+  (make-form bowl)
    ;+  (poke-form bowl)
    ;+  (tomb-form bowl)
  ==
::
++  make-form
  |=  =bowl:neo
  ;form.make-form.hidden.bd.br2.fr.jb.g2.p2.wf.bg-white
  =hx-post  "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-make"
  =hx-swap     "innerHTML"
  =hx-target  ".error-box"
    ;input.hidden
    =type   "text"
    =name   "here"
    =value  (en-tape:pith:neo here.bowl)
    ;
    ==
    ;input.bd.br2.p2.ml05 
    =type  "text"
    =name  "pith"
    =oninput  "this.setAttribute('value', this.value);"
    =placeholder  "/some/pith"
    =required      ""
    ;
    ==
    ;input.bd.br2.p2 
    =type  "text"
    =name  "stud"
    =oninput  "this.setAttribute('value', this.value);"
    =placeholder  "%shrub-type"
    =required      ""
    ;
    ==
    ;input.bd.br2.p2 
    =type  "text"
    =name  "head-pail"
    =oninput  "this.setAttribute('value', this.value);"
    =placeholder  "%state-type"
    =required      ""
    ;
    ==
    ;input.bd.br2.p2.grow
    =type  "text"
    =name  "vase"
    =oninput  "this.setAttribute('value', this.value);"
    =placeholder  "state value"
    =required      ""
    ;
    ==
    ;input.bd.br2.p2.grow
    =type  "text"
    =name  "conf"
    =oninput  "this.setAttribute('value', this.value);"
    =placeholder  "(map term pith:neo)"
    =required      ""
    ;
    ==
    ;button.bd.br2.p2.mr05.loader
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
::
++  poke-form
  |=  =bowl:neo
  ^-  manx
  ;form.poke-form.hidden.bd.br2.fr.jb.g2.p2.wf
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-poke"
  =hx-swap     "innerHTML"
  =hx-target  ".error-box"
    ;input.p2.bd.br2.ml05 
    =type          "text"
    =name          "stud"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "%diff-type"
    =required      ""
    ;
    ==
    ;input.hidden
    =type   "text"
    =name   "pith"
    =value  (en-tape:pith:neo here.bowl)
    ;
    ==
    ;input.p2.bd.br2.grow
    =type          "text"
    =name          "vase"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "[@ @]"
    =required      ""
    ;
    ==
    ;button.loader.bd.br2.mr05
      ;span.loaded:  poke
      ;span.loading:  loading
    ==
  ==
::
++  tomb-form
  |=  =bowl:neo
  =/  warning  
  ?~  ~(key by (~(kid of:neo kids.bowl) here.bowl))  
    "Are you sure you want to delete this shrub?"
  "Are you sure you want to delete this shrub and all their kids?"
  ^-  manx
  ;form.tomb-form.hidden.bd.br2.p2.wf
  =style  "border: 2px solid #FF0000; border-radius: 6px;"
  =hx-post     "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-tomb"
  ::=hx-trigger  "click from:find .tomb-trigger"
  ::  FIX THIS
::   =hx-target   "previous .loader"
::   =hx-swap     "innerHTML"
    ;div.fc.ac
      ;p:  {warning}
    ==
    ;div.fr.jc.g8.p2
      ;button.tomb-trigger.hfc.p2.red-hover
    ::   =onclick  "$(this).parent().parent().parent().parent().parent().addClass('hidden');"
      =name     "pith"
      =value    (en-tape:pith:neo here.bowl)
        ;span:  yes
      ==
      ;span.hfc.p2.red-hover.pointer
      =onclick  
      """
      $(this).parent().parent().addClass('hidden');
      $(this).parent().parent().parent().parent().find('.top').find('.buttons').find('.tomb').toggleClass('toggled');
      """
        ;span:  no
      ==
    ==
  ==
::
--