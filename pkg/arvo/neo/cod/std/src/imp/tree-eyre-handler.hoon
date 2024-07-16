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
:: static dep to imp folder to create new shrub 
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
    =/  this  !<([eyre-id=@ta req=inbound-request:eyre] q.pail)
    =/  eyre-id  eyre-id.this
    :_  eyre-task/q.pail
    ?+  stud  !!
    ::
        %ack
      ?~  !<((unit quit:neo) vase)
        =/  success-manx
          ;div.p2
            ;p:  success
          ==
        %:  eyre-cards
        eyre-id
        bowl
        200
        ['content-type' 'text/html']~
        success-manx
        ==
      =/  =quit:neo  (need !<((unit quit:neo) vase))
      ::  if not goof crush 404
      ?+  -.quit  
        %:  eyre-cards
        eyre-id
        bowl
        404
        ['content-type' 'text/html']~
        *manx
        ==
      ::  if goof return err stack trace 
          %goof
        =/  =tang  +.quit
        =/  trace-manx
          ;div.p2
            ;*  %+  turn  tang
              |=  =tank
              ^-  manx
              (err-manx ~(ram re tank))
          ==
        %:  eyre-cards
        eyre-id
        bowl
        200
        ['content-type' 'text/html']~
        trace-manx
        ==
      ==
      ::
        %tree-diff
      =/  diff  !<(tree-diff vase)
      ~&  >>>  diff-tree-imp/diff
      ?-  -.diff  
        ::
          %send-poke
        =/  =pith:neo  pith.diff
        =/  poke-stud=stud:neo  stud.diff
        =/  vax  vase.diff
        :~  
            [pith %poke [poke-stud vax]]
        ==
        ::
          %send-tomb
        =/  =pith:neo  +.diff
        ~&  >>>  pith-tomb/pith
        :~  
            [pith %cull ~]
            [pith %tomb ~]
        ==
      ==
    ==
  ++  init 
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    ~&  id/eyre-id
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
      ~&  >>>  'in GET'
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  kids.bol  q.u.src
      ::~&   ~(key by (~(kid of:neo kids.bol) (tail here)))
      =/  some-view 
      ;div
      ;h1:  some kids
      ==
      %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          ::some-view
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
          %send-poke 
        ?:  =(stud.diff-type %vase-error)
          %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          (err-manx ~)
          ==
        :~  (poke-tree-card here.bowl diff-vase)
        ==
          %send-tomb
        =/  pax=pith:neo  (tail (tail +.diff-type))
        =/  poke-card=(list card:neo)  ~[(poke-tree-card here.bowl diff-vase)]
        =/  kids  (kids-to-card pax q.u.src here.bowl here)
        =/  bol  *bowl:neo
        =.  here.bol  here
        =.  kids.bol  q.u.src
        ;:  welp
          (flop kids)  
          poke-card
          %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          (view bol)
          ==
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
  =/  kids  (get-kids pith lore)
  ?~  kids  ~
  %+  turn  kids
    |=  p=pith:neo 
    (poke-tree-card here !>([%send-tomb (welp head-pith p)]))
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
++  err-manx       
  |=  =tape
  =/  msg  
    ?~  tape  "failed to parse entered value to hoon"
  tape
  ^-  manx
  ;div.p2.error
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
  .bd {
  border: 0.8px solid black;
  }
  .error{
  color: #FF0000; 
  }
  '''
::
++  body-view
  |=  =bowl:neo
  ;div.fc.js.p2.wf.p1
    ;+  (kids-view bowl)
  ==
::
++  kids-view
  |=  =bowl:neo
  =/  first-kids=(list @t)  
  :: TODO: if all kids
  :: only pith 
  %+  turn 
    ::only short paths
    %+  sort
      %+  skim  ~(tap of:neo kids.bowl) 
      |=  [=pith:neo *] 
      ~&  kids-view/pith
      =(1 (lent pith))
    aor
  |=  [=pith:neo *] 
  %-  crip  "[{(en-tape:pith:neo pith)}]"
  =/  row-template=tape  (join ' auto ' (weld "[first-row]" `tape`first-kids))
  ;div
  =style  "display: grid; grid-template-rows: {row-template}; grid-template-columns: auto; padding: 12px; margin:20px"
    ;+
      ?~  node=(~(get of:neo kids.bowl) /)
        ;div: no kids
    =/  =pail:neo  q.saga.u.node
    ;div 
    =style  
    """
    grid-column-start: 1;
    grid-column-end: 2; 
    grid-row-start: first-row; 
    grid-row-end: first-row; 
    padding: 8px; 
    border: 2px solid black;
    border-radius: 6px; 
    margin-top: 1rem;
    margin-right: 1rem;
    """
      ;p: {<p.pail>}
      ;+  (state-print pail)
    ==
    ;*
    %+  turn  
        %+  sort  ~(tap of:neo kids.bowl) 
        aor
    |=  [=pith =idea:neo]
    ^-  manx
    ?~  pith  
      ;div
      =style  "visibility: hidden"
        nothing
      ==
    ~&  >  pith
    ;div
    ;h1:  {(en-tape:pith:neo pith)}
    ==
    :: ;div.fc
    :: =style  "grid-column-start: {(scow %ud +((lent pith)))}; grid-column-end: {(scow %ud (add 2 (lent pith)))}; grid-row-start: {(en-tape:pith:neo [-.pith ~])};grid-row-end: {(en-tape:pith:neo [-.pith ~])}; padding: 8px; border: 2px solid black; border-radius: 6px; margin-top: 1rem; margin-right: 1rem;"
    ::   ;div.fc.p2
    ::     ::;+  (forms bowl pith)
    ::     ;div.top.fr.jb
    ::       ;h3.p2.hfc.p2
    ::       ;  {(en-tape:pith:neo pith)}
    ::       ==
    ::       ;div.btn.fr.g4.p2.hover.pointer
    ::         ;div.tomb.hidden
    ::           ;div.loader.p2.bd.br2
    ::           =onclick  
    ::           """
    ::           $(this).parent().parent().parent().next().find('.tomb-form').toggleClass('hidden'); 
    ::           $(this).parent().parent().parent().next().find('.poke-form').toggleClass('hidden');
    ::           $(this).parent().toggleClass('hidden');
    ::           """
    ::             ;span.loaded:  tomb
    ::             ;span.loading:  loading
    ::           ==
    ::         ==
    ::         ;div.p2.bd.br2.hover.pointer
    ::         =onclick  
    ::         """
    ::         $(this).parent().parent().next().toggleClass('hidden');  
    ::         $(this).prev().toggleClass('hidden'); 
    ::         $(this).parent().parent().parent().toggleClass('bd');
    ::         $(this).parent().parent().parent().toggleClass('br2');
    ::         """
    ::           ;span:  ***
    ::         ==
    ::       ==
    ::     ==
    ::     ;+  (forms bowl pith)
    ::   ==
    ::   ;div.state.p2
    ::   =style  "margin-top:auto;"
    ::     ;+  (state-print q.saga.idea)
    ::   ==
    :: ==
  ==
::
++  forms
  |=  [=bowl:neo =pith:neo]  
  ;div.hidden.forms
    ;+  (poke-form bowl pith)
    ;+  (tomb-form bowl pith)
  ==
::
++  state-print
  |=  =pail:neo
  ^-  manx
  ;div.fr.js.p2
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 750)  "vase too large to print: {<size>}"
    (of-wall:format (~(win re (sell q.pail)) 0 80))
  ==
::
++  tomb-form
  |=  [=bowl:neo =pith:neo]
  =/  warning  
  ?~  ~(key by (~(kid of:neo kids.bowl) pith))  
    "Are you sure you want to delete this shrub?"
  "Are you sure you want to delete this shrub and all their kids?"
  ^-  manx
  ;form.tomb-form.p2.br2.bd.hidden
  =style  "border: 2px solid #FF0000; border-radius: 6px;"
  =hx-post     "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-tomb"
  =hx-trigger  "click from:find .tomb-trigger"
  ::  FIX THIS
::   =hx-target   "previous .loader"
::   =hx-swap     "innerHTML"
    ;div.fc.ac
      ;p:  {warning}
    ==
    ;div.fr.ja.p2
      ;button.tomb-trigger.hfc.p2.red-hover
      =onclick  "$(this).parent().parent().parent().parent().parent().addClass('hidden');"
      =name     "pith"
      =value    (en-tape:pith:neo (welp here.bowl pith))
        ;span:  yes
      ==
      ;span.hfc.p2.red-hover.pointer
      =onclick  
      """
      $(this).parent().parent().addClass('hidden');
      $(this).parent().parent().parent().find('.poke-form').toggleClass('hidden');
      $(this).parent().parent().parent().prev().find('.btn').find('.tomb').toggleClass('hidden');
      """
        ;span:  no
      ==
    ==
  ==
::
++  poke-form
  |=  [=bowl:neo =pith:neo]
  ^-  manx
  ;form.poke-form.fr.jb.g2
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-poke"
  =hx-swap     "beforebegin"
    ;input.p2.bd.br2.grow
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
    =value  (en-tape:pith:neo (welp here.bowl pith))
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
    ;button.loader.bd.br2
      ;span.loaded:  poke
      ;span.loading:  loading
    ==
  ==
::
--