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
++  poke   *(set stud:neo)
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
    !!
  ++  init 
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    ?~  src=(~(get by deps.bowl) %src)
      =/  stub=manx
        ;div
          ;h1: nothing here
        ==
      %:  eyre-cards
        eyre-id
        bowl
        404
        ['content-type' 'text/html']~
        stub
      ==
    =/  here  p.u.src
    ^-  (list card:neo)
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      ?~  reet=(~(get of:neo q.u.src) /)
        =/  bol  *bowl:neo
        =.  here.bol  here
        =/  stub=manx
          ;div
            ;h1: nothing here
          ==
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            stub
        ==
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  now.bol  now.bowl
      =.  eny.bol  eny.bowl
      =.  kids.bol  q.u.src
      ~&  >  pail/q.saga.u.reet
      ~&  ~(key by (~(kid of:neo q.u.src) /tasks))
      =/  stub=manx
        ;div
          ;+  (view bol)
        ==
      %:  eyre-cards
          eyre-id
          bowl
          200
          ['content-type' 'text/html']~
          stub
      ==
        %'POST'
      =/  purl  (parse-url:serv request.req)
      =/  content-type  (~(gut by pam.purl) 'content-type' 'text/html')
      =/  body  (parse-body:serv request.req)
      =/  poke-stud
        ^-  stud:neo
        ~|  %no-stud-specified
        (~(got by pam.purl) 'stud')
      =/  diff-vase  
      (http-request [poke-stud `request:http`request.req])
      =/  diff-type   !<(tree-diff diff-vase)
      ~&  >>  diff-type/diff-type
      =/  cards=(list card:neo)
        ?-  -.diff-type
            %send-poke 
          ~[(poke-tree-card our.bowl diff-vase)]
            %send-tomb
          =/  pax=pith:neo  (tail (tail +.diff-type))
          =/  poke-card=(list card:neo)  ~[(poke-tree-card our.bowl diff-vase)]
          =/  kids  (kids-to-card pax q.u.src our.bowl here)
          ?~  kids  poke-card
          %+  welp  (flop kids)  poke-card
        ==
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  now.bol  now.bowl
      =.  eny.bol  eny.bowl
      %+  welp  cards
      %:  eyre-cards
      eyre-id
      bol
      200
      ['content-type' 'text/html']~
      del-stub
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
::
++  poke-tree-card
  |=  [our=@p vax=vase]
  ^-  card:neo
  [#/[p/our]/tree %poke %tree-diff vax]
::
++  kids-to-card
  |=  [=pith:neo =lore:neo our=@p head-pith=pith:neo]
  ^-  (list card:neo)
  =/  cards  *(list card:neo)
  =/  kids  (get-kids pith lore)
  ~&  >>  head-pith/head-pith
  ~&  >  all-kids/kids
  ?~  kids  ~
  %+  turn  kids
    |=  p=pith:neo 
    (poke-tree-card our !>([%send-tomb (welp head-pith p)]))
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
++  del-stub
  ^-  manx
  ;div.p2
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
  '''
::
++  body-view
  |=  =bowl:neo
  ;div
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
      =(1 (lent pith))
    aor
  |=  [=pith:neo *] 
  %-  crip  "[{(en-tape:pith:neo pith)}]"
  =/  row-template=tape  (join ' auto ' (weld "[first-row]" `tape`first-kids))
  ;div
  =style  "display: grid; grid-template-rows: {row-template}; grid-template-columns: auto; padding: 12px;"
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
    ;div
    =style  "grid-column-start: {(scow %ud +((lent pith)))}; grid-column-end: {(scow %ud (add 2 (lent pith)))}; grid-row-start: {(en-tape:pith:neo [-.pith ~])};grid-row-end: {(en-tape:pith:neo [-.pith ~])}; padding: 8px; border: 2px solid black; border-radius: 6px; margin-top: 1rem; margin-right: 1rem;"
      ;div
      =style 
      """
      display: grid; 
      grid-template-rows: auto auto auto; 
      grid-template-columns: auto auto;
      row-gap: 8px;
      """
        ;h3.p2.hfc
        =style  "grid-column: 1 ; grid-row: 1"
        ;  {(en-tape:pith:neo pith)}
        ==
        ;div.loader.p2.hover.pointer
        =style  "grid-column: 3 ; grid-row: 1; justify-self: end; border: 2px solid black; border-radius: 6px;"
        =onclick  "$(this).next().toggleClass('hidden');"
          ;span.loaded:  tomb
          ;span.loading:  loading
        ==
        ;+  (tomb-button bowl pith)
      ==
      ;div
        ;h3.p2:  state:
        ;+  (state-print q.saga.idea)
        ;+  (poke-form bowl pith)
      ==
    ==
  ==
::
++  state-print
  |=  =pail:neo
  ^-  manx
  ::;div.pre.mono.scroll-x.fr.jc.p2
  ;div.fr.js.p2
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 750)  "vase too large to print: {<size>}"
    (of-wall:format (~(win re (sell q.pail)) 0 80))
  ==
::
++  tomb-button
  |=  [=bowl:neo =pith:neo]
  =/  warning  
  ?~  ~(key by (~(kid of:neo kids.bowl) pith))  
    "Are you sure you want to delete this shrub?"
  "Are you sure you want to delete this shrub and all their kids?"
  ^-  manx
  ;form.hidden.fc.ae.g2.grow
  =style       "grid-column: 1 / 4; grid-row: 2; justify-self: center;"
  =hx-post     "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-tomb"
  =hx-trigger  "click from:find .tomb-trigger"
  =hx-target   "previous .loader"
  =hx-swap     "outerHTML"
    ;div.p2.grow
    =style  "border: 2px solid #FF0000; border-radius: 6px;"
      ;div.fc.ac
        ;p:  {warning}
      ==
      ;div.fr.ja.p2
        ;button.tomb-trigger.hfc.p2.red-hover
        =onclick  "$(this).parent().parent().parent().addClass('hidden');"
        =name     "pith"
        =value    (en-tape:pith:neo (welp here.bowl pith))
          ;span:  yes
        ==
        ;span.hfc.p2.red-hover.pointer
        =onclick  "$(this).parent().parent().parent().addClass('hidden');"
          ;span:  no
        ==
      ==
    ==
  ==
::
++  poke-form
  |=  [=bowl:neo =pith:neo]
  ^-  manx
  ;form
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-poke"
  =hx-target  "find .loading"
  =hx-swap     "outerHTML"
    ;input 
    =type          "text"
    =name          "stud"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "diff-type"
    =required      ""
    ;
    ==
    ;input.hidden
    =type   "text"
    =name   "pith"
    =value  (en-tape:pith:neo (welp here.bowl pith))
    ;
    ==
    ;input 
    =type          "text"
    =name          "vase"
    =oninput       "this.setAttribute('value', this.value);"
    =autocomplete  "off"
    =placeholder   "[@ @]"
    =required      ""
    ;
    ==
    ;button.loader
      ;span.loaded:  create
      ;span.loading:  loading
    ==
  ==
::
--