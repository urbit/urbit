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
    ~&  >  req/req
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
    :: ~&  >  src/p.u.src
    :: ~&  >  all-deps/~(tap by deps)
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
      ::  get lash from dep and pass pokes to poke-form 
      ::  from src
    ::   ~&  >>>
    ::   %+  turn  ~(tap of:neo q.u.src)
    ::   |=  [=pith:neo =idea:neo]
    ::   pail/(sell q.pail.idea)
      =/  bol  *bowl:neo
      =.  here.bol  here
      =.  our.bol  our.bowl
      =.  now.bol  now.bowl
      =.  eny.bol  eny.bowl
      =.  kids.bol  q.u.src
      ~&  >  pail/q.saga.u.reet
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
    ~&  >>  poke-stud/poke-stud
      =/  diff-vase  
      (http-request [poke-stud `request:http`request.req])
      ~&  >>>  diff-vase/diff-vase
      =/  diff-type   -:!<(tree-diff diff-vase)
      =/  bol  *bowl:neo
        =.  here.bol  here
        =.  our.bol  our.bowl
        =.  now.bol  now.bowl
        =.  eny.bol  eny.bowl
        =/  stub=manx
        ::   ?+  diff-type  
            ::   ;div
            ::     ;p:  error 
            ::   ==
        ::       %send-poke
        ::     ;div
        ::       ;p:  sent
        ::     ==
        ::       %send-tomb 
            ;div
              ;p:  deleted
            ==
        ::   ==
        :-  [#/[p/our.bowl]/tree %poke %tree-diff diff-vase]
        %:  eyre-cards
            eyre-id
            bol
            200
            ['content-type' 'text/html']~
            stub
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
    ==
    ;body 
      ;+  (body-view bowl)
    ==
  ==
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
    =style  "grid-column-start: {(scow %ud +((lent pith)))}; grid-column-end: {(scow %ud (add 2 (lent pith)))}; grid-row-start: {(en-tape:pith:neo [-.pith ~])};grid-row-end: {(en-tape:pith:neo [-.pith ~])}; padding: 8px; border: 2px solid black;border-radius: 6px;margin-top: 1rem;"
      ;h3:  {(en-tape:pith:neo pith)}
      ;+  (tomb-button bowl pith)
      ;div
        ;h3.wfc:  state
        ;+  (state-print q.saga.idea)
        :: ;+  (poke-form pith)
      ==
    ==
  ==
::
++  state-print
  |=  =pail:neo
  ^-  manx
  ;div.pre.mono.scroll-x.p2
    ;+  ;/
    =/  size  (met 3 (jam q.q.pail))
    ?:  (gth size 750)  "vase too large to print: {<size>}"
    (of-wall:format (~(win re (sell q.pail)) 0 80))
  ==
::
++  poke-form
  |=  =pith:neo
  ^-  manx
    ;form
    =hx-post    "/neo/tree{(en-tape:pith:neo pith)}?stud=type-diff"
    =hx-target  "find .loading"
      ;input 
      =type     "text"
      =name     "diff-type"
      =oninput  "this.setAttribute('value', this.value); this.parentNode.setAttribute('head', this.value);"
      =autocomplete  "off"
      =placeholder   "diff-type"
      =required      ""
      ;
      ==
      ;input 
      =type   "text"
      =name   "pith"
      =value  "{(en-tape:pith:neo pith)}"
      ;
      ==
      ;input 
      =type   "text"
      =name   "value1"
      =oninput  "this.setAttribute('value', this.value);"
      =autocomplete  "off"
      =placeholder   "value"
      =required      ""
      ;
      ==
      ;button.loader
        ;span.loaded:  create
        ;span.loading:  loading
      ==
    ==
::
++  tomb-button
  |=  [=bowl:neo =pith:neo]
  ^-  manx
  ;form
  =hx-post    "/neo/tree{(en-tape:pith:neo here.bowl)}?stud=tree-diff&head=send-tomb"
  =hx-target  "find .loader"
  =hx-swap    "outerHTML"
    ;button.loader
    =name       "pith"
    =value      (en-tape:pith:neo (welp here.bowl pith))
      ;span.loaded:  tomb
      ;span.loading:  loading
    ==
  ==
::
--