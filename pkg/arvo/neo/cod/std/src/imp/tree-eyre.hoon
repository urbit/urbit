/@  eyre-reqs
/@  htmx-type=htmx
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
++  poke   (sy %eyre-task ~)
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
  ++  poke
    |=  [=stud:neo =vase]
    ^-  (quip card:neo pail:neo)
    ?+  stud  ~|(bad-stud/stud !!)
        %eyre-task
      =+  !<(=task:eyre:neo vase)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      ?.  authenticated.req
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        :_  pail
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request.req)
      =/  purl  (parse-url:serv request.req)
      =/  inner=pith:neo  (pave:neo pax.purl)
      =/  src  (~(got by deps.bowl) %src)
      =/  here  (tail inner)
      ~&  >>  here/here
      :_  sig/!>(~)
      ?+    method.request.req  ~|(%unsupported-http-method !!)
          %'GET'
        ?~  idea=(~(get of:neo q.src) here)
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
        =/  local=pail:neo  q.saga:(need idea)
        =/  kids=(list pith:neo)  (get-kids here q.src)
        ~&  >>  local-pail/local
        ~&  >  kids-pith/kids
        :: =/  bol  *bowl:neo
        :: =.  here.bol  here
        =/  some-view 
        ;div
        ;h1:  some kids
        ==
        %:  eyre-cards
            eyre-id
            bowl
            200
            ['content-type' 'text/html']~
            (view here local kids)
        ==
      ==
    ==
  ++  init
    |=  pal=(unit pail:neo)
    :_  sig/!>(~)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %tree]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
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
++  view 
  |=  [here=pith:neo =pail:neo kids=(list pith:neo)]
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
      ;+  (body-view here pail kids)
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
  |=  [here=pith:neo =pail:neo kids=(list pith:neo)]
  ;div.fc.js.p2.wf.p1
    ;+  (kids-view here pail kids)
  ==
::
++  kids-view
  |=  [here=pith:neo =pail:neo kids=(list pith:neo)]
  ;div
  =style  "padding: 12px; margin:20px"
    ;+
      ?~  kids
        ;div: no kids
    ;div 
    =style  
    """
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
        %+  sort  kids
        aor
    |=  =pith
    ^-  manx
    ?~  pith  
      ;div
      =style  "visibility: hidden"
        nothing
      ==
    ;div
    ;h1:  {(en-tape:pith:neo pith)}
    ==
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
--