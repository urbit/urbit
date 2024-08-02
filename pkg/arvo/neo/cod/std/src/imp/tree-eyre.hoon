/@  eyre-reqs
/@  tree-diff
/-  serv=sky-server
/-  srv=server
/-  su=shrub-utils
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
++  state  pro/%eyre-id
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
    :_  eyre-id/!>(*@ta)
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =/  =binding:eyre  [~ ~[%neo %tree]]
    =/  =req:eyre:neo  [%connect binding here.bowl]
    :~  [pith %poke eyre-req/!>(req)]
    ==
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+  stud  ~|(bad-stud/stud !!)
        %tree-diff  
      :_  eyre-id/q.pail
      =/  diff  !<(tree-diff vax)
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
        :~  
            [pith %poke [stud.diff vase.diff]]
        ==
        ::
          %send-cull
        ~&  >>>  pith-cull/pith
        :~  
            [pith %cull ~]
        ==
        ::  
          %req-parsing-err
        ~&  >  tang
        ~&  !<(@ta q.pail)
        %:  eyre-cards
        !<(@ta q.pail)
        bowl 
        200 
        ['content-type'^'text/html']~ 
        (err-trace-manx tang)
        ==
      ==
      ::
        %ack  
      :_  eyre-id/q.pail
      =/  eyre-id  !<(@ta q.pail)
      =+  ^=  resp
          :*  
            eyre-id
            bowl 
            200 
            headers=`header-list:http`['content-type'^'text/html']~ 
            manx=*manx
          ==
      ?~  !<((unit quit:neo) vax)
        =.  headers.resp  
          %+  snoc  headers.resp  
          'HX-Refresh'^'true'
        (eyre-cards resp)
      ?:  =(*pail:neo pail)  ~
      =/  =quit:neo  (need !<((unit quit:neo) vax))
      ?+  -.quit  ~
          %goof
        =.  manx.resp  (err-trace-manx +.quit)
        (eyre-cards resp)
      ==
      ::
        %eyre-task
      =+  !<(=task:eyre:neo vax)
      =/  [eyre-id=@ta req=inbound-request:eyre]  task
      =/  request=request:http  request.req
      ?.  authenticated.req
        :_  eyre-id/q.pail
        =/  eyre=pith:neo  #/[p/our.bowl]/$/eyre
        %+  ~(respond neo:srv eyre)   eyre-id
        (login-redirect:gen:srv request)
      =/  purl  (parse-url:serv request)
      =/  inner=pith:neo  (pave:neo pax.purl)
      =/  src  (~(got by deps.bowl) %src)
      =/  here  (tail inner)
      =+  ^=  resp
          :*  
            eyre-id
            bowl 
            status=200 
            headers=`header-list:http`['content-type'^'text/html']~ 
            manx=*manx
          ==
      ?+    method.request  ~|(%unsupported-http-method !!)
        ::
          %'GET'
        :_  eyre-id/q.pail
        ?.  =((head inner) p/our.bowl)
          =.  status.resp  404
          =.  manx.resp  ;div:  not on your ship
          %-  eyre-cards  resp
        =/  kids=(list pith:neo)  (kids-at-pith:su q.src here)
        =.  manx.resp              
          ?~  idea=(~(get of:neo q.src) here)
            (view inner [%$ !>(~)] kids bowl)
          =/  local=pail:neo  q.saga:(need idea)
          (view inner local kids bowl)
        %-  eyre-cards  resp
        ::
          %'POST'
        =/  poke-stud
          ^-  stud:neo
          ~|  %no-stud-specified
          (~(got by pam.purl) 'stud')
        =/  diff-vase  (http-request [poke-stud `request:http`request])
        =/  tree-diff  !<(tree-diff diff-vase)
        ?-  -.tree-diff
        ::
            %send-make
          ::  %make cards don't have error(%goof) %acks yet 
          ::  sending eyre response here for now
          :_  eyre-id/!>(eyre-id)
          =/  mule-vax  (mule-vase bowl request)
          =/  mule-conf=(each conf:neo tang)
            %-  mule  |.  
            !<  conf:neo  
            %+  to-hoon  bowl
            (get-by-name request 'conf') 
          ?:  |(?=(%| -.mule-vax) ?=(%| -.mule-conf))
          %+  poke-tree-card  here.bowl
            !>  :-  %req-parsing-err
            ?:  ?=(%| -.mule-vax)
              p.mule-vax
            p.mule-conf
          =/  vax=vase  p.mule-vax
          =/  =conf:neo  p.mule-conf
          =+  diff=tree-diff
          =.  init.diff
            %-  some  
            :-  p:(need init.diff)
                vax
          =.  conf.diff  conf
          =.  pith.diff  (get-pith bowl request) 
          =.  headers.resp  
            %+  snoc  headers.resp  
            'HX-Refresh'^'true'
          %+  welp 
            (poke-tree-card here.bowl !>(diff))
            %-  eyre-cards  resp
          ::
            %send-poke
          :_  eyre-id/!>(eyre-id)  
          =/  mule-vax  (mule-vase bowl request)
          ?-  -.mule-vax
              %|  
            (poke-tree-card here.bowl !>([%req-parsing-err p.mule-vax]))
            ::
              %&
            =+  diff=tree-diff 
            =.  vase.diff  p.mule-vax
            (poke-tree-card here.bowl !>(diff))
          ==
          ::
            %send-cull
          :_  eyre-id/q.pail
          =/  poke-card=(list card:neo)  (poke-tree-card here.bowl diff-vase)
          =/  location  
            %-  crip 
              %+  weld  "/neo/tree" 
                (en-tape:pith:neo (snip inner))
          =.  headers.resp  
            %+  snoc  headers.resp  'HX-Redirect'^location
          %+  welp
            poke-card
          %-  eyre-cards  resp
          ::
            %req-parsing-err  [~ eyre-id/q.pail]
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
  =/  head=sign:eyre:neo  [eyre-id %head [status header-list]]
  =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
  =/  done=sign:eyre:neo  [eyre-id %done ~]
  :~  [pith %poke eyre-sign/!>(head)]
      [pith %poke eyre-sign/!>(data)]
      [pith %poke eyre-sign/!>(done)]
  ==
::
++  poke-tree-card
  |=  [here=pith:neo vax=vase]
  ^-  (list card:neo)
  :~  [here %poke %tree-diff vax]
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
  %+  turn  (kids-at-pith:su lore parent)
    |=  p=pith:neo 
    %+  welp  parent  p
::
++  get-pith
  |=  [=bowl:neo req=request:http]
  ^-  pith:neo
  =/  parent=pith:neo  
    %-  pave:neo 
    %-  stab  (get-by-name req 'here')
  =/  kid  (get-by-name req 'pith')
  ?~  kid  parent
  =/  kid-pith  !<  pith:neo  (to-hoon bowl kid)
  (welp parent kid-pith)
::
++  mule-vase
  |=  [=bowl:neo req=request:http]
  ^-  (each vase tang)
  %-  mule  |.
  %+  to-hoon  bowl
  (get-by-name req 'vase') 
::
++  get-by-name
  |=  [req=request:http hoon=cord]
  ^-  cord
  =/  pam  (~(uni by pam:(parse-url:serv req)) (parse-form-body:serv req))
  =/  bod  ~(. by pam)
  ?~  value=(get:bod hoon)  ''
  u:value
::
++  to-hoon
  |=  [=bowl:neo =cord]
  ^-  vase
  %+  slap  :(slop !>(..zuse) !>(bowl) !>(neo))
  %-  ream  cord
::
++  err-trace-manx
|=  =tang
^-  manx
  ;div.p2
    ;*  %+  turn  tang
      |=  =tank
      ^-  manx
      ;div.wf.fr.js.p1.error.mono
        ;  {~(ram re tank)}
      ==
  ==
::
++  empty-manx
  ;div.hidden
  ;
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
      ;+  ?:  &(=(kids ~) =(pail [%$ !>(~)]))
        (nothing-view here)
      (body-view here pail kids bowl)
    ==
  ==
::
++  tree-style 
  ^~
  %-  trip 
  '''
  *{
  font-size: calc(1.2 * var(--font-size));
  }
  input{
  font-family: var(--font-mono);
  font-size: calc(1em * var(--mono-scale));
  }
  .red-hover:hover{
  background-color: #FF0000; 
  color: var(--b0);
  border-radius: 6px;
  }
  .bd.bd2{
  border: 1.2px solid var(--f1);
  }
  .error{
  color: #FF0000; 
  }
  @media (max-width: 900px) {
    * {
    font-size: calc(1.2 * var(--font-size));
    }
    input {
    font-size: calc(1em * var(--mono-scale));
    }
    .top, .pro-btn, .import-btn, .buttons {
    display: flex;
    flex-wrap: wrap;
    justify-content: flex-end;
    }
    .buttons, .form-btn {
    display: flex;
    justify-content: end;
    }
    .cull-btn{
    justify-content: space-around;
    }
    .red-hover{
    margin-top: 1rem;
    background-color: #FF0000; 
    color: var(--b0);
    border-radius: 6px;
    padding: 4px;
    }
  }
  '''
::
++  nothing-view
  |=  here=pith:neo
  =/  parent  (en-tape:pith:neo (snip here))
  ^-  manx
  ;div.p2
    ;div.wf.fc.p2.ac.g2.bd.bd2.br2
      ;div.top.fr.jb.g2.wf
        ;h1.p2:  nothing at {(en-tape:pith:neo here)}
        ;div.buttons.fr.g2
          ;a.loader.p2.tc.hover.b0.bd.bd2.br2.grow
          =href  "/neo/tree{parent}"
            ;span.loaded.hf: back to {parent}
            ;span.loading.hf:  loading
          ==
          ;+  make-btn
        ==
      ==
      ;div.fr.jb.g2.wf
        ;div.forms.fc.as.g2.wf
          ;div.error-box
          ;
          ==
          ;+  (make-form here |)
        ==
      ==
    ==
  ==
::
++  body-view
  |=  [here=pith:neo =pail:neo kids=(list pith:neo) =bowl:neo]
  ^-  manx
  ;div.wf
    ;div.fc.js.p2.g2
      ;+  (shrub-view here pail bowl)
      ;+  (kids-view here pail kids)
    ==
  ==
::
++  shrub-view 
  |=  [here=pith:neo =pail:neo =bowl:neo] 
  ^-  manx
  ;div
    ;div.fc.g2.bd.bd2.br2.p2
      ;div.top.fr.jb.g2
        ;p.p2.hfc.wfh.grow:  {(en-tape:pith:neo here)}
        ;+  buttons
      ==
      ;div.fr.jb.g2
        ;+  ?.  =(p.pail %hoon)
          (state-print pail)
        empty-manx
        ;div.fr.je.grow.g2.pro-btn
          ;+  (pro-files pail here bowl)
          ;+  ?:  =(p.pail %$)
            empty-manx
          ;a.loader.p2.bd.bd2.br2.hover.b0
          =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(p.pail p.pail mark.p.pail))}"
            ;span.loaded.hf: {<p.pail>}
            ;span.loading.hf:  loading
          ==
        ==
      ==
      ;+  (forms here)
      ;+  ?:  =(p.pail %hoon)
        (state-print pail)
      empty-manx
    ==
  ==
++  kids-view
  |=  [here=pith:neo =pail:neo kids=(list pith:neo)]
  ^-  manx
  ;div.fc.g2
    ;*
    %+  turn  
        %+  sort  kids
        aor
    |=  =pith:neo
    ^-  manx
    ?~  pith  
      empty-manx
    ;a.fr.jb.g1.bd.bd2.br2.hover.b0
    =href  "/neo/tree{(en-tape:pith:neo (weld here pith))}"
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
    ;div.fc.g1.p2.grow
      ;* 
      %+  turn  wain
      |=  lin=@t 
      ;p.mono:  {(trip lin)}\0a
    ==
  ;div.fr.js.p2.mono
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
    empty-manx
  =/  =name:neo  [our.bowl here]
  =/  fool=(each file:ford:neo tang)
    %-  mule
    |.
    (scan (trip !<(@t q.pail)) (rein:ford:neo name))
  ?:  ?=(%| -.fool)  
    empty-manx
  ;div.fr.g2.import-btn
    ;*  %+  turn  pro.p.fool
    |=  =pro:ford:neo
    ^-  manx
    ;a.loader.bd.bd2.br2.p2.hover.b0
      =href  "/neo/tree/{(scow %p our.bowl)}/cod/std/src/pro/{(trip ?@(stud.pro stud.pro mark.stud.pro))}"
        ;span.loaded.hfc: {<stud.pro>}
        ;span.loading.hfc:  loading
    ==
  ==
++  buttons
  ^-  manx
  ;div.buttons.fr.g2
    ;+  make-btn
    ;button.poke.p2.bd.bd2.br2.hover.b2
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
      ;span.p2:  poke
    ==
    ;button.cull.p2.bd.bd2.br2.hover.b2
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
      ;span.p2:  cull
    ==
  ==
::
++  make-btn
  ;button.make.p2.bd.bd2.br2.hover.b2
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
    ;span.p2:  make
  ==
::
++  forms
  |=  here=pith:neo
  ^-  manx
    ;div.forms.fc.as.g2.wf
      ;div.error-box
      ;
      ==
      ;+  (make-form here &)
      ;+  (poke-form here)
      ;+  (cull-form here)
    ==
::
++  make-form
  |=  [here=pith:neo main=?]
  ^-  manx
  ;form.make-form.hidden.bd.bd2.br2.fc.g2.p2.wf.hfc
  =hx-post    "/neo/tree{(en-tape:pith:neo here)}?stud=tree-diff&head=send-make"
  =hx-swap    "outterHTML"
  =hx-target  ".error-box"
    ;div.frw.g2
      ;input.hidden
      =type   "text"
      =name   "here"
      =value  (en-tape:pith:neo here)
      ;
      ==
      ;+  
      =;  m
      ?.  main  -
      m(a.g [[%required ""] a.g.m])
      ^-  manx
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "pith"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "/some/pith"
      ;
      ==
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "stud"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "%shrub-type"
      =required      ""
      ;
      ==
      ;input.bd.bd2.br2.p2.grow
      =type          "text"
      =name          "head-pail"
      =oninput       "this.setAttribute('value', this.value);"
      =placeholder   "%state-type"
      =required      ""
      ;
      ==
    ==
    ;div.frw.g2
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
      ;div.form-btn
        ;button.loader.bd.bd2.br2.hover.b0.p2
          ;span.loaded.p2:  make
          ;span.loading.p2:  loading
        ==
      ==
    ==
  ==
::
++  poke-form
  |=  here=pith:neo
  ^-  manx
  ;form.poke-form.hidden.bd.bd2.br2.frw.jb.g2.p2.wf
  =hx-post    "/neo/tree{(en-tape:pith:neo here)}?stud=tree-diff&head=send-poke"
  =hx-swap    "outterHTML"
  =hx-target  ".error-box"
    ;input.hidden
    =type   "text"
    =name   "pith"
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
    ;div.form-btn
      ;button.loader.bd.bd2.br2.hover.b0.p2
        ;span.loaded.p2:  poke
        ;span.loading.p2:  loading
      ==
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
    ;div.fr.jc.g8.p2.cull-btn
      ;button.cull-trigger.hfc.p2.red-hover
      =name     "pith"
      =value    (en-tape:pith:neo here)
        ;span.p2:  yes
      ==
      ;span.hfc.p2.red-hover.pointer
      =onclick  
      """
      $(this).parent().parent().addClass('hidden');
      $(this).parent().parent().parent().parent().find('.top').find('.buttons').find('.cull').toggleClass('toggled');
      """
        ;span.p2:  no
      ==
    ==
  ==
--
