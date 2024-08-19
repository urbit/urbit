/@  diary-diff
/-  serv=sky-server
/-  feather-icons
/-  manx-utils
^-  kook:neo
=<
|%
++  state  pro/%eyre-task
++  poke   (sy %rely ~)
++  kids  *kids:neo
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      :-  req=&
      :-  [pro/%diary (sy %diary-diff ~)]
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  [|/%da |]
          [[%only %txt] ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  !!
        %rely
      :_  pail
      =/  task  !<(task:eyre:neo q.pail)
      (eyre-cards [bowl task])
    ==
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<(=task:eyre:neo vase)
    =/  [eyre-id=@ta req=inbound-request:eyre]  task
    ?+    method.request.req  ~|(%unsupported-http-method !!)
        %'GET'
      :_  [stud vase]
      (eyre-cards [bowl task])
    ::
        %'POST'
      =;  poke
        :_  [stud vase]
        :~  :+  p:(~(got by deps.bowl) %src)
              %poke
            [%diary-diff !>(poke)]
        ==
      ^-  diary-diff
      =/  body  (parse-body:serv request.req)
      =/  mu  ~(. manx-utils body)
      =/  head  (@tas (got:mu %head))
      ?+    head  !!
          %put-entry
        =/  text  (vol:mu "text")
        [%put-entry now.bowl text]
      ::
          %del-entry
        [%del-entry (slav %da (got:mu %diary-id))]
      ==
    ==
  --
--
::
|%
++  eyre-cards
  |=  [=bowl:neo [eyre-id=@ta req=inbound-request:eyre]]
  =+  #/[p/our.bowl]/$/eyre
  :~  (head-card - eyre-id)
  ::
      :*  -
          %poke
          %eyre-sign
          !>
          :+  eyre-id
            %data
          :-  ~
          %-  manx-to-octs
          %~  render
            web
          :-  bowl
          (pave:neo pax:(parse-url-frfr:serv request.req))
      ==
  ::
      (done-card - eyre-id)
      [here.bowl %cull ~]
      [here.bowl %tomb ~]
  ==
::
++  head-card
  |=  [=pith eyre-id=@ta]
  :*  pith
      %poke
      %eyre-sign
      !>
      :^    eyre-id
          %head
        200
      ['content-type' 'text/html']~
  ==
::
++  done-card
  |=  [=pith eyre-id=@ta]
  [pith %poke eyre-sign/!>([eyre-id %done ~])]
::
++  manx-to-octs
  |=  man=manx
  (as-octt:mimes:html (en-xml:html man))
::
++  web
  |_  [=bowl:neo name=pith]
  ++  render
    ^-  manx
    ;html
      ;head
        ;*  old-standard-head-tags:serv
        ;*  standard-head-tags:serv
      ==
      ;body
        =hx-ext  "dom-enc"
        ;main.p-page.mw-page.ma.fc.g5
          ;h1.bold.f-2: BLUE
          ;+  diary-form
          ;+  diary-items
          ;+  refresher
        ==
      ==
    ==
  ::
  ++  diary-form
    ;form.fc.g2.as
      =hx-post       (en-tape:pith:neo name)
      =hx-target     "closest .p-page"
      =hx-select     ".p-page"
      =hx-swap       "outerHTML"
      =head          "put-entry"
      ;textarea.p2.bd1.br1.wf
        =name  "text"
        =placeholder  "today, i ..."
        =oninput  "this.setAttribute('value', this.value)"
        =rows  "5"
        =required  ""
        =autocomplete  "off"
        ;
      ==
      ;button.p2.b1.br1.bd1.wfc.hover.loader
        ;span.loaded: create
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ::
  ++  diary-items
    ;div#items.fc.g2
      ;*
      %-  turn
      :_  link-entry
      %+  sort
        %~  tap
          of:neo
        %.  /
        %~  del
          of:neo
        q:(~(got by deps.bowl) %src)
      |=  [a=[=pith *] b=[=pith *]]
      (gth ->.pith.a ->.pith.b)
    ==
    ::
  ++  link-entry
    |=  [pax=pith =idea:neo]
    =/  tape  (trip !<(@t q.q.saga.idea))
    ;div.fr.g2
      ;div.fc.g1.grow.br1.p-2.b1
        ;span.f3: {(pretty-date `@da`->:pax)}
        ;span.bold: {tape}
      ==
      ;button.p2.br1.b1.hover.loader
        =hx-post  (en-tape:pith:neo name)
        =head          "del-entry"
        =hx-target     "closest .p-page"
        =hx-select     ".p-page"
        =hx-swap  "outerHTML"
        =diary-id  (trip (snag 0 (pout pax)))
        ;span.loaded
          ;+  close.feather-icons
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ++  refresher
    ;div
      =hx-get  (en-tape:pith:neo name)
      =hx-target     "#items"
      =hx-select     "#items"
      =hx-swap       "outerHTML"
      =hx-trigger    "every 10s"
      ;
    ==
  ::
  ++  pretty-date
    |=  date=@da
    ^-  tape
    =/  d  (yore date)
    "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
  --
--
