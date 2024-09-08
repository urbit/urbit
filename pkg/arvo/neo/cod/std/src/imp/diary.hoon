/@  diary     
/@  diary-diff
/-  oxy=oxygen
/-  feather-icons
/-  serv=sky-server
^-  kook:neo
|%
++  state  pro/%diary
++  poke   (sy %diary-diff %eyre-task ~)
++  deps  *deps:neo
++  kids
  %-  some
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da &]
      [only/%txt ~]
  ==
::
++  form
  ^-  form:neo
  =<
  |_  [=bowl:neo =aeon:neo =pail:neo]
  +*  web  ~(. +> [bowl pail])
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  [(bind:oxy bowl) ~]
    ?~  old
      diary/!>(*diary)
    u.old
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(our ship.src):bowl
    ?+    stud  !!
        %eyre-task
      (handle:web !<(task:eyre:neo vax))
        %diary-diff
      =/  act  !<(diary-diff vax)
      :_  pail
      :~  ?-    -.act
              %put-entry  
            :-  (welp here.bowl [[%da id.act] ~])
            [%make %txt `[%txt !>(txt.act)] ~]
          ::
              %del-entry  
            :-  (welp here.bowl [[%da id.act] ~])
            [%cull ~]
          ==
      ==
    ==
  --
  ::
  |_  [=bowl:neo =pail:neo]
  ++  handle
    |=  [eyre-id=@ta req=inbound-request:eyre]
    ^-  (quip card:neo pail:neo)
    :_  pail
    ?+    method.request.req  
        ~|(%unsupported-http-method !!)
    ::
        %'GET'
      =;  manx
        (respond:oxy [bowl eyre-id req manx])
      %~  render
        ui
      (pave:neo pax:(parse-url:oxy request.req))
    ::
        %'POST'
      =;  poke
        [here.bowl %poke [%diary-diff !>(poke)]]~
      ^-  diary-diff
      =/  body  (parse-body:oxy request.req)
      =/  mu  ~(. manx-utils:oxy body)
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
  ::
  ++  ui
    |_  here=pith
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
            ;h1.bold.f-3: Diary
            ;+  diary-form
            ;+  diary-items
            ;+  refresher
          ==
        ==
      ==
    ::
    ++  diary-form
      ;form.fc.g2.as
        =hx-post       (en-tape:pith:neo here)
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
          (~(del of:neo kids.bowl) /)
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
          =hx-post  (en-tape:pith:neo here)
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
    ::
    ++  refresher
      ;div
        =hx-get  (en-tape:pith:neo here)
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
--