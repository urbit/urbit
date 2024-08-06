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
        =/  id  (slav %da (vol:mu "now"))
        =/  text  (vol:mu "text")
        [%put-entry id text]
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
        ;*  standard-head-tags:serv
      ==
      ;body
        =hx-ext  "dom-enc"
        ;div.p-page
          ;div.ma.fc.g2.mw-page
            ;+  form-put-entry
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
        ==
      ==
    ==
  ::
  ++  form-put-entry
    ;form.fc.g2
      =style         "margin-bottom: 30px;"
      =hx-post       "{(en-tape:pith:neo name)}"
      =hx-on-submit  "this.reset()"
      =hx-target     "closest .p-page"
      =hx-select     ".p-page"
      =hx-swap       "outerHTML"
      =head          "put-entry"
      ;date-now(name "now");
      ;textarea.p2.bd1.br1
        =name  "text"
        =placeholder  "today, i ..."
        =oninput  "this.setAttribute('value', this.value)"
        =rows  "5"
        =required  ""
        =autocomplete  "off"
        ;
      ==
      ;button.p2.b1.br1.bd1.wfc.hover.loader
        ;span.loaded.s2: create
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ::
  ++  link-entry
    |=  [pax=pith =idea:neo]
    =/  tape  (trip !<(@t q.q.saga.idea))
    =/  subject-end  (fall (find [10]~ tape) 56)
    =/  subject  (scag subject-end tape)
    =/  id  (trip (snag 0 (pout pax)))
    ;div.fr.g2
      ;a.p2.br1.grow.b1.hover.loader
        ::  =href  "{(en-tape:pith:neo (weld /neo/blue sesh))}/{id}"
        ;div.loaded.fc.g1.js.as.g2
          ;span.f3: {(pretty-date `@da`->:pax)}
          ;span.bold: {subject}
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
      ;button.p2.br1.fr.g2.b1.hover.fc.ac.jc.loader
        =hx-post  "{(en-tape:pith:neo name)}"
        =head  "del-entry"
        =hx-target     "closest .p-page"
        =hx-select     ".p-page"
        =hx-swap  "outerHTML"
        =diary-id  id
        ;span.loaded
          ;+  close.feather-icons
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  ::
  ++  pretty-date
    |=  date=@da
    ^-  tape
    =/  d  (yore date)
    "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
  --
--