/@  renderer
/@  diary-diff
/-  feather-icons
/-  manx-utils
^-  kook:neo
=<
|%
++  state  pro/%renderer
++  poke   (sy %manx %rely %gift ~)
++  kids
  :+  ~  %y
  %-  malt
  :~  :-  [|/%uv |]
      [pro/%renderer (sy %manx ~)]
  ==
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
    ::  rely: The shrub we're rendering updated its state.
    ::        Re-render the UI and save it to our state.
        %rely
      =/  sesh  session:!<(renderer q.pail)
      [~ renderer/!>([sesh ~(render html [bowl sesh])])]
    ::
    ::  manx: A poke comes in as a manx. Parse it,
    ::        forward it, and then wait for the %rely.
        %manx
      =;  poke
        :_  pail
        :~  :+  p:(~(got by deps.bowl) %src) 
              %poke
            [%diary-diff !>(poke)]
        ==
      ^-  diary-diff
      =/  post  !<(manx vax)
      =/  mu  ~(. manx-utils post)
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
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =/  sesh  session:!<(renderer vase)
    [~ renderer/!>([sesh ~(render html [bowl sesh])])]
  --
--
::
|%
++  html
  |_  [=bowl:neo sesh=road:neo]
  ++  render
    ^-  manx
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
  ::
  ++  form-put-entry
    ;form.fc.g2
      =style         "margin-bottom: 30px;"
      =hx-post       "/neo/blue{(en-tape:pith:neo sesh)}"
      =hx-on-submit  "this.reset()"
      =hx-target     "find .loading"
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
        ::  XX can't open this on its own until blue
        ::     can use other top-level renderers
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
        =hx-post  "/neo/blue{(en-tape:pith:neo sesh)}"
        =head  "del-entry"
        =hx-target  "find .loading"
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