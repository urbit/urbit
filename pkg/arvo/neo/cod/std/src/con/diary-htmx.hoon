/@  diary  ::  name=@t
::  import /lib/feather-icons
/-  feather-icons
::  declare that this is a conversion from diary to HTMX
:-  [%diary %$ %htmx]
::  gate takes a diary and a bowl:neo,
::  so we can access here.bowl
|=  dia=diary
|=  =bowl:neo
::  returns a manx, in this case HTMX
^-  manx
::  construct Sail using helper arms
|^
  ::  <div class="p2" label="Diary">
  ;div.p-page
    ::  <div class="ma fc g2 mw-page">
    ;div.ma.fc.g2.mw-page
      ::  render the text input for new entries
      ;+  form-put-entry
      ::  compose several link-entry elements generated
      ::  by the +turn gate into one HTMX node
      ;*
      %+  turn
        %+  sort
          %+  murn
            ~(tap of:neo kids.bowl)
          |=  [=pith =idea:neo]
          ?~  pith  ~
          `[pith idea]
        |=  [a=[=pith *] b=[=pith *]]
        (gth ->.pith.a ->.pith.b)
      link-entry
    ==  ::  </div>
  ==  ::  </div>
::
::  text input for new entries
++  form-put-entry
  ::  <form
  ::    class="fc g2"
  ::    style="margin-bottom: 30px"
  ::    head="put-entry"
  ::    hx-post="/hawk{(en-tape:pith:neo here.bowl)}?stud=diary-diff"
  ::    hx-on-submit="this.reset()"
  ::    hx-target="find .loading"
  ::    hx-swap="outerHTML"
  ::  >
  ;form.fc.g2
    =style         "margin-bottom: 30px;"
    =hx-post       "/hawk{(en-tape:pith:neo here.bowl)}?stud=diary-diff"
    =hx-on-submit  "this.reset()"
    =hx-target     "find .loading"
    =hx-swap       "outerHTML"
    =head          "put-entry"
    ::  <date-now name="now" />
    ;date-now(name "now");
    ::  <textarea
    ::    class="p2 bd1 br1"
    ::    name="text"
    ::    placeholder="today, i ..."
    ::    oninput="this.setAttribute('value', this.value)"
    ::    rows="5"
    ::    required=""
    ::    autocomplete=""
    ::  />
    ;textarea.p2.bd1.br1
      =name  "text"
      =placeholder  "today, i ..."
      =oninput  "this.setAttribute('value', this.value)"
      =rows  "5"
      =required  ""
      =autocomplete  "off"
      ;
    ==  ::  </textarea>
    ::  <button class="p2 b1 br1 bd1 wfc hover loader">
    ;button.p2.b1.br1.bd1.wfc.hover.loader
      ::  <span class="loaded s2">create</span>
      ;span.loaded.s2: create
      ::  <span>
      ;span.loading
        ;+  loading.feather-icons
      ==  ::  </span>
    ==  ::  </button>
  ==  ::  </form>
::
::  entry box
++  link-entry
  |=  [pax=pith =idea:neo]
  ::  make sure the pail does in fact contain %txt
  ?.  =(%txt p.q.saga.idea)
    ;div: error
  ::  extract information from the given pith and pail:neo
  =/  tape  (trip !<(@t q.q.saga.idea))
  =/  subject-end  (fall (find [10]~ tape) 56)
  =/  subject  (scag subject-end tape)
  =/  id  (trip (snag 0 (pout pax)))
  ::  <div class="fr g2">
  ;div.fr.g2
    ::  <a
    ::    class="p2 br1 grow b1 hover loader"
    ::    href="{(en-tape:pith:neo (weld /hawk here.bowl))}/{id}"
    ::  >
    ;a.p2.br1.grow.b1.hover.loader
      =href  "{(en-tape:pith:neo (weld /hawk here.bowl))}/{id}"
      ::  <div class="loaded fc g1 js as g2">
      ;div.loaded.fc.g1.js.as.g2
        ::  <span class="f3">{YYYY-MM-DD}</span>
        ;span.f3: {(pretty-date `@da`->:pax)}
        ::  <span class="bold">{subject}</span>
        ;span.bold: {subject}
      ==  ::  </div>
      ::  <span class="loading">
      ;span.loading
        ;+  loading.feather-icons
      ==  ::  </span>
    ==  ::  </a>
    ;button.p2.br1.fr.g2.b1.hover.fc.ac.jc.loader
      =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=diary-diff"
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
::  styled date string
++  pretty-date
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
--
