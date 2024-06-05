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
  ;div.p2
    =label  "Diary"
    ::  <div class="ma fc g2" style="max-width: 650px">
    ;div.ma.fc.g2
      =style  "max-width: 650px;"
      ::  render the text input for new entries
      ;+  form-put-entry
      ::  compose several link-entry elements generated
      ::  by the +turn gate into one HTMX node
      ;*
      %+  turn
        ~
        ::  %+  sort  ~(tap of:neo kids.bowl)
        ::  |=  [a=[=pith *] b=[=pith *]]
        ::  (gth ->.pith.a ->.pith.b)
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
  ::    hx-post="{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?stud=diary-diff"
  ::    hx-on-submit="this.reset()"
  ::    hx-target="this"
  ::    hx-swap="afterend"
  ::  >
  ;form.fc.g2
    =style         "margin-bottom: 30px;"
    =hx-post       "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?stud=diary-diff"
    =hx-on-submit  "this.reset()"
    =hx-target     "this"
    =hx-swap       "afterend"
    =head          "put-entry"
    ::  <date-now name="id" />
    ;date-now(name "id");
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
      ;  ::  XX unnecessary?
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
  |=  [pax=pith =pail:neo]
  ::  extract information from the given pith and pail:neo
  =/  tape  (trip !<(@t q.pail))
  =/  subject-end  (fall (find [10]~ tape) 56)
  =/  subject  (scag subject-end tape)
  =/  id  (trip (snag 0 (pout pax)))
  ::  <div class="fr g2">
  ;div.fr.g2
    ::  <a
    ::    class="p2 br1 grow b1 hover loader"
    ::    href="{(en-tape:pith:neo (weld /neo/hawk here.bowl))}/{id}"
    ::  >
    ;a.p2.br1.grow.b1.hover.loader
      =href  "{(en-tape:pith:neo (weld /neo/hawk here.bowl))}/{id}"
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
    ::  <button
    ::    class="p2 br1 fr g2 b1 hover fc ac jc loader"
    ::    onclick="alert('not yet implemented. no tombstoning?')"
    ::  >
    ;button.p2.br1.fr.g2.b1.hover.fc.ac.jc.loader
      =onclick  "alert('not yet implemented. no tombstoning?')"
      ::  <span class="loaded">
      ;span.loaded
        ;+  close.feather-icons
      ==  ::  </span>
      ::  <span class="loading">
      ;span.loading
        ;+  loading.feather-icons
      ==  ::  </span>
    ==  ::  </button>
  ==  ::  </div>
::
::  styled date string
++  pretty-date
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
--
