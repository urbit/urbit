/@  number  ::  @ud
::  import /lib/feather-icons
/-  feather-icons
::  declare that this is a conversion from number to HTMX
:-  [%number %$ %htmx]
::
::  this gate accepts a number and a bowl:neo;
::  we'll access bowl:neo in the UI to access the
::  here.bowl of the shrub that's using this /con file
|=  =number
|=  =bowl:neo
::
::  this gate returns a manx, which is what Hoon uses
::  to store dynamic XML nodes; in this case we'll use
::  Sail to specify a manx that expects the HTMX library
::  to be available on the frontend
^-  manx
::
::  open a <div class="p3 fc g2 ac br2">
::  these utility classes are specified in feather.css,
::  which this /con file expects on the frontend
;div.p3.fc.g2.ac.br2
  ::  <h1>Counter</h1>
  ;h1:  Counter
  ::  <p>{number}</p>
  ;p:  {<number>}
  ::  open a <form> with HTMX attributes
  ;form
    ::
    ::  hx-post will issue a POST request to the provided
    ::  url and swap response into the DOM
    =hx-post  "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=counter-diff"
    ::
    ::  hx-target specifies the target for hx-post's DOM
    ::  swap: the element with class "loading"
    =hx-target  "find .loading"
    ::
    ::  hx-swap specifies how the response to hx-post's
    ::  request will be swapped in relative to the target
    =hx-swap  "outerHTML"
    ::
    ::  here, the head attribute specifies the poke that
    ::  hx-post will send to the target shrub; look at
    ::  /con/node-counter-diff.hoon for more on =head
    =head  "inc"
    ::
    ::  below, the classes "loaded", "loader", and
    ::  "loading" provide loading spinner behavior on
    ::  sending and receiving this form's POST request
    ::
    ::  <button class="bd1 br1 pr b1 hover loader">
    ;button.bd1.br1.p2.b1.hover.loader
      ::  <span class="loaded">Increment</span>
      ;span.loaded:  Increment
      ::  <span class="loading">
      ;span.loading
        ::  import +loading sail from /lib/feather-icons
        ;+  loading.feather-icons
      ==  ::  </span>
    ==  ::  </button>
  ==  ::  </form>
==  ::  </div>
