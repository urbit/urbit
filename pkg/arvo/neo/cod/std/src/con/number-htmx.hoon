/@  number
::  import lib/feather-icons 
/-  feather-icons
::  specifying conversion from number to htmx 
:-  [%number %$ %htmx]
::  gater takes number and bowl
|=  =number
|=  =bowl:neo
::  returns manx
::  manx is a dynamic XML node, whitch means we going to write Sail with HTMX
^-  manx
::  opening a div that has class of "fc ac br2", this classes are corespond to Feather styling library
::
::  div will include h1 and h2-tag with our state value
::  h-tags take a tape value 
::
::  it will also include form, that will have hx-post, hx-target, hx-swap and head attributes
::  hx-post is issuing a POST request to provided url and swaps HTML into the DOM
::  hx-target allows us to target element with a class of "loading" for swapping
::  hx-swap specifies how response will be swapped in relative to the target 
::  in this case head attribute specifies what kind of poke it attempts to send, we will unpack more of logic behind it in con/node-counter-diff.hoon 
;div.fc.ac.br2
  ;h1:  Counter
  ;h2:  {<number>}
  ;form
  =hx-post       "/neo/hawk{(en-tape:pith:neo here.bowl)}?stud=counter-diff"
  =hx-target     "find .loading"
  =hx-swap       "outterHTML"
  =head          "inc"
  ::  button will submit POST request form
  ::  classes of "loader", "loaded" and "loading" provided for loading spinner behavior on sending request 
  ::  loading behavior includes changing of button innerHTML span to loading icon and back to "Increment" span
    ;button.bd3.p2.hover.loader
      ;span.loaded:  Increment
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
==
