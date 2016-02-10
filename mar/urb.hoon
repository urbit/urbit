::
::::  /hoon/urb/mar
  ::
/?  314
!:
|_  [dep=@uvH own=manx]
::
++  etag-js
  '''
  urb.waspAll = function(sel){
    Array.prototype.map.call(document.querySelectorAll(sel), urb.waspElem)
  }
  if(urb.wasp){urb.waspAll('script'); urb.waspAll('link')}
  '''
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  ++  html  (crip (poxo hymn))                          ::  convert to %html
  ++  hymn                                              ::  inject dependencies
    ^-  manx
    =+  ^=  max                       ::  XX types
      ~|  [%malformed-urb own]
      ?>  ?=([[%html ~] [[%head ~] *] [[%body ~] *] ~] own)
      `[[%html ~] [[%head ~] hed=marl] [[%body ~] tal=marl] ~]`own
    ::
    =:  hed.max  :_(hed.max ;meta(charset "utf-8", urb_injected "");)
        tal.max  (welp tal.max ;script(urb_injected ""):"{(trip etag-js)}" ~)
      ==
    ?~  dep  max
    max(hed :_(hed.max ;script@"/~/on/{<dep>}.js"(urb_injected "");))
  --
++  grab  
  |%                                                    ::  convert from
  ++  noun  ,[@uvH manx]                                ::  clam from %noun
  --
--
