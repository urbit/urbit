::
::::  /hoon/urb/mar
  ::
/?  314
!:
|_  [dep=@uvH own=[[%html ~] [[%head ~] hed=marl] [[%body ~] tal=marl] ~]]
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
    =:  hed.own  :_(hed.own ;meta(charset "utf-8", urb_injected "");)
        tal.own  (welp tal.own ;script(urb_injected ""):"{(trip etag-js)}" ~)
      ==
    ?~  dep  own
    own(hed :_(hed.own ;script@"/~/on/{<dep>}.js"(urb_injected "");))
  --
++  grab  
  |%                                                    ::  convert from
  ++  noun  ,[@uvH manx]                                ::  clam from %noun
  --
--
