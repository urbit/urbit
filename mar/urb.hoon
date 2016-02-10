::
::::  /hoon/urb/mar
  ::
/?  314
/=  urb-wasp-data-js    /:    /%/wasp-data    /js/
!:
|_  [[dep=@uvH hed=marl] [dep-bod=@uvH bod=marl]]
++  linked-deps-js
  '''
  urb.waspAll = function(sel){
    [].map.call(document.querySelectorAll(sel), urb.waspElem)
  }
  if(urb.wasp){urb.waspAll('script'); urb.waspAll('link')}
  '''
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  ++  html  (crip (poxo hymn))                          ::  convert to %html
  ++  hymn                                              ::  inject dependencies
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8", urb_injected "");
        ;*  ?~  dep  ~
            :~  ;script@"/~/on/{<dep>}.js"(urb_injected "");  
                ;script(urb_injected "")
                  ;-  (trip urb-wasp-data-js)
                  ;-  "urb.waspData({(pojo %s (scot %uv dep-bod))})"
                ==
            ==
        ;*  hed
      ==
      ;body
        ;*  bod
        ;script(urb_injected ""):"{(trip linked-deps-js)}"
      ==
    ==
  --
++  grab  
  |%                                                    ::  convert from
  ++  noun  ,[@uvH manx]                                ::  clam from %noun
  --
--
