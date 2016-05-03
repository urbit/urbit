::
::::  /hoon/urb/mar
  ::
/?    310
/=  urb-wasp-data-js    /:    /%/wasp-data    /js/
!:
|_  {{dep/@uvH hed/marl} {dep-bod/@uvH bod/marl}}
++  linked-deps-js
  '''
  urb.waspAll = function(sel){
    [].map.call(document.querySelectorAll(sel), urb.waspElem)
  }
  urb.waspAll('script'); urb.waspAll('link')
  
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
        ;*  hed
      ==
      ;body
        ;*  bod
        ;*  ?~  dep  ~
            :~  ;script@"/~/on/{<dep>}.js"(urb_injected "", async "", onload "setTimeout(urb.onDep,2000)");  
                ;script(urb_injected "")
                  ;-  (trip urb-wasp-data-js)
                  ;  window.urb = window.urb || \{}
                  ;  urb.onDep = function()\{
                  ;   urb.waspDeps();
                  ;   urb.waspData({(pojo %s (scot %uv dep-bod))});
                  ;-  (trip linked-deps-js)
                  ;  }
                ==
            ==
      ==
    ==
  --
++  grab  
  |%                                                    ::  convert from
  ++  noun  {@uvH manx}                                 ::  clam from %noun
  --
--
