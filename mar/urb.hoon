::
::::  /hoon/urb/mar
  ::
/?    310
/=  urb-wasp-data-js    /:    /%/wasp-data    /js/
=,  format
=,  xml:eyre
=,  bytes:eyre
=,  html
::
|_  {{dep/@uvH hed/marl} {dep-bod/@uvH bod/marl}}
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/html (taco html)]                    ::  convert to %mime
  ++  html  (crip (print hymn))                         ::  convert to %html
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
                  ;  urb.waspWait = []
                  ;  urb.wasp = urb.wasp || [].push.bind(urb.waspWait)
                  ;  urb.onDep = function()\{
                  ;    urb.waspWait.map(urb.wasp)
                  ;    urb.onLoadUrbJS()
                  ;    urb.waspData({(en-json %s (scot %uv dep-bod))})
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
