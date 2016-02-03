::    Console front-end
::
::::  /hook/hymn/dojo/web
  ::
/?    310
|%
++  cdnj  |=(a=tape ;script(src "//cdnjs.cloudflare.com/ajax/libs/{a}");)
--
::
::::
  ::
^-  manx
;module
    =nav_title   "Sole"
    =nav_subnav  "div"
  ;*  %-  turn  :_  cdnj  ^-  wall
      :~  :: "jquery/2.1.1/jquery.min.js"
          "mousetrap/1.4.6/mousetrap.js"
          :: "react/0.11.0/react.js"
      ==
  ::;script@"/~~/~/at/lib/js/urb.js";
  ;style:'''
         #term {
           width: 100%;
         }
         #term * {
           margin: 0px;
         }
         '''
  ;div#err;
  ;div#term:""
  ;script@"/lib/js/sole.js";
  ;sole(appl "dojo");
==
