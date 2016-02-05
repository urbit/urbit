::    Console front-end
::
::::  /hoon/dojo/web
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
    =nav_title   "Dojo"
    =nav_no-dpad  ""
    =nav_no-sibs  ""
  ;script(src "//cdnjs.cloudflare.com/ajax/libs/mousetrap/1.4.6/mousetrap.js");
  ;style:'''
         #term { width: 100%; }
         #term * { margin: 0px; }
         .module pre { margin-bottom: 0; }
         '''
  ;div#err;
  ;div#term:""
  ;script@"/lib/js/sole.js";
  ;sole(appl "dojo");
==
