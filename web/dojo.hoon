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
;html
  ;head 
    ;title: Sole
    ;*  %-  turn  :_  cdnj  ^-  wall
        :~  "jquery/2.1.1/jquery.min.js"
            "mousetrap/1.4.6/mousetrap.js"
            "react/0.11.0/react.js"
        ==
    ;script@"/~~/~/at/lib/urb.js";
    ;script: urb.appl = 'dojo'
    ;style:'''
           #term {
             width: 100%;
           }
           #term * {
             margin: 0px;
           }
           '''
  ==
  ;body
    ;div#err;
    ;div#term:""
    ;script@"dojo/share.coffee"(type "text/coffeescript");
    ;script@"dojo/main.coffee"(type "text/coffeescript");
    ;+  (cdnj "coffee-script/1.7.1/coffee-script.min.js")
  ==  
==
