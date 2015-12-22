::    Talk log front-end
::
::::  /hoon/hymn/talklog/ren
  ::
/?    310
:: /&    |=([beam path] ~&(+< ~))
/=    mez
  /;    pojo
  /;    |=(a=(list ,[@ p=json]) =.(a (flop a) ?~(a !! p.i.a)))
  /&    /json/
|%
++  cdnj  |=(a=tape ;script(src "//cdnjs.cloudflare.com/ajax/libs/{a}");)
--
::
::::
  ::
^-  manx
;html
  ;head 
    ;title: Talk Log
    ;*  %-  turn  :_  cdnj  ^-  wall
        :~  "jquery/2.1.1/jquery.min.js"
            "lodash.js/2.4.1/lodash.min.js"
            "react/0.13.0/react.js"
  ==    ==
  ;body
    ;div#cont;
    ;script(type "text/javascript", src "/~/at/home/lib/urb.js");
    ;link/"/home/pub/talk/src/css/main.css"(type "text/css", rel "stylesheet");
    ;script: window.MessageData = {mez}
    ;script@"/home/pub/talklog/src/MessagesComponent.coffee"(type "text/coffeescript");
    ;script@"/home/pub/talklog/src/util.coffee"(type "text/coffeescript");
    ;script@"/home/pub/talklog/src/main.coffee"(type "text/coffeescript");
    ;+  (cdnj "coffee-script/1.7.1/coffee-script.min.js")
  ==  
==
