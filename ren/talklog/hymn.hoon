::    Talk log front-end
::
::::  /hoon/hymn/talklog/ren
  ::
/?    310
/=    mez
  /;    pojo
  /;    |=(a=(list ,[@ p=json]) =.(a (flop a) ?~(a [%a ~] p.i.a)))
  /@    /json/
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
    ;script(type "text/javascript", src "/~/at/lib/js/urb.js");
    ;link/"/talk/main.css"(type "text/css", rel "stylesheet");
    ;script: window.MessageData = {mez}
    ;script@"js/MessagesComponent.coffee"(type "text/coffeescript");
    ;script@"js/util.coffee"(type "text/coffeescript");
    ;script@"js/main.coffee"(type "text/coffeescript");
    ;+  (cdnj "coffee-script/1.7.1/coffee-script.min.js")
  ==  
==
