::
::::
  :::
/?    310
/=    gas    /$    fuel
::
::::
  ::
  !:
^-  manx
=+  do=(~(get by qix.gas) %'code')

;html
  ;head
    ;script@"/~/at/lib/js/urb.js";
    ;script: urb.appl = 'cloud'
    ;script@"https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.js";
    ;script@"https://cdnjs.cloudflare.com/ajax/libs/react/0.12.2/react.js";
    ::;link/"/cloud/main.css"(rel "stylesheet");
    ;title: DO & GCE Manager
  ==
  ;body
  ;script:"""
          var authcode = \{}
          authcode.do='{?~(do ~ (trip u.do))}'
          """
    ;div#container;
    ;script@"/cloud/main.js";
  ==
==
