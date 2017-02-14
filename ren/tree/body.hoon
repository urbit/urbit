::
::::  /hoon/body/tree/ren
  ::
/?    310
/=    dat    /%  /tree-json/ :: default include
/=    dat-sen  /|   /:  /%%/  /%  /tree-json/ :: default include
                    /~  ~
               ==
=,  format
=,  html
^-  marl
=/  tree  (en-json (pairs:enjs data+dat sein+dat-sen ~))
;=  ;script(type "text/javascript"): window.tree = {tree}
    ;div#tree;
==
