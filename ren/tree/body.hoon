::
::::  /hoon/body/tree/ren
  ::
/?    310
/=    dat    /%  /tree-json/ :: default include
/=    dat-sen  /|   /:  /%%/  /%  /tree-json/ :: default include
                    /~  ~
               ==
=,  xml:eyre
=,  js:eyre
=,  html
^-  marl
;=  ;script(type "text/javascript"): window.tree = {(en-json (jobe data+dat sein+dat-sen ~))}
    ;div#tree;
==
