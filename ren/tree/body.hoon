::
::::  /hoon/body/tree/ren
  ::
/?    310
/=    dat    /%  /tree-json/ :: default include
/=    dat-sen  /|   /:  /%%/  /%  /tree-json/ :: default include
                    /~  ~
               ==
^-  marl
;=  ;script(type "text/javascript"): window.tree = {(pojo (jobe data+dat sein+dat-sen ~))}
    ;div#tree;
==
