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
::
|%
++  script-safe
  !.
  |=  a/tape  ^-  tape
  ?~  a  a
  ?.  ?=({$'<' $'/' *} a)  [i.a $(a t.a)]
  ['<' '\\' '/' $(a t.t.a)]
--
::
^-  marl
=/  tree  (script-safe (en-json (pairs:enjs data+dat sein+dat-sen ~)))
;=  ;script(type "text/javascript"): window.tree = {tree}
    ;div#tree;
==
