::
::::  /hoon/body/tree/ren
  ::
/?    310
/=    dat    /%  /tree-json/ :: default include
/=    dat-sen  /|   /:  /%%/  /%  /tree-json/ :: default include
                    /~  ~
               ==
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
;=  ;script(type "text/javascript")
    ; window.tree =
    ;-  (script-safe (pojo (jobe data+dat sein+dat-sen ~)))
    ==
    ;div#tree;
==
