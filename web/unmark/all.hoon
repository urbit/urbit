::  Render all %%/{@u}.txt test cases
::
::::  /hoon/all/unmark/web
  ::
/+    cram
::
/=    cor
  /^  (list {@ud wain})
  /:  /%%/
  /;  |=  a/(map knot wain)
      =;  kid/(list {@ud wain})  (sort kid dor)
      %+  murn  ~(tap by a)
      |=  {b/knot c/wain}
      %+  bind  (slaw %ud b)
      |=(d/@ud [d c])
  /_  /txt/
::
|%
++  rolt  |=(a/wall `tape`?~(a ~ ?~(t.a i.a :(weld i.a "\0a" $(a t.a)))))
++  wush
  |=  {wid/@u tan/tang}  ^-  tape
  (rolt (zing (turn tan |=(a/tank (wash 0^wid a)))))
::
++  mads
  =,    userlib
  |=  a/wain  ^-  manx
  =/  try/(each manx tang)
    %-  mule  |.
    elm:(static:cram (rash (nule:unix ';>' a) apex:(sail &):vast))
  ?-  -.try
    $&  p.try
    $|  ;div
          ;h3: ERROR
          ;pre: {(wush 120 p.try)}
  ==    ==
::
++  split-on
  =|  hed/wain
  |=  {mid/@t all/wain}  ^+  [hed all]
  ?~  all  !!
  ?:  =(mid i.all)  [(flop hed) t.all]
  $(all t.all, hed :_(hed i.all))
::
++  strip
  |=  a/manx  ^-  manx
  :_  (turn c.a ..$)
  ?+  g.a  g.a
    {@ {$id *} *}   g.a(a t.a.g.a)
    {$$ {$$ *} $~}
      =<  g.a(v.i.a (tufa (turn (tuba v.i.a.g.a) .)))
      |=(b/@c `@`?+(b b $~-~201c. '"', $~-~201d. '"'))
  ==
--
::
^-  manx
;ul
  ;li
    ;ul
      ;*  ^-  marl
          %+  turn  cor
          |=  {num/@u txt/wain}
          ;li: ;{p -[<num>]} +{(mads txt)} ;{hr}
    ==
  ==
==
