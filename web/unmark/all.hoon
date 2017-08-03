::  Render all %%/{@u}.txt test cases
::
::::  /hoon/all/unmark/web
  ::
/=  sam  /^  (list (pair @ud wain))
         /:  /%%/  /_  @ud  /txt/
::
|%
++  rolt  |=(a/wall `tape`?~(a ~ ?~(t.a i.a :(weld i.a "\0a" $(a t.a)))))
++  wush
  |=  {wid/@u tan/tang}
  ^-  tape
  (rolt (zing (turn tan |=(a/tank (wash 0^wid a)))))
::
++  mads
  |=  a/wain  ^-  marl
  =/  try  (mule |.(~(shut ap (ream (nule '---' a)))))
  ?-  -.try
    $&  p.try
    $|  ;=  ;h3: ERROR
            ;pre: {(wush 120 p.try)}
  ==    ==
--
^-  manx
;ul
  ;*  |-
      ?~  sam  ~
      :+  ;li: ;{p -[<p.i.sam>]} *{(mads q.i.sam)}
        ;hr;
      $(sam t.sam)
==
