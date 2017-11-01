::  Render all %%/{@u}.txt test cases
::
::::  /hoon/all/unmark/web
  ::
/-    down, markdown
/+    vast2
::
/=    cor    /^  (list {@ud wain})
             /:  /%%/    /_  @ud    /txt/
/=    mad    /:  /%%/cm-spec    /down/
::
|%
++  rolt  |=(a/wall `tape`?~(a ~ ?~(t.a i.a :(weld i.a "\0a" $(a t.a)))))
++  wush
  |=  {wid/@u tan/tang}  ^-  tape
  (rolt (zing (turn tan |=(a/tank (wash 0^wid a)))))
::
++  mads
  |=  a/wain  ^-  marl
  =/  try  (mule |.(~(shut ap (rash (nule ';>' a) apex:(sail &):vast2))))
  ?-  -.try
    $&  p.try
    $|  ;=  ;div
              ;h3: ERROR
              ;pre: {(wush 120 p.try)}
  ==    ==  ==
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
    ;h2: Core
    ;ul
      ;*  ^-  marl
          %+  turn  cor
          |=  {num/@u txt/wain}
          ;li: ;{p -[<num>]} *{(mads txt)} ;{hr}
    ==
  ==
  ;li
    ;h2: CommonMark
    ;ol
      ;*  ?:  [disabled=&]  ; DISABLED
          ^-  marl
          %+  murn  `down`mad
          |=  a/elem:markdown
          ?:  ?=($head -.a)
            ?.  ?=({{$$ *} $~} q.a)
              ~
            (some /(crip "h{<p.a>}") ;"{p.i.q.a}")
          ?.  ?=({$code ^ *} a)  ~
          ?.  =("example" r.u.p.a)  ~
          %-  some
          ^-  manx
          |-
          =+  [inp out]=(split-on '.' q.a)
          =/  mar  c:(snag 0 (mads inp))
          ;li
            ;pre: {(trip (role inp))}
            ;p: =>
            ;pre: {(trip (role out))}
            ;p: vs
            ;pre: {(many:poxo mar "")}
            ;p
              ;-  =/  pox  (rush (role out) many:poxa)
                  ?~  pox  "INVALID"
                  ?:  =(u.pox mar)  "EQUIVALENT"
                  ?:  =(u.pox (turn mar strip))  "COMPATIBLE"
                  "DIVERGE"
            ==
  ==      ==
==  ==
