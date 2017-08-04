::  Render all %%/{@u}.txt test cases
::
::::  /hoon/all/unmark/web
  ::
/-    down, markdown
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
  =/  try  (mule |.(~(shut ap (ream (nule '---' a)))))
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
      ;*  ^-  marl
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
                  ?:  =(u.pox (strip mar))  "COMPATIBLE"
                  "DIVERGE"
            ==
  ==      ==
==  ==
