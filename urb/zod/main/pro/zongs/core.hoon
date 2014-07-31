::
::::  /hoon/core/zing/pro
  ::
/?  314
/-  zong
|_  zos=(list zong)
::
++  grab                                                ::  convert from
  |%
  ++  noun                                              ::  convert from %noun
    |=  src=*
    ^+  +>+
    +>+(zos ((list zong) src))
  --
::
++  grow                                                ::  convert to
  |%
  ++  hymn                                              ::  convert to %hymn
    ^-  manx
    ;html
      ;head
        ;title: Zongs!
      ==
      ;body   ;table
        ;*  %+  turn  zos
        |=  zog=zong
        =.  p.zog  (lsh 6 1 (rsh 6 1 p.zog))      ::  round off subsecond
        ;tr   ;td: {<p.zog>}
              ;td: {<q.zog>}
              ;+  ?-  -.r.zog
                      %do
                    ;td: {(trip p.r.zog)}
                      %say
                    ;td: & {(trip p.r.zog)}
                      %exp
                    ;td
                      ;code:"{(trip p.r.zog)}"
                      ; \a0 {~(ram re q.r.zog)}
        ==          ==
      ==          ==
    ==          ==
  ++  html
    (crip (xmlt | hymn ~))
  ++  mime
    [/text/html (taco html)]
  --
--
