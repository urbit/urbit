/-  *hoon-parser
/+  *recoverable-parser
|%
::  $rune-kind: kind of rune
::
::    Either %t (tall), %w (wide) or %i (irregular)
++  main
  |=  =path
  ^-  (like hast)
  =/  =cord
    .^(@t %cx path)
  (tall [1 1] (trip cord))
++  nest-rule
  |*  [gar=* sef=rule]
  |=  tub=nail
  ^-  (like _gar)
  (sef tub)
::
:: gap, but using an ace accidentally is unambigous
++  gai
;~(pose gap (parse-with-error 'Incorrect whitespace' ace))
++  irregular-adjacent
  %+  stag  %irregular-adjacent
  ;~  plug
    irregular-unknown-core
    ;~  pose
      irregular-cncl
      rune
    ==
  ==


++  irregular-unknown-core
  %-  plus
  ;~  less
    ace
    (just `@`10)
    lit
    rit
    lac
    rac
    prn
  ==
::
::  +irregular-unknown: irregular unbalanced syntax
++  irregular-unknown
  %+  nest-rule  *hast
  %+  stag  %irregular
  %-  plus
  ;~  less
    ace
    (just `@`10)
    lit
    rit
    ;~(plug hep hep)
    lac
    rac
    prn
  ==
++  foo
  ;~  pose
    (plus ace)
    irregular-unknown
  ==
::  +make-irregular-balanced: build parser for an irregular balanced syntax
++  make-irregular-balanced
  |*  [start=rule end=rule name=@ta]
  %+  nest-rule  *hast
  %+  stag  %rune
  %+  stag  %i
  %+  stag  name
  %+  ifix
    [start end]
  %-  plus
  ;~(sfix wide ;~(pose ace (easy ~)))
::
++  irregular-dtts
  (make-irregular-balanced ;~(plug tis lit) rit %dtts)
::  +irregular-cncl: irregular %cncl syntax
++  irregular-cncl
  (make-irregular-balanced lit rit %cncl)
::  +irregular-cltr: irregular %cltr syntax
++  irregular-cltr
  (make-irregular-balanced lac rac %cltr)
::  +irregular-cltr: irregular %cltr syntax (deprecated)
++  irregular-cltr-dep
  (make-irregular-balanced leb reb %cltr)
::  +irregular-yell: irregular prettyprinter syntax
++  irregular-yell
  (make-irregular-balanced ban led %yell)
::  +irregular-tell: irregular prettyprinter syntax
++  irregular-tell
  (make-irregular-balanced led ban %tell)
++  irregular-balanced
  ;~  pose
    irregular-cncl
    irregular-cltr
    irregular-dtts
    irregular-cltr-dep
    irregular-yell
    irregular-tell
  ==
::
::  +make-runechildren-wide: build parser for wide runes
++  make-runechildren-wide
  |*  arity=@ud
  %+  stun
    [0 arity]
  ;~(sfix wide ;~(pose ace (easy ~)))
::  +make-runechildren-tall: build parser for tall runes
++  make-runechildren-tall
  |*  arity=@ud
  %+  stun
    [0 arity]
  ;~(pfix gai tall)
::  +make-runechildren-tall: build parser for a rune
++  make-rune
  |*  [a=rule b=rule name=@ta arity=@ud]
  %+  stag  %rune
  ;~  pfix
    ;~(plug a b)
    ;~  pose
      %+  ifix
        [lit rit]
      (stag %w (stag name (make-runechildren-wide arity)))
      ::
      (stag %t (stag name (make-runechildren-tall arity)))
    ==
  ==
::  +rune: rune parser
::    TODO: variadic runes
++  rune
  ;~  pose
    (make-rune bar tis %brts 2)
    (make-rune ket hep %kthp 2)
    (make-rune tis fas %tsnt 3)
    (make-rune cen lus %cnls 3)
    (make-rune wut dot %wtdt 3)
    (make-rune wut sig %wtsg 3)
    (make-rune wut col %wtcl 3)
    (make-rune wut tis %wtts 2)
    (make-rune cen hep %cnhp 2)
    (make-rune bar hep %brhp 1)
    (make-rune wut led %wtld 2)
    (make-rune tis hep %tshp 2)
  ==
::  +arm-name: parse arm name
++  arm-name
  ;~(sfix ;~(plug low (star ;~(pose nud low hep))) gap)

::  +luslus-arm: parse luslus arm
++  luslus-arm
  %+  stag  %core-arm
  ;~  plug
    (cold %lsls ;~(plug gap (jest '++')))
    ;~  pfix
      gap
      ;~  plug
        arm-name
        tall
    ==
  ==
::  +core-arm: parse core arm
++  core-arm
  ;~  pose
    luslus-arm
  ==

::  +core: parse core
++  core
  %+  stag  %core
  %+  stag  %brcn
  %+  stag  ~
  %+  ifix
    [;~(plug bar cen) ;~(plug gap hep hep)]
  (star core-arm)

::  +tall: parse tall form hoon expression
++  tall
  %+  knee  *hast  |.  ~+
  ;~  pose
    core
    rune
    irregular-adjacent
    irregular-balanced
    irregular-unknown
  ==
::  +wide: parse wide form hoon expression
++  wide
  %+  knee  *hast  |.  ~+
  ;~  pose
    core
    rune
    irregular-adjacent
    irregular-balanced
    irregular-unknown
  ==
--
