/-  *hoon-parser
/+  *recoverable-parser
|%
::
++  bug  %.n
::
++  main
  |=  =path
  ^-  (like hast)
  =/  =cord
    .^(@t %cx path)
  ((ifix [gay gay] tall) [1 1] (trip cord))


++  nest-rule
  |*  [gar=* sef=rule]
  |=  tub=nail
  ^-  (like _gar)
  (sef tub)
++  parse-until
  |*  terminator=rule
  |=  tub=nail
  =/  until
    ((star ;~(less terminator prn)) tub)
  ?~  q.until
    until
  (terminator q.u.q.until)











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
      irregular-cltr
      irregular-cord
      irregular-tape
      rune
    ==
  ==

++  irregular-suffix
  %+  stag  %irregular-suffix
  ;~  plug
    ;~  pose
      irregular-cncl
      irregular-cltr
      irregular-cord
      rune
    ==
    irregular-unknown-core
  ==

++  irregular-unknown-core
  %-  plus
  ;~  less
    ace
    lit
    rit
    say
    lac
    yel
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
    leb
    reb
    yel
    ;~(plug hep hep)
    ;~(plug tis tis)
    (jest '++')
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
++  irregular-tape-char
  ;~  pose
    ;~(less bat yel prn)
    ;~(pfix bat ;~(pose yel prn))
  ==
++  irregular-tape
  %+  cook
    |=  =tape
    [%irregular "\"{tape}\""]
  %+  ifix
    [yel yel]
  (star irregular-tape-char)
++  irregular-cord
  %+  cook
    |=  =tape
    [%irregular "'{tape}'"]
  %+  ifix
    [say say]
  (star ;~(less say prn))
++  irregular-balanced
  ;~  pose
    irregular-cncl
    irregular-cltr
    irregular-dtts
    irregular-cltr-dep
    irregular-yell
    irregular-tell
    irregular-cord
    irregular-tape
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
++  t-runechildren-tall-variadic
  %+  nest-rule  *(list hast)
  %-  star
  ;~(pfix gai tall)
++  runechildren-tall-variadic-raw
  ;~  sfix
    %-  star
    ;~(pfix gai tall)
    ::
    ;~(plug gap (jest '=='))
  ==

++  runechildren-tall-variadic-marker
  ;~  sfix
    (star ;~(less (jest '==') ;~(pose resync-skip-variadic next)))
    (jest '==')
  ==
++  resync-variadic-start
  ;~  pose
    (jest '$%')
    (jest '$:')
    (jest '$?')
    (jest '?-')
    (jest '?+')
    (jest ':*')
    (jest '%*')
    (jest ':~')
    (jest '?&')
    (jest '%_')
    (jest '%=')
    (jest '=:')
    (jest '%~')
  ==


++  resync-skip-cord
  %+  knee  *~  |.  ~+
  %+  cold  ~
  %+  ifix
    [say say]
  %-  star
  ;~  pose
    resync-skip-cord
    ;~(less say next)
  ==
++  resync-skip-variadic
  %+  knee  *~  |.  ~+
  %+  cold  ~
  %+  ifix
    [resync-variadic-start (jest '==')]
  %-  star
  ;~  pose
    resync-skip-variadic
    ;~(less (jest '==') next)
  ==
++  runechildren-tall-variadic
  %+  nest-rule  *(list hast)
  %^  resync-context
      runechildren-tall-variadic-raw
    runechildren-tall-variadic-marker
  `(list hast)`~

++  runechildren-wide-variadic
  %-  star
  ;~(sfix tall ;~(pose ace (easy ~)))
++  make-variadic-rune
  |*  [a=rule b=rule name=rune-name]
  %+  nest-rule  *hast
  %+  stag  %rune
  ;~  pfix
    ;~(plug a b)
    ;~  pose
      %+  ifix
        [lit rit]
      (stag %w (stag name runechildren-wide-variadic))
      ::
      (stag %t (stag name runechildren-tall-variadic))

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
    (make-rune sig ban %sgbn 2)
    (make-rune tis hep %tshp 2)
    (make-rune tis led %tsld 2)
    (make-rune tis ban %tsbn 2)
    (make-rune sig cen %sgcn 4)
    (make-rune sig fas %sgnt 2)
    (make-rune bar buc %brbs 2)
    (make-rune tis ket %tskt 4)
    (make-rune wut ban %wtbn 2)
    (make-rune wut dot %wtdt 3)
    (make-rune buc sig %bssg 2)
    (make-rune buc hep %bshp 2)
    (make-rune bar sig %brsg 2)
    (make-rune tis lus %tsls 2)
    (make-rune tis tar %tstr 3)
    (make-rune tis dot %tsdt 3)
    (make-rune mic mic %mcmc 2)
    (make-rune sig cab %sgcb 2)
    (make-rune tis bar %tsbr 2)
    (make-rune ket lus %ktls 2)
    (make-rune bar dot %brdt 1)
    (make-rune sig bar %sgbr 2)
    (make-rune sig pad %sgpd 2)
    (make-rune sig wut %sgwt 3)
    (make-rune dot tar %dttr 2)
    (make-rune tis wut %tswt 4)
    (make-rune cen ket %cnkt 4)
    (make-rune cen dot %cndt 2)
    (make-rune col ket %clkt 4)
    (make-rune col hep %clhp 2)
    (make-rune col cab %clcb 2)
    (make-rune col lus %clls 3)
    (make-rune wut ket %wtkt 3)
    (make-rune ket tis %ktts 2)
    (make-rune cen sig %cnsg 3)
    (make-rune wut vat %wtvt 3)
    (make-variadic-rune buc cen %bscn)
    (make-variadic-rune buc col %bscl)
    (make-variadic-rune buc wut %bswt)
    (make-variadic-rune wut hep %wthp)
    (make-variadic-rune wut lus %wtls)
    (make-variadic-rune col tar %cltr)
    (make-variadic-rune cen tar %cntr)
    (make-variadic-rune col sig %clsg)
    (make-variadic-rune wut pad %wtpd)
    (make-variadic-rune cen cab %cncb)
    (make-variadic-rune cen tis %cnts)
    (make-variadic-rune tis col %tscl)
    (make-variadic-rune cen sig %cnsg)
    (make-nothing-rune zap col)
  ==
++  make-nothing-rune
  |*  [a=rule b=rule]
  ;~  pfix
    ;~(plug a b gap)
    tall
  ==

::  +arm-name: parse arm name
++  arm-name
  %+  nest-rule  *tape
  ;~(sfix ;~(plug low (star ;~(pose nud low hep))) gap)
::  +lusbuc-arm: parse lusbuc-arm
++  lusbuc-arm
  %+  stag  %core-arm
  ;~  plug
    (cold %lsbc (jest '+$'))
    ;~  pfix
      gap
      ;~  plug
        arm-name
        (resync-context ;~(sfix tall gap) resync-arm [%irregular ""])
      ==
    ==
  ==
::  +luslus-arm: parse luslus arm
++  luslus-arm
  %+  stag  %core-arm
  ;~  plug
    (cold %lsls (jest '++'))
    ;~  pfix
      gap
      ;~  plug
        arm-name
        (resync-context ;~(sfix tall gap) resync-arm [%irregular ""])
      ==
    ==
  ==
++  lusbar-arm
  %+  inspect  "lusbar arm"
  ;~  pfix
    ;~(plug (jest '+|') gap cen)
    %+  cook
      |=  chapter=tape
      ^-  hast
      [%core-arm %lsbr chapter `hast`[%irregular ""]]
  %+  inspect  "lusbar name"
    arm-name
  ==
++   alas-arm
  %+  stag  %core-arm
  %+  stag  %lstr
  %+  stag  "alas"
  %+  stag  %alas
  %+  ifix
    [;~(plug (jest '+*') gap) (easy ~)]
  %-  plus
  ;~  plug
    arm-name
    ;~(sfix tall gap)
  ==
++  resync-core-start
  ;~  pose
    (jest '|%')
    (jest '|_')
  ==
++  resync-skip-core
  %+  knee  *~  |.  ~+
  %+  cold  ~
  %+  ifix
    [resync-core-start (jest '--')]
  %-  star
  ;~  pose
    resync-skip-core
    ;~(less (jest '--') next)
  ==


:: TODO: only works at one core deep
++  resync-arm
  %-  star
  ;~  less
    (jest '++')
    (jest '+$')
    (jest '+| ')
    (jest '--')
    ;~  pose
      resync-skip-core
      next
    ==
  ==
++  resync-core
  %-  star
  ;~  less
    (jest '--')
    ;~  pose
      resync-skip-core
      next
    ==
  ==
::  +core-arm: parse core arm
++  core-arm
  ;~  pose
    luslus-arm
    lusbuc-arm
    lusbar-arm
    alas-arm
  ==
++  brcb
  %+  stag  %core
  %+  stag  %brcb
  %+  ifix
    [;~(plug bar cab) ;~(plug hep hep)]
  ;~  plug
    %+  stag  ~
    %+  ifix
      [gap gap]
    tall
    ::
    (star core-arm)
  ==


::  +core: parse core
++  core
  ;~  pose
    brcb
    brcn
  ==
::
++  brcn
  %+  stag  %core
  %+  stag  %brcn
  %+  stag  ~
  %+  ifix
    [;~(plug bar cen gap) ;~(plug hep hep)]
  (star core-arm)

++  wart
  |*  r=rule
  %+  here
    |=  [a=pint b=hast]
    ^-  hast
    ?:(bug [%trace a b] b)
  r
::  +tall: parse tall form hoon expression
++  tall
  %+  knee  *hast  |.  ~+
  ;~  pose
    core
    rune
    irregular-adjacent
    irregular-suffix
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
    irregular-suffix
    irregular-balanced
    irregular-unknown
  ==
--
