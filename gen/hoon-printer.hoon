::  Hoon printer
::
::::  /hoon/hoon-printer/gen
  ::
/?    310
!:
::
::::
  ::
:-  %say
|=  {^ {{demo=hoon ~} ~}}
:-  %txt
^-  wain
=<  =/  plum=plum  (hoon-to-plum demo)
    ~(tall plume plum)    
|%
++  limb-to-plum
  |=  =limb
  ?@  limb  limb
  ?-  -.limb
    %&  (scot %ui p.limb)
    %|  (crip (runt [0 p.limb] ?~(q.limb "," (trip u.q.limb))))
  ==
::
++  wing-to-plum
  |=  =wing
  ^-  plum
  :+  %&
    [`['.' ~] ~]
  (turn wing limb-to-plum)
::
++  battery-to-plum
  |=  =(map term spec)
  %+  turn  ~(tap by map)
  |=  [=term =spec]
  :+  %&
    [`['  ' ~] `['' ~]]
  [term (spec-to-plum spec) ~]
::
++  core-to-plum
  |=  [=knot =spec =(map term spec)]
  ^-  plum
  :+  %&
    [~ `[knot ~]]
  :~  (spec-to-plum spec)
      :+  %&
        [~ `['' `['++' '--']]]
      (battery-to-plum map)
  ==
::
++  varying
  |=  [intro=knot final=knot]
  [`[' ' `[(cat 3 intro '(') ')']] `[intro `['' final]]]
::
++  fixed
  |=  @ta
  [`[' ' `[(cat 3 +< '(') ')']] `[+< ~]]
::
++  standard
  |=  =stud
  ^-  plum
  ?@  stud  stud
  :+  %&
    [`['/' ~] ~]
  `(list plum)`[auth.stud type.stud]
::
++  hoon-to-plum
  |=  =hoon
  ^-  plum
  ::  XX fill this in please
  ::
  ?:  ?=([%limb *] hoon)
    p.hoon
  %hooon
::
++  skin-to-plum
  |=  =skin
  ^-  plum
  %skinny
::
++  spec-to-plum
  |=  =spec
  ^-  plum
  ?-  -.spec
    %base  ?-  p.spec
             %noun  '*'
             %cell  '^'
             %flag  '?'
             %null  '~'
             %void  '!!'
             [%atom *]  (cat 3 '@' p.p.spec)
           ==
    %dbug  $(spec q.spec)
    %leaf  =+((scot p.spec q.spec) ?:(=('~' -) - (cat 3 '%' -)))
    %like  &/[[`[':' ~] ~] (turn `(list wing)`+.spec wing-to-plum)]
    %loop  (cat 3 '$' p.spec)
    %name  $(spec q.spec)
    %made  $(spec q.spec)
    %over  $(spec q.spec)
    %make  =+  (lent q.spec)
           :+  %&
             :-  `[' ' `['(' ')']]
             :-  ~
             ?:  |((gth - 3) =(- 0))
               ['%:' `['' '==']]
             :_  ~
             ?:  =(- 3)  '%^'
             ?:  =(- 2)  '%+'  '%-'
           [(hoon-to-plum p.spec) (turn q.spec ..$)]
    %bsbs  (core-to-plum '$$' p.spec q.spec)
    %bsbr  &/[(fixed '$|') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bscb  (hoon-to-plum p.spec)
    %bscl  :+  %&
             [`[' ' `['[' ']']] `['$:' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bscn  &/[(varying '$%' '==') (turn `(list ^spec)`+.spec ..$)]
    %bsdt  (core-to-plum '$.' p.spec q.spec)
    %bsld  &/[(fixed '$<') $(spec p.spec) $(spec q.spec) ~]
    %bsbn  &/[(fixed '$>') $(spec p.spec) $(spec q.spec) ~]
    %bshp  &/[(fixed '$-') $(spec p.spec) $(spec q.spec) ~]
    %bskt  &/[(fixed '$-') $(spec p.spec) $(spec q.spec) ~]
    %bsls  &/[(fixed '$+') (standard p.spec) $(spec q.spec) ~]
    %bsnt  (core-to-plum '$/' p.spec q.spec)
    %bsmc  &/[(fixed '$;') (hoon-to-plum p.spec) ~]
    %bspd  &/[(fixed '$&') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bssg  &/[(fixed '$~') (hoon-to-plum p.spec) $(spec q.spec) ~]
    %bstc  (core-to-plum '$`' p.spec q.spec)
    %bsts  :+  %&
             [`['=' ~] `['$=' ~]]
           :~  (skin-to-plum p.spec)
               $(spec q.spec)
           ==
    %bsvt  &/[(fixed '$@') $(spec p.spec) $(spec q.spec) ~]
    %bswt  :+  %&
              [`[' ' `['?(' ')']] `['$?' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bszp  (core-to-plum '$.' p.spec q.spec)
  ==
--
