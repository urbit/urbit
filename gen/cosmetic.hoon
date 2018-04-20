::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  *
:-  %noun
=<  "hello, world"
|%
++  plume
  |_  =plum
  ::
  ::  +flat: print as a single line
  ::
  ++  flat
    text:linear
  ::
  ::  +tall: print as multiple lines
  ::
  ++  tall
    ^-  wain
    %+  turn  window
    |=  [indent=@ud text=tape]
    (crip (runt [indent ' '] text))
  ::  
  ::  +window: print as list of tabbed lines
  ::
  ++  window
    ^-  (list [indent=@ud text=tape])
    ::  memoize for random access
    ::
    ~+  
    ::  trivial text
    ::
    ?@  plum  [0 (trip plum)]~
    ?-  -.plum
      ::  %|: text wrap
      ::
      %|  ::  wrapping stub, should wrap text to 40 characters
          ::
          [0 +:linear]~
      ::
      ::  %&: text tree
      ::
      %&  ::  trial: attempt at wide form
          ::
          =/  trial  ?~(wide.plum ~ `linear)
          ::  if wide form is available optimal
          ::
          ?:  ?&  ?=(^ trial)
                  ?|  ?=(~ tall.plum)
                      (lte length.line 40)
              ==  ==
            ::  then produce wide form
            ::
            [0 text.trial]
          ::  else assert tall style (you gotta set either wide or tall)
          ::
          ?>  ?=(^ tall.plum)
          ::  family:  subwindows
          ::  prelude: intro as tape
          ::
          =/  family   (turn list.plum |=(plum window(plum plum)))
          =/  prelude  (trip intro.u.tall.plum)
          =/  prelen   (lent prelude)
          ::  if :indef is empty
          ::
          ?~  indef.u.tall.plum
            ::  then print in sloping mode
            ::
            !!
          ::  else print in vertical mode
          :: 
          ::  prefix: before each entry
          ::  finale: after all entries
          ::
          =/  prefix  (trip sigil.u.indef.u.tall.plum)
          =/  finale  (trip final.u.indef.u.tall.plum)
          ::  if no children, then just prelude and finale
          ::
          ?:  =(~ family)
            %+  weld
              ?~(prelude ~ [0 prelude]~)
            ?~(finale ~ [0 finale]~)
          ::  if no :prefix
          ::
          ?:  =(~ prefix)
            ::  kids: flat list of child lines
            ::  tab:  amount to indent kids
            ::
            =*  tab   ?+(prelen 2 %0 0, %1 2, %2 4)
            =/  kids  (zing family)
            ::  indent kids by tab
            ::
            =.  kids  (turn kids |=([@ud tape] [(add tab +<-) +<+]))
            ::  prepend or inject prelude
            ::
            =.  kids  
              ?:  =(~ prelude)  kids
              ::  if no kids, or prelude doesn't fit
              ::
              ?:  |(?=(~ kids) (gte +(prelen) indent.i.kids))
                ::  don't inject, just add to head if needed
                ::
                [[0 prelude] kids]
              ::  inject: prelude 
              ::
              =*  inject  %+  weld
                            prelude
                          %+  runt 
                            [(sub indent.i.kids prelen) ' ']
                          text.i.kids
              [[0 inject] t.kids]
            ::  append finale
            ::
            ?~  finale  kids
            (weld kids [0 finale]~)
          ::  else, with :prefix
          ::  tab: amount to indent 
          ::
          =*  tab  (add 2 prelen)
          ::  append :finale 
          ::
          =-  ?~  finale  -
              (weld - [0 finale]~)
          %-  zing
          ::  combine each subtree with the prefix
          ::
          %+  turn  family
          |=  =(list [indent=@ud text=tape])
          ^+  +<
          =.  list  (turn list |=([@ud tape] [(add tab +<-) +<+]))
          :_  t.list
          :-  0
          %+  weld  
            prefix
          (runt [(sub length.i.list (lent prefix)) ' '] text.i.list)
    ==
    ::
    ::  
    ::
    ::  if wide form is undesirable or too long, don't even try
    ::
    ?:  ?|  ?=(~ wide.plum)
        ==
  ::
  ::  +linear: make length and tape
  ::
  ++  linear
    ^-  $:  length=@ud
            text=tape
        ==
    ::  memoize for random access
    ::
    ~+  
    ::  atomic plums are just text
    ::
    ?@  plum  [(met 3 plum) (trip plum)]
    ?-  -.plum
      ::  %|: text wrap
      ::
      %|  ::  lay the text out flat, regardless of length
          ::
          |-  ^-  [length=@ud text=tape]
          ?~  list.plum  [0 ~]
          =/  next  $(list.plum t.list.plum)
          =/  this  [length=(met 3 i.list.plum) text=(trip i.list.plum)]
          :-  (add +(length.this) length.next)
          (weld text.this `tape`[' ' text.next])
      ::
      ::  %&: text tree
      ::
      %&  ::  if there is no wide representation
          ::
          ?~  wide.plum
            ::  then lay out a window, then separate with double-spaces
            ::
            =/  window  window           
            |-  ^-  [length=@ud text=tape]
            ?~  window  [0 ~]
            =/  next  $(window t.window)
            :-  :(add (lent text.i.window) 2 length.next)
            :(weld text.i.window "  " text.next)
          ::
          ::  else use wide layout
          ::
          =-  ::  add enclosure if any
              ::
              ?~  enclose.u.wide.plum -
              =*  clamps  u.enclose.u.wide.plum
              =/  close  [(trip -.clamps) (trip +.clamps)]
              :-  (add length.0 (lent -.close) (lent +.close))
              :(weld -.close text.- +.close) 
          ::  compose body
          ::
          =/  stop  (trip delimit.u.wide.plum)
          |-  ^-  [length=@ud text=tape]
          ?~  list.plum  [0 ~]
          =/  next  $(list.plum t.list.plum)
          =/  this  ^$(plum i.list.plum) 
          :-  :(add length.this (lent stop) length.next)
          :(weld text.this stop text.next)
    ==
  --
::
++  plum-to-tank
  |=  =plum
  ^-  tank
  ?@  plum  [%leaf (trip plum)]
  ?-  -.plum
    %|  :+  %rose
          ["" " " ""]
        (turn list.plum |=(@ta [%leaf (trip +<)]))
    %&  =/  list  (turn list.plum ..$)
        ?~  tall.plum
          ?>  ?=(^ wide.plum)
          =?  enclose.u.wide.plum  ?=(~ enclose.u.wide.plum)  `['{' '}']
          :+  %rose
            :*  (trip +<:enclose.u.wide.plum)
                (trip delimit.u.wide.plum)
                (trip +>:enclose.u.wide.plum)
            ==
          list
        ?:  ?=(^ indef.u.tall.plum)
          :+  %rose
            :*  (weld (trip intro.u.tall.plum) "<")
                (weld (trip sigil.u.indef.u.tall.plum) " ")
                (weld "(" (trip final.u.indef.u.tall.plum))
            ==
          list
        :+  %palm
          :*  (weld (trip intro.u.tall.plum) "(")
              " "
              " "
              ")"
          ==
        list
  ==
++  limb-to-plum
  |=  =limb
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
    [`[' ' ~] `['' ~]]
  [term (spec-to-plum spec) ~]
::
++  core-to-plum
  |=  [=knot =spec =(map term spec)]
  ^-  plum
  :+  %&
    [~ `[knot ~]]
  :~  (spec-to-plum spec)
      :+  %&
        [~ `['' `['++' '==']]]
      (battery-to-plum map)
  ==
::
++  regular
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
  %hooon
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
    %leaf  (cat 3 '%' (scot p.spec q.spec))
    %like  &/[[`[':' ~] ~] (turn `(list wing)`+.spec wing-to-plum)]
    %loop  (cat 3 '$' p.spec)
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
    %bcbc  (core-to-plum '$$' p.spec q.spec)
    %bcbr  &/[(regular '$|') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bccb  (hoon-to-plum p.spec)
    %bccl  :+  %&
             [`[' ' `['[' ']']] `['$:' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bccn  &/[(regular '$%') (turn `(list ^spec)`+.spec ..$)]
    %bcdt  (core-to-plum '$.' p.spec q.spec)
    %bcgl  &/[(regular '$<') $(spec p.spec) $(spec q.spec) ~]
    %bcgr  &/[(regular '$>') $(spec p.spec) $(spec q.spec) ~]
    %bchp  &/[(regular '$-') $(spec p.spec) $(spec q.spec) ~]
    %bckt  &/[(regular '$-') $(spec p.spec) $(spec q.spec) ~]
    %bcls  &/[(regular '$+') (standard p.spec) $(spec q.spec) ~]
    %bcnt  (core-to-plum '$/' p.spec q.spec)
    %bcmc  &/[(regular '$;') (hoon-to-plum p.spec) ~]
    %bcpd  &/[(regular '$&') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bcsg  &/[(regular '$~') (hoon-to-plum p.spec) $(spec q.spec) ~]
    %bctc  (core-to-plum '$`' p.spec q.spec)
    %bcts  :+  %&
             [`['=' ~] `['$=' ~]]
           :~  ?@(p.spec p.spec q.p.spec)
               $(spec q.spec)
           ==
    %bcvt  &/[(regular '$@') $(spec p.spec) $(spec q.spec) ~]
    %bcwt  &/[(regular '$?') (turn `(list ^spec)`+.spec ..$)]
    %bczp  (core-to-plum '$.' p.spec q.spec)
  ==
--

