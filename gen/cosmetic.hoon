::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  {^ {{subject=type ~} ~}}
:-  %txt
^-  wain
=<  =/  spec=spec  ~(structure cosmetic subject)
    =/  plum=plum  (spec-to-plum spec)
    ~(tall plume plum)    
|%
++  cosmetic
  =|  ::  coat: contextual metadata
      ::
      $=  coat
      $:  ::  trace: type analysis stack
          ::
          trace=(set type)
      ==
  =|  ::  load: accumulating metadata (state)
      ::
      $=  load
      $:  ::  count: cumulative blocks detected
          ::  pairs: blocking numbers and specs
          ::
          count=@ud
          pairs=(map type (pair @ud spec))
      ==
  ::
  ::  sut: type we're analyzing
  ::
  |_  sut/type
  ::
  ::  +structure: make cosmetic spec from :sut
  ::
  ++  structure
    ^-  spec
    ::  spec: raw analysis product
    ::
    =^  spec  load  specify
    ::  if we didn't block, just use it
    ::
    ?:  =(~ pairs.load)  spec
    ::  otherwise, insert hygienic recursion
    ::
    :+  %bcbc  spec
    %-  ~(gas by *(map term ^spec))
    %+  turn
      ~(tap by pairs.load)
    |=  [=type index=@ud spec=^spec]
    [(synthetic index) spec]
  ::
  ::  +pattern: pattern and context for data inspection
  ::
  ++  pattern
    ^-  $:  ::  main: rendering pattern 
            ::  context: recursion points by counter
            ::
            main=plot
            loop=(map @ud plot)
        ==
    !!
  ::
  ::  +synthetic: convert :number to a synthetic name
  ::
  ++  synthetic
    |=  number=@ud
    ^-  @tas
    =/  alf/(list term)
        ^~  :~  %alf  %bet  %gim  %dal  %hej  %vav  %zay  %het
                %tet  %yod  %kaf  %lam  %mem  %nun  %sam  %ayn
                %pej  %sad  %qof  %res  %sin  %tav
            ==
    ?:  (lth number 22)
      (snag number alf)
    (cat 3 (snag (mod number 22) alf) $(number (div number 22)))
  ::
  ::  +specify: make spec that matches :sut
  ::
  ++  specify
    ^-  [spec _load]
    =<  entry
    |%
    ::  +entry: make spec at potential entry point
    ::
    ++  entry
      ^-  [spec _load]
      ::  old: old recursion binding for :sut
      ::
      =/  old  (~(get by pairs.load) sut)
      ::  if, already bound, reuse binding
      ::
      ?^  old  [[%loop (synthetic p.u.old)] load]
      ::  if, we are already inside :sut
      ::
      ?:  (~(has in trace.coat) sut)
        ::  then, produce and record a block reference
        ::
        =+  [%loop (synthetic count.load)]
        :-  -
        %_  load
          count  +(count.load)            
          pairs  (~(put by pairs.load) sut [count.load -])
        ==
      ::  else, filter main loop for block promotion
      ::
      =^  spec  load  main(trace.coat (~(put in trace.coat) sut))
      ::  check if we re-entered :sut while traversing
      ::
      =/  new  (~(get by pairs.load) sut)
      ::  if, we did not find :sut inside itself
      ::
      ?~  new
        ::  then, :sut is not a true entry point
        ::
        [spec load]
      ::  else, produce a reference and record the analysis
      ::
      :-  [%loop (synthetic p.u.new)] 
      load(pairs (~(put by pairs.load) sut [p.u.new spec]))
    ::
    ::  +meta: try to make spec from type of filter
    ::
    ::++  meta ^-  [(unit spec) _load]
    ::
    ::  +main: make spec from any type
    ::
    ++  main
      ^-  [spec _load]
      ?-  sut
        %void      :_(load [%base %void])
        %noun      :_(load [%base %noun])
      ::
        [%atom *]  (atom p.sut q.sut)
        [%cell *]  (cell p.sut q.sut)
        [%core *]  (core p.sut q.sut)
        [%face *]  (face p.sut q.sut)
        [%hint *]  =+((rely p.p.sut q.p.sut) ?^(- u.- main(sut q.sut)))
        [%fork *]  (fork p.sut)
        [%hold *]  entry(sut ~(repo ut sut))
      == 
    ::
    ::  +rely: rationalize structure from type (stub)
    ::
    ++  rely
      |=  [=type =note]
      ^-  (unit [spec _load])
      ?+  -.note  ~
        $army  `[`spec`[%like [[p.note ~] ~]] load]
        $navy  =-  `[[%make [%limb p.note] -<] ->]
               |-  ^-  [(list spec) _load]
               ?~  q.note  [~ load]
               =^  more  load  $(q.note t.q.note)
               =/  part  (~(play ut type) [%tsgl [%limb %$] [%wing i.q.note]])
               =^  spec  load  entry(sut part)
               [[spec more] load]
      ==
    ::
    ::  +atom: convert atomic type to spec
    ::
    ++  atom
      |=  $:  ::  aura: flavor of atom
              ::  constant: one value, or all values
              ::
              aura=term 
              constant=(unit @)
          ==
      ::  pure function
      ::
      :_  load  ^-  spec
      ::  if atom is not constant
      ::
      ?~  constant
        ::  %base: flavored atom with arbitrary value
        ::
        [%base atom/aura]
      ::  %leaf: flavored constant
      ::
      [%leaf aura u.constant]
    ::
    ::  +cell: convert a %cell to a spec
    ::
    ++  cell
      |=  $:  ::  left: head of cell
              ::  rite: tail of cell
              ::
              left=type
              rite=type
          ==
      ^-  [spec _load]
      ::  head: cosmetic structure of head
      ::  tail: cosmetic structure of tail
      ::
      =^  head  load  main(sut left)
      =^  tail  load  main(sut rite)
      :_  load
      ::  %bccl: raw tuple
      ::
      ?:  ?=(%bccl -.tail)
        [%bccl head +.tail]
      [%bccl head tail ~]
    ::
    ::  +core: convert a %core to a spec
    ::
    ++  core
      |=  $:  ::  payload: data 
              ::  battery: code
              ::
              payload=type
              battery=coil
          ==
      ^-  [spec _load]
      ::  payload-spec: converted payload
      ::
      =^  payload-spec  load  main(sut payload)
      ::  arms: all arms in the core, as hoons
      ::
      =/  arms
        ^-  (list (pair term hoon))
        %-  zing
        ^-  (list (list (pair term hoon)))
        %+  turn  ~(tap by q.s.battery)
        |=  [term =tomb]
        ^-  (list (pair term hoon))
        %+  turn  ~(tap by q.tomb)
        |=  [=term =what =foot]
        ^-  (pair @tas hoon)
        [term p.foot]
      ::  arm-specs: all arms in the core, as specs
      ::
      =^  arm-specs  load
        |-  ^-  [(list (pair term spec)) _load]
        ?~  arms  [~ load]
        =^  mor  load  $(arms t.arms)
        =^  les  load
          main(sut [%hold [%core payload battery] q.i.arms])
        [[[p.i.arms les] mor] load]
      ::  arm-map: all arms in the core, as a a spec map
      ::
      =*  arm-map  (~(gas by *(map term spec)) arm-specs)
      :_  load
      ?-  p.battery
        %lead  [%bczp payload-spec arm-map]
        %gold  [%bcdt payload-spec arm-map]
        %zinc  [%bctc payload-spec arm-map]
        %iron  [%bcnt payload-spec arm-map]
      ==
    ::
    ::  +face: convert a %face to a +spec
    ::
    ++  face
      |=  $:  ::  decor: decoration 
              ::  content: decorated content
              ::
              decor=(pair what $@(term tune))
              content=type
          ==
      ^-  [spec _load]
      =^  body  load  main(sut content)
      :_  load
      ?@  q.decor  [%bcts q.decor body]
      ::  discard aliases, etc
      ::
      body
    ::
    ::  +fork: convert a %fork to a +spec
    ::
    ++  fork
      |=  types=(set type)
      ^-  [spec _load]
      ::  type-list: type set as a list
      ::
      =/  type-list  ~(tap by types)
      ::  specs: type set as a list of specs
      ::
      =^  specs  load
        |-  ^-  [(list spec) _load]
        ?~  type-list  [~ load]
        =^  mor  load  $(type-list t.type-list)
        =^  les  load  main(sut i.type-list)
        [[les mor] load]
      ?<  ?=(~ specs)
      :_(load [%bcwt specs])
    --
  ::
  ::  +explore:cosmetic: convert :sut to an inspection pattern (+plot).
  ::
  ++  explore
    ^-  [plot _.]
    =<  [- +>] 
    |^  ^-  [plot _.]
        ?+  sut  !!
          %void  :_(. [%base %void])
          %noun  :_(. [%base %noun])
        ==
    ++  foo  !!
    --
  --
::
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
  ::  +adjust: adjust lines to right
  ::
  ++  adjust
    |=  [tab=@ud =(list [length=@ud text=tape])]
    (turn list |=([@ud tape] [(add tab +<-) +<+]))
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
      %&  ::  trial: attempt at wide hint
          ::
          =/  trial  ?~(wide.plum ~ [~ u=linear])
          ::  if wide hint is available or optimal
          ::
          ?:  ?&  ?=(^ trial)
                  ?|  ?=(~ tall.plum)
                      (lte length.u.trial 40)
              ==  ==
            ::  then produce wide hint
            ::
            [0 text.u.trial]~
          ::  else assert tall style (you gotta set either wide or tall)
          ::
          ?>  ?=(^ tall.plum)
          ::  blocks:  subwindows
          ::  prelude: intro as tape
          ::
          =/  blocks   (turn list.plum |=(=^plum window(plum plum)))
          =/  prelude  (trip intro.u.tall.plum)
          ::  if, :indef is empty
          ::
          ?~  indef.u.tall.plum
            ::  then, print in sloping mode
            ::
            ::  if, no children
            ::
            ?:  =(~ blocks)
              ::  then, the prelude if any
              ::
              ?~(prelude ~ [0 prelude]~)
            ::  else, format children and inject any prelude
            ::
            ^-  (list [indent=@ud text=tape])
            ::  concatenate child blocks into a single output
            ::
            %-  zing
            ::  count: number of children
            ::  index: current child from 1 to n
            ::
            =/  count  (lent blocks)
            =/  index  1
            |-  ^+  blocks
            ?~  blocks  ~
            :_  $(blocks t.blocks, index +(index))
            ^-  (list [indent=@ud text=tape])
            ::  indent: backstep indentation level
            ::
            =/  indent  (mul 2 (sub count index))
            ::  unless, we're on the first block
            ::
            ?.  =(1 index)
              ::  else, apply normal backstep indentation
              ::
              (adjust indent i.blocks)
            ::  then, apply and/or inject prelude
            ::
            ::    this fixes the naive representations
            ::
            ::      :+  
            ::          foo
            ::        bar
            ::      baz
            ::
            ::    and
            ::
            ::      :-
            ::        foo
            ::      bar
            ::
            =.  indent  (max indent (add 2 (lent prelude)))
            =.  i.blocks  (adjust indent i.blocks)
            ?~  i.blocks  ?~(prelude ~ [0 prelude]~)
            ?~  prelude   i.blocks
            :_  t.i.blocks
            :-  0
            ~|  [%indent indent]
            ~|  [%prelude prelude]
            ~|  [%kids list.plum]
            ~|  [%blocks blocks]
            %+  weld  prelude
            (runt [(sub indent.i.i.blocks (lent prelude)) ' '] text.i.i.blocks)
          ::
          ::  else, print in vertical mode
          :: 
          ::  prefix: before each entry
          ::  finale: after all entries
          ::
          =/  prefix  (trip sigil.u.indef.u.tall.plum)
          =/  finale  (trip final.u.indef.u.tall.plum)
          ::  if, no children, then, just prelude and finale
          ::
          ?:  =(~ blocks)
            %+  weld
              ?~(prelude ~ [0 prelude]~)
            ?~(finale ~ [0 finale]~)
          ::  if, no :prefix
          ::
          ?:  =(~ prefix)
            ::  kids: flat list of child lines
            ::  tab:  amount to indent kids
            ::
            =/  kids  `(list [indent=@ud text=tape])`(zing blocks)
            =*  tab   =+((lent prelude) ?+(- 2 %0 0, %1 2, %2 4))
            ::  indent kids by tab
            ::
            =.  kids  (turn kids |=([@ud tape] [(add tab +<-) +<+]))
            ::  prepend or inject prelude
            ::
            =.  kids  
              ?:  =(~ prelude)  kids
              ::  if, no kids, or prelude doesn't fit
              ::
              ?:  |(?=(~ kids) (gte +((lent prelude)) indent.i.kids))
                ::  don't inject, just add to head if needed
                ::
                [[0 prelude] kids]
              ::  inject: prelude 
              ::
              =*  inject  %+  weld
                            prelude
                          %+  runt 
                            [(sub indent.i.kids (lent prelude)) ' ']
                          text.i.kids
              [[0 inject] t.kids]
            ::  append finale
            ::
            ?~  finale  kids
            (weld kids ^+(kids [0 finale]~))
          ::  else, with :prefix
          ::
          ::  append :finale 
          ::
          =-  ?~  finale  -
              (weld - ^+(- [0 finale]~))
          ^-  (list [indent=@ud text=tape])
          ::  clear: clearance needed to miss prefix
          ::
          =/  clear  (add 2 (lent prefix))
          %-  zing
          ::  combine each subtree with the prefix
          ::
          %+  turn  blocks
          |=  =(list [indent=@ud text=tape])
          ^+  +<
          ::  tab: depth to indent
          ::
          =*  tab  ?~(list 0 (sub clear (min clear indent.i.list)))
          =.  list  (turn list |=([@ud tape] [(add tab +<-) +<+]))
          ?~  list  ~
          :_  t.list
          :-  0
          %+  weld  
            prefix
          (runt [(sub indent.i.list (lent prefix)) ' '] text.i.list)
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
            ?~(text.next text.i.window :(weld text.i.window "  " text.next))
          ::
          ::  else use wide layout
          ::
          =-  ::  add enclosure if any
              ::
              ?~  enclose.u.wide.plum  body
              =*  clamps  u.enclose.u.wide.plum
              =/  close  [(trip -.clamps) (trip +.clamps)]
              :-  :(add length.body (lent -.close) (lent +.close))
              :(weld -.close text.body +.close) 
          ::
          ::  body: body of wide rendering 
          :: 
          ^=  body
          =/  stop  (trip delimit.u.wide.plum)
          |-  ^-  [length=@ud text=tape]
          ?~  list.plum  [0 ~]
          =/  next  $(list.plum t.list.plum)
          =/  this  linear(plum i.list.plum) 
          ?~  text.next  this
          :-  :(add length.this (lent stop) length.next)
          :(weld text.this stop text.next)
    ==
  --
::  highly unsatisfactory temporary converter
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
            :*  (trip delimit.u.wide.plum)
                (trip +<:enclose.u.wide.plum)
                (trip +>:enclose.u.wide.plum)
            ==
          list
        ?:  ?=(^ indef.u.tall.plum)
          :+  %rose
            :*  (trip sigil.u.indef.u.tall.plum)
                (weld (trip intro.u.tall.plum) "[")
                (weld "]" (trip final.u.indef.u.tall.plum))
            ==
          list
        :+  %palm
          :*  (weld (trip intro.u.tall.plum) "(")
              ""
              ""
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
    %bcbc  (core-to-plum '$$' p.spec q.spec)
    %bcbr  &/[(fixed '$|') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bccb  (hoon-to-plum p.spec)
    %bccl  :+  %&
             [`[' ' `['[' ']']] `['$:' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bccn  &/[(varying '$%' '==') (turn `(list ^spec)`+.spec ..$)]
    %bcdt  (core-to-plum '$.' p.spec q.spec)
    %bcgl  &/[(fixed '$<') $(spec p.spec) $(spec q.spec) ~]
    %bcgr  &/[(fixed '$>') $(spec p.spec) $(spec q.spec) ~]
    %bchp  &/[(fixed '$-') $(spec p.spec) $(spec q.spec) ~]
    %bckt  &/[(fixed '$-') $(spec p.spec) $(spec q.spec) ~]
    %bcls  &/[(fixed '$+') (standard p.spec) $(spec q.spec) ~]
    %bcnt  (core-to-plum '$/' p.spec q.spec)
    %bcmc  &/[(fixed '$;') (hoon-to-plum p.spec) ~]
    %bcpd  &/[(fixed '$&') $(spec p.spec) (hoon-to-plum q.spec) ~]
    %bcsg  &/[(fixed '$~') (hoon-to-plum p.spec) $(spec q.spec) ~]
    %bctc  (core-to-plum '$`' p.spec q.spec)
    %bcts  :+  %&
             [`['=' ~] `['$=' ~]]
           :~  ?@(p.spec p.spec q.p.spec)
               $(spec q.spec)
           ==
    %bcvt  &/[(fixed '$@') $(spec p.spec) $(spec q.spec) ~]
    %bcwt  :+  %&
              [`[' ' `['?(' ')']] `['$?' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bczp  (core-to-plum '$.' p.spec q.spec)
  ==
--
