::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
!:
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
::
::  *cosmetic-state: for cosmetic below
::
+*  cosmetic-state  [form]
  $:  ::  count: cumulative blocks detected
      ::  pairs: blocking numbers and specs
      ::
      count=@ud
      pairs=(map type (pair @ud form))
  ==
::
+*  analyst  [form]
  $_  ^?
  |%
  ++  atom  *$-([aura=term constant=(unit @)] form)
  ++  cell  *$-([=form =form] form)
  ++  core  *$-([=vair =form =(map term form)] form)
  ++  face  *$-([decor=$@(term tune) =form] form)
  ++  fork  *$-(=(list form) form)
  ++  loop  *$-([=form =(map @ud form)] form)
  --
::
::  $xray: initial type analysis
::

+$  scan
          $-  
          $@  $?  %noun
                  %void
              ==
          $%  [%bare =atom =aura]                       ::  constant
              [%bark =(map atom aura)]                  ::  constant set



+$  shoe
          $@  %void                                     ::  empty
          $%  [%bark =(map atom type)]                  ::  flat constants
              [%bush wide=shoe long=shoe]               ::  by cell/atom head
              [%leaf =type]                             ::  any atom
              [%root flat=shoe deep=shoe]               ::  by atom/cell
              [%tree =type]                             ::  any cell
              [%wood =(map atom (pair type type))]      ::  by atom head
          ==                                            ::
::
::  =merge: combine two shoes
::
++  merge-shoes
  |=  [one=shoe two=shoe]
  ^-  (unit shoe)
  ?@  one  `two
  ?@  two  `one
  ?-    -.one
      %bark  
    ?-    -.two
        %bark
      :+  ~  %bark
      ::  (both maps merged, combining collisions)
      ::
      =/  list  ~(tap by map.one)
      |-  ^-  (map atom type)
      ?~  list  map.two
      %=    $
          list  t.list
          map.two 
        %+  ~(put by map.two)
          p.i.list
        =+  (~(get by map.two) p.i.list)
        ?~  -  q.i.list
        (fork u.- q.i.list ~)
      ==
    ::
        %bush 
      `[%root one two]
    ::
        %leaf
      :-  ~
      :-  %leaf
      %+  fork 
        type.two 
      (turn ~(tap by map.one) |=([* =type] type))
    ::
        %root
      %+  bind  $(two flat.two)
      |=  new=shoe 
      [%root new deep.two]
    ::
        %tree
      `[%root one two]
    ::
        %wood
      `[%root one two]
    ==
  ::
      %bush
    ?+    -.two  $(one two, two one)
    ::
        %bush 
      %^    clef
          $(one wide.one, two wide.two)
        $(one long.one, two long.two)
      |=([wide=shoe long=shoe] `[%root wide long])
    ::
        %leaf
      `[%root two one]
    ::
        %root
      %+  bind  $(two deep.two)
      |=  new=shoe 
      [%root flat.two new]
    ::
        %tree
      ~
    ::
        %wood
      %+  bind  $(one long.one)
      |=  new=shoe
      [%bush wide.one new]
    ==
  ::
      %leaf
    ?+    -.two  $(one two, two one)
    ::
        %leaf
      `[%leaf (fork type.one type.two ~)]
    ::
        %root
      %+  bind  $(two flat.two)
      |=  new=shoe
      [%root new deep.two]
    ::
        %tree
      `[%root one two]
    ::
        %wood
      `[%root one two]
    ==
  ::
      %root
    ?+    -.two  $(one two, two one)
    ::
        %root
      %^    clef
          $(one flat.one, two flat.two)
        $(one deep.one, two deep.two)
      |=([flat=shoe deep=shoe] `[%root flat deep])
    ::
        %tree
      %+  bind  $(one deep.one)
      |=  new=shoe
      [%root one new]
    ::
        %wood
      %+  bind  $(one deep.one)
      |=  new=shoe
      [%root one new]
    ==
  ::
      %tree
    ?+    -.two  $(one two, two one)
    ::
        %tree
      `[%tree (fork type.one type.two ~)]
    ::
        %wood
      ~
    ==
  ::
      %wood
    ?+    -.two  $(one two, two one)
    ::
        %wood
      ::  (both maps merged, combining collisions)
      ::
      :-  ~
      :-  %wood
      =/  list  ~(tap by map.one)
      |-  ^-  (map atom (pair type type))
      ?~  list  map.two
      %=    $
          list  t.list
          map.two 
        %+  ~(put by map.two)
          p.i.list
        =+  (~(get by map.two) p.i.list)
        ?~  -  q.i.list
        [(fork p.u.- p.q.i.list ~) (fork q.u.- q.q.i.list ~)]
      ==
    ==
  == 
::
::  =shew: deconstruct type
::
++  shew
  |=  =type
  ::  (analysis of .type, if well-formed)
  ::
  ^-  (unit shoe)
  =-  ::  (analysis without trivial cases)
      ::
      %+  biff  -
      |=  =shoe 
      ?-  shoe
        %void      ~
        [%bark *]  ?:(?=([* ~ ~] map.shoe) ~ `shoe)
        [%bush *]  `shoe
        [%leaf *]  ~
        [%root *]  `shoe
        [%tree *]  ~
        [%wood *]  ?:(?=([* ~ ~] map.shoe) ~ `shoe)
      ==
  |^  =|  gil/(set ^type)
      |-  ^-  (unit shoe)
      ?-    type
          %void  `%void
          %noun  ~
      ::
          [%atom *]  as-done
          [%cell *]  ::  if the head of .type is always a cell
                     ::
                     ?:  is-cell(type p.type)
                       ::  (a trivial bush)
                       ::
                       `[%bush [%tree type] %void]
                     ::  unless the head of .type is an atom
                     ::
                     ?.  is-atom(type p.type)  
                       ::  (an opaque cell)
                       ::
                       `[%tree type]
                     %+  biff  to-atom(type p.type)
                     |=  =atom
                     ::  (a trivial wood)
                     ::
                     `[%wood [%atom p.type q.type] ~ ~]
          [%core *]  ~
          [%face *]  as-done
          [%fork *]  =/  list  ~(tap in p.type)
                     |-  ^-  (unit shoe)
                     ?~  list  `%void
                     %+  biff  ^$(type i.list)
                     |=  =shoe
                     %+  biff  ^$(list t.list)
                     |=  =^shoe
                     (merge-shoes shoe ^shoe)
      :: 
          [%hint *]  $(type q.type)
          [%hold *]  ?:  (~(has in gil) type)  `%void 
                     $(gil (~(put in gil) type), type ~(repo ut type))
      ==
  ::
  ++  is-atom  (~(nest ut [%atom %$ ~]) %| type)
  ++  is-cell  (~(nest ut [%cell %noun %noun]) %| type)
  ++  as-done  ?:  is-atom 
                 =+  to-atom 
                 ?~  - 
                   `[%leaf type] 
                 `[%bark [u.- type] ~ ~]
               ?:  is-cell 
                 `[%tree type]
               ~
  ++  to-atom  |-  ^-  (unit atom)
               ?-  type
                  %void  ~
                  %noun  !!
                  [%atom *]  q.type
                  [%cell *]  !!
                  [%core *]  !!
                  [%fork *]  =/  list  ~(tap in p.type)
                             ?~  list  !!
                             |-  ^-  (unit atom)
                             =+  ^$(type i.list)
                             ?~  t.list  -
                             %+  biff  -
                             |=  =atom
                             %+  biff  ^$(i.list i.t.list, t.list t.t.list)
                             |=  =^atom
                             ?.(=(atom ^atom) ~ `atom)
                  [%face *]  $(type q.type)
                  [%hint *]  $(type q.type)
                  [%hold *]  $(type ~(repo ut type))
               ==
  --
::
++  cosmetic
  %-  (kismet spec)
  |%
  ++  atom  
    |=  $:  ::  aura: flavor of atom
            ::  constant: one value, or all values
            ::
            aura=term 
            constant=(unit @)
        ==
    ^-  spec
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
  ++  cell  
    |=  $:  ::  head: head of cell
            ::  tail: tail of cell
            ::
            head=spec
            tail=spec
        ==
    ^-  spec
    ::  %bscl: raw tuple
    ::
    ?:  ?=(%bscl -.tail)
      [%bscl head +.tail]
    [%bscl head tail ~]
  ::      
  ++  core 
    |=  $:  ::  variance: co/contra/in/bi
            ::  payload: data 
            ::  battery: code
            ::
            variance=vair
            payload=spec
            battery=(map term spec)
        ==
    ?-  variance
      %lead  [%bszp payload battery]
      %gold  [%bsdt payload battery]
      %zinc  [%bstc payload battery]
      %iron  [%bsnt payload battery]
    ==
  ::
  ++  face  
    |=  $:  ::  decor: decoration 
            ::  content: decorated content
            ::
            decor=$@(term tune)
            body=spec
        ==
    ^-  spec
    ?@  decor  [%bsts decor body]
    ::  discard aliases, etc
    ::
    body
  ::
  ++  fork
    |=  specs=(list spec)
    ^-  spec
    ?<  ?=(~ specs)
    [%bswt specs]
  --
::
::  type-to-spec
::
++  kismet
  |*  form=mold
  |=  producer=(analyst form)
  =|  ::  coat: contextual metadata
      ::
      $=  coat
      $:  ::  trace: type analysis stack
          ::
          trace=(set type)
      ==
  =|  ::  load: accumulating metadata (state)
      ::
      load=(cosmetic-state form)
  ::
  ::  sut: type we're analyzing
  ::
  |_  sut/type
  ::
  ::  +structure: make cosmetic spec from :sut
  ::
  ++  structure
    ^-  spec
    ::  clear superficial structure and hints
    ::
    =.  sut  |-  ^-  type
             ?.  ?=([?(%hint %hold) *] sut)  sut
             $(sut ~(repo ut sut)) 
    ::
    ::  spec: raw analysis product
    ::
    =^  spec  load  specify
    ::  if we didn't block, just use it
    ::
    ?:  =(~ pairs.load)  spec
    ::  otherwise, insert hygienic recursion
    ::
    :+  %bsbs  spec
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
      ?.  ?=(%made -.note)  ~
      ?~  q.note
        `[`spec`[%like [[p.note ~] ~]] load]
      =-  `[[%make [%limb p.note] -<] ->]
      |-  ^-  [(list spec) _load]
      ?~  u.q.note  [~ load]
      =^  more  load  $(u.q.note t.u.q.note)
      =/  part  (~(play ut type) [%tsld [%limb %$] [%wing i.u.q.note]])
      =^  spec  load  entry(sut part)
      [[spec more] load]
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
      [(atom:producer aura constant) load] 
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
      [(cell:producer head tail) load]
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
        %+  turn  ~(tap by q.r.battery)
        |=  [term =tome]
        ~(tap by q.tome)
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
      [(core:producer r.p.battery payload-spec arm-map) load]
    ::
    ::  +face: convert a %face to a +spec
    ::
    ++  face
      |=  $:  ::  decor: decoration 
              ::  content: decorated content
              ::
              decor=$@(term tune)
              content=type
          ==
      ^-  [spec _load]
      =^  body  load  main(sut content)
      [(face:producer decor body) load]
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
      [(fork:producer specs) load]
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
::  highly unsatisfactory temporary tank printer
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
