:-  %say
|=  *
:-  %noun
=-  "hello, world"
|%
+$  scan  
  $:  ::  xray: type analysis record
      ::  loop-map: loop dictionary
      ::
      =xray
      =loop=(map =@ud =xray) 
  ==
+$  xray
  $~  %void
  $@  $?  ::  %noun: any noun
          ::  %void: no nouns
          ::
          %noun
          %void
      ==
  $%  ::  %bark: atom selection
      ::
      [%bark =(map atom aura)]
      ::  %bush: head-of-cell superposition
      ::
      [%bush wide=xray tall=xray]
      ::  %cell: ordered pair
      ::
      [%cell head=xray tail=xray]
      ::  %core: functional attribute battery
      ::
      [%core =vair =xray =(map term (map term xray))]
      ::  %face: namespace
      ::
      [%face =term =xray]
      ::  %fork: disordered superposition
      ::
      [%fork =(set xray)]
      ::  %hint: context-preserving type hint
      ::
      [%hint [=type =note] =xray]
      ::  %knot: recursion root
      ::
      [%knot =(set @ud) =xray]
      ::  %loop: recursion point
      ::
      [%loop index=@ud]
      ::  %rock: constant
      ::
      [%rock =atom =aura]
      ::  %root: atom/cell superposition
      ::
      [%root flat=xray deep=xray]
      ::  %sand: variable atom
      ::
      [%sand =aura]
      ::  %wood: tagged superposition
      ::
      [%wood =(map atom (pair aura xray))]
  ==
::
::  =untangle: convert $type to $scan
::
++  untangle
  |=  =type
  =|  $:  ::  trace: set of holds that current analysis is within
          ::
          trace=(set ^type) 
          ::  state: accumulated state
          ::
          $=  state
          $:  ::  count: cumulative loops detected
              ::  table: loop number and analysis
              ::
              count=@ud
              table=(map ^type (pair @ud xray))
      ==  ==
  |^  ::  (full analysis of .type)
      ::
      ^-  scan
      =^  xray  state  specify
      :-  xray
      ::  (loop dictionary, from loop table)
      ::
      %-  ~(gas by *(map @ud ^xray)) 
      %+  turn  
        ~(tap by table.state)
      |=  [* index=@ud =^xray] 
      [index xray]
  ::
  ::  -specify: analyze type, producing analysis and raw state
  :: 
  ++  specify
    |-  ^-  [xray _state]
    =<  entry
    |%
    ::  -entry: analyze at possible entry point
    ::
    ++  entry
      ::  ($xray of .type; updated .state)
      ::
      |-  ^-  [xray _state]
      ::  old: existing entry for .type in .table.state
      ::
      =/  old  (~(get by table.state) type)
      ::  if, old entry is found in loop table, reuse loop number
      ::
      ?^  old  [[%loop p.u.old] state]
      ::  if, .type is already on our stack .trace
      ::
      ?:  (~(has in trace) type)
        ::  then, build a %loop $xray, and put it in the loop table as a stub
        ::
        =+  [%loop count.state]
        :-  -
        %=  state
          count  ::  (count.state incremented to the next unused index)
                 ::
                 +(count.state)
          table  ::  (table.state 
                 ::
                 (~(put by table.state) type [count.state -])
        ==
      ::  else, apply main analysis loop
      ::
      =^  =xray  state  main(trace (~(put in trace) type))
      ::  new: any xray we added to .table.state for .type
      ::
      =/  new  (~(get by table.state) type)
      ::  if, .new is empty, then .type is not an entry point
      ::
      ?~  new  [xray state]
      ::  else, build a loop declaration and corrected table state
      ::
      :-  [%knot [p.u.new ~ ~] xray]
      state(table (~(put by table.state) type [p.u.new xray]))
    ::
    ::  -main: main analysis without entry control
    ::
    ++  main
      |^  ^-  [xray _state]
          ?-  type
            %void      [%void state]
            %noun      [%noun state]
          ::
            [%atom *]  (atom p.type q.type)
            [%cell *]  (cell p.type q.type)
            [%core *]  (core p.type q.type)
            [%face *]  (face p.type q.type)
            [%hint *]  (hint p.type q.type)
            [%fork *]  (fork p.type)
            [%hold *]  entry(type ~(repo ut type))
          == 
      ::  =atom: convert an %atom $type to an $xray
      ::
      ++  atom
        |=  $:  ::  aura: flavor of atom
                ::  constant: one value, or all values
                ::
                =aura
                constant=(unit @)
            ==
        ^-  [xray _state]
        :_  state
        ?~  constant
          [%sand aura]
        [%rock u.constant aura]
      ::
      ::  =cell: convert a %cell $type to an $xray
      ::
      ++  cell
        |=  $:  ::  left: head of cell
                ::  rite: tail of cell
                ::
                left=^type
                rite=^type
            ==
        ^-  [xray _state]
        ::  head: analysis of head
        ::  tail: analysis of tail
        ::
        =^  head  state  main(type left)
        =^  tail  state  main(type rite)
        [[%cell head tail] state]
      ::
      ::  =core: convert a %core $type to an $xray
      ::
      ++  core
        |=  $:  ::  payload-type: type of payload data
                ::  coil: battery source
                ::
                =payload=^type
                =coil
            ==
        ^-  [xray _state]
        ::  payload-xray: analyzed payload
        ::
        =^  payload-xray  state  main(type payload-type)
        ::  chapters: analyzed chapters 
        ::
        =^  chapters=(list (pair term (map term xray)))  state
          =/  chapters=(list (pair term tome))  ~(tap by q.r.coil)
          |-  ^-  [(list (pair term (map term xray))) _state]
          ?~  chapters  [~ state]
          =^  more-chapters  state  $(chapters t.chapters)
          =^  this-chapter  state
            ^-  [(pair term (map term xray)) _state] 
            =/  arms=(list (pair term hoon))  ~(tap by q.q.i.chapters)
            =-  :_(-> [p.i.chapters (~(gas by *(map term xray)) -<)])
            |-  ^-  [(list (pair term xray)) _state]
            ?~  arms  [~ state]
            =^  more-arms  state  $(arms t.arms)
            =^  this-arm  state  
              main(type [%hold [%core payload-type coil] q.i.arms])
            [[[p.i.arms this-arm] more-arms] state]
          [[this-chapter more-chapters] state]
        :_  state
        ^-  xray
        :^    %core
            r.p.coil
          payload-xray
        (~(gas by *(map term (map term xray))) chapters)
      ::
      ::  =face: convert a %face $type to an $xray
      ::
      ++  face
        |=  $:  ::  decor: decoration 
                ::  content-type: decorated content
                ::
                decor=$@(term tune)
                =content=^type
            ==
        ^-  [xray _state]
        ?^  decor  main(type content-type)
        =^  =xray  state  main(type content-type)
        [[%face decor xray] state]
      ::
      ::  =hint: convert a %hint $type to an $xray
      ::
      ++  hint
        |=  $:  ::  subject-type: subject of note
                ::  note: hint information
                ::  content-type: type of hinted content
                ::
                [=subject=^type =note]
                =content=^type
            ==
        ^-  [xray _state]
        =^  =xray  state  main(type content-type)
        [[%hint [subject-type note] xray] state]
      ::
      ::  +fork: convert a %fork $type to an $xray
      ::
      ++  fork
        |=  ::  set: set of union types
            ::
            =(set ^type)
        ^-  [xray _state]
        =/  list  ~(tap in set)
        =-  :_(-> [%fork (~(gas in *(^set xray)) -<)])
        |-  ^-  [(^list xray) _state]
        ?~  list  [~ state]
        =^  this-xray  state  main(type i.list)
        =^  more-xrays  state  $(list t.list)
        [[this-xray more-xrays] state]
      --
    --
  --
::
::  =realign: analyze superpositions 
::
++  realign
  |=  scan
  ^-  ^xray
  |^  ?@  xray  xray
      ?-  -.xray
        %bark  xray
        %bush  [%bush $(xray wide.xray) $(xray tall.xray)]
        %cell  [%cell $(xray head.xray) $(xray tail.xray)]
        %core  :^    %core 
                   vair.xray
                 xray.xray
               %-  ~(run by map.xray) 
               |=  (map term ^xray) 
               (~(run by +<) |=(^xray ^^$(xray +<)))
        %face  [%face term.xray xray.xray]
        %fork  =/  list  ~(tap in set.xray)
               |-  ^-  ^xray
               ?~  list  %void
               (merge(xray i.list) `^xray`$(list t.list))
        %hint  [%hint [type.xray note.xray] $(xray xray.xray)]
        %knot  [%knot set.xray $(xray xray.xray)]
        %loop  xray
        %rock  xray
        %root  [%root $(xray flat.xray) $(xray deep.xray)]
        %sand  xray
        %wood  :-  %wood
               %-  ~(run by map.xray)
               |=([aura ^xray] [+<- ^$(xray +<+)])
      ==
  ::
  ++  aura-merge
    |=  [=aura =aura]
    ?:  =(aura ^aura)  aura
    =/  byte  0
    |-  ^-  ^^aura
    ?:  (lte (met 3 aura) byte)  aura
    ?:  (lte (met 3 ^aura) byte)  ^aura
    ?:  !=((cut 3 [byte 1] aura) (cut 3 [byte 1] ^aura))
      (end 3 byte aura)
    $(byte +(byte))
  ::
  ++  bark-merge
    |=  [=one=(map atom aura) =two=(map atom aura)]
    ^-  (map atom aura)
    ?:  &(?=([* ~ ~] two-map) !?=([* ~ ~] one-map))
      $(one-map two-map, two-map one-map) 
    =/  list  ~(tap by one-map)
    |-  ^-  (map atom aura)
    ?~  list  two-map
    %=    $
        list  t.list
        two-map 
      %+  ~(put by two-map)
        p.i.list
      =+  (~(get by two-map) p.i.list)
      ?~  -  q.i.list
      (aura-merge u.- q.i.list)
    == 
  ::
  ++  wood-merge
    |=  [=one=(map atom [=aura =^xray]) =two=(map atom [=aura =^xray])]
    ^-  (map atom [=aura =^xray])
    ?:  &(?=([* ~ ~] two-map) !?=([* ~ ~] one-map))
      $(one-map two-map, two-map one-map) 
    =/  list  ~(tap by one-map)
    |-  ^-  (map atom [=aura =^xray])
    ?~  list  two-map
    %=    $
        list  t.list
        two-map 
      %+  ~(put by two-map)
        p.i.list
      =+  (~(get by two-map) p.i.list)
      ?~  -  q.i.list
      :-  (aura-merge aura.u.- aura.q.i.list)
      (merge(xray xray.u.-) xray.q.i.list)
    ==
  ::
  ++  merge
    |=  =new=^xray
    ::  (the superposition of .xray and .new-xray)
    ::
    ^-  ^xray
    ::  identify trivial cases
    ::
    ?:  ?=(%void xray)  new-xray
    ?:  ?=(%void new-xray)  xray
    ?:  |(?=(%noun xray) ?=(%noun new-xray))  %noun
    ?-  -.xray
      ::
      ::  %bark: atom selection
      ::
      %bark  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%bark (bark-merge map.new-xray map.xray)]
               %bush  [%root xray new-xray]
               %cell  [%root xray new-xray]
               %core  [%root xray new-xray]
               %root  [%root $(new-xray flat.new-xray) deep.new-xray]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%root xray new-xray]
             ==
      ::
      ::  %bush: head-of-cell superposition
      ::
      %bush  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bush  :+  %bush 
                        $(xray wide.xray, new-xray wide.new-xray)
                      $(xray tall.xray, new-xray tall.new-xray)
               %cell  $(xray [%fork xray ~ ~])
               %core  $(xray [%fork xray ~ ~])
               %root  [%root flat.new-xray $(new-xray deep.new-xray)]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%bush wide.xray $(xray tall.xray)]
             ==
      ::
      ::  %cell: unstructured cell
      ::
      %cell  ::  terminate cell collision
             ::
             ?:  ?=(%cell -.new-xray)
               :+  %cell 
                 $(xray head.xray, new-xray head.new-xray)
               $(xray tail.xray, new-xray tail.new-xray)
             ::  terminate core-cell collision
             ::
             ?:  ?=(%core -.new-xray)
               $(xray [%fork xray ~ ~])
             ::  normally, reverse
             ::
             $(xray new-xray, new-xray xray)
      ::
      ::  %core: functional attribute battery
      ::
      %core  ::  terminate core collision
             ::
             ?:  ?=(%core -.new-xray)
               $(xray [%fork xray ~ ~])
             ::  normally, reverse
             ::
             $(xray new-xray, new-xray xray)
      ::
      ::  %face: namespace
      ::
      %face  ::  if face matches, unify
             ::
             ?:  ?&(?=(%face -.new-xray) =(term.xray term.new-xray))
               [%face term.xray $(xray xray.xray, new-xray xray.xray)]
             ::  otherwise erase
             ::
             $(xray xray.xray)
      ::
      ::  %fork: unstructured superposition
      ::
      %fork  ::  forks are indigestible but merge with other forks
             ::
             ?:  ?=(%fork -.new-xray)
               [%fork (~(uni in set.xray) set.new-xray)]
             [%fork (~(put in set.xray) new-xray)]
      ::
      ::  %hint: type extension
      ::
      %hint  ::  merging a hint destroys it
             ::
             $(xray xray.xray)
      ::
      ::  %knot: recursion root
      ::
      %knot  ::  try to combine knots for cleanliness
             ::
             ?:  ?=(%knot -.new-xray)
               :+  %knot 
                 (~(uni in set.xray) set.new-xray) 
               $(xray xray.xray, new-xray xray.new-xray)
             [%knot set.xray $(xray xray.xray)]
      ::  %loop: recursion point
      ::
      %loop  ::  expand through loop
             ::
             $(xray (~(got by loop-map) index.xray))
      ::
      ::  %rock: atomic constant
      ::
      %rock  ::  reduce to trivial set
             ::
             $(xray [%bark [[atom.xray aura.xray] ~ ~]])
      ::
      ::  %root: atom-cell superposition
      ::
      %root  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%root $(xray flat.xray) deep.xray]
               %bush  [%root flat.xray $(xray deep.xray)]
               %cell  [%root flat.xray $(xray deep.xray)]
               %core  [%root flat.xray $(xray deep.xray)]
               %root  :+  %root
                        $(xray flat.xray, new-xray flat.new-xray)
                      $(xray deep.xray, new-xray deep.new-xray)
               %sand  [%root $(xray flat.xray) deep.xray]
               %wood  [%root flat.xray $(xray deep.xray)]
             ==        
      ::
      ::  %sand: atomic variable
      ::
      %sand  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  $(xray [%fork xray ~ ~])
               %bush  $(xray [%fork xray ~ ~])
               %cell  [%root xray new-xray]
               %core  [%root xray new-xray]
               %root  [%root $(new-xray flat.new-xray) deep.new-xray]
               %sand  $(xray [%fork xray ~ ~])
               %wood  [%root xray new-xray]
             ==        
      ::
      ::  %wood: tagged values
      ::
      %wood  ?+  -.new-xray  
                      $(xray new-xray, new-xray xray)
               %bark  [%root new-xray xray]
               %bush  [%bush wide.new-xray $(new-xray tall.new-xray)]
               %cell  $(xray [%fork xray ~ ~])
               %core  $(xray [%fork xray ~ ~])
               %root  [%root flat.new-xray $(new-xray deep.new-xray)]
               %sand  [%root new-xray xray]
               %wood  [%wood (wood-merge map.xray map.new-xray)]
    ==       ==
  --
--

