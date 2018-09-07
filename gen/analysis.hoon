:-  %say
|=  *
:-  %noun
=-  "hello, world"
|%
::  _an: type analysis gear
::
++  an
  =>  |%
      ::  $notebook: type analysis with explicit loops
      ::
      +$  notebook
        $:  ::  xray: analysis record
            ::  loop-map: loop dictionary
            ::
            =xray
            =loop=(map index zray) 
        ==
      ::  $index: loop index
      ::
      +$  index  @ud
      ::
      ::  $shape: structural analysis
      ::
      +$  shape
        $~  %void
        $@  $?  ::  %atom: any atom
                ::  %cell: any cell
                ::  %noun: any noun
                ::  %void: no nouns
                ::  %wide: cell with cell head
                ::
                %atom
                %cell 
                %noun 
                %void 
                %wide
            ==
        $%  ::  %constant: constant atom
            ::  %instance: constant atom head
            ::  %option: fork of atomic choices
            ::  %union: fork of atom/tag head, noun/record tail
            ::  %junction: fork of atom or cell
            ::  %conjunction: fork of cell with atom or cell head
            ::  %misjunction: malformed superposition
            ::
            [%constant =atom]
            [%instance =atom]
            [%option =(map atom xray)]
            [%union =(map atom xray)]
            [%junction flat=xray deep=xray]
            [%conjunction wide=xray tall=xray]
            [%misjunction one=xray two=xray]
        ==
      ::  $battery: battery analysis
      ::
      +$  battery  (map term (pair what (map term xray)))
      ::
      ::  $report: all analysis metadata
      ::
      +$  report
        $:  ::  type: original type 
            ::
            ::    just so we don't lose it.  set .type to %void
            ::    before shipping a report over the network.
            ::
            =type
            ::
            ::  shape-unit: geometric analysis
            ::
            ::    declare well-known local structural features 
            ::    of .type, including geometry and superposition.
            ::    
            =shape=(unit shape)
            ::
            ::  pattern-set: well-known patterns
            ::
            ::    recognize common structures, containers, etc.
            ::
            =pattern=(set pattern)
            ::
            ::  standard-set: standards compliance declarations
            ::
            =standard=(set stud)
            ::
            ::  entry-set: loop entry points
            ::
            ::    recognize set of recursive types, as indexes
            ::    into the global notebook loop map, which both
            ::    occur within this $xray and resolve back to it.
            ::    should be denormalized reverse of .loop-map.
            ::
            =entry=(set index)
            ::
            ::  recipe-set: construction declarations
            ::
            ::    recognize symbolic constructions provided by hints.
            ::
            =recipe=(set recipe)
            ::
            ::  comment-set: literate comments 
            ::
            =comment=(set help)
        ==
      ::  $recipe: construction trace (for type printing)
      ::
      +$  recipe
        $~  [%direct %$]
        $%  ::  %direct: named structure
            ::  %synthetic: generic construction
            ::
            [%direct =term]
            [%synthetic =term =(list xray)]
        ==
      ::  $pattern: common data patterns
      ::
      +$  pattern
        $@  $?  ::  %hoon: $hoon (hoon program)
                ::  %json: $json (json data)
                ::  %manx: $manx (xml node) 
                ::  %nock: $nock (nock formula)
                ::  %path: $path (list @ta)
                ::  %plum: $plum (printable)
                ::  %skin: $skin (hoon texture)
                ::  %spec: $spec (hoon structure)
                ::  %tape: $tape (list @tD, utf8 string)
                ::  %tour: $tour (list @c, utf32/nfc grapheme clusters)
                ::  %type: $type (hoon inference type)
                ::  %vase: $vase (hoon dynamically-typed value)
                ::  %wall: $wall (list @t, text line)
                ::
                %hoon
                %manx
                %nock
                %path
                %plum
                %skin
                %spec
                %tape
                %tour
                %type
                %vase
            == 
        $%  ::  %gate: function
            ::  %gear: multifunction
            ::  %tree: n-l-r tree (including maps and sets)
            ::  %list: i-t list
            ::  %unit: ~-u unit (maybe)
            ::
            [%gate sample=xray product=xray]
            [%gear sample=xray context=xray =battery]
            [%list item=xray]
            [%tree item=xray]
            [%unit item=xray]
        ==
      ::
      ::  $chassis: data structure of direct node
      ::
      +$  chassis
        $~  %void
        $@  $?  ::  %noun: any noun
                ::  %void: no nouns
                ::
                %noun
                %void
            ==
        $%  ::  %atom: atom, variable constant
            ::  %cell: ordered pair
            ::  %core: functional attribute battery
            ::  %face: name or namespace
            ::  %fork: superposition
            ::
            [%atom =aura =constant=(unit @)]
            [%cell head=xray tail=xray]
            [%core =garb =xray =battery]
            [%face face=$@(term tune) =xray]
            [%fork =(set xray)]
        ==
      ::
      ::  $xray: direct or 
      ::
      +$  xray  $@(=index zray)
      ::
      ::  $zray: direct xray
      ::
      +$  zray  [meta=report data=chassis]
      --
  |_  notebook
  ::
  ::  |make: constructions
  ::
  +|  %make
  ::
  ::  =enter: create with first-pass recursion analysis
  ::
  ++  enter
    |=  =type
    ^+  +>
    =<  =+  [xray state]=entry
        %_  +>+>+
          xray      xray
          loop-map  %-  ~(gas by *(map index zray)) 
                    %+  turn  
                      ~(tap by table.state)
                    |=  [* =index =zray] 
                    [index zray]
        ==
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
                table=(map ^type (pair index zray))
        ==  ==
    |%
    ::  -entry: analyze at possible entry point
    ::
    ++  entry
      ::  ($xray of .type; updated .state)
      ::
      |-  ^-  [^xray _state]
      ::  old: existing entry for .type in .table.state
      ::
      =/  old  (~(get by table.state) type)
      ::  if old entry is found in loop table, produce it
      ::
      ?^  old  [q.u.old state]
      ::  if .type is already on our stack .trace
      ::
      ?:  (~(has in trace) type)
        ::  then produce and record a loop
        ::
        :-  count.state
        %=  state
          count  ::  (count.state incremented to the next unused index)
                 ::
                 +(count.state)
          table  ::  (table.state with the loop index and stub) 
                 ::
                 (~(put by table.state) type [count.state *zray])
        ==
      ::  else compute main analysis loop
      ::
      =^  =^xray  state  main(trace (~(put in trace) type))
      |-  ^-  [^^xray _state]
      ?@  xray  [xray state]
      ::  new: any xray we added to .table.state for .type
      ::
      =/  new  (~(get by table.state) type)
      ::  if .new is empty, then .type is not an entry point
      ::
      ?~  new  [xray state]
      ::  else add a loop entry report, and replace stub in table
      ::
      =.  entry-set.meta.xray  (~(put in entry-set.meta.xray) p.u.new)
      [xray state(table (~(put by table.state) type [p.u.new xray]))]
    ::
    ::  -main: main analysis without entry control
    ::
    ++  main
      |^  ^-  [^xray _state]
          ::  (main analysis)
          ::
          =-  ?@  -<  - 
              ::  save type in nontrivial results
              ::
              [-<(type.meta type) ->]
          ^-  [^xray _state]
          ?-    type
              %void      [[*report type] state]
              %noun      [[*report type] state]
          ::
              [%atom *]  [[*report type] state]
              [%cell *]  =^  hed-xray  state  $(type p.type)
                         =^  tyl-xray  state  $(type q.type)
                         [[*report %cell hed-xray tyl-xray] state]
              [%core *]  (core p.type q.type)
              [%face *]  =^  xray  state  $(type q.type)
                         [[*report %face p.type xray] state]
              [%hint *]  (hint p.type q.type)
              [%fork *]  (fork p.type)
              [%hold *]  entry(type ~(repo ut type))
          == 
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
        ^-  [^xray _state]
        ::  payload-xray: analyzed payload
        ::
        =^  payload-xray  state  main(type payload-type)
        ::  chapters: analyzed chapters 
        ::
        ::  this code looks complicated but is just an overly 
        ::  bulky monadic traverse.
        ::
        =^  chapters=(list (pair term (pair what (map term ^xray))))  state
          =/  chapters=(list (pair term tome))  ~(tap by q.r.coil)
          |-  ^-  [(list (pair term (pair what (map term ^xray)))) _state]
          ?~  chapters  [~ state]
          =^  more-chapters  state  $(chapters t.chapters)
          =^  this-chapter  state
            =-  :_  -> 
                :+  p.i.chapters
                  `what`p.q.i.chapters
                (~(gas by *(map term ^xray)) -<)
            =/  arms=(list (pair term hoon))  ~(tap by q.q.i.chapters)
            |-  ^-  [(list (pair term ^xray)) _state]
            ?~  arms  [~ state]
            =^  more-arms  state  $(arms t.arms)
            =^  this-arm  state  
              main(type [%hold [%core payload-type coil] q.i.arms])
            [[[p.i.arms this-arm] more-arms] state]
          [[this-chapter more-chapters] state]
        :_  state
        ^-  ^xray
        :-  *report
        :^    %core
            p.coil
          payload-xray
        (~(gas by *(map term (pair what (map term ^xray)))) chapters)
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
        =^  =^xray  state  main(type content-type)
        |-  ^-  [^^xray _state]
        ?@  xray  $(xray (~(got by loop-map) index.xray))
        ?-    -.note
            %help
          :_  state
          xray(comment-set.meta (~(put in comment-set.meta.xray) p.note))
        ::
            %know  
          :_  state 
          xray(standard-set.meta (~(put in standard-set.meta.xray) p.note))
        ::
            %made
          =^  =recipe  state
            ?~  q.note
              :_(state [%direct p.note])   
            =-  [`recipe`[%synthetic p.note -<] `_state`->]
            |-  ^-  [(list ^^xray) _state]
            ?~  u.q.note  [~ state]
            =/  part  
              (~(play ut subject-type) [%tsld [%limb %$] [%wing i.u.q.note]])
            =^  this  state  entry(type part)
            =^  more  state  $(u.q.note t.u.q.note)
            [[this more] state]
          :_  state
          xray(recipe-set.meta (~(put in recipe-set.meta.xray) recipe))
        ==
      ::
      ::  +fork: convert a %fork $type to an $xray
      ::
      ++  fork
        |=  ::  set: set of union types
            ::
            =(set ^type)
        ^-  [^xray _state]
        =/  list  ~(tap in set)
        =-  :_(-> `^xray`[*report %fork (~(gas in *(^set ^xray)) -<)])
        |-  ^-  [(^list ^xray) _state]
        ?~  list  [~ state]
        =^  this-xray  state  main(type i.list)
        =^  more-xrays  state  $(list t.list)
        [[this-xray more-xrays] state]
      --
    --
  ::
  ::  |grow: continuations
  ::
  +|  %grow
  ::
  ::  -measure: add shape to metadata, possibly restructuring
  ::
  ++  measure
    |-  ^+  +
    =<  +>(+< complete:analyze)
    |%
    ++  complete  `notebook`+>+<
    ::
    ::  -remember: save measured xray in loop map as needed
    ::
    ++  remember
      ^+  .
      ?:  ?=(@ xray)  .
      =+  entry-list=~(tap in entry-set.meta.xray)
      |-  ^+  +>
      ?~  entry-list  +>
      %=  $
        entry-list  t.entry-list 
        loop-map    (~(put by loop-map) i.entry-list xray)
      ==
    ::
    ::  -require: produce best currently available shape
    ::
    ++  require
      |-  ^-  shape
      ::  resolve indirections
      ::
      ?@  xray  $(xray (~(got by loop-map) index.xray))
      ::  produce already-computed shape
      ::
      ?^  shape-unit.meta.xray  u.shape-unit.meta.xray
      ::  (minimal shape which has not angered the recursion gods)
      ::
      ^-  shape
      ?-    -.data.xray
          %atom
        ?~  constant-unit.data.xray
          %atom 
        [%constant u.constant-unit.data.xray]
      ::
          %cell
        =/  head-shape  $(xray head.data.xray)
        ?:  ?|  ?=(?(%cell %wide) head-shape) 
                ?=([?(%instance %union %junction %conjunction) *] head-shape)
            ==
          %wide
        ?:  ?=([%constant *] head-shape)
          [%instance atom.head-shape]
        %cell
      ::
          %core  %cell
          %face  $(xray xray.data.xray)
          %fork  $(xray (forge ~(tap in set.data.xray)))
      ==
    ::
    ::  =join-report: try to consolidate metadata
    ::
    ++  join-report
      |=  [this=report that=report]
      ^-  (unit report)
      !!
    ::
    ::  =join-chassis: try to consolidate data
    ::
    ++  join-chassis
      |=  [this=chassis that=chassis]
      ^-  (unit chassis)
      !!
    ::
    ::  =join-simple: compose without consolidating
    ::
    ++  join-simple
      |=  [this=^xray that=^xray]
      ^-  ^xray
      !!
    ::
    ::  =join: compose union of two xrays
    ::
    ++  join
      |=  [this=^xray that=^xray]
      ^-  ^xray
      ?:  |(?=(@ this) ?=(@ that))  (join-simple this that)
      =+  data-unit=(join-chassis data.this data.that)
      ?~  data-unit  (join-simple this that)
      =+  meta-unit=(join-report meta.this meta.that)
      ?~  meta-unit  (join-simple this that)
      [u.meta-unit u.data-unit]
    ::
    ::  =binary: compose binary junction/conjunction/misjunction
    ::
    ++  binary
      |*  joint=?(%junction %conjunction %misjunction)
      |=  [this=^xray that=^xray]
      ^-  [shape ^xray]
      [[joint this that] (join this that)]
    ::
    ::  -frame: computed shape with xray
    ::
    ++  frame  `[shape ^xray]`[require xray]
    ::
    ::  =merge: merge two xrays intelligently, using shape
    ::
    ++  merge
      |=  [this=^xray that=^xray]
      ^-  ^xray
      =+  (combine frame(xray this) frame(xray that))
      ?@(-> -> ->(shape-unit.meta `-<))
    ::
    ::  =collate: merge option maps
    ::
    ++  collate
      |=  [thick=(map atom ^xray) thin=(map atom ^xray)]
      =/  list  ~(tap by thin)
      |-  ^-  (map atom ^xray)
      ?~  list  thick
      =/  item  (~(get by thick) p.i.list)
      %=    $
          list  t.list
          thick
        %+  ~(put by thick)
          p.i.list
        ?~(item q.i.list (merge u.item q.i.list))
      ==
    ::
    ::  =forge: combine list of shape-described xrays
    ::
    ++  forge
      |=  =(list ^xray)
      =/  new-xray  `^xray`[*report %void]
      |-  ^-  ^xray
      ?~  list  new-xray
      $(list t.list, new-xray (merge i.list new-xray))
    ::
    ::  =combine: combine shape-described xrays
    ::
    ++  combine
      |=  [this=[=shape =^xray] that=[=shape =^xray]]
      ^-  [shape ^xray]
      ?@  shape.this
        ?^  shape.that  $(this that, that this)
        :_  (join xray.this xray.that)
        ?:  =(shape.this shape.that)  shape.this
        ?:  ?=(%void shape.this)  shape.that
        ?:  ?=(%void shape.that)  shape.this
        ?:  |(?=(%noun shape.this) ?=(%noun shape.that))  %noun
        ?-  shape.this
          %atom  [%junction xray.this xray.that]
          %cell  ?:  ?=(%wide shape.that)
                   %cell
                 [%junction xray.that xray.this]
          %wide  ?:  ?=(%cell shape.that)
                   %cell
                 [%junction xray.that xray.this]
        ==
      ?@  shape.that
        ?:  ?=(%void shape.that)  this
        ?:  ?=(%noun shape.that)  that
        ?:  ?=(%atom shape.that)
          ?+  -.shape.this  
                       ((binary %misjunction) xray.this xray.that)
            %instance  ((binary %junction) xray.this xray.that)
            %union     ((binary %junction) xray.this xray.that)
            %junction  %+  (binary %junction) 
                         (merge xray.that flat.shape.this) 
                       deep.shape.this
          ==
        ?+    -.shape.this
                       ((binary %misjunction) xray.this xray.that)
            %constant  ((binary %junction) xray.that xray.this)
            %option    ((binary %junction) xray.that xray.this)
            %junction  %+  (binary %junction) 
                         flat.shape.this 
                       (merge xray.that deep.shape.this)
        ==
      ?:  |(?=(%misjunction -.shape.this) ?=(%misjunction -.shape.that))
        ((binary %misjunction) xray.this xray.that)
      ?-    -.shape.this
          %constant
        ?-  -.shape.that
          %constant     :_  (join xray.this xray.that) 
                        :-  %option 
                        %+  collate
                          [[atom.shape.this xray.this] ~ ~]
                        [[atom.shape.that xray.that] ~ ~]
          %instance     ((binary %junction) xray.this xray.that)
          %option       ((binary %misjunction) xray.this xray.that)
          %union        ((binary %junction) xray.this xray.that)
          %junction     %+  (binary %junction)
                          (merge xray.this flat.shape.that)
                        deep.shape.that
          %conjunction  ((binary %junction) xray.this xray.that)
        ==
      ::
          %instance
        ?+  -.shape.that  $(this that, that this)
          %instance     :_  (join xray.this xray.that)
                        :-  %union
                        %+  collate
                          [[atom.shape.this xray.this] ~ ~]
                        [[atom.shape.that xray.that] ~ ~]
          %option       ((binary %junction) xray.this xray.that)
          %union        :_  (join xray.this xray.that)
                        :-  %union
                        %+  collate 
                          map.shape.that 
                        [[atom.shape.this xray.this] ~ ~]
          %junction     %+  (binary %junction)
                          flat.shape.that
                        (merge xray.this deep.shape.that)
          %conjunction  %+  (binary %junction)
                          wide.shape.that
                        (merge xray.this tall.shape.that)
        ==
      ::
          %option
        ?+  -.shape.that  $(this that, that this)
          %option       :_  (join xray.this xray.that)
                        :-  %option
                        (collate map.shape.this map.shape.that)
          %union        ((binary %junction) xray.this xray.that)
          %junction     %+  (binary %junction)
                          (merge xray.this flat.shape.that)
                        deep.shape.that
          %conjunction  ((binary %junction) xray.this xray.that)
        ==
      ::
          %union
        ?+  -.shape.that  $(this that, that this)
          %union        :_  (join xray.this xray.that)
                        :-  %union
                        (collate map.shape.this map.shape.that)
          %junction     %+  (binary %junction)
                          flat.shape.that
                        (merge xray.this deep.shape.that)
          %conjunction  %+  (binary %conjunction)
                          wide.shape.that
                        (merge xray.this tall.shape.that)
        ==
      ::
          %junction
        ?+  -.shape.that  $(this that, that this)
          %junction     %+  (binary %junction)
                          (merge flat.shape.this flat.shape.that)
                        (merge deep.shape.this deep.shape.that)
          %conjunction  %+  (binary %junction)
                          flat.shape.this
                        (merge deep.shape.this xray.that)
        ==
      ::
          %conjunction
        ?+  -.shape.that  $(this that, that this)
          %conjunction  %+  (binary %conjunction)
                          (merge wide.shape.this wide.shape.that)
                        (merge tall.shape.this tall.shape.that)
        ==
      == 
    ::
    ::  -analyze
    ::
    ++  analyze
      ::  afterward, record result in loop map as needed
      ::
      =<  remember
      ::  loop-set: set of loops we are analyzing 
      ::
      =|  loop-set=(set index)
      |-  ^+  +>
      ::  if .xray is a loop reference
      ::
      ?@  xray
        ::  zray: direct target of indirect .xray
        ::
        =/  =zray  (~(got by loop-map) index.xray)
        ::  if we have already measured .zray, do nothing
        ::
        ?^  shape-unit.meta.zray  +>+
        ::  otherwise, measure it eagerly; it will save itself
        ::
        +>+(loop-map loop-map:complete:analyze(xray zray))
      ::  if we've already measured this xray, do nothing
      ::
      ?^  shape-unit.meta.xray  +>
      ::  if we're currently measuring this xray, do nothing
      ::
      ?:  !=(~ (~(int in loop-set) entry-set.meta.xray))  +>
      ::  record any loops we enter
      ::
      =.  loop-set  (~(uni in loop-set) entry-set.meta.xray)
      ::  %noun and %void are their own shape
      ::
      ?@  data.xray  +>(shape-unit.meta.xray `data.xray)
      ?-    -.data.xray
          %atom
        +>(shape-unit.meta.xray `require)
      ::
          %cell
        =^  head  loop-map  complete:analyze(xray head.data.xray)
        =^  tail  loop-map  complete:analyze(xray tail.data.xray)
        =.  head.data.xray  head
        =.  tail.data.xray  tail
        +>+>(shape-unit.meta.xray `require)
      ::
          %core
        ::
        ::  this code looks complicated but is just an overly 
        ::  bulky monadic traverse.
        ::
        =^  payload  loop-map  complete:analyze(xray xray.data.xray)
        =^  chapters  loop-map
          =/  chapters  ~(tap by battery.data.xray)
          |-  ^+  [chapters loop-map]
          ?~  chapters  [~ loop-map]
          =^  more-chapters  loop-map  $(chapters t.chapters)
          =^  this-chapter  loop-map
            =-  :_  -> 
                :+  p.i.chapters
                  `what`p.q.i.chapters
                (~(gas by *(map term ^xray)) -<)
            =/  arms=(list (pair term ^xray))  ~(tap by q.q.i.chapters)
            |-  ^-  [(list (pair term ^xray)) _loop-map]
            ?~  arms  [~ loop-map]
            =^  more-arms  loop-map  $(arms t.arms)
            =^  this-arm  loop-map  complete:analyze(xray q.i.arms)
            [[[p.i.arms this-arm] more-arms] loop-map]
          [[this-chapter more-chapters] loop-map]
        =.  xray.data.xray  payload
        =.  battery.data.xray  (~(gas by *battery) chapters)
        +>+>(shape-unit.meta.xray `%cell)
      ::
          %face
        =^  body  loop-map  complete:analyze(xray xray.data.xray)
        =.  xray.data.xray  body
        +>+(shape-unit.meta.xray `require(xray body))
      ::
          %fork 
        =^  list  loop-map
          =/  list  ~(tap in set.data.xray)
          |-  ^-  [(^list ^xray) _loop-map]
          ?~  list  [~ loop-map]
          =^  this  loop-map  complete:analyze(xray i.list)
          =^  rest  loop-map  $(list t.list)
          [[this rest] loop-map]
        =/  new-xray  (forge list)
        ?@  new-xray  +>+>(xray new-xray)
        +>+>(xray new-xray(entry-set.meta entry-set.meta.xray))
      ==
    --
  ::
  ::  -match: refine pattern analysis
  ::
  ++  match
    |-  ^+  +
    ::  (self with $pattern report on every relevant $xray)
    ::
    !!
  ::
  ::  |take: completions
  ::
  +|  %take
  ::
  ::  -specify: measure and convert to spec
  ::
  ++  specify
    =>  measure 
    |-  ^-  spec
    !!
  ::  -display: measure, convert to spec, convert spec to plum
  ::
  ++  display
    |-  ^-  plum
    !!
  ::  =present: convert noun to plum
  ::
  ++  present
    |=  =noun
    ^-  plum
    !!
  ::
  ::  |work: productions
  ::
  +|  %work
  ++  foo  %bar
  ::
  ::  |tool: functions
  ::
  +|  %tool
  ++  moo  %bar
  --
--
