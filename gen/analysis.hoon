!:
:-  %say
|=  {^ {{subject=type ~} ~}}
:-  %txt
^-  wain
=<  =/  =spec  specify:measure:(enter:an subject)
    =/  =plum  (spec-to-plum spec)
    ~(tall plume plum)    
|%
::
::  =spec-to-plum: convert $spec to $plum
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
      ::  $body: metadata (description) and data (structure)
      ::
      +$  body  [=meta =data]
      ::
      ::
      ::  $meta: all analysis metadata
      ::
      +$  meta
        $:  ::  shape-unit: geometric analysis
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
            ::  standard-set: compliance declarations
            ::
            =standard=(set stud)
            ::
            ::  entry-unit: loop entry point (reverse of .loop-map)
            ::
            =entry=(unit index)
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
      ::  $data: data structure of direct node
      ::
      +$  data
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
      ::  $xray: direct or indirect (index in loop map) view
      ::
      +$  xray  $@(index zray)
      ::
      ::  $wray: direct xray without type
      ::
      +$  wray  [=meta =data]
      ::
      ::  $zray: direct xray, complete
      ::
      +$  zray  [=type wray]
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
      ::  else add a loop entry meta, and replace stub in table
      ::
      =.  entry-unit.meta.xray  `p.u.new
      ?<  (~(has by table.state) p.u.new)
      [xray state(table (~(put by table.state) type [p.u.new xray]))]
    ::
    ::  -main: main analysis without entry control
    ::
    ++  main
      |^  ^-  [^xray _state]
          ?-    type
              %void      [[type *meta type] state]
              %noun      [[type *meta type] state]
          ::
              [%atom *]  [[type *meta type] state]
              [%cell *]  =^  hed-xray  state  $(type p.type)
                         =^  tyl-xray  state  $(type q.type)
                         [[type *meta %cell hed-xray tyl-xray] state]
              [%core *]  =^  wray  state  (core p.type q.type)
                         [[type wray] state]
              [%face *]  =^  xray  state  $(type q.type)
                         [[type *meta %face p.type xray] state]
              [%hint *]  =^  wray  state  (hint p.type q.type)
                         [[type wray] state]
              [%fork *]  =^  wray  state  (fork p.type)
                         [[type wray] state]
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
        ^-  [wray _state]
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
        ^-  wray
        :-  *meta
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
        |-  ^-  [wray _state]
        ?@  xray  $(xray (~(got by loop-map) xray))
        ?-    -.note
            %help
          :_  state
          +:xray(comment-set.meta (~(put in comment-set.meta.xray) p.note))
        ::
            %know  
          :_  state 
          +:xray(standard-set.meta (~(put in standard-set.meta.xray) p.note))
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
          +:xray(recipe-set.meta (~(put in recipe-set.meta.xray) recipe))
        ==
      ::
      ::  +fork: convert a %fork $type to an $xray
      ::
      ++  fork
        |=  ::  set: set of union types
            ::
            =(set ^type)
        ^-  [wray _state]
        =/  list  ~(tap in set)
        =-  :_(-> `wray`[*meta %fork (~(gas in *(^set ^xray)) -<)])
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
      ?~  entry-unit.meta.xray  .
      .(loop-map (~(put by loop-map) u.entry-unit.meta.xray xray))
    ::
    ::  -require: produce best currently available shape
    ::
    ++  require
      |-  ^-  shape
      ::  resolve indirections
      ::
      ?@  xray  $(xray (~(got by loop-map) xray))
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
    ::  -original: produce $type of xray
    ::
    ++  original
      |-  ^-  type
      ?@(xray $(xray (~(got by loop-map) xray)) type.xray)
    ::
    ::  -fresh: produce trivial fork set if any
    ::
    ++  fresh
      ^-  (unit (set ^xray))
      ?@  xray  ~
      ?.  ?=(%fork -.data.xray)  ~
      ?^  entry-unit.meta.xray  ~
      `set.data.xray
    ::
    ::  =join: compose union of two xrays, collapsing simple forks
    ::
    ++  join
      |=  [this=^xray that=^xray]
      ^-  ^xray
      :-  (fork original(xray this) original(xray that) ~)
      :-  *meta
      ^-  data
      :-  %fork
      ^-  (set ^xray)
      =/  this-set-unit  fresh(xray this)
      =/  that-set-unit  fresh(xray that)
      ?~  this-set-unit
        ?~  that-set-unit 
          (sy this that ~)
        (~(put in u.that-set-unit) this)
      ?~  that-set-unit
        (~(put in u.this-set-unit) that)
      (~(uni in u.this-set-unit) u.that-set-unit)
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
      =/  new-xray  `^xray`[%void *meta %void]
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
        =/  =zray  (~(got by loop-map) xray)
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
      ?:  ?&  ?=(^ entry-unit.meta.xray) 
              (~(has in loop-set) u.entry-unit.meta.xray)
          ==
        +>
      ::  record any loops we enter
      ::
      =.  loop-set  ?~  entry-unit.meta.xray  loop-set  
                    (~(put in loop-set) u.entry-unit.meta.xray)
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
        +>+>(xray new-xray(entry-unit.meta entry-unit.meta.xray))
      ==
    --
  ::
  ::  -match: refine pattern analysis
  ::
  ++  match
    |-  ^+  +
    ::  (self with $pattern metadata on every relevant $xray)
    ::
    !!
  ::
  ::  |take: completions
  ::
  +|  %take
  ::
  ::  -specify: convert to spec
  ::
  ++  specify
    =|  loop-set=(set index)
    |-  ^-  spec
    ?@  xray  
      ?:  (~(has in loop-set) xray)
        [%loop (synthetic xray)]
      $(xray (~(got by loop-map) xray))
    ?^  entry-unit.meta.xray
      =/  =spec  $(entry-unit.meta.xray ~)
      :+  %bsbs
        spec
      [[(synthetic u.entry-unit.meta.xray) spec] ~ ~]
    ?^  recipe-set.meta.xray
      =/  =recipe  n.recipe-set.meta.xray
      ?-  -.recipe
        %direct     `spec`[%like `wing`[term.recipe ~] ~]
        %synthetic  :+  %make 
                      [%limb term.recipe]
                    %+  turn  list.recipe
                    |=(=^xray `spec`^$(xray xray))
      ==
    ?@  data.xray  [%base data.xray]
    ?-  -.data.xray
      %atom  ?~  constant-unit.data.xray
               [%base %atom aura.data.xray]
             ?:  &(=(%n aura.data.xray) =(`@`0 u.constant-unit.data.xray))
               [%base %null]
             [%leaf aura.data.xray u.constant-unit.data.xray]
      %cell  =/  head  $(xray head.data.xray)
             =/  tail  $(xray tail.data.xray)
             ?:  &(=([%base %noun] head) =([%base %noun] tail))
               [%base %cell]
             ?:  ?=(%bscl -.tail)
               [%bscl head +.tail]
             [%bscl head tail ~]
      %core  =/  payload  $(xray xray.data.xray)
             =/  battery
               ^-  (map term spec)
               %-  ~(run by (flatten-battery battery.data.xray))
               |=  =^xray
               ^$(xray xray)
             ?-  r.garb.data.xray
               %lead  [%bszp payload battery]
               %gold  [%bsdt payload battery]
               %zinc  [%bstc payload battery]
               %iron  [%bsnt payload battery]
             == 
      %face  =/  =spec  $(xray xray.data.xray)
             ::  we discard the $tune case, a $spec can't express it
             ::
             ::  XX: should exist a %misjunction spec
             ::
             ?^(face.data.xray spec [%bsts face.data.xray spec])
      %fork  =/  =shape  (need shape-unit.meta.xray)
             |^  ?+  shape           ~|([%strange-fork-shape shape] !!)
                   [%option *]       [%bswt choices]
                   [%union *]        [%bscn choices]
                   [%junction *]     :+  %bsvt
                                       ^$(xray flat.shape)
                                     ^$(xray deep.shape)
                   [%conjunction *]  :+  %bskt 
                                       ^$(xray wide.shape) 
                                     ^$(xray tall.shape)
                   [%misjunction *]  [%bswt choices]
                 ==
             ::
             ++  choices
               ^-  [i=spec t=(list spec)]
               =-  ?>(?=(^ -) -)
               (turn ~(tap in set.data.xray) |=(=^xray ^^$(xray xray)))
             --
    ==
  ::
  ::  -display: convert to plum via spec
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
  ::
  ::  =flatten-battery: temporary function (XX)
  ::
  ::    $spec should have chapters but it doesn't.  so we flatten.
  ::
  ++  flatten-battery
    |=  =battery
    =/  chapter-list  ~(tap by battery)
    |-  ^-  (map term ^xray)
    ?~  chapter-list  ~
    (~(uni by q.q.i.chapter-list) $(chapter-list t.chapter-list))
  ::
  ::  =synthetic: convert :number to a synthetic name
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
  --
--
