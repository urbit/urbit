::  A library for printing doccords
=>
  |%
  :>    an overview of all named things in the type.
  :>
  :>  each element in the overview list is either a documentation for a sublist
  :>  or an association betwen a term and documentation for it
  +$  overview  (list overview-item)
  ::
  :>  an element of an overview
  +$  overview-item
    $%  [%header doc=what children=overview]
        [%item name=tape doc=what]
    ==
  ::
  :>  the part of a type being inspected
  +$  item
    $%
        ::  overview of a type
        ::
        [%view items=overview]
        ::  inspecting a full core
        $:  %core
            name=tape
            docs=what
            sut=type
            con=coil
            children=(unit item)
        ==
        ::  inspecting a single arm on a core
        $:  %arm
            name=tape
            docs=what
            gen=hoon
            sut=type
        ==
        ::  inspecting a face and what's behind it
        $:  %face
            name=tape
            docs=what
            children=(unit item)
        ==
        ::  inspecting a single chapter on a core
        $:  %chapter
            name=tape
            docs=what
            sut=type
            con=coil
            chapter-id=term
        ==
    ==
  ::
  --
|%
::  arms for finding docs in types
::+|  %searching
:>    returns the item to print while searching through topic
:>
:>  this gate is called recursively to find the path (topic) in the type
:>  (sut). once it finds the correct part of the type, it switches to
:>  +build-inspectable-recursively to describe that part of the type
++  find-item-in-type
  |=  [topics=(list term) sut=type]
  ^-  (unit item)
  ?~  topics
    ~  ::(build-inspectable-recursively sut)
  ?-  sut
      [%atom *]  ~
  ::
      [%cell *]
    =+  lhs=$(sut p.sut)
    ?~  lhs
      $(sut q.sut)
    lhs
  ::
      [%core *]
    ::  cores don't have any doc structure inside of them. i probably need to
    ::  check that they're wrapped with a %hint type. so this just looks for
    ::  docs on the arms
    =+  arm=(find-arm-in-coil i.topics q.sut)
    ?~  arm
      ::  the current topic is not an arm in the core
      $(sut p.sut)
    ::  check to see if the arm is wrapped with a note
    =+  wat=(unwrap-note u.arm)
    `[%arm (trip i.topics) wat u.arm p.sut]
    ::  TODO: check for chapter docs
  ::
      [%face *]
    ?.  ?=(term p.sut)
      ::  TODO: handle tune case
      ~
    ?.  =(i.topics p.sut)
      ::  this face has a name, but not the one we're looking for
      ~
    ::  faces need to be checked to see if they're wrapped
    ~
  ::
      [%fork *]
    =/  types=(list type)  ~(tap in p.sut)
    |-
    ?~  types
      ~
    =+  res=^$(sut i.types)
    ?~  res
      $(types t.types)
    res
  ::
     [%hint *]
  ::  this is probably where most of the action should take place. it should
  ::  grab the docs from the hint and then look inside of the type to see where
  ::  it ought to go
    ~
  ::
     [%hold *]  $(sut (~(play ut p.sut) q.sut))
  ::
     %noun  ~
     %void  ~
  ==
::
:>  checks if a hoon is wrapped with a help note, and returns it if so
++  unwrap-note
  |=  gen=hoon
  ^-  what
  ?:  ?=([%note *] gen)
    ?:  ?=([%help *] p.gen)
      `crib.p.p.gen
    ~
  ~
::
:>  if arm-name is an arm in con, return its hoon and potentially the note wrapping it (?)
++  find-arm-in-coil
  |=  [arm-name=term con=coil]
  ^-  (unit hoon)
  ::
  =/  tomes=(list [p=term q=tome])  ~(tap by q.r.con)
  |-
  ?~  tomes
    ~
  =+  item=(~(get by q.q.i.tomes) arm-name)
  ?~  item
    $(tomes t.tomes)
::  ?:  =([%note *] u.item)  :: the arm is wrapped with a %note
::    [~ p.u.item u.item]  :: maybe i should check for the note later
  `u.item
::
:>    gets the documentation inside of a type
++  docs-from-type
  |=  sut=type
  ^-  what
  ?+  sut  ~
    [%core *]  ~  :: should this get the chapter docs?
    [%hint *]  ?>(?=(%help -.q.p.sut) `crib.p.q.p.sut)
    [%hold *]  $(sut (~(play ut p.sut) q.sut))
  ==
:>    grabs the docs for an arm.
++  select-arm-docs
  |=  [arm-doc=what gen=hoon sut=type]
  ^-  [what what what]
  =+  foot-type=(~(play ut sut) gen)
  =/  raw-doc=what  (docs-from-type foot-type)
  ::  if the arm builds a core, get the docs for the default arm
  ::  in that core
  =/  core-doc=what
    ?.  ?=([%core *] foot-type)
      ~
    (docs-from-type (~(play ut foot-type) [%limb %$]))
  ::  i think at least one of these will always be empty
  :+  arm-doc  raw-doc  core-doc
::
:>    returns an overview of the arms in a specific chapter
++  arms-in-chapter
  |=  [sut=type con=coil chapter-id=term]
  ^-  overview
  =/  chapter-tome  (~(got by q.r.con) chapter-id)
  (sort-overview (arms-as-overview q.chapter-tome sut))
::
:>    sort items in an overview in alphabetical order
++  sort-overview
  |=  ovr=overview
  ^-  overview
  %+  sort  ovr
    |=  [lhs=overview-item rhs=overview-item]
    (aor (get-overview-name lhs) (get-overview-name rhs))
::
:>    returns the name of an overview
++  get-overview-name
  |=  ovr=overview-item
  ?-  ovr
    [%header *]  ""
    [%item *]    name.ovr
  ==
::
:>    translate a tome into an overview
++  arms-as-overview
  |=  [a=(map term hoon) sut=type]
  ^-  overview
  *overview
::
::  :>  #
::  :>  #  %printing
::  :>  #
::  :>    functions which display output of various types
::  +|  %printing
:>    prints a doccords item
++  print-item
  |=  =item
  ^-  tang
  ?-  item
    [%view *]     (print-overview items.item)
    [%core *]     (print-core +.item)
    [%arm *]      (print-arm +.item)
    [%chapter *]  (print-chapter +.item)
    [%face *]     (print-face +.item)
  ==
::
:>    renders documentation for a full core
++  print-core
  |=  [name=tape docs=what sut=type con=coil uit=(unit item)]
  ^-  tang
  *tang
::
++  print-chapter
  |=  [name=tape doc=what sut=type con=coil chapter-id=term]
  ^-  tang
  ;:  weld
    (print-header name doc)
  ::
    ?~  doc
      ~
    (print-sections q.u.doc)
  ::
    =+  arms=(arms-in-chapter sut con chapter-id)
    ?~  arms
      ~
    (print-overview [%header `['arms:' ~] arms]~)
  ==
::
:>    renders documentation for a single arm in a core
++  print-arm
  |=  [name=tape docs=what gen=hoon sut=type]
  ^-  tang
  =+  [main-doc raw-doc product-doc]=(select-arm-docs docs gen sut)
  %+  weld
    (print-header name main-doc)
    ?~  product-doc
      ~
    %+  weld
      `tang`[[%leaf ""] [%leaf "product:"] ~]
      (print-header "" product-doc)
::
++  print-face
  |=  [name=tape doc=what children=(unit item)]
  ^-  tang
  %+  weld
    (print-header name doc)
    ?~  children
      ~
    (print-item u.children)
::
++  print-header
  |=  [name=tape doc=what]
  ^-  tang
  ?~  name
    ?~  doc
      [%leaf "(undocumented)"]~
    %+  weld
     `tang`[%leaf "{(trip p.u.doc)}"]~
     (print-sections q.u.doc)
  ?~  doc
    [%leaf name]~
  %+  weld
    `tang`[%leaf "{name}: {(trip p.u.doc)}"]~
    (print-sections q.u.doc)
::
++  print-overview
  |=  ovr=overview
  ^-  tang
  *tang
::
:>    renders a list of sections as tang
:>
:>  prints the longform documentation
++  print-sections
  |=  sections=(list sect)
  ^-  tang
  =|  out=tang
  |-
  ?~  sections  out
  =.  out
    ;:  weld
      out
      `tang`[%leaf ""]~
      (print-section i.sections)
    ==
  $(sections t.sections)
::
:>    renders a sect as a tang
++  print-section
  |=  section=sect
  ^-  tang
  %+  turn  section
  |=  =pica
  ^-  tank
  ?:  p.pica
    [%leaf (trip q.pica)]
  [%leaf "    {(trip q.pica)}"]
--
