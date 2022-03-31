::  A library for printing doccords
=/  debug  &
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
:>  +signify to describe that part of the type
++  find-item-in-type
  ::  TODO make this work with a list of topics
  |=  [topic=term sut=type]
  ^-  (unit item)
  ::  ?~  topics
  ::    ::  we have no more search paths TODO: return the rest as an overview
  ::    (signify sut)
  ?-  sut
      %noun      ~
      %void      ~
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
    ::  docs on the arms.
    ::
    ::  cores also can't have names right now. %brcn and %brpt have a slot in
    ::  their AST for the name, but it doesn't end up in the their type. so
    ::  if i want core names right now, i'd have to look inside their AST, and
    ::  only |% and |@ cores would have names, which is silly.
    =+  arm=(find-arm-in-coil topic q.sut)
    ?~  arm
      ::  the current topic is not an arm in the core
      $(sut p.sut)
    ::  check to see if the arm is wrapped with a note
    ::  TODO: this is taking docs from the AST rather than the type. is that
    ::  the right thing to do here?
    =+  wat=(unwrap-note u.arm)
    `[%arm (trip topic) wat u.arm p.sut]
   :: `[%arm (trip topic) wat u.arm q.q.sut]  :: what's the difference if i use the type in the coil?
    ::  TODO: check for chapter docs
  ::
      [%face *]
    ?.  ?=(term p.sut)
      ::  TODO: handle tune case
      ~
    ?.  =(topic p.sut)
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
   ::  If we found a help hint, it is wrapping a type for which we might want to
   ::  produce an item, so we should peek inside of it to see what type it is
   ::  and grab the docs from +signify
   ::
   ::  check to see if type inside the hint is a match
   ::  TODO: should i be trying to match both types in the hint?
   ::  TODO: actually hints can be nested, if e.g. an arm has a product with a hint, whose
   ::  product also has a hint. so this won't actually work for nested hints as written
   ?:  (shallow-match topic q.sut)
     =/  wat=what  (unwrap-hint sut)
     =/  itm=(unit item)  (signify q.sut)
     ?~  itm
       ~
     `(emblazon u.itm wat)
::     (emblazon (need (signify q.sut)) (unwrap-hint sut))
   $(sut q.sut)
  ::
     [%hold *]  $(sut (~(play ut p.sut) q.sut))
  ::
  ==
::
:>    non-recursive check to see if type matches search
:>
:>  this is for applying help hints to types when searching for a match. hints
:>  are only for the type theyre immediately wrapping, not something nested
:>  deeper, so we dont always want to recurse
++  shallow-match
  |=  [topic=term sut=type]
  ^-  ?
  ?+  sut  %.n
    [%atom *]  %.n  :: should we allow doccords on individual atoms? i think they should be for faces
    [%core *]  !=(~ (find ~[topic] (sloe sut)))
    [%face *]  ?.  ?=(term p.sut)
                 %.n  :: TODO: handle tune case
               =(topic p.sut)
  ==
::
:>    changes a type into a item
:>
:>  this does not actually assign the docs, since they usually come from a hint
:>  wrapping the type.
++  signify
  |=  sut=type
  ^-  (unit item)
  ?-    sut
  ::
      [%atom *]  ~
    ::
      [%cell *]
    %+  join-items
      $(sut p.sut)
    $(sut q.sut)
  ::
      [%core *]
    =/  name  ~  :: should check if core is built with an arm and use that name?
    =*  compiled-against  $(sut p.sut)
    `[%core (trip name) *what p.sut q.sut compiled-against]
  ::
      [%face *]
    ?.  ?=(term p.sut)
      ~  ::  TODO: handle tune case
    =*  compiled-against  $(sut q.sut)
    `[%face (trip p.sut) *what compiled-against]
  ::
      [%fork *]
    =*  types  ~(tap in p.sut)
    =*  items  (turn types signify)
    (roll items join-items)
    ::
      [%hint *]
    =*  rest-type  $(sut q.sut)
    ::  check to see if it is a help hint
    ?>  ?=(%help -.q.p.sut)
    `[%view [%header `crib.p.q.p.sut (item-as-overview rest-type)]~]
    ::
      [%hold *]  $(sut (~(play ut p.sut) q.sut))
      %noun  ~
      %void  ~
  ==

:>    checks if a hoon is wrapped with a help note, and returns it if so
++  unwrap-note
  |=  gen=hoon
  ^-  what
  ?:  ?=([%note *] gen)
    ?:  ?=([%help *] p.gen)
      `crib.p.p.gen
    ~
  ~
::
:>    checks if a hint type is a help hint and returns the docs if so
++  unwrap-hint
  |=  sut=type
  ^-  what
  ::  should I care what the type in the (pair type note) is?
  ?.  ?=([%hint *] sut)
    ~?  >  debug  %not-hint-type
    ~
  ?>(?=(%help -.q.p.sut) `crib.p.q.p.sut)
::
:>    inserts docs into an item
:>
:>  most docs are going to be found in hint types wrapping another type. when
:>  we come across a hint, we grab the docs from the hint and then build the
:>  item for the type it wrapped. since building an item is handled separately,
:>  this item will initially have no docs in it, so we add it in after with this
:>
:>  the exceptions to this are %chapter and %view items. chapters have an axis
:>  for docs in their $tome structure, and %views are summaries of several types
++  emblazon
  |=  [=item =what]
  ~?  >>  debug  %emblazon
  ^+  item
  ?+  item  item  :: no-op on %chapter and %view
    ?([%core *] [%arm *] [%face *])  ?~  docs.item
                                       item(docs what)
                                     ~?  >  debug  %docs-in-item
                                     item(docs what)
  ==
::
:>    looks for an arm in a coil and returns its hoon
++  find-arm-in-coil
  |=  [arm-name=term con=coil]
  ~?  >>  debug  %find-arm-in-coil
  ^-  (unit hoon)
  =/  tomes=(list [p=term q=tome])  ~(tap by q.r.con)
  |-
  ?~  tomes
    ~
  =+  gen=(~(get by q.q.i.tomes) arm-name)
  ?~  gen
    $(tomes t.tomes)
  `u.gen
::
:>    gets the documentation inside of a type
++  what-from-type
  :>  testing
  |=  sut=type
  ^-  what
  ?+    sut
      ~
  ::
      [%core *]
    ~?  >>  debug  %what-from-type-core  ~  :: should this get the chapter docs?
  ::
      [%hold *]
    ~?  >>  debug  %what-from-type-hold  $(sut (~(play ut p.sut) q.sut))
  ::
      [%hint *]
    ~?  >>  debug  :-  %what-from-type-hint  -.q.p.sut
    ?:  ?=(%help -.q.p.sut)  `crib.p.q.p.sut  ~
  ==
::
:>    grabs the docs for an arm.
:>
:>  there are three possible places with relevant docs for an arm:
:>  docs for the arm itself, docs for the product of the arm, and
:>  if the arm builds a core, docs for the default arm of that core.
:>
:>  arm-doc: docs wrapping the arm - this should have already been found
:>  product-doc: docs for the product of the arm
:>  core-doc: docs for the default arm of the core produced by the arm
++  select-arm-docs
  |=  [arm-doc=what gen=hoon sut=type]
  ~?  >>  debug  %select-arm-docs
  ^-  [what what what]
  =+  hoon-type=(~(play ut sut) gen)
  ::~?  >>>  debug  hoon-type
  =/  product-doc=what  (what-from-type hoon-type)
  ~?  >  debug  product-doc
  ::  if the arm builds a core, get the docs for the default arm
  ::  in that core
  ::
  ::  this is broken if it also has an arm-doc. if i have an arm-doc, i
  ::  need to strip off the first hint and then check if i have a core, i think
  =/  core-doc=what
    :: if product-doc is empty, just go right on and check if its a core
    ?~  product-doc
      ?.  ?=([%core *] hoon-type)
        ~?  >  debug  %no-core-product
        ~
      (what-from-type (~(play ut hoon-type) [%limb %$]))
    :: if there is a product doc, then step through the hint and check for a core
    ?:  ?=([%hint *] hoon-type)
      ~?  >  debug  %step-through-hint
      ::=+  inner-type=(~(play ut q.hoon-type) gen)
      ::=/  res  (mule |.((~(play ut inner-type) [%limb %$])))
      ::  pretty sure having to use mule twice here means im doing something wrong
      =/  h-res  (mule |.((~(play ut q.hoon-type) gen)))
      ?-    -.h-res
          %|
        ~
      ::
          %&
        =/  in-res  (mule |.((~(play ut p.h-res) [%limb %$])))
        ?-  -.in-res
          %|  ~
          %&  (what-from-type p.in-res)
        ==
      ==
    ~
  ::  i think arm-doc and product-doc might always be the same
  ::  upon further reflection, i think this is a limitation of the wrapping
  ::  approach
  ~?  >  debug  :*  %arm-doc      arm-doc
                    %product-doc  product-doc
                    %core-doc     core-doc
                ==
  :+  arm-doc  product-doc  core-doc
::
:>    returns an overview for a cores arms and chapters
:>
:>  returns an overview for arms which are part of unnamed chapters, and
:>  an overview of the named chapters
++  arm-and-chapter-overviews
  |=  [sut=type con=coil core-name=tape]
  ^-  [overview overview]
  =|  arm-docs=overview
  =|  chapter-docs=overview
  =/  tomes  ~(tap by q.r.con)
  |-
  ?~  tomes
    [(sort-overview arm-docs) (sort-overview chapter-docs)]
  =*  current  i.tomes  ::[term tome]
  ?~  p.current
    :: chapter has no name. add documentation for its arms to arm-docs
    =.  arm-docs  (weld arm-docs (arms-as-overview q.q.current sut))
    $(tomes t.tomes)
  ::  chapter has a name. add to list of chapters
  =.  chapter-docs
    %+  weld  chapter-docs
    ^-  overview
    [%item :(weld (trip -.current) ":" core-name) p.q.current]~
  $(tomes t.tomes)
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
  ::  currently this doesn't do anything until i implement arm-doc
  |=  [a=(map term hoon) sut=type]
  ^-  overview
  %+  turn  ~(tap by a)
  |=  ar=(pair term hoon)
  =/  doc  (select-arm-docs *what q.ar sut)  :: *what should be from the hint wrapper
  [%item (weld "++" (trip p.ar)) -.doc]
::
:>    changes an item into an overview
++  item-as-overview
  |=  uit=(unit item)
  ^-  overview
  ?~  uit  ~
  =+  itm=(need uit)
  ?-  itm
  ::
      [%view *]  items.itm
  ::
      [%core *]
    ?~  name.itm
      (item-as-overview children.itm)
    :-  [%item name.itm docs.itm]
    (item-as-overview children.itm)
  ::
      [%arm *]
    [%item name.itm docs.itm]~
  ::
      [%chapter *]
    [%item name.itm docs.itm]~
  ::
      [%face *]
    ?~  name.itm
      ~
    [%item name.itm docs.itm]~
  ==
::
:>    combines two (unit items) together
++  join-items
  |=  [lhs=(unit item) rhs=(unit item)]
  ^-  (unit item)
  ?~  lhs  rhs
  ?~  rhs  lhs
  `[%view (weld (item-as-overview lhs) (item-as-overview rhs))]
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
  =+  [arms chapters]=(arm-and-chapter-overviews sut con name)
  ;:  weld
    ::  cores don't have names
    (print-header *tape *what)
  ::
    ?~  arms
      ~
    (print-overview [%header `['arms:' ~] arms]~)
  ::
    ?~  chapters
      ~
    (print-overview [%header `['chapters:' ~] chapters]~)
  ::
    =+  compiled=(item-as-overview uit)
    ?~  compiled
      ~
    (print-overview [%header `['compiled against: ' ~] compiled]~)
  ==
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
  ~?  >>  debug  %print-arm
  =+  [main-doc product-doc core-doc]=(select-arm-docs docs gen sut)
  ;:  weld
    (print-header name main-doc)
    `tang`[[%leaf ""] [%leaf "product:"] ~]
    (print-header "" product-doc)
    `tang`[[%leaf ""] [%leaf "default arm in core:"] ~]
    (print-header "" core-doc)
  ==
::
:>    renders documentation for a face
++  print-face
  |=  [name=tape doc=what children=(unit item)]
  ^-  tang
  %+  weld
    (print-header name doc)
    ?~  children
      ~
    (print-item u.children)
::
:>    prints name and docs only
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
  |=  =overview
  ^-  tang
  =|  out=tang
  |-
  ?~  overview  out
  =/  oitem  i.overview
  ?-    oitem
      [%header *]
    %=  $
      overview  t.overview
      out  ;:  weld
             out
             ?~  doc.oitem  ~
             `tang`[[%leaf ""] [%leaf "{(trip p.u.doc.oitem)}"] ~]
             ?~  doc.oitem  ~
             (print-sections q.u.doc.oitem)
             ^$(overview children.oitem)
           ==
    ==
  ::
      [%item *]
    %=  $
      overview  t.overview
      out  ;:  weld
             out
             `tang`[[%leaf ""] [%leaf name.oitem] ~]
             ?~  doc.oitem  ~
             `tang`[[%leaf ""] [%leaf "{(trip p.u.doc.oitem)}"] ~]
             ?~  doc.oitem  ~
             (print-sections q.u.doc.oitem)
           ==
    ==
  ==
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
