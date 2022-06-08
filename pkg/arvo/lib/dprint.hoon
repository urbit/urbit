::  A library for printing doccords
=/  debug  |
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
            adoc=what
            pdoc=what
            cdoc=what
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
|%  %dprint
:>    contains arms used for looking for docs inside of a type
:>
:>  the entrypoint for finding docs within a type is +find-item-in-type.
+|  %searching
:>    returns the item to print while searching through topic
:>
:>  this gate is optionally called recursively to find the path (topic) in the
:>  type (sut). once it finds the correct part of the type, it switches to
:>  +signify to describe that part of the type. recursion is turned off (for some
:>  cases) when a hint type is found, in case it is wrapping a match.
++  find-item-in-type
  ::  TODO make this work with a list of topics
  |=  [topics=(list term) sut=type rec=?]
  ^-  (unit item)
  ~?  >  debug  topics
  ?~  topics
      ::  we have no more search paths, return an overview of what remains
      (signify sut)
  ?-  sut
      %noun      ~
      %void      ~
      [%atom *]  ~
  ::
      [%cell *]
    =+  lhs=$(sut p.sut)
    ?~  lhs
      ::  not sure if this should recurse when rec=%.n
      $(sut q.sut)
    lhs
  ::
      [%core *]
    ::  cores don't have any doc structure inside of them. i probably need to
    ::  check that they're wrapped with a %hint type. so this just looks for
    ::  docs on the arms.
    ::
    =+  core-name=p.p.q.sut
    ?:  !=(`i.topics core-name)
      ::  the current topic isn't the top level core name
      ::  else, look for an arm matching the name
      =+  arm=(find-arm-in-coil i.topics q.sut)
      ?~  arm
        ::  the current topic is not an arm in the core, recurse into sut
        ?:(rec $(sut p.sut) ~)
      :: else, return the arm as docs
      =+  [adoc pdoc cdoc]=(all-arm-docs u.arm sut (trip i.topics))
      `[%arm (trip i.topics) adoc pdoc cdoc u.arm p.sut]
    ::  the core name matches. check to see if there are any topics left
    ?~  t.topics
      ::  we matched the core name and have no further topics. return the core
      =*  compiled-against  (signify p.sut)
      `[%core (trip i.topics) *what sut q.sut compiled-against]
    ::  we matched the core name, but there are still topics left
    ::  check to see if one the chapters matches the next topic
    =+  chapters=~(key by q.r.q.sut)
    ?.  (~(has in chapters) i.t.topics)
      ::  the core name matched, but nothing inside of it did. return null
      ~
    ::  if there is a chapter with a name matching the topic, return chapter
    ::  as a (unit item)
    =/  docs=what  p:(~(got by q.r.q.sut) i.t.topics)
    `[%chapter (trip i.t.topics) docs sut q.sut i.t.topics]
  ::
      [%face *]
    ?.  ?=(term p.sut)
      ::  TODO: handle tune case
      ~
    ?.  =(i.topics p.sut)
      ::  this face has a name, but not the one we're looking for
      ~
    ?~  t.topics
      ::  we found a match, and there are no further topics
      ::  this might have been wrapped with a hint type, that case will handle
      ::  docs for this face
      `[%face (trip p.sut) *what (signify q.sut)]
    ::  the first topic matched the face, but there are more left.
    ?:(rec $(topics t.topics, sut q.sut) ~)
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
   ::
   ::  this should only be doing something for cores right now. you run into an
   ::  arm's name before you run into its docs
   ::
   =/  shallow-match=(unit item)  $(sut q.sut, rec %.n)
   ?~  shallow-match
     ::  hint isn't wrapping a match, so step through it
     $(sut q.sut, rec %.y)
   ::  hint was wrapping a match, so signify the type and emblazon it
  :: =/  wat=what  (unwrap-hint sut)
   `(emblazon u.shallow-match (unwrap-hint sut))
   ::=/  uitm=(unit item)  (signify q.sut)
   ::?~  uitm
   ::  ~
  ::
     [%hold *]  $(sut (~(play ut p.sut) q.sut))
  ::
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
    =/  name=(unit term)  p.p.q.sut  :: should check if core is built with an arm and use that name?
    =*  compiled-against  $(sut p.sut)
    ?~  name
      `[%core ~ *what sut q.sut compiled-against]
    `[%core (trip u.name) *what sut q.sut compiled-against]
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
:>  when matching for a core or a face type, the docs for that type will be in
:>  a hint that wraps it. thus we end up producing an item for that type, then
:>  need to add the docs to it.
++  emblazon
  |=  [=item =what]
  ~?  >>  debug  %emblazon
  ^+  item
  ?+  item  item  :: no-op on %chapter, %arm, $view
    ?([%core *] [%face *])  item(docs what)
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
:>    gets the documentation inside of a hint, or a hold that plays to a hint
++  what-from-hint
  |=  sut=type
  ^-  what
  ?-    sut
   ::   ~?  >  debug  %what-from-hint-miss  ~
      %noun  ~?  >>  debug  %what-from-hint-noun-miss  ~
      %void  ~?  >>  debug  %what-from-hint-void-miss  ~
  ::
      [%core *]
    ~?  >>  debug  %what-from-hint-core-miss  ~
  ::
      [%cell *]
    ~?  >>  debug  %what-from-hint-cell-miss  ~
  ::
      [%face *]
    ~?  >>  debug  %what-from-hint-face-miss  ~
  ::
      [%fork *]
    ~?  >>  debug  %what-from-hint-fork-miss  ~
  ::
      [%atom *]
    ~?  >>  debug  %what-from-hint-atom-miss  ~
  ::
      [%hold *]
    ~?  >>  debug  %what-from-hint-hold  $(sut (~(play ut p.sut) q.sut))
  ::
      [%hint *]
    ~?  >>  debug  :-  %what-from-hint-hint  -.q.p.sut
    ?:  ?=(%help -.q.p.sut)  `crib.p.q.p.sut  ~
  ==
:>    gets the $help from a %help %hint type and returns it as a unit
++  help-from-hint
  |=  sut=type
  ^-  (unit help)
  ?+    sut  ~
      [%hold *]
    ~?  >>  debug  %help-from-hold  $(sut (~(play ut p.sut) q.sut))
  ::
      [%hint *]
    ~?  >>  debug  %help-from-hint
    ?.  ?=(%help -.q.p.sut)
      ~
    `p.q.p.sut
  ==
::
:>    returns 0, 1, or 2 whats for an arm
:>
:>  this arm should be handed the compiled type of the hoon of an arm, as well
:>  as the name of that arm. it checks for up to 2 nested hints on the outermost
:>  layer of the type. if you have 2, it is presumed to be arm-doc followed by
:>  product-doc. if you only have one, we check links in the $help of the hint
:>  to determine whether it is an arm doc or product doc.
:>
:>  this returns ~ if there are no docs. if there are docs, the first one is the
:>  arm-doc, and the second one is the product-doc.
++  arm-product-docs
  |=  [sut=type name=term]
  ^-  (unit [what what])
  =/  doc-one=(unit help)
    (help-from-hint sut)
  ?~  doc-one
    ~?  >  debug  %doc-one-empty
    ~  ::  no need to look for a second doc if there is no first one
  ?:  =(~ links.u.doc-one)
      :: doc-one is a product-doc
      [~ [~ `crib.u.doc-one]]
  ?:  !=([%funk name] -.links.u.doc-one)
    :: link on doc-one doesnt match arm name, so that means its calling a
    :: different arm and trying to use its docs. don't let it
    ~
  ::  technically doc-two doesn't need to be a help, i could just grab the what
  ::  directly since we aren't testing it to see if its an arm-doc, but it makes
  ::  the code more confusing to use a different structure.
  ~?  >  debug  :-  %doc-one  doc-one
  =/  doc-two=(unit help)
    ?+    sut  ~
        [%hint *]
      (help-from-hint p.p.sut)
        [%hold *]
      ~?  >  debug  %doc-two-hold
      ~
    ==
  ?~  doc-two
    ~?  >  debug  %doc-two-empty
    ?~  links.u.doc-one
      :: if links are empty, doc-one is a product-doc
      ~?  >  debug  %link-empty
      [~ [~ `crib.u.doc-one]]
    ?:  =([%funk name] i.links.u.doc-one)
      :: if links are non-empty, check that the link is for the arm
      ~?  >  debug  %link-match
      [~ [`crib.u.doc-one ~]]
    ~?  >  debug  %link-doesnt-match-arm
    ::  this can happen if +bar calls +foo which has doccords
    [~ [`crib.u.doc-one ~]]
  ::  doc-two is non-empty. make sure that doc-one is an arm-doc
  ?~  links.u.doc-one
    ~?  >  debug  %doc-one-empty-link
    [~ [`crib.u.doc-one `crib.u.doc-two]]
  [~ [`crib.u.doc-one `crib.u.doc-two]]
::
:>    grabs the docs for an arm.
:>
:>  there are three possible places with relevant docs for an arm:
:>  docs for the arm itself, docs for the product of the arm, and
:>  if the arm builds a core, docs for the default arm of that core.
:>
:>  arm-doc: docs written above the the arm
:>  product-doc: docs for the product of the arm
:>  core-doc: docs for the default arm of the core produced by the arm
:>  this will be the first of the arm-doc or product-doc on the default
:>  arm. maybe this should be recursive and/or give both but its a decision
:>  ill leave for later
++  all-arm-docs
  |=  [gen=hoon sut=type name=tape]
  ~?  >  debug  %all-arm-docs
  ^-  [what what what]
  =+  hoon-type=(~(play ut sut) gen)
  =+  arm-prod=(arm-product-docs hoon-type `@tas`(crip name))
  ::~?  >>  debug  :-  %arm-prod  arm-prod
  |^
  :: check arm-prod to determine how many layers to look into the type
  :: for core docs
  =/  depth=@  ?~  arm-prod  0
    (add =(~ +<.arm-prod) =(~ +>.arm-prod))
  ?+  depth  ``~
    %0  ``(extract hoon-type)
    %1  :+  +<.arm-prod
          +>.arm-prod
        ?>  ?=([%hint *] hoon-type)
        (extract q.hoon-type)
    %2  :+  +<.arm-prod
          +>.arm-prod
        ?>  ?=([%hint *] hoon-type)
        ?>  ?=([%hint *] q.hoon-type)
        (extract q.q.hoon-type)
  ==
  :>    grabs the first doc for the default arm of a core
  :>
  :>  this could end up being an arm doc or a product doc.
  ++  extract
    |=  sut=type
    ^-  what
    ?.  ?=([%core *] sut)
      ~?  >  debug  %no-nested-core  ~
    ~?  >  debug  %found-nested-core
    =+  carm=(find-arm-in-coil %$ q.sut)
    ?~  carm  ~?  >  debug  %empty-carm  ~
    ~?  >  debug  %found-default-arm
    ::~?  >>>  debug  u.carm
    =+  carm-type=(~(play ut p.sut) u.carm)
    (what-from-hint carm-type)
  --
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
  |=  [a=(map term hoon) sut=type]
  ^-  overview
  %+  turn  ~(tap by a)
  |=  ar=(pair term hoon)
  =+  [adoc pdoc cdoc]=(all-arm-docs q.ar sut (trip p.ar))
  [%item (weld "++" (trip p.ar)) adoc]
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
    [%item name.itm adoc.itm]~
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
::
+|  %printing
:>    prints a doccords item
++  print-item
  |=  =item
  ~?  >>  debug  %print-item
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
    (print-header name docs)
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
  ~?  >  debug  %print-chapter
  ;:  weld
    (print-header name doc)
  ::
    ::?~  doc
    ::  ~
    ::(print-sections q.u.doc)
  ::
    =+  arms=(arms-in-chapter sut con chapter-id)
    ?~  arms
      ~
    (print-overview [%header `['arms:' ~] arms]~)
  ==
::
:>    renders documentation for a single arm in a core
++  print-arm
  |=  [name=tape adoc=what pdoc=what cdoc=what gen=hoon sut=type]
  ^-  tang
  ~?  >>  debug  %print-arm
  ;:  weld
    (print-header name adoc)
    `tang`[[%leaf ""] [%leaf "product:"] ~]
    (print-header "" pdoc)
    `tang`[[%leaf ""] [%leaf "default arm in core:"] ~]
    (print-header "" cdoc)
  ==
::
:>    renders documentation for a face
++  print-face
  |=  [name=tape doc=what children=(unit item)]
  ^-  tang
  ~?  >>  debug  %print-face
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
  ~?  >>  debug  %print-header
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
  ~?  >>  debug  %print-overview
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
             ::?~  doc.oitem  ~
             ::(print-sections q.u.doc.oitem)
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
             ::?~  doc.oitem  ~
             ::(print-sections q.u.doc.oitem)
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
