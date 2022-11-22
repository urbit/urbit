/-  *sole
::    a library for printing doccords
::
=/  debug  |
=>
  ::    dprint-types
  |%
  ::    $overview: an overview of all named things in the type.
  ::
  ::  each element in the overview list is either a documentation for a sublist
  ::  or an association betwen a term and documentation for it
  +$  overview  (list overview-item)
  ::
  ::  $overview-item: an element of an overview
  +$  overview-item
    $%  [%header doc=what children=overview]
        [%item name=tape doc=what]
    ==
  ::
  ::  $item: the part of a type being inspected
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
        ==
    ==
  ::
  --
::    dprint
::
::  core containing doccords search and printing utilities
|%
::    contains arms used for looking for docs inside of a type
::
::  the entrypoint for finding docs within a type is +find-item-in-type.
+|  %searching
::    +find-item-in-type: returns the item to print while searching through topic
::
::  this gate is a thin wrapper around _hunt for usability, since the only entry
::  point most users should care about is find-item:hunt
::
++  find-item-in-type
  |=  [topics=(list term) sut=type]
  ?~  topics  !!
  =/  top=(lest term)  topics
  ~(find-item hunt [top sut])
::
::    +hunt: door used for refining the type while searching for doccords
::
++  hunt
  |_  [topics=(lest term) sut=type]
  ++  this  .
  ::+|  %find
  ++  find-item
    ^-  (unit item)
    ?-    sut
        %noun      ~
        %void      ~
        [%atom *]  ~
        [%cell *]  find-cell
        [%core *]  find-core
        [%face *]  find-face
        [%fork *]  find-fork
        [%hint *]  find-hint
        [%hold *]  find-item:this(sut (~(play ut p.sut) q.sut))
    ==
  ::
  ++  find-cell
    ?>  ?=([%cell *] sut)
    =/  lhs  find-item:this(sut p.sut)
    ?~  lhs
      find-item:this(sut q.sut)
    lhs
  ::
  ++  find-core
    ?>  ?=([%core *] sut)
    ?:  check-arm
      ?:  check-search
        ?:  check-arm-core
          return-arm-core
        return-arm
      recurse-arm-core
    ?:  check-chap
      ?:  check-search
        return-chap
      recurse-chap
    recurse-core
  ::
  ++  find-face
    ?>  ?=([%face *] sut)
    ?.  ?=(term p.sut)
      ::TODO: handle $tune case
      find-item:this(sut q.sut)
    ?.  =(i.topics p.sut)
      ~
    ?~  t.topics
      return-face
    find-item:this(sut q.sut, topics t.topics)
  ::
  ++  find-fork
    ?>  ?=([%fork *] sut)
    =/  types=(list type)  ~(tap in p.sut)
    |-
    ?~  types  ~
    =+  res=find-item:this(sut i.types)
    ?~  res
      $(types t.types)
    res
  ::
  ++  find-hint
    |^
    ?>  ?=([%hint *] sut)
    ?.  ?=([%help *] q.p.sut)
      find-item:this(sut q.sut)
    ?+    q.sut  ~
        [%cell *]  find-cell:this(sut q.sut)
        [%core *]  find-hint-core
        [%face *]  find-hint-face
        [%fork *]  find-fork:this(sut q.sut)
        [%hint *]  find-hint:this(sut q.sut)
        [%hold *]  find-hint:this(q.sut (~(play ut p.q.sut) q.q.sut))
    ==
    ::
    ++  find-hint-core
      ?>  &(?=([%hint *] sut) ?=([%help *] q.p.sut) ?=([%core *] q.sut))
      ::
      ?.  ?&  ((sane %tas) summary.crib.p.q.p.sut)
              =(summary.crib.p.q.p.sut i.topics)
          ==
        find-core:this(sut q.sut)
      ?~  t.topics
        return-hint-core
      find-item:this(sut q.sut, topics t.topics)
    ::
    ++  find-hint-face
      ?>  &(?=([%hint *] sut) ?=([%help *] q.p.sut) ?=([%face *] q.sut))
      ?:  check-face:this(sut q.sut)
        ?~  t.topics
          return-hint-face
        find-item:this(sut q.q.sut, topics t.topics)
      find-item:this(sut q.q.sut)
  --
  ::
  ::+|  %recurse
  ++  recurse-core
    ?>  ?=([%core *] sut)
    find-item:this(sut p.sut)
  ++  recurse-chap
    ?>  ?=([%core *] sut)
    ?~  t.topics  !!
    find-item:this(topics t.topics)
  ++  recurse-arm-core
    ?>  ?=([%core *] sut)
    ?~  t.topics  !!
    find-item:this(sut arm-type, topics t.topics)
  ::
  ::+|  %check
  ++  check-arm  !=(~ (find ~[i.topics] (sloe sut)))
  ++  check-chap
    ?>  ?=([%core *] sut)
    (~(has by q.r.q.sut) i.topics)
  ++  check-face
    ?>  ?=([%face *] sut)
    ?.  ?=(term p.sut)
      ::TODO: handle $tune case
      %.n
    =(p.sut i.topics)
  ++  check-search  =(~ t.topics)
  ++  check-arm-core
    ^-  ?
    =+  arm-list=(sloe (~(play ut sut) arm-hoon))
    &(!=(arm-list ~) !=(arm-list ~[%$]))
  ::
  ::+|  %return
  ++  return-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =*  compiled-against  find-item:this(sut p.sut)
    `[%core (trip i.topics) *what sut q.sut compiled-against]
  ::
  ++  return-face
    ^-  (unit item)
    ?>  ?=([%face *] sut)
    ::  TODO: handle tune case
    ?>  ?=(term p.sut)
    =*  compiled-against  return-item:this(sut q.sut)
    `[%face (trip p.sut) *what compiled-against]
  ::
  ++  return-fork
    ^-  (unit item)
    ?>  ?=([%fork *] sut)
    =*  types  ~(tap in p.sut)
    =*  items  (turn types |=(a=type return-item:this(sut a)))
    (roll items join-items)
  ::
  ++  return-hint
    ^-  (unit item)
    ?>  ?=([%hint *] sut)
    =*  res  return-item:this(sut q.sut)
    ?.  ?=([%help *] q.p.sut)
      ~
    `[%view [%header `crib.p.q.p.sut (item-as-overview res)]~]
  ::
  ++  return-arm
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =+  [adoc pdoc cdoc]=(all-arm-docs arm-hoon sut (trip i.topics))
    ::TODO: should this p.sut be sut? or the compiled type of the arm?
    `[%arm (trip i.topics) adoc pdoc cdoc arm-hoon p.sut]
  ::
  ++  return-chap
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =/  tom=tome  (~(got by q.r.q.sut) i.topics)
    `[%chapter (trip i.topics) p.tom sut q.sut]
  ::
  ++  return-arm-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =+  [adoc pdoc cdoc]=(all-arm-docs arm-hoon sut (trip i.topics))
    =/  dox=what  ?~(adoc ?~(pdoc ~ pdoc) adoc)
    =/  at  arm-type
    ?>  ?=([%core *] at)
    =*  compiled-against  return-item:this(sut p.sut)
    `[%core (trip i.topics) dox at q.at compiled-against]
  ::
  ++  return-item
    ^-  (unit item)
    ?-    sut
        %noun      ~
        %void      ~
        [%atom *]  ~
        [%cell *]  (join-items return-item:this(sut p.sut) return-item:this(sut q.sut))
        [%core *]  return-core
        [%face *]  return-face
        [%fork *]  return-fork
        [%hint *]  return-hint
        [%hold *]  return-item:this(sut (~(play ut p.sut) q.sut))
    ==
  ::
  ++  return-hint-core
    ^-  (unit item)
    ?>  &(?=([%hint *] sut) ?=([%core *] q.sut))
    (apply-hint return-core:this(sut q.sut))
  ::
  ++  return-hint-face
    ^-  (unit item)
    ?>  &(?=([%hint *] sut) ?=([%face *] q.sut))
    (apply-hint return-face:this(sut q.sut))
  ::
  ++  apply-hint
    |=  uit=(unit item)
    ^-  (unit item)
    ?~  uit  ~
    ?>  &(?=([%hint *] sut) ?=([%help *] q.p.sut))
    ?+  u.uit  ~
      ?([%core *] [%face *])  (some u.uit(docs `crib.p.q.p.sut))
    ==
  ::
  ::+|  %misc
  ++  arm-hoon
    ^-  hoon
    ?>  ?=([%core *] sut)
    (need (^arm-hoon i.topics q.sut))
  ::
  ++  arm-type
    ^-  type
    ?>  ?=([%core *] sut)
    (~(play ut sut) arm-hoon)
  --
::
::  +arm-hoon: looks for an arm in a coil and returns its hoon
++  arm-hoon
  |=  [arm-name=term con=coil]
  ~?  >>  debug  %arm-hoon
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
::  +help-from-hint: gets the help from a %help $hint and returns it as a unit
++  help-from-hint
  |=  sut=type
  ^-  (unit help)
  ?+    sut  ~
      [%hold *]
    ~?  >>  debug  %help-from-hold
    $(sut (~(play ut p.sut) q.sut))
  ::
      [%hint *]
    ~?  >>  debug  %help-from-hint
    ?.  ?=(%help -.q.p.sut)
      ~
    `p.q.p.sut
  ==
::
::    +arm-product-docs: returns 0, 1, or 2 whats for an arm
::
::  this arm should be handed the compiled type of the hoon of an arm, as well
::  as the name of that arm. it checks for up to 2 nested hints on the outermost
::  layer of the type. if you have 2, it is presumed to be arm-doc followed by
::  product-doc. if you only have one, we check .cuff in the $help of the hint
::  to determine whether it is an arm doc or product doc.
::
::  this returns ~ if there are no docs. if there are docs, the first one is the
::  arm-doc, and the second one is the product-doc.
::
++  arm-product-docs
  |=  [sut=type name=term]
  ^-  (unit [what what])
  =/  doc-one=(unit help)
    (help-from-hint sut)
  ?~  doc-one
    ~?  >  debug  %doc-one-empty
    ~  ::  no need to look for a second doc if there is no first one
  ?:  =(~ cuff.u.doc-one)
      :: doc-one is a product-doc
      [~ [~ `crib.u.doc-one]]
  ?:  !=(name ->.cuff.u.doc-one)
    :: link on doc-one doesnt match arm name, so that means its calling a
    :: different arm and trying to use its docs. don't let it
    ~
  ~?  >  debug  :-  %doc-one  doc-one
  =/  doc-two=(unit help)
    ?.  ?=([%hint *] sut)
      ~
    (help-from-hint q.sut)
  ?~  doc-two
    ~?  >  debug  %doc-two-empty
    :: if .cuff is non-empty, check that the link is for the arm
    ?:  =(name ->.cuff.u.doc-one)
      ~?  >  debug  %link-match
      [~ [`crib.u.doc-one ~]]
    ~?  >  debug  %link-doesnt-match-arm
    ::  this can happen if +bar calls +foo which has doccords
    [~ [`crib.u.doc-one ~]]
  ::  doc-two is non-empty. make sure that doc-one is an arm-doc for this arm
  ?>  =(name ->.cuff.u.doc-one)
  [~ [`crib.u.doc-one `crib.u.doc-two]]
::
::    +all-arm-docs: grabs the docs for an arm.
::
::  there are three possible places with relevant docs for an arm:
::  docs for the arm itself, docs for the product of the arm, and
::  if the arm builds a core, docs for the default arm of that core.
::
::  arm-doc: docs written above the the arm
::  product-doc: docs for the product of the arm
::  core-doc: docs for the default arm of the core produced by the arm
::  this will be the first of the arm-doc or product-doc on the default
::  arm. maybe this should be recursive and/or give both but its a decision
::  ill leave for later
::
++  all-arm-docs
  |=  [gen=hoon sut=type name=tape]
  ~?  >  debug  %all-arm-docs
  ^-  [what what what]
  =+  hoon-type=(~(play ut sut) gen)
  =+  arm-prod=(arm-product-docs hoon-type `@tas`(crip name))
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
  ::    +extract: grabs the first doc for the default arm of a core
  ::
  ::  this could end up being an arm doc or a product doc.
  ::
  ++  extract
    |=  sut=type
    ^-  what
    ?.  ?=([%core *] sut)
      ~?  >  debug  %no-nested-core  ~
    ~?  >  debug  %found-nested-core
    =+  carm=(arm-hoon %$ q.sut)
    ?~  carm  ~?  >  debug  %empty-carm  ~
    ~?  >  debug  %found-default-arm
    =+  carm-type=(~(play ut sut) u.carm)
    =/  hel=(unit help)  (help-from-hint carm-type)
    ?~  hel
      ~
    `what``crib.u.hel
  --
::
::    +arm-and-chapter-overviews: returns an overview of a cores contents
::
::  returns an overview for arms which are part of unnamed chapters, and
::  an overview of the named chapters
::
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
    [%item :(weld "^" core-name "|" (trip -.current)) p.q.current]~
  $(tomes t.tomes)
::
::  +arms-in-chapter: returns an overview of the arms in a specific chapter
++  arms-in-chapter
  |=  [sut=type con=coil name=tape]
  ^-  overview
  =/  tom=tome  (~(got by q.r.con) (crip name))
  (sort-overview (arms-as-overview q.tom sut))
::
::  +sort-overview: sort items in an overview in alphabetical order
++  sort-overview
  |=  ovr=overview
  ^-  overview
  %+  sort  ovr
  |=  [lhs=overview-item rhs=overview-item]
  (aor (get-overview-name lhs) (get-overview-name rhs))
::
::  +get-overview-name: returns the name of an overview
++  get-overview-name
  |=  ovr=overview-item
  ?-  ovr
    [%header *]  ""
    [%item *]    name.ovr
  ==
::
::  +arms-as-overview: translate a tome into an overview
++  arms-as-overview
  |=  [a=(map term hoon) sut=type]
  ^-  overview
  %+  turn  ~(tap by a)
  |=  ar=(pair term hoon)
  =+  [adoc pdoc cdoc]=(all-arm-docs q.ar sut (trip p.ar))
  [%item (weld "+" (trip p.ar)) adoc]
::
::  +item-as-overview: changes an item into an overview
++  item-as-overview
  |=  uit=(unit item)
  ~?  >>  debug  %item-as-overview
  ^-  overview
  ?~  uit  ~
  =+  itm=(need uit)
  ?-  itm
      [%view *]  items.itm
  ::
      [%core *]
    ?~  name.itm
      (item-as-overview children.itm)
    :-  [%item (weld "^" name.itm) docs.itm]
    (item-as-overview children.itm)
  ::
      [%arm *]
    [%item (weld "+" name.itm) adoc.itm]~
  ::
      [%chapter *]
    [%item (weld "|" name.itm) docs.itm]~
  ::
      [%face *]
    ?~  name.itm
      ~
    [%item (weld "." name.itm) docs.itm]~
  ==
::
::  +join-items: combines two (unit items) together
++  join-items
  |=  [lhs=(unit item) rhs=(unit item)]
  ^-  (unit item)
  ?~  lhs  rhs
  ?~  rhs  lhs
  `[%view (weld (item-as-overview lhs) (item-as-overview rhs))]
::
::    contains arms using for printing doccords items
+|  %printing
::  +print-item: prints a doccords item
++  print-item
  |=  =item
  ~?  >>  debug  %print-item
  ^-  (list sole-effect)
  ?-  item
    [%view *]     (print-overview items.item *(pair styl styl))
    [%core *]     (print-core +.item)
    [%arm *]      (print-arm +.item)
    [%chapter *]  (print-chapter +.item)
    [%face *]     (print-face +.item)
  ==
::
::  +print-core: renders documentation for a full core
++  print-core
  |=  [name=tape docs=what sut=type con=coil uit=(unit item)]
  ^-  (list sole-effect)
  =+  [arms chapters]=(arm-and-chapter-overviews sut con name)
  =/  styles=(pair styl styl)  [[`%br ~ `%b] [`%br ~ `%m]]
  ;:  weld
    (print-header (weld "^" name) docs)
  ::
    ?~  arms
      ~
    (print-overview [%header `['arms:' ~] arms]~ styles)
  ::
    ?~  chapters
      ~
    (print-overview [%header `['chapters:' ~] chapters]~ styles)
  ::
    =+  compiled=(item-as-overview uit)
    ?~  compiled
      ~
    (print-overview [%header `['compiled against: ' ~] compiled]~ styles)
  ==
::
::  +print-chapter: renders documentation for a single chapter
++  print-chapter
  |=  [name=tape doc=what sut=type con=coil]
  ^-  (list sole-effect)
  ~?  >  debug  %print-chapter
  =/  styles=(pair styl styl)  [[`%br ~ `%b] [`%br ~ `%m]]
  ;:  weld
    (print-header (weld "|" name) doc)
  ::
    =+  arms=(arms-in-chapter sut con name)
    ?~  arms
      ~
    (print-overview [%header `['arms:' ~] arms]~ styles)
  ==
::
::  +print-arm: renders documentation for a single arm in a core
++  print-arm
  |=  [name=tape adoc=what pdoc=what cdoc=what gen=hoon sut=type]
  ^-  (list sole-effect)
  ~?  >>  debug  %print-arm
  ;:  weld
    (print-header (weld "+" name) adoc)
    [%txt ""]~
    (styled [[`%br ~ `%b] 'product:']~)
    (print-header "" pdoc)
    [%txt ""]~
    (styled [[`%br ~ `%b] 'default arm in core:']~)
    (print-header "" cdoc)
  ==
::
::  +print-face: renders documentation for a face
++  print-face
  |=  [name=tape doc=what children=(unit item)]
  ^-  (list sole-effect)
  ~?  >>  debug  %print-face
  ;:  weld
    (print-header (weld "." name) doc)
    [%txt ""]~
    ?~  children
      ~
    (print-item u.children)
  ==
::
::  +print-header: prints name and docs only
++  print-header
  |=  [name=tape doc=what]
  ^-  (list sole-effect)
  ~?  >>  debug  %print-header
  ;:  weld
    (styled [[`%br ~ `%g] (crip name)]~)
    ?~  doc
      (styled [[`%br ~ `%r] '(undocumented)']~)
    :~  :-  %tan
        %-  flop
        ;:  weld
          [%leaf "{(trip p.u.doc)}"]~
          (print-sections q.u.doc)
    ==  ==
  ==
::
::    +print-overview: prints summaries of several items
::
::  the (list styl) provides styles for each generation of child
::  items
++  print-overview
  |=  [=overview styles=(pair styl styl)]
  ~?  >>  debug  %print-overview
  =|  out=(list sole-effect)
  |-  ^-  (list sole-effect)
  ?~  overview  out
  =/  oitem  i.overview
  ?-    oitem
      [%header *]
    %=  $
      overview  t.overview
      out  ;:  weld
             out
             ?~  doc.oitem  ~
             (styled [p.styles (crip "{(trip p.u.doc.oitem)}")]~)
             ^$(overview children.oitem)
          ==
    ==
  ::
      [%item *]
    %=  $
      overview  t.overview
      out  ;:  weld
             out
             (styled [q.styles (crip name.oitem)]~)
             ?~  doc.oitem
               %-  styled
               :~  [[`%br ~ `%r] '(undocumented)']
                   [[~ ~ ~] '']
               ==
             ^-  (list sole-effect)
             [%tan [[%leaf ""] [%leaf "{(trip p.u.doc.oitem)}"] ~]]~
           ==
    ==
  ==
::
::    +print-sections: renders a list of sections as tang
::
::  prints the longform documentation
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
::  +print-section: renders a sect as a tang
++  print-section
  |=  section=sect
  ^-  tang
  %+  turn  section
  |=  =pica
  ^-  tank
  ?:  p.pica
    [%leaf (trip q.pica)]
  [%leaf "    {(trip q.pica)}"]
::
::  +styled: makes $sole-effects out of $styls and $cords
++  styled
  |=  [in=(list (pair styl cord))]
  ^-  (list sole-effect)
  =|  out=(list sole-effect)
  |-
  ?~  in  out
  =/  eff=styx  [p.i.in [q.i.in]~]~
  %=  $
    in   t.in
    out  (snoc out [%klr eff])
  ==
--
