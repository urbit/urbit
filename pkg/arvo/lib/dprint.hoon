/-  *sole
/+  easy-print=language-server-easy-print
::    a library for printing doccords
=/  debug  |
=>
  ::    dprint-types
  |%
  ::  $overview: an overview of all named things in the type.
  ::
  ::    each element in the overview list is either a documentation for a sublist
  ::    or an association betwen a term and documentation for it
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
        ::    overview of a type
        ::
        [%view items=overview]
        ::    inspecting a full core
        $:  %core
            name=tape                                   ::  arm that built it
            docs=what                                   ::
            sut=type                                    ::  [%core *]
            children=(unit item)                        ::  compiled against
        ==
        ::    inspecting a single arm on a core
        $:  %arm
            name=tape                                   ::  arm name
            adoc=what                                   ::  arm doc
            pdoc=what                                   ::  product doc
            cdoc=what                                   ::  $ arm/prod doc
            gen=hoon                                    ::  arm hoon AST
            sut=type                                    ::  subject of arm
        ==
        ::    inspecting a face and what's behind it
        $:  %face
            name=tape                                   ::  name of face
            docs=what                                   ::
            children=(unit item)                        ::  face referent
        ==
        ::    inspecting a single chapter on a core
        $:  %chapter
            name=tape                                   ::  name of chapter
            docs=what                                   ::
            sut=type                                    ::  [%core *]
            tom=tome                                    ::  tome of chapter
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
::  +find-item-in-type: returns the item to print while searching through topic
::
::    this gate is a thin wrapper around _hunt for usability, since the only entry
::    point most users should care about is find-item:hunt
::
++  find-item-in-type
  |=  [topics=(list term) sut=type]
  ?~  topics  !!
  =/  top=(lest term)  topics
  ~(find-item hunt [top sut])
::
::  +hunt: door used for refining the type while searching for doccords
::
++  hunt
  =|  gil=(set type)
  |_  [topics=(lest term) sut=type]
  +*  this  .
  ::
  +|  %find
  ::
  ++  find-item
    ~?  >>  debug  %find-item
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
    ~?  >>  debug  %find-cell
    ^-  (unit item)
    ?>  ?=([%cell *] sut)
    =/  lhs  find-item:this(sut p.sut)
    ?~  lhs
      find-item:this(sut q.sut)
    lhs
  ::
  ++  find-core
    ~?  >>  debug  %find-core
    ^-  (unit item)
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
    ~?  >>  debug  %find-face
    ^-  (unit item)
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
    ~?  >>  debug  %find-fork
    ^-  (unit item)
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
    ~?  >>  debug  %find-hint
    ^-  (unit item)
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
      ~?  >>  debug  %find-hint-core
      ^-  (unit item)
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
      ~?  >>  debug  %find-hint-face
      ^-  (unit item)
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
    ~?  >>  debug  %recurse-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    find-item:this(sut p.sut)
  ++  recurse-chap
    ~?  >>  debug  %recurse-chap
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    ?~  t.topics  !!
    find-item:this(topics t.topics)
  ++  recurse-arm-core
    ~?  >>  debug  %recurse-arm-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    ?~  t.topics  !!
    find-item:this(sut arm-type, topics t.topics)
  ::
  +|  %check
  ::
  ++  check-arm
    ~?  >>  debug  %recurse-core
    ^-  ?
    !=(~ (find ~[i.topics] (sloe sut)))
  ++  check-chap
    ~?  >>  debug  %check-chap
    ^-  ?
    ?>  ?=([%core *] sut)
    (~(has by q.r.q.sut) i.topics)
  ++  check-face
    ~?  >>  debug  %check-face
    ^-  ?
    ?>  ?=([%face *] sut)
    ?.  ?=(term p.sut)
      ::TODO: handle $tune case
      %.n
    =(p.sut i.topics)
  ++  check-search
    ~?  >>  debug  %check-search
    ^-  ?
    =(~ t.topics)
  ++  check-arm-core
    ~?  >>  debug  %check-arm-core
    ^-  ?
    =+  arm-list=(sloe (~(play ut sut) arm-hoon))
    &(!=(arm-list ~) !=(arm-list ~[%$]) ?=([%core *] arm-type))
  ::
  +|  %return
  ::
  ++  return-cell
    ~?  >>>  debug  %return-cell
    ^-  (unit item)
    ?>  ?=([%cell *] sut)
    (join-items return-item:this(sut p.sut) return-item:this(sut q.sut))
  ::
  ++  return-core
    ~?  >>>  debug  %return-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =*  compiled-against  return-item:this(sut p.sut)
    `[%core (trip i.topics) *what sut compiled-against]
  ::
  ++  return-face
    ~?  >>>  debug  %return-face
    ^-  (unit item)
    ?>  ?=([%face *] sut)
    ::  TODO: handle tune case
    ?.  ?=(term p.sut)
      return-item:this(sut q.sut)
    =*  compiled-against  return-item:this(sut q.sut)
    `[%face (trip p.sut) *what compiled-against]
  ::
  ++  return-fork
    ~?  >>>  debug  %return-fork
    ^-  (unit item)
    ?>  ?=([%fork *] sut)
    =*  types  ~(tap in p.sut)
    =*  items  (turn types |=(a=type return-item:this(sut a)))
    (roll items join-items)
  ::
  ++  return-hint
    ~?  >>>  debug  %return-hint
    ^-  (unit item)
    ?>  ?=([%hint *] sut)
    =*  res  return-item:this(sut q.sut)
    ?.  ?=([%help *] q.p.sut)
      ~
    ?:  ?=([%core *] q.sut)
      return-hint-core
    ?:  ?=([%face *] q.sut)
      return-hint-face
    `[%view [%header `crib.p.q.p.sut (item-as-overview res)]~]
  ::
  ++  return-arm
    ~?  >>>  debug  %return-arm
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =+  [adoc pdoc cdoc]=(arm-docs i.topics sut)
    ::TODO: should this p.sut be sut? or the compiled type of the arm?
    `[%arm (trip i.topics) adoc pdoc cdoc arm-hoon sut]
  ::
  ++  return-chap
    ~?  >>>  debug  %return-chap
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =/  tom=tome  (~(got by q.r.q.sut) i.topics)
    `[%chapter (trip i.topics) p.tom sut (~(got by q.r.q.sut) i.topics)]
  ::
  ++  return-arm-core
    ~?  >>>  debug  %return-arm-core
    ^-  (unit item)
    ?>  ?=([%core *] sut)
    =+  [adoc pdoc cdoc]=(arm-docs i.topics sut)
    =/  dox=what  ?~(adoc ?~(pdoc ~ pdoc) adoc)
    =/  at  arm-type
    ?>  ?=([%core *] at)
    =*  compiled-against  return-item:this(sut p.sut)
    `[%core (trip i.topics) dox at compiled-against]
  ::
  ++  return-item
    ~?  >>>  debug  %return-item
    ^-  (unit item)
    ?-    sut
        %noun      ~
        %void      ~
        [%atom *]  ~
        [%cell *]  return-cell
        [%core *]  return-core
        [%face *]  return-face
        [%fork *]  return-fork
        [%hint *]  return-hint
        [%hold *]
      ?:  (~(has in gil) sut)
        ~
      =<  return-item
      %=  this
        gil  (~(put in gil) sut)
        sut  (~(play ut p.sut) q.sut)
      ==  
    ==
  ::
  ++  return-hint-core
    ~?  >>>  debug  %return-hint-core
    ^-  (unit item)
    ?>  &(?=([%hint *] sut) ?=([%core *] q.sut))
    (apply-hint return-core:this(sut q.sut))
  ::
  ++  return-hint-face
    ~?  >>>  debug  %return-hint-face
    ^-  (unit item)
    ?>  &(?=([%hint *] sut) ?=([%face *] q.sut))
    (apply-hint return-face:this(sut q.sut))
  ::
  ++  apply-hint
    ~?  >>  debug  %apply-hint
    |=  uit=(unit item)
    ^-  (unit item)
    ?~  uit  ~
    ?>  &(?=([%hint *] sut) ?=([%help *] q.p.sut))
    ?+  u.uit  ~
      ?([%core *] [%face *])  (some u.uit(docs `crib.p.q.p.sut))
    ==
  ::
  +|  %misc
  ++  arm-hoon
    ^-  hoon
    ?>  ?=([%core *] sut)
    (^arm-hoon i.topics sut)
  ::
  ++  arm-type
    ^-  type
    ?>  ?=([%core *] sut)
    (^arm-type i.topics sut)
  --
::
::  +arm-hoon: looks for an arm in a core type and returns its hoon
++  arm-hoon
  |=  [nom=term sut=type]
  ^-  hoon
  ?>  ?=([%core *] sut)
  =/  tomes=(list [p=term q=tome])  ~(tap by q.r.q.sut)
  |-
  ?~  tomes  !!
  =+  gen=(~(get by q.q.i.tomes) nom)
  ?~  gen
    $(tomes t.tomes)
  u.gen
::
::  +arm-type: looks for an arm in a core type and returns its type
++  arm-type
  |=  [nom=term sut=type]
  ^-  type
  ?>  ?=([%core *] sut)
  (~(play ut sut) (arm-hoon nom sut))
::
::  +hint-doc: returns docs if type is %help $hint w/ matching cuff
++  hint-doc
  |=  [=cuff sut=type]
  ^-  what
  ?.  &(?=([%hint *] sut) ?=([%help *] q.p.sut) =(cuff cuff.p.q.p.sut))
    ~
  `crib.p.q.p.sut
::
::  +arm-doc: returns arm doc of an arm
::
::    we just check if the $cuff is from a ++ or +$ arm but this will
::    probably need to be revisited once more sophisticated cuffs are used
++  arm-doc
  |=  [nom=term sut=type]
  ^-  what
  ?~  (hint-doc [%funk nom]~ sut)
    (hint-doc [%plan nom]~ sut)
  (hint-doc [%funk nom]~ sut)
::
::  +prod-doc: wrapper for +hint-doc with empty cuff
++  prod-doc
  |=  sut=type
  ^-  what
  (hint-doc ~ sut)
::
::  +buc-doc: checks if type is core and returns docs on $ arm if it exists
++  buc-doc
  |=  sut=type
  ^-  what
  ?.  ?=([%core *] sut)
    ~
  ?~  (find [%$]~ (sloe sut))
    ~
  =/  sat=type  (arm-type %$ sut)
  ?~  (arm-doc %$ sat)
    (prod-doc sat)
  (arm-doc %$ sat)
::
::  +arm-docs: grabs the docs for an arm.
::
::    there are three possible places with relevant docs for an arm:
::    docs for the arm itself, docs for the product of the arm, and
::    if the arm builds a core, docs for the default arm of that core.
::
::    .adoc: docs written above the the arm
::    .pdoc: docs for the product of the arm
::    .cdoc: docs for the default arm of the core produced by the arm
++  arm-docs
  |=  [nom=term sut=type]
  ^-  [what what what]
  ?>  ?=([%core *] sut)
  =/  sat=type  (~(play ut sut) (arm-hoon nom sut))
  =/  adoc=what  (arm-doc nom sat)
  =/  pdoc=what
    ?~  adoc
      (prod-doc sat)
    ?>  ?=([%hint *] sat)
    (prod-doc q.sat)
  =/  cdoc=what
    ?~  adoc
      ?~  pdoc
        (buc-doc sat)
      ?>  ?=([%hint *] sat)
      (buc-doc q.sat)
    ?~  pdoc
      ?>  ?=([%hint *] sat)
      (buc-doc q.sat)
    ?>  &(?=([%hint *] sat) ?=([%hint *] q.sat))
    (buc-doc q.q.sat)
  [adoc pdoc cdoc]
::
::    +arm-and-chapter-overviews: returns an overview of a core's contents
::
::  returns an overview for arms which are part of unnamed chapters, and
::  an overview of the named chapters
::
++  arm-and-chapter-overviews
  |=  =item
  ^-  [overview overview]
  ?>  &(?=([%core *] item) ?=([%core *] sut.item))
  =|  [adocs=overview cdocs=overview]
  =/  tomes  ~(tap by q.r.q.sut.item)
  |-
  ?~  tomes
    [(sort-overview adocs) (sort-overview cdocs)]
  ?~  p.i.tomes
    :: chapter has no name. add documentation for its arms to arm-docs
    =.  adocs  (weld adocs (tome-as-overview q.i.tomes sut.item))
    $(tomes t.tomes)
  ::  chapter has a name. add to list of chapters
  =.  cdocs
    %+  weld  cdocs
    ^-  overview
    [%item :(weld "^" name.item "|" (trip -.i.tomes)) p.q.i.tomes]~
  $(tomes t.tomes)
::
::  +arms-in-chapter: returns an overview of the arms in a specific chapter
++  arms-in-chapter
  |=  [sut=type tom=tome]
  ^-  overview
  (sort-overview (tome-as-overview tom sut))
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
::  +tome-as-overview: translate a tome into an overview
++  tome-as-overview
  |=  [tom=tome sut=type]
  ^-  overview
  %+  turn  ~(tap by q.tom)
  |=  ar=(pair term hoon)
  :*  %item
      ::TODO make this distinguish between ++ and +$ arms
      (weld "+" (trip p.ar))
      =/  adoc  (arm-doc p.ar (~(play ut sut) q.ar))
      =/  pdoc  (prod-doc (~(play ut sut) q.ar))
      ?~  adoc
        pdoc
      adoc
  ==
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
    :_  ~
    ::TODO make this distinguish between ++ and +$ arms
    :*  %item  (weld "+" name.itm)
      ?~  adoc.itm
        ?~  pdoc.itm
          cdoc.itm
        pdoc.itm
      adoc.itm
    ==
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
    [%view *]     (print-overview item *(pair styl styl))
    [%core *]     (print-core item)
    [%arm *]      (print-arm item)
    [%chapter *]  (print-chapter item)
    [%face *]     (print-face item)
  ==
::
::  +print-core: renders documentation for a full core
++  print-core
  |=  =item
  ^-  (list sole-effect)
  ?>  ?=([%core *] item)
  =+  [arms chapters]=(arm-and-chapter-overviews item)
  =/  styles=(pair styl styl)  [[`%br ~ `%b] [`%br ~ `%m]]
  ;:  weld
    (print-header (weld "^" name.item) docs.item)
  ::
    [%txt ""]~
  ::
    (print-signature ~(duck easy-print sut.item))
  ::
    [%txt ""]~
  ::
    ?~  arms
      ~
    (print-overview [%view [%header `['arms:' ~] arms]~] styles)
  ::
    ?~  chapters
      ~
    (print-overview [%view [%header `['chapters:' ~] chapters]~] styles)
  ::
    ?~  children.item
      ~
    =/  child  ?:  ?=([%core *] u.children.item)
                 u.children.item(children ~)
               ?:  ?=([%face *] u.children.item)
                 u.children.item(children ~)
               u.children.item
    =+  compiled=(item-as-overview `child)
    ?~  compiled
      ~
    (print-overview [%view [%header `['compiled against: ' ~] [i.compiled]~]~] styles)
  ==
::
::  +print-chapter: renders documentation for a single chapter
++  print-chapter
  |=  =item
  ^-  (list sole-effect)
  ?>  ?=([%chapter *] item)
  ~?  >  debug  %print-chapter
  =/  styles=(pair styl styl)  [[`%br ~ `%b] [`%br ~ `%m]]
  ;:  weld
    (print-header (weld "|" name.item) docs.item)
  ::
    =+  arms=(arms-in-chapter sut.item tom.item)
    ?~  arms
      ~
    (print-overview [%view [%header `['arms:' ~] arms]~] styles)
  ==
::
::  +print-signature: turns product of duck:easy-print into a (list sole-effect)
++  print-signature
  |=  =tank
  ^-  (list sole-effect)
  =/  tan  (wash [3 80] tank)
  ?.  (gte (lent tan) 3)
    (turn tan |=(a=tape [%txt a]))
  %+  weld
    (turn (scag 3 tan) |=(a=tape [%txt a]))
  (styled [[`%br ~ `%g] '   ...']~)
::
::  +print-arm: renders documentation for a single arm in a core
++  print-arm
  |=  =item
  ^-  (list sole-effect)
  ?>  ?=([%arm *] item)
  ~?  >>  debug  %print-arm
  ;:  weld
    (print-header (weld "+" name.item) adoc.item)
    [%txt ""]~
  ::
    (print-signature ~(duck easy-print (~(play ut sut.item) gen.item)))
  ::
    [%txt ""]~
  ::
    ?~  pdoc.item
      *(list sole-effect)
    %-  zing  :~  (styled [[`%br ~ `%b] 'product:']~)
                  (print-header "" pdoc.item)
                  [%txt ""]~
              ==
  ::
    ?~  cdoc.item
      *(list sole-effect)
    %-  zing  :~  (styled [[`%br ~ `%b] '$:']~)
                  (print-header "" cdoc.item)
              ==
  ==
::
::  +print-face: renders documentation for a face
++  print-face
  |=  =item
  ^-  (list sole-effect)
  ?>  ?=([%face *] item)
  ~?  >>  debug  %print-face
  ;:  weld
    (print-header (weld "." name.item) docs.item)
    [%txt ""]~
  ::
    ?~  children.item
      ~
    (print-item u.children.item)
  ==
::
::  +print-header: prints name and docs only
++  print-header
  |=  [name=tape doc=what]
  ^-  (list sole-effect)
  ~?  >>  debug  %print-header
  ;:  weld
    (styled [[`%br ~ `%g] (crip name)]~)
    ?~  doc  *(list sole-effect)
::      (styled [[`%br ~ `%r] '(undocumented)']~)
    :~  :-  %tan
        %-  flop
        ;:  weld
          [%leaf "{(trip p.u.doc)}"]~
          (print-sections q.u.doc)
    ==  ==
  ==
::
::  +print-overview: prints summaries of several items
::
::    the (pair styl styl) provides styles for each generation of child items
++  print-overview
  |=  [view=item styles=(pair styl styl)]
  ?>  ?=([%view *] view)
  ~?  >>  debug  %print-overview
  =|  out=(list sole-effect)
  |-  ^-  (list sole-effect)
  ?~  items.view  out
  =/  oitem  i.items.view
  ?-    oitem
      [%header *]
    %=  $
      items.view  t.items.view
      out         ;:  weld
                    out
                    ?~  doc.oitem  ~
                    (styled [p.styles (crip "{(trip p.u.doc.oitem)}")]~)
                    ^$(view [%view children.oitem])
    ==            ==
  ::
      [%item *]
    %=  $
      items.view  t.items.view
      out         ;:  weld
                    out
                    (styled [q.styles (crip name.oitem)]~)
                    ?~  doc.oitem
                      %-  styled
                      :~  [[`%br ~ `%r] '(undocumented)']
                          [[~ ~ ~] '']
                      ==
                    ^-  (list sole-effect)
                    [%tan [[%leaf ""] [%leaf "{(trip p.u.doc.oitem)}"] ~]]~
    ==            ==
  ==
::
::  +print-sections: renders a list of sections as tang
::
::    prints the longform documentation
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
