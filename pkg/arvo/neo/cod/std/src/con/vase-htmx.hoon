=/  prelude  !>(.)
:-  [%vase %$ %htmx]
|=  vax=vase
|=  =bowl:neo
=;  revert                                          ::  XX fix vase printing
  ;/  (of-wall:format (~(win re (sell vax)) 0 80))  ::
=/  depth=@ud  2
=/  ty  p.vax
|-  ^-  manx
=*  loop  $
|^  ^-  manx
  ?:  =(depth 0)
    ;/  "Bottoming out"
  =/  arms  (slow:neo ty)
  =/  items=(list item:dprint:neo)
    (zing (turn arms |=(ls=(list term) `(list item:dprint:neo)`(drop (find-item-in-type:dprint:neo ls ty)))))
  ;div
    ;*
    ?:  =(0 (lent items))
      ;=
        ;+  ;/  (of-wall:format (~(win re (sell vax)) 0 80))
      ==
    (turn items print-item)
  ==
++  print-item
  |=  val=item:dprint:neo
  ^-  manx
  ?+  -.val   ;/  "Unsupported {(trip -.val)}"
      %core  (print-core +.val)
      %chapter  (print-chapter +.val)
      %arm   (print-arm +.val)
      %view
    ;div
      ;*  (turn items.val print-overview-item)
    ==
  ==
++  print-overview-item
  |=  tim=overview-item:dprint:neo
  ^-  manx
  ?-  -.tim
    %header  (print-overview-header +.tim)
    %item    (print-overview-item-actual +.tim)
  ==
++  print-overview-header
  |=  [doc=what children=overview:dprint:neo]
  ^-  manx
  ;div
    ;*  (print-what "" doc)
    :+  (print-overview children)
  ==
++  print-overview-item-actual
  |=  [name=tape doc=what]
  ;div
    ;div: {name}
    ;*  (print-what "" doc)
  ==
++  print-core
  |=  $:  name=tape                                   ::  arm that built it
          docs=what                                   ::
          sut=type                                    ::  [%core *]
          children=(unit item:dprint:neo)                        ::  compiled against
      ==
  ^-  manx
  ;div
    ;h3
      ;  Core
      ;  {name}
    ==
    ;div
       ;*  (print-what "" docs)
       ;div: Children
       ;+  loop(ty sut)
    ==
  ==
++  print-chapter
  |=   $:  name=tape                                   ::  name of chapter
           docs=what                                   ::
           sut=type                                    ::  [%core *]
           tom=tome                                    ::  tome of chapter
       ==
  ^-  manx
  ;div
    ;h3
      ;  Chapter
      ;  {name}
    ==
    ;div
       ;*  (print-what "Overview" docs)
       ;*  (print-what "Tome" p.tom)
       ;*
       %+  turn  ~(tap by q.tom)
       |=  [=term =hoon]
       ^-  manx
       ;span: Arm {(trip term)}
       :: loop(vax (slap sut wing/~[term]), depth (dec depth))
       :: loop(ty (~(play ut sut) hoon), depth (dec depth))
    ==
  ==
++  print-arm
  |=  $:
        name=tape                                   ::  arm name
        adoc=what                                   ::  arm doc
        pdoc=what                                   ::  product doc
        cdoc=what                                   ::  $ arm/prod doc
        gen=hoon                                    ::  arm hoon AST
        sut=type                                    ::  subject of arm
    ==
   =/  t  (~(play ut sut) gen)

  ^-  manx
  ;div.fc.g2
    ;h3: {(weld "+" name)}
    ;details.br1.bd1
      ;summary.br1.b1.p-1: signature
      ;code.p2
        ;pre.mono
          ;*
          %+  turn  (wash [0 80] ~(duck easy-print:neo t))
          |=  tap=tape
          ;div: {tap}
        ==
      ==
    ==
    ;details.br1.bd1
      ;summary.br1.b1.p-1: docs
      ;div.p2.fc.g1
        ;*  (print-what "" adoc)
        ;*  (print-what "Product" pdoc)
        ;*  (print-what "$" cdoc)
      ==
    ==
    ;details.br1.bd1
      ;summary.br1.b1.p-1: children
      ;div.p2
        ;+  loop(ty (~(play ut ty) gen), depth (dec depth))
      ==
    ==
  ==
::
++  print-what
  |=  [name=tape wat=what]
  ^-  (list manx)
  ?~  wat
    ~
  :_  ~
  ?:  =(~ q.u.wat)
    ;div
      ;  {(trip p.u.wat)}
    ==
  ;details.br1.bd1
    ;summary.p-1.b1.br1
      ;  {(trip p.u.wat)}
    ==
    ;div.p2
      ;*  (turn q.u.wat print-sect)
    ==
  ==
++  print-sect
  |=  sec=sect
  ^-  manx
  ;div.p2
    ;*  (turn sec print-pica)
  ==
++  print-pica
  |=  pic=pica
  ^-  manx
  ?:  p.pic
    ;div.prose
      ;  {(trip q.pic)}
    ==
  ;code
    ;  {(trip q.pic)}
  ==
--
