=>
|%
::  $href: Where a tile links to
::
+$  href
  $%  [%glob base=term *]
      [%site =path]
  ==
+$  url  cord
+$  docket
  $:  %1
      title=@t
      info=@t
      color=@ux
      =href
      image=(unit url)
      =version
      website=url
      license=cord
  ==
+$  version  *
++  get-iframe
  |=  d=docket
  ^-  @t
  %-  crip
  ?-  -.href.d  
    %glob  "/apps/{(trip base.href.d)}"
    %site  (spud path.href.d)
  ==
--
^-  kook:neo
|%
++  state  pro/%sig
++  poke   (sy %gall-res %ack ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  ~
++  deps  
  %-  ~(gas by *band:neo)
  :~  :-  %charges
      :-  req=&
      [[pro/%noun ~] ~]
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
         %rely
      =/  [pith:neo =lore:neo]  (~(got by deps.bowl) %charges)
      =/  =idea:neo  (~(got of:neo lore) ~)
      ?>  =(p.pail.idea %noun)
      =+  ;;(charges=(map desk [=docket *]) q.q.pail.idea)
      :_  sig/!>(~)
      %+  murn  ~(tap by charges)
      |=  [=desk =docket *]
      ^-  (unit card:neo)
      ?:  (~(has of:neo kids.bowl) #/[desk])
        ~
      `[(welp here.bowl #/[desk]) %make %iframe ~^cord/!>((get-iframe docket)) ~]
    ==
  ++  init
    |=  pal=(unit pail:neo)
    `sig/!>(~)
  --
--
