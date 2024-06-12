/@  message
^-  kook:neo
|%
++  state  pro/%sig
++  poke  ~
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%message ~]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  :-  %pub
      :+  req=|  [pro/%sig (sy %sig ~)]
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  [|/%da |]
          [pro/%message ~]
      ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    [~ sig/!>(~)]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%rely stud)
    :_  state
    =+  !<([=term =stem:neo] vax)
    ?>  ?=(%y -.q.stem)
    ::  only get new kids
    =/  kids
      %+  skim 
        ~(val by kids.q.stem) 
      |=  [=ever:neo =mode:neo =pail:neo] 
      =(%add mode)
    ?:  =(~ kids)
      ~
    =/  pai=pail:neo  pail:(snag 0 kids)
    =/  mes  !<(message q.pai)
    :~  :-  (welp here.bowl ~[da/now.mes])
        [%make [%message `pai ~]]
    ==
  --
--
