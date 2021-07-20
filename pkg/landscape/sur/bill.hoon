|%
+$  bill  (list chit)
+$  chit
  $%  [%apes duz=(list dude:gall)]
      [%fish duz=(list dude:gall)]
  ==
::
++  read-apes
  |=  =bill
  ^-  (list dude:gall)
  ?~  bill  ~
  ?:  ?=(%apes -.i.bill)
    duz.i.bill
  $(bill t.bill)
::
++  read-fish
  |=  =bill
  ^-  (list dude:gall)
  ?~  bill  ~
  ?:  ?=(%fish -.i.bill)
    duz.i.bill
  $(bill t.bill)
--
