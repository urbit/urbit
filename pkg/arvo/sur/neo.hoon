|%
+$  note
  %+  pair  pith
  $%  [%make src=path init=(unit *) conf=(map ~ ~)] :: todo: configuration values, init cannot be ^ if installing over
      [%poke val=*]
      [%tomb =case]
      [%link from=pith]
  ==
+$  poke
  (pair pith *)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
+$  sign  ~
+$  card  ~
+$  span  (pair path form)
+$  icon
  [state=* history=(list *) migration=(list *)]
+$  hall
  $%  [%exit pith]
      [%room room]
  ==
+$  room
  $~  [*span ~ *icon]
  $:  =span
      =yard
      =icon
  ==
+$  bowl  ~
+$  form
  $_  ^|
  |_  [=bowl =icon]
  ++  call
    |~  [prev=* val=*]
    *(list card)
  ++  reduce
    |~  val=*
    **
  ++  take
    |~  =sign
    *(list card)
  ++  born
    *(list card)
  ++  init
    |~  old=(unit *)
    **
  ++  echo
    |=  [=pith val=*]
    *(list card)
  --
--
