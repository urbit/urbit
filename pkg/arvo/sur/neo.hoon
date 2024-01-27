|%
+$  pith  $+(pith ^pith)
+$  pate  [[%p p=ship] q=pith]
++  petty-port
  |*  a=mold
  ^-  port
  [a a]
+$  dita  (each iota aura)
+$  pish  (list dita)
+$  conf  (map term pith)
+$  card
  $%  [%arvo note-arvo]
      [%neo note]
  ==
+$  note
  %+  pair  pith
  $%  [%make src=path init=(unit *) =conf] :: todo: configuration values, init cannot be ^ if installing over
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
+$  sign
  $+  sign
  $%  [%arvo p=sign-arvo]
      [%neo ~]
  ==

+$  move  (pair pith card)
+$  span  (pair path firm)
+$  icon
  [state=* history=(list *) migration=(list *)]
+$  hall
  $%  [%exit pith]
      [%room room]
  ==
+$  room
  $~  [*span ~ ~ *icon]
  $:  =span
      =conf
      =yard
      =icon
  ==
+$  bowl
  $:  our=@p
      were=pith
      now=@da
      deps=(map term (pair pith *))
      kids=(map pith *)
  ==
+$  fief
  [required=? =port]
+$  port :: TODO: how to specify behaviour
  [state=mold action=mold]
+$  deps  (map term fief)
+$  kids  (map pish port)
::
+$  firm
  $_  ^&
  |%
  ++  state  *mold
  ++  poke   *mold
  ++  form   *^form
  ++  kids   *(map pish port)
  ++  deps   *(map term fief)
  --
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
    |~  [=pith val=*]
    *(list card)
  --
--
