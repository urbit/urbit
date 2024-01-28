::  $neo: New Shrub
::
::    Urbit is a namespace, from a path -> data
::    /~hastuc-dibtux/chats/unit-731 :: this a chat 
::    /~hastuc-dibtux/chats/unit-731/msg/~2024.1.27..10.30 :: this is a
::    message inside the chat
::
::    neo is a recipe for defining the kinds of data that live at these
::    paths. For instance, you would maybe like to define a chat
::    datatype, that could be bound into your namespace, so that your
::    friends could send you memes.
::
::
::
::
::
::  
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
  $:  src=@p
      our=@p
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
::  $firm: type of the value in the urbit namespace
::
+$  firm
 
  $_  ^&
  |%
  ::  $state: the state of this value in the urbit namespace
  ::
  ::    For instance, a message would be
  ::    ```hoon
  ::    [author=ship time-sent=time message=txt]
  ::    ```
  ::
  ::    ```
  ++  state  *mold
  ::  $poke: a poke is a request to change a value in teh urbit
  ::  namespace.
  ::
  ::    For instance a blocked list that is a set of users would be
  ::      [%add who=user]
  ::      [%del who=user]
  ::
  ::
  ++  poke   *mold
  ++  form   *^form
  ::
  ::  +kids: Some nodes in the namespace define what children are
  ::  allowed to be under them. For instance, it should not  be allowed
  ::  to create /~hastuc-dibtux/chats/unit-731/blog-post-1. This is
  ::  nonsensical because blog posts don't go in chats.
  ++  kids   *(map pish port)
  ::
  ::  +deps: Some nodes in the namespace might like to hear about other
  ::  things that happen in the namespace. For instance, a substack-type
  ::  software would like to know where the wallet software is located
  ::  in the name
  ++  deps   *(map term fief)
  --
+$  form
  $_  ^|
  |_  [=bowl =icon]
  ++  call
    |~  [prev=* val=*]
    *(list card)
  ::  +reduce: apply %poke, producing state
  ::
  ::    ('liam'' ~) [%add who='ruby'] -> ('liam' 'ruby')
  ::    ('liam' 'ruby' ~) [%del who='ruby'] -> ('liam')
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
