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
::  $stud: mark name
+$  stud
  $@  @tas                                 ::  auth=urbit
  $:  mark=@tas                            :: 
      =name                                :: 
  ==                                            ::
::
++  pith
  |^  $+(pith ^pith)
  ++  en-tape
    |=  pit=$
    (spud (pout pit))
  --
++  name
  |^  ,[=ship =pith]  
  ++  en-pith
    |=  nam=$
    ^-  pith
    [p/ship.nam pith.nam]
  ++  en-tape
    |=  nam=$
    (spud (pout (en-pith nam)))
  ++  en-path
    |=  nam=$
    (pout (en-pith nam)) 
  ++  de-pith  |=(pith (need (de-pith-soft +<)))
  ++  de-pith-soft
    |=  =pith
    ^-  (unit ^$)
    ?.  ?=([[%p @] *] pith)
      ~
    `[+.i.pith t.pith]
  --
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
+$  request
  [src=pith dest=pith val=*]
+$  response
  [src=pith dest=pith status=response-status]
+$  diff
  $%  [%poke p=*]
      [%init p=*]
  ==
+$  update
  $:  =pith
      case=@ud
      =diff
  ==
+$  watch  (list update)
::
+$  err
  $%  [%here =pith]
      [%goof err=*]
      [%fail err=*]
  ==

+$  response-status
  $%  [%done ~]
      err
  ==
::
+$  note
  %+  pair  pith
  $%  [%make =stud init=(unit vase) =conf] :: todo: configuration values, init cannot be ^ if installing over
      [%poke val=*]
      [%tomb =case]
      [%link from=pith src=stud]
  ==
+$  poke
  (pair pith *)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
+$  sign-neo
  [%poke dest=pith status=response-status]
+$  sign
  $+  sign
  $%  [%arvo p=sign-arvo]
      [%neo ~]
  ==

+$  move  (pair pith card)
+$  span  (pair stud firm)
+$  icon
  [case=@ud state=vase history=(list *) migration=(list *)]
+$  hall
  $%  [%exit pith]
      [%room room]
  ==
:: subscription metadata
+$  jail
  $+  jail
  $~  ~
  (map iota cell)
+$  cell
  $+  cell
  [case=@ud state=vase =span =jail]
+$  brig
  $+  brig
  $~  ~
  (map pith cell)
+$  fleet
  $+  fleet
  $~  ~
  (map ship brig)
::  $room: state of a shrub
::    
::    TODO: refactor for networking?
+$  room
  $+  room
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
    |~  [prev=vase val=*]
    *(list card)
  ::  +reduce: apply %poke, producing state
  ::
  ::    ('liam'' ~) [%add who='ruby'] -> ('liam' 'ruby')
  ::    ('liam' 'ruby' ~) [%del who='ruby'] -> ('liam')
  ++  reduce
    |~  val=*
    *vase
  ++  take
    |~  =sign
    *(list card)
  ++  born
    *(list card)
  ++  init
    |~  old=(unit vase)
    *vase
  ++  echo
    |~  [=pith val=*]
    *(list card)
  --
--
