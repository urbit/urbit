/@  node        ::  manx
/@  diary-diff  ::  ?([%put-entry id=@da txt=@t] [%del-entry id=@da])
::  import /lib/manx-utils
/-  manx-utils
::  declare that this is a conversion from a
::  dynamic XML node to diary-diff
:-  [%node %$ %diary-diff]
|=  nod=node
^-  diary-diff
=/  mu  ~(. manx-utils nod)
::  extract head, id, and text atttributes from XML node
=/  head  (@tas (got:mu %head))
=/  id  (slav %da (vol:mu "id"))
=/  text  (vol:mu "text")
::  construct the diary-diff
[%put-entry id text]
