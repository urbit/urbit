::
|%
++  etyp
  $?  ::  static
      %address  %bool
      %int      %uint
      %real     %ureal
      [%bytes-n n=@ud]
      ::  dynamic
      %bytes    %string
      [%array-n t=etyp n=@ud]
      [%array t=etyp]
  ==
::
++  data                                                :>  typed data
  $%  [%address p=@ux]
      [%string p=tape]
      [%bool p=?]
      [%int p=@sd]
      [%uint p=@ud]
      [%real p=@rs]
      [%ureal p=@urs]
      [%bytes-n p=octs]
      [%bytes p=octs]
      [%array-n p=(list data)]
      [%array p=(list data)]
  ==
::
::  ethereum json rpc api
::
++  request
  $%  [%eth-block-number ~]
      [%eth-call cal=call deb=default-block]
      [%eth-estimate-gas cal=call deb=default-block]
  ==
::
++  call
  $:  from=(unit @i)
      to=@i
      gas=(unit @ud)
      gas-price=(unit @ud)
      value=(unit @ud)
      data=tape
  ==
::
++  default-block
  $%  [%quantity n=@ud]
      [%label l=?(%earliest %latest %pending)]
  ==
--
