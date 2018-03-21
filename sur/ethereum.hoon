::
|%
::  ethereum types. integer bitsizes ignored.
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
::  ethereum-style typed data. integer bitsizes ignored.
++  data
  $%  [%address p=address]
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
::  ethereum address, 20 bytes.
++  address  @ux
::
::  ethereum json rpc api
::
::  supported requests.
++  request
  $%  [%eth-block-number ~]
      [%eth-call cal=call deb=block]
  ==
::
::  data for eth_call.
++  call
  $:  from=(unit address)
      to=address
      gas=(unit @ud)
      gas-price=(unit @ud)
      value=(unit @ud)
      data=tape
  ==
::
::  block to operate on.
++  block
  $%  [%number n=@ud]
      [%label l=?(%earliest %latest %pending)]
  ==
--
