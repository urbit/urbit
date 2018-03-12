::
|%
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
::TODO  make single ++eth-rpc core, or split off into separate sur+lib
::
++  jrpc-api
  |%
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
::
::  json-rpc
::
++  json-rpc
  |%
  ++  response
    $%  [%result res=@t]
        [%error code=@t message=@t]  ::TODO  data?
    ==
  ::
  ++  request
    $:  method=@t
        params=parameters
        id=(unit @t)  ::TODO  ?
    ==
  ::
  ++  parameters
    $%  [%array (list @t)]
        [%object (map @t @t)]
    ==
  --
--
