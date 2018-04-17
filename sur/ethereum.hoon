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
      $:  %eth-new-filter
          fro=(unit block)
          tob=(unit block)
          adr=(list address)
          top=(list octs)
      ==
      [%eth-get-filter-logs fid=@ud]
      [%eth-get-filter-changes fid=@ud]
  ==
::
++  filter-change
++  event-log
  $:  ::  null for pending logs
      $=  mined  %-  unit
      $:  log-index=@ud
          transaction-index=@ud
          transaction-hash=@ux
          block-number=@ud
          block-hash=@ux
      ==
    ::
      address=@ux
      data=@t
      event=@ux
      topics=(list @t)
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
::  minimum data needed to construct a read call
++  proto-read-request
  $:  id=(unit @t)
      to=address
      call-data
  ==
::
::  raw call data
++  call-data
  $:  function=@t
      arguments=(list data)
  ==
::
::  block to operate on.
++  block
  $%  [%number n=@ud]
      [%label l=?(%earliest %latest %pending)]
  ==
--
