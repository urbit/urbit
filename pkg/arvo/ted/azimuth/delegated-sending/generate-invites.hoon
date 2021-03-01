::  azimuth/delegated-sender/generate-invites: generate invite codes we can send
::
::NOTE  the returned list contains _at most_ the specified amount of invites,
::      but will contain less if we don't have that many available.
::
/+  strandio, azio, keygen
=,  ethereum-types
::
|=  args=vase
=/  [url=@t amount=@ud ~]
  !<([@t @ud ~] args)
=/  m  (strand:strandio ,vase)  ::  (list [=ship code=@q =address])
^-  form:m
;<  our=ship  bind:m
  get-our:strandio
;<  eny=@uvJ  bind:m
  get-entropy:strandio
;<  planets=(list ship)  bind:m
  %.  [our amount]
  =<  get-planets-to-send:delegated-sending
  ~(. azio [url [azimuth ecliptic delegated-sending]:contracts:azimuth])
%-  pure:m
!>  ^-  (list [=ship code=@q =address])
%+  turn  planets
|=  planet=ship
:-  planet
=/  ticket=byts
  :-  8
  %^  end  3  8
  (shas planet eny)
:-  dat.ticket
=<  addr.keys
::NOTE  invite wallets always have ~zod as the @p
(ownership-wallet-from-ticket:keygen ~zod ticket ~)
