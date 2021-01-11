::  -claz-prefab: generate prefab planet wallets
::
::    writes to .txt with line-format ~planet,~ticket,0xaddress,0xauth,0xcrypt
::
::    eg: *%/prefab-invites/txt -claz-prefab 'http://some-endpoint' ~marzod 10
::
/+  keygen, azio, azimuth, *ethereum, strandio
::
|=  args=vase
=/  [~ url=@t star=@p amount=@ud]
  !<([~ @t @p @ud] args)
=*  az  ~(. azio url [azimuth ecliptic delegated-sending]:contracts:azimuth)
=/  m  (strand:strandio ,vase)
^-  form:m
::
;<  spawn-count=@ud  bind:m
  (get-spawn-count:azimuth:az star)
;<  now=@da  bind:m
  get-time:strandio
;<  spawn-limit=@ud  bind:m
  (get-spawn-limit:ecliptic:az star now)
?:  (gte (add spawn-count amount) spawn-limit)
  ~|  :-  %would-hit-spawn-limit
      [now=spawn-count then=(add spawn-count amount) max=spawn-limit]
  !!
::
;<  kids=(list @p)  bind:m
  (get-unspawned-children:azimuth:az star)
?:  (lth (lent kids) amount)
  ~|  [%unexpected-too-few-kids have=(lent kids) want=amount]
  !!
=.  kids  (scag amount kids)
::
;<  eny=@uvJ  bind:m  get-entropy:strandio
::
~&  'patience, slow derivation...'
%-  pure:m
!>  ^-  (list @t)
%+  turn  kids
|=  child=@p
^-  @t
=/  ticket=@q  (end 3^8 (shas child eny))
=+  (full-wallet-from-ticket:keygen child 8^ticket 0 ~)
%-  crip
;:  weld
  (scow %p child)                           ","
  (slag 1 (scow %q ticket))                 ","
  (address-to-hex addr.keys.ownership)      ","
  (address-to-hex addr.keys.management)     ","
  ((x-co:co 64) public.crypt.keys.network)  ","
  ((x-co:co 64) public.auth.keys.network)
==
