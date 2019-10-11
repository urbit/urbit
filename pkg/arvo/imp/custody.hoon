/-  spider
/+  *threadio, *azimuth, *custody, *csv
=,  thread=thread:libthread
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
^-  form:m
=/  url  'http://eth-mainnet.urbit.org:8545'
=/  key-path  /=home/(scot %da now.bowl)/keys/txt
=/  loc-path  /=home/(scot %da now.bowl)/locations/txt
=/  vases=(list vase)
  %+  select
    !,  *hoon
    (need ship.left)
  %^    join
      (load:keys key-path)
    (load:locations loc-path)
  !,  *hoon
  ?&  !=(~ ship.left)
      =(location.left location.right)
      =(%master role.left)
  ==
=/  ships  (turn vases |=(=vase !<(ship vase)))
|-  ^-  form:m
=*  loop  $
?~  ships
  (pure:m)
;<  =point:azimuth  bind:m  (fetch-point url i.ships)
%-  (slog >[i.ships owner.own.point]< ~)
loop(ships t.ships)
