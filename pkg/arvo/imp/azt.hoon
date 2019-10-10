/-  spider
/+  *threadio
=,  thread=thread:libthread
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
^-  form:m
~&  >  'starting azt'
=/  url    'http://eth-mainnet.urbit.org:8545'
=/  poke-vase  !>([%azimuth-tracker %azimuth-tracker-poke !>([%watch url])])
;<  ~              bind:m  (poke-our %spider %spider-imput poke-vase)
;<  ~              bind:m  (subscribe-our /sub %spider /imp/azimuth-tracker)
|-  ^-  form:m
=*  loop  $
;<  [=mark =vase]  bind:m  (take-subscription-update /sub)
%-  (slog (sell vase) ~)
loop
