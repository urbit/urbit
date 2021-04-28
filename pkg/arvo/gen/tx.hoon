::  ethereum.request({method: 'personal_sign', params: ['tx',count]}).then(console.log)
::  ethereum.request({method: 'eth_sendTransaction', params: [{from: count, gasPrice: '0x2540be400', to: '0xb58101cd3bbbcc6fa40bcdb04bb71623b5c7d39b', gas: '0x10000', data: 'batch', chainId: '0x3'}]}).then(console.log)
::
/+  eth=ethereum
/=  tt  /tests/lib/naive
|%
++  print-for-web3
  |=  =octs
  ^-  @t
  =/  txt  (crip (render-hex-bytes:eth octs))
  ?>  =(p.octs (met 4 txt))
  (cat 3 '0x' (rev 4 (met 4 txt) txt))
::
++  print-for-batch
  |=  =octs
  ^-  @t
  (crip (render-hex-bytes:eth octs))
--
|=  sig=(unit @t)
^-  @t
=/  account  (hex-to-num:eth '0xb026b0AA6e686F2386051b31A03E5fB95513e1c0')
=/  tx=octs  (set-spawn-proxy:l2:tt 0 ~ravmun-mitbus %ravmun %own account)
=/  prep=octs  [(sub p.tx 65) (rsh 3^65 q.tx)]
=/  nonced=octs  [(add p.prep 4) (can 3 4^1 prep ~)]
?~  sig
  (cat 3 'sign: ' (print-for-web3 nonced))
=/  batch=@t
  %:  rap  3
    '0x26887f26'
    (print-for-batch prep)
    (rsh 3^2 u.sig)
    ~
  ==
(cat 3 'batch: ' batch)
