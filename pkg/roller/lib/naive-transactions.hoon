/+  naive, ethereum
::  Types
|%
+$  address  address:ethereum
+$  nonce    @ud
+$  proxy    ?(%own %spawn %manage %vote %transfer)
--
::
|%
::
::  TODO: does this uniquely produce the pubkey?
++  verifier
  ^-  ^verifier:naive
  |=  [dat=octs v=@ r=@ s=@]
  ?:  (gth v 3)  ~  ::  TODO: move to jet
  =/  result
    %-  mule
    |.
    =,  secp256k1:secp:crypto
    %-  address-from-pub:key:ethereum
    %-  serialize-point
    (ecdsa-raw-recover (hash-tx dat) v r s)
  ?-  -.result
    %|  ~
    %&  `p.result
  ==
::  Verify signature and produce signer address
::
++  verify-sig
  |=  [sig=@ txdata=octs]
  ^-  (unit address)
  |^
  ::  Reversed of the usual r-s-v order because Ethereum integers are
  ::  big-endian
  ::
  =^  v  sig  (take 3)
  =^  s  sig  (take 3 32)
  =^  r  sig  (take 3 32)
  ::  In Ethereum, v is generally 27 + recid, and verifier expects a
  ::  recid.  Old versions of geth used 0 + recid, so most software
  ::  now supports either format.  See:
  ::
  ::  https://github.com/ethereum/go-ethereum/issues/2053
  ::
  =?  v  (gte v 27)  (sub v 27)
  (verifier txdata v r s)
  ::
  ++  take
    |=  =bite
    [(end bite sig) (rsh bite sig)]
  --
::
++  unsigned-tx
  |=  [chain-id=@ud =nonce tx=octs]
  ^-  octs
  =/  prepared-data  (prepare-for-sig chain-id nonce tx)
  =/  len  (rsh [3 2] (scot %ui p.prepared-data))
  %:  cad:naive  3
    26^'\19Ethereum Signed Message:\0a'
    (met 3 len)^len
    prepared-data
    ~
  ==
::
++  sign-tx
  |=  [pk=@ =nonce tx=octs]  ^-  octs
  =/  sign-data
    %-  hash-tx
    (unsigned-tx 1.337 nonce tx)
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto sign-data pk)
  (cad:naive 3 1^v 32^s 32^r tx ~)
::
++  prepare-for-sig
  |=  [chain-id=@ud =nonce tx=octs]
  ^-  octs
  =/  chain-t  (rsh [3 2] (scot %ui chain-id))
  %:  cad:naive  3
    14^'UrbitIDV1Chain'
    (met 3 chain-t)^chain-t
    1^':'
    4^nonce
    tx
    ~
  ==
::
++  extract-address
  |=  [=raw-tx:naive nas=^state:naive chain-id=@]
  ^-  (unit @ux)
  ?~  point=(get:orm:naive points.nas ship.from.tx.raw-tx)
    ~
  =/  =nonce:naive
    =<  nonce
    (proxy-from-point:naive proxy.from.tx.raw-tx u.point)
  =/  message=octs
    (unsigned-tx chain-id nonce raw.raw-tx)
  (verify-sig sig.raw-tx message)
::
++  gen-tx
  |=  [=nonce tx=tx:naive pk=@]  ^-  octs
  :: takes in a nonce, tx:naive, and private key and returned a signed transactions as octs
  %^  sign-tx  pk  nonce  (gen-tx-octs tx)
::
++  gen-tx-octs
  ::  generates octs for a transaction
  |=  tx=tx:naive
  |^
  ^-  octs
  =/  raw=octs
    ?-  +<.tx
      %spawn  (get-spawn +>.tx)
      %transfer-point  (get-transfer +>.tx)
      %configure-keys  (get-keys +>.tx)
      %escape  (get-escape +.tx)
      %cancel-escape  (get-escape +.tx)
      %adopt  (get-escape +.tx)
      %reject  (get-escape +.tx)
      %detach  (get-escape +.tx)
      %set-management-proxy  (get-ship-address +.tx)
      %set-spawn-proxy  (get-ship-address +.tx)
      %set-transfer-proxy  (get-ship-address +.tx)
    ==
  raw
  ::
  ++  get-spawn
    |=  [child=ship to=address]  ^-  octs
    %:  cad:naive  3
      (from-proxy proxy.from.tx)
      4^ship.from.tx
      1^%1                                       :: %spawn
      4^child
      20^to
      ~
    ==
  ::
  ++  get-transfer
    |=  [=address reset=?]  ^-  octs
    %:  cad:naive  3
      (from-proxy proxy.from.tx)
      4^ship.from.tx
      1^(can 0 7^%0 1^reset ~)                   :: %transfer-point
      20^address
      ~
    ==
  ::
  ++  get-keys
    |=  [crypt=@ auth=@ suite=@ breach=?]  ^-  octs
    %:  cad:naive  3
      (from-proxy proxy.from.tx)
      4^ship.from.tx
      1^(can 0 7^%2 1^breach ~)                 :: %configure-keys
      32^crypt
      32^auth
      4^suite
      ~
    ==
  ::
  ++  get-escape
    |=  [action=@tas other=ship]  ^-  octs
    =/  op
      ?+  action  !!
        %escape         %3
        %cancel-escape  %4
        %adopt          %5
        %reject         %6
        %detach         %7
      ==
    %:  cad:naive  3
      (from-proxy proxy.from.tx)
      4^ship.from.tx
      1^(can 0 7^op 1^0 ~)
      4^other
      ~
    ==
  ::
  ++  get-ship-address
    |=  [action=@tas =address]  ^-  octs
    =/  op
      ?+  action  !!
        %set-management-proxy     %8
        %set-spawn-proxy          %9
        %set-transfer-proxy       %10
      ==
    %:  cad:naive  3
      (from-proxy proxy.from.tx)
      4^ship.from.tx
      1^(can 0 7^op 1^0 ~)
      20^address
      ~
    ==
  ::
  ++  from-proxy
    |=  prx=@tas
    ^-  [@ @]
    =/  proxy
      ?+  prx  !!
        %own       %0
        %spawn     %1
        %manage    %2
        %vote      %3
        %transfer  %4
      ==
    1^(can 0 3^proxy 5^0 ~)
  ::
  --
::
++  hash-tx  keccak-256:keccak:crypto
::
++  hash-raw-tx
  |=  raw-tx:naive
  ^-  @ux
  %-  hash-tx
  %:  cad:naive  3
    65^sig
    raw
    ~
  ==
::
--
