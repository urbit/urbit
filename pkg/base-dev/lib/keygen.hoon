::  urbit-style key generation and derivation functions
::
/-  keygen
::
/+  ethereum, bip32, bip39
::
=,  keygen
::
|%
++  argon2u
  |=  [who=ship tic=byts]
  ^-  @
  ~|  [%who who (met 3 who)]
  :: ?>  (lte (met 3 who) 4)
  %-  (argon2-urbit:argon2:crypto 32)
  :-  tic
  =-  [(met 3 -) (swp 3 -)]
  %-  crip
  (weld "urbitkeygen" (a-co:co who))
::
++  child-node-from-seed
  |=  [seed=@ typ=tape pass=(unit @t)]
  ^-  node
  =+  sed=(seed:ds 32^seed typ)
  =+  nom=(from-entropy:bip39 32^sed)
  :+  typ  nom
  %-  wallet:ds
  %+  to-seed:bip39  nom
  (trip (fall pass ''))
::
++  derive-network-seed
  |=  [mngs=@ rev=@ud]
  ^-  @ux
  =+  (seed:ds 64^mngs (weld "network" (a-co:co rev)))
  ?:  =(0 rev)  -
  ::  hash again to prevent length extension attacks
  (sha-256l:sha 32 -)
::
++  ownership-wallet-from-ticket
  |=  [who=ship ticket=byts pass=(unit @t)]
  ^-  node
  =+  master-seed=(argon2u who ticket)
  (child-node-from-seed master-seed "ownership" pass)
::
++  full-wallet-from-ticket
  ::  who:    username
  ::  ticket: password
  ::  rev:    network key revision
  ::  pass:   optional passphrase
  ::
  |=  [who=ship ticket=byts rev=@ud pass=(unit @t)]
  ^-  vault
  =+  master-seed=(argon2u who ticket)
  =/  cn  ::  child node
    |=  typ=nodetype
    (child-node-from-seed master-seed typ pass)
  ::
  :-  ^=  ownership  ^-  node
      (cn "ownership")
  ::
  :-  ^=  voting  ^-  node
      (cn "voting")
  ::
  =/  management=node
    (cn "management")
  :-  management=management
  ::
  :-  ^=  transfer  ^-  node
      (cn "transfer")
  ::
  :-  ^=  spawn  ^-  node
      (cn "spawn")
  ::
  ^=  network  ^-  uode
  =/  mad  ::  management seed
    %+  to-seed:bip39
      seed:management
    (trip (fall pass ''))
  =+  sed=(derive-network-seed mad rev)
  [rev sed (urbit:ds sed)]
::
++  ds                                                  ::  derive from raw seed
  |%
  ++  wallet
    |=  seed=@
    ^-  ^wallet
    =+  =>  (from-seed:bip32 64^seed)
        (derive-path "m/44'/60'/0'/0/0")
    :+  [public-key private-key]
      (address-from-prv:key:ethereum private-key)
    chain-code
  ::
  ++  urbit
    |=  seed=@
    ^-  edkeys
    =+  =<  [pub=pub:ex sec=sec:ex]
        (pit:nu:crub:crypto 256 seed)
    :-  ^=  auth
        :-  (rsh 3 (end [3 33] pub))
            (rsh 3 (end [3 33] sec))
    ^=  crypt
    :-  (rsh [3 33] pub)
        (rsh [3 33] sec)
  ::
  ++  seed
    |=  [seed=byts salt=tape]
    ^-  @ux
    %-  sha-256l:sha
    :-  (add wid.seed (lent salt))
    (cat 3 (crip (flop salt)) dat.seed)
  --
--
