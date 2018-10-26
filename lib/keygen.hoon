::  urbit-style key generation and derivation functions
::
/-  keygen
::
/+  bip32, bip39
::
::
=,  sha
=,  keygen
::
|%
++  to-byts
  |=  a=@t
  =+  (met 3 a)
  [- (rev 3 - a)]
::
++  argon2u
  |=  inp=byts
  ^-  @
  %-  (argon2-urbit:argon2:crypto 32)
  [inp (to-byts 'urbitwallet')]
::
++  child-node-from-seed
  |=  [seed=@ met=meta pass=(unit @t)]
  ^-  node
  =+  dr=~(. ds pass)
  =+  sed=(seed:dr seed met)
  =+  nom=(from-entropy:bip39 32^sed)
  :+  met  nom
  %-  wallet:dr
  %+  to-seed:bip39  nom
  (trip (fall pass ''))
::
++  full-wallet-from-ticket
  |=  [ticket=byts sis=(set ship) pass=(unit @t) revs=revisions]
  =+  master-seed=(argon2u ticket)
  =/  nn
    |=  [typ=tape rev=@ud]
    %-  ~(rep in sis)
    |=  [who=ship nos=nodes]
    %+  ~(put by nos)  who
    %^  child-node-from-seed
        master-seed
      [typ rev who]
    pass
  ::
  :-  ^=  owner  ^-  nodes
      (nn "owner" owner.revs)
  ::
  :-  ^=  delegate  ^-  nodes
      (nn "delegate" delegate.revs)
  ::
  =/  manage=nodes
    (nn "manage" manage.revs)
  :-  manage=manage
  ::
  :-  ^=  transfer  ^-  nodes
      (nn "transfer" transfer.revs)
  ::
  :-  ^=  spawn  ^-  nodes
      (nn "spawn" spawn.revs)
  ::
  ^=  network  ^-  uodes
  =+  dr=~(. ds pass)
  %-  ~(rep in sis)
  |=  [who=ship nus=uodes]
  %+  ~(put by nus)  who
  =/  mad
    %+  to-seed:bip39
      seed:(~(got by manage) who)
    (trip (fall pass ''))
  =+  met=["network" network.revs who]
  =+  sed=(seed:dr mad met)
  [met sed (urbit:dr sed)]
::
++  ds                                                  ::  derive from raw seed
  |_  pass=(unit @t)
  ++  wallet
    |=  seed=@ux
    ^-  ^wallet
    =+  =>  (from-seed:bip32 32^seed)
        (derive-path "m/44’/60’/0’/0/0")
    :+  [public-key private-key]
      (address-from-pub:ethereum public-key)
    chain-code
  ::
  ++  urbit
    |=  seed=@ux
    ^-  edkeys
    =+  =<  [pub=pub:ex sec=sec:ex]
        (pit:nu:crub:crypto 256 seed)
    :-  ^=  auth
        :-  (rsh 3 1 (end 3 33 pub))
            (rsh 3 1 (end 3 33 sec))
    ^=  crypt
    :-  (rsh 3 33 pub)
        (rsh 3 33 sec)
  ::
  ++  seed
    |=  [seed=@ux meta]
    ^-  @ux
    =/  salt=tape
      ;:  weld
        typ
        ['-' (a-co:co who)]
        ['-' (a-co:co rev)]
      ==
    %-  sha-256l
    :-  (add 32 (lent salt))
    (cat 3 (crip (flop salt)) seed)
  --
--
