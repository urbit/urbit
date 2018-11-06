::  urbit-style key generation and derivation functions
::
/-  keygen
::
/+  bip32, bip39
::
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
  [inp (to-byts 'urbitkeygen')]
::
++  child-node-from-seed
  |=  [seed=@ met=meta pass=(unit @t)]
  ^-  node
  =+  sed=(seed:ds 32^seed met)
  =+  nom=(from-entropy:bip39 32^sed)
  :+  met  nom
  %-  wallet:ds
  %+  to-seed:bip39  nom
  (trip (fall pass ''))
::
++  full-wallet-from-ticket
  |=  [ticket=byts sis=(set ship) revs=revisions pass=(unit @t)]
  ^-  vault
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
  :-  ^=  ownership  ^-  nodes
      (nn "ownership" ownership.revs)
  ::
  :-  ^=  voting  ^-  nodes
      (nn "voting" voting.revs)
  ::
  =/  management=nodes
    (nn "management" management.revs)
  :-  management=management
  ::
  :-  ^=  transfer  ^-  nodes
      (nn "transfer" transfer.revs)
  ::
  :-  ^=  spawn  ^-  nodes
      (nn "spawn" spawn.revs)
  ::
  ^=  network  ^-  uodes
  %-  ~(rep in sis)
  |=  [who=ship nus=uodes]
  %+  ~(put by nus)  who
  =/  mad
    %+  to-seed:bip39
      seed:(~(got by management) who)
    (trip (fall pass ''))
  =+  met=["network" network.revs who]
  =+  sed=(seed:ds 64^mad met)
  [met sed (urbit:ds sed)]
::
++  ds                                                  ::  derive from raw seed
  |%
  ++  wallet
    |=  seed=@
    ^-  ^wallet
    =+  =>  (from-seed:bip32 64^seed)
        (derive-path "m/44'/60'/0'/0/0")
    :+  [public-key private-key]
      (address-from-prv:ethereum private-key)
    chain-code
  ::
  ++  urbit
    |=  seed=@
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
    |=  [seed=byts meta]
    ^-  @ux
    =/  salt=tape
      ;:  weld
        typ
        ['-' (a-co:co who)]
        ['-' (a-co:co rev)]
      ==
    %-  sha-256l:sha
    :-  (add wid.seed (lent salt))
    (cat 3 (crip (flop salt)) dat.seed)
  --
--
