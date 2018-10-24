::  urbit-style key generation and derivation functions
::
/-  keygen
::
/+  bip32
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
  [inp (to-byts 'urbitkeygen')]
::
++  child-node-from-seed
  |=  [seed=@ met=meta pass=(unit @t)]
  ^-  node
  =+  dr=~(. sd pass)
  =+  child-seed=(seed:dr seed met)
  :+  met  child-seed
  (wallet:dr child-seed)
::
++  full-wallet-from-ticket
  |=  [ticket=byts sis=(set ship) pass=(unit @t) revs=revisions]
  =+  master-seed=(argon2u ticket)
  =+  dr=~(. sd pass)
  =+  cn=|=([s=@ m=meta] (child-node-from-seed s m pass))
  ::
  :-  ^=  owner  ^-  node
      (cn master-seed "owner" owner.revs ~)
  ::
  :-  ^=  delegate
      (cn master-seed "delegate" delegate.revs ~)
  ::
  =/  manage=node
    (cn master-seed "manage" manage.revs ~)
  :-  manage=manage
  ::
  :-  ^=  transfer
      %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn master-seed "transfer" transfer.revs `s)
  ::
  :-  ^=  spawn
      %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn master-seed "spawn" spawn.revs `s)
  ::
  ^=  network
  %-  ~(rep in sis)
  |=  [s=ship u=uodes]
  %+  ~(put by u)  s
  =+  m=["network" network.revs `s]
  =+  s=(seed:dr seed.manage m)
  [m s (urbit:dr s)]
::
++  sd                                                  ::  seed derivation
  |_  pass=(unit @t)
  ++  wallet
    |=  seed=@ux
    ^-  ^wallet
    =>  (from-seed:bip32 32^seed)
    [public-key private-key chain-code]
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
        typ  "-"  (a-co:co rev)
        ?~(who ~ ['-' (a-co:co u.who)])
      ==
    %-  sha-256l
    :-  (add 32 (lent salt))
    (cat 3 (crip (flop salt)) seed)
  --
--
