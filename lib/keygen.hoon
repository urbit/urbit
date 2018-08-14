::  urbit-style key generation and derivation functions
::
/-  keygen
::
/+  bip32
::
::
=,  sha
=,  ^keygen
::
|%
++  to-byts
  |=  a=@t
  =+  (met 3 a)
  [- (rev 3 - a)]
::
++  argon2u
  |=  [inp=byts out=@ud]
  ^-  @
  %-  (argon2-urbit:argon2:crypto out)
  [inp (to-byts 'urbitkeygen')]
::
++  child-node-from-seed
  |=  [seed=byts met=meta pass=(unit @t)]
  ^-  node
  =+  dr=~(. sd pass)
  =+  child-seed=(seed:dr seed met)
  :+  met  dat.child-seed
  (wallet:dr child-seed)
::
++  full-wallet-from-ticket
  |=  [ticket=byts seed-size=@ud sis=(set ship) pass=(unit @t) revs=revisions]
  =+  owner-seed=seed-size^(argon2u ticket seed-size)
  (full-wallet-from-seed owner-seed sis pass revs)
::
++  full-wallet-from-seed
  |=  [owner-seed=byts sis=(set ship) pass=(unit @t) revs=revisions]
  =+  dr=~(. sd pass)
  =+  cn=|=([s=byts m=meta] (child-node-from-seed s m pass))
  ::
  :-  ^=  owner  ^-  node
      :+  *meta  dat.owner-seed
      (wallet:dr owner-seed)
  ::
  :-  ^=  delegate
      (cn owner-seed "delegate" delegate.revs ~)
  ::
  =/  manage=node
    (cn owner-seed "manage" manage.revs ~)
  :-  manage=manage
  ::
  :-  ^=  transfer
      %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn owner-seed "transfer" transfer.revs `s)
  ::
  :-  ^=  spawn
      %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn owner-seed "spawn" spawn.revs `s)
  ::
  ^=  network
  %-  ~(rep in sis)
  |=  [s=ship u=uodes]
  %+  ~(put by u)  s
  =+  m=["network" network.revs `s]
  =+  s=(seed:dr [wid.owner-seed seed.manage] m)
  [m dat.s (urbit:dr s)]
::
++  sd                                                  ::  seed derivation
  |_  pass=(unit @t)
  ++  append-pass
    |=  b=byts
    ^-  byts
    =+  (fall pass '')
    :-  (add wid.b (met 3 -))
    (cat 3 (swp 3 -) dat.b)
  ::
  ++  wallet
    %+  cork  append-pass
    |=  seed=byts
    ^-  ^wallet
    =>  (from-seed:bip32 64^(sha-512l seed))
    [public-key private-key chain-code]
  ::
  ++  urbit
    %+  cork  append-pass
    |=  seed=byts
    ^-  edkeys
    =+  =<  [pub=pub:ex sec=sec:ex]
        (pit:nu:crub:crypto (mul 8 wid.seed) dat.seed)
    :-  ^=  auth
        :-  (rsh 3 1 (end 3 33 pub))
            (rsh 3 1 (end 3 33 sec))
    ^=  crypt
    :-  (rsh 3 33 pub)
        (rsh 3 33 sec)
  ::
  ++  seed
    |=  [seed=byts meta]
    ^-  byts
    :-  wid.seed
    %^  rsh  3  (sub 64 wid.seed)
    %-  sha-512l
    %-  append-pass
    =+  ;:  weld
          typ  "-"  (a-co:co rev)
          ?~(who ~ ['-' (a-co:co u.who)])
        ==
    :-  (add wid.seed (lent -))
    (cat 3 (crip (flop -)) dat.seed)
  --
--
