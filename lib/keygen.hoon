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
++  to-byts
  |=  a=@t
  =+  (met 3 a)
  [- (rev 3 - a)]
::
++  argon2u
  |=  [inp=byts out=@ud]
  ^-  @
  %.  [inp (to-byts 'urbitkeygen')]
  %-  argon2:argon2:crypto
  [out %u 0x13 1 1.024 10 *byts *byts]
::
++  full-wallet-from-entropy
  |=  [entropy=byts seed-size=@ud sis=(set ship) pass=(unit @t)]
  =+  owner-seed=seed-size^(argon2u entropy seed-size)
  (full-wallet-from-seed owner-seed sis pass)
::
++  full-wallet-from-seed
  |=  [owner-seed=byts sis=(set ship) pass=(unit @t)]
  =+  dr=~(. sd pass)
  ::
  :-  ^=  owner  ^-  node
      :+  *meta  dat.owner-seed
      (wallet:dr owner-seed)
  ::
  =/  delegate-meta=meta  ["delegate" 0 ~]
  =+  delegate-seed=(seed:dr owner-seed delegate-meta)
  :-  ^=  delegate  ^-  node
      :-  meta=delegate-meta
      :-  seed=`@ux`dat.delegate-seed
      keys=(wallet:dr delegate-seed)
  ::
  =/  manage-meta=meta  ["manage" 0 ~]
  =+  manage-seed=(seed:dr owner-seed manage-meta)
  :-  ^=  manage  ^-  node
      :-  meta=manage-meta
      :-  seed=`@ux`dat.manage-seed
      keys=(wallet:dr manage-seed)
  ::
  =/  transfer=(map ship (pair byts node))
    %-  ~(rep in sis)
    |=  [w=ship n=(map ship (pair byts node))]
    %+  ~(put by n)  w
    =+  m=["transfer" 0 `w]
    =+  s=(seed:dr owner-seed -)
    [s [m dat.s (wallet:dr s)]]
  :-  ^=  transfer  ^-  nodes
      (~(run by transfer) tail)
  ::
  :-  ^=  spawn  ^-  nodes
      %-  ~(rep by transfer)
      |=  [[w=ship s=byts *] n=(map ship node)]
      %+  ~(put by n)  w
      =+  m=["spawn" 0 `w]
      =+  s=(seed:dr s m)
      [m dat.s (wallet:dr s)]
  ::
  ^=  network  ^-  uodes
  %-  ~(rep in sis)
  |=  [w=ship u=(map ship uode)]
  %+  ~(put by u)  w
  =+  m=["network" 0 `w]
  =+  s=(seed:dr manage-seed m)
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
