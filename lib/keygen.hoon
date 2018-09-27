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
  |=  $:  ticket=byts
          seed-size=@ud
          sis=(set ship)
          pass=(unit @t)
          revs=revisions
          boot=?
      ==
  ^-  vault
  =/  owner-seed=byts
    seed-size^(argon2u ticket seed-size)
  =+  dr=~(. sd pass)
  =/  cn
    |=  m=meta
    (child-node-from-seed owner-seed m pass)
  ::
  :-  `@q`dat.ticket
  ::
  :-  :+  *meta  dat.owner-seed
      (wallet:dr owner-seed)
  ::
  =/  manage=nodes
    %-  ~(rep in sis)
    |=  [s=ship n=nodes]
    %+  ~(put by n)  s
    (cn "manage" manage.revs `s)
  :-  manage
  ::
  :-  %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      ?.  =(%czar (clan:title s))  n
      %+  ~(put by n)  s
      (cn "voting" voting.revs `s)
  ::
  :-  %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn "transfer" transfer.revs `s)
  ::
  :-  %-  ~(rep in sis)
      |=  [s=ship n=nodes]
      %+  ~(put by n)  s
      (cn "spawn" spawn.revs `s)
  ::
  ?.  boot  ~
  %-  ~(rep in sis)
  |=  [s=ship u=uodes]
  %+  ~(put by u)  s
  =+  m=["network" network.revs `s]
  =+  s=(seed:dr wid.owner-seed^seed:(~(got by manage) s) m)
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
    :+  [public-key private-key]
      chain-code
    (address-from-pub:ethereum public-key)
  ::
  ++  urbit
    %+  cork  append-pass
    |=  seed=byts
    ^-  edkeys
    =+  =<  [pub=pub:ex sec=sec:ex]
        (pit:nu:crub:crypto (mul 8 wid.seed) dat.seed)
    ::  crypt
    :-  :-  (rsh 3 33 pub)
            (rsh 3 33 sec)
    ::  auth
    :-  (rsh 3 1 (end 3 33 pub))
        (rsh 3 1 (end 3 33 sec))
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
