::  urbit-style key generation and derivation functions
::
/+  bip32
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
  %.  [inp (to-byts 'urbitkeygen')]
  %-  argon2:argon2:crypto
  [out %u 0x13 1 1.024 10 *byts *byts]
::
++  child-seed
  |=  [seed=byts type=tape series=@ud ship=@p]
  ^-  byts
  :-  64
  %-  sha-512l:sha
  =+  :(weld type "-" (a-co:co series) "-" (a-co:co ship))
  :-  (add wid.seed (lent -))
  (cat 3 (crip (flop -)) dat.seed)
::
++  wallet-from-seed
  |=  seed=byts
  =>  (from-seed:bip32 seed)
  :*  public=`@ux`public-key
      private=`@ux`private-key
      chain=`@ux`chain-code
  ==
::
++  generate-full-wallet
  |=  [entropy=byts shh=ship]  ::TODO  set of ships
  =+  owner-seed=16^(argon2u entropy 16)
  :-  owner=(wallet-from-seed owner-seed)
  =+  transfer-seed=(child-seed owner-seed "transfer" 0 shh)
  :-  transfer=(wallet-from-seed transfer-seed)
  =+  spawn-seed=(child-seed transfer-seed "spawn" 0 shh)
  :-  spawn=(wallet-from-seed spawn-seed)
  =+  manage-seed=(child-seed transfer-seed "manage" 0 shh)
  :-  manage=(wallet-from-seed manage-seed)
  =+  urbit-seed=(child-seed manage-seed "urbit" 0 shh)
  ~  ::TODO  networking keypairs from seed
--
