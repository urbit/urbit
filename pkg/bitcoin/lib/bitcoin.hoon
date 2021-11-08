::  bitcoin.hoon
::  top-level Bitcoin constants
::  expose BIP libraries
::
/-  *bitcoin
/+  bech32=bip-b173, pbt=bip-b174, bcu=bitcoin-utils, bip-b158
~%  %bitcoin-lib  ..part  ~
|%
++  overhead-weight  ^-(vbytes 11)
++  input-weight
  ~/  %input-weight
  |=  =bipt
  ^-  vbytes
  ?-  bipt
    %44  148
    %49  91
    %84  68
  ==
++  output-weight
  |=  =bipt
  ^-  vbytes
  ?-  bipt
    %44  34
    %49  32
    %84  31
  ==
::
++  xpub-type
  |=  =xpub
  ^-  [=bipt =network]
  =/  prefix=tape  (scag 4 (trip xpub))
  ?:  =("tpub" prefix)  [%44 %testnet]
  ?:  =("upub" prefix)  [%49 %testnet]
  ?:  =("vpub" prefix)  [%84 %testnet]
  ?:  =("xpub" prefix)  [%44 %main]
  ?:  =("ypub" prefix)  [%49 %main]
  ?:  =("zpub" prefix)  [%84 %main]
  ~|("invalid xpub: {<xpub>}" !!)
::
::  adr: address manipulation
::
++  adr
  ~%  %adr  ..overhead-weight  ~
  |%
  ++  get-bipt
    ~/  %get-bipt
    |=  a=address
    ^-  bipt
    =/  spk=hexb  (to-script-pubkey:adr a)
    ?:  =(25 wid.spk)  %44
    ?:  =(23 wid.spk)  %49
    ?:  =(22 wid.spk)  %84
    ?:  =(34 wid.spk)  %84
    ~|("Invalid address" !!)
  ::
  ++  to-cord
    ~/  %to-cord
    |=  a=address  ^-  cord
    ?:  ?=([%base58 *] a)
      %-  crip
      %+  slag  2
      (scow %uc +.a)
    +.a
  ::
  ++  from-pubkey
    ~/  %from-pubkey
    |=  [=bipt =network pubkey=hexb]
    ^-  address
    ?-  bipt
        %44
      :-  %base58
      =<  ^-(@uc dat)
      %-  cat:byt:bcu
      :-  ?-  network
              %main     1^0x0
              %testnet  1^0x6f
          ==
      ~[(hash-160:bcu pubkey)]
      ::
        %49
      :-  %base58
      =<  ^-(@uc dat)
      %-  cat:byt:bcu
      :~  ?-  network
            %main     1^0x5
            %testnet  1^0xc4
          ==
          %-  hash-160:bcu
          (cat:byt:bcu ~[2^0x14 (hash-160:bcu pubkey)])
      ==
      ::
        %84
      :-  %bech32
      (need (encode-pubkey:bech32 network pubkey))
    ==
  ::
  ++  from-cord
    ~/  %from-cord
    |=  addrc=@t
    |^
    =/  addrt=tape  (trip addrc)
    ^-  address
    ?:  (is-base58 addrt)
      [%base58 `@uc`(scan addrt fim:ag)]
    ?:  (is-bech32 addrt)
      [%bech32 addrc]
    ~|("Invalid address: {<addrc>}" !!)
    ::
    ++  is-base58
      |=  at=tape
      ^-  ?
      ?|  =("m" (scag 1 at))
          =("1" (scag 1 at))
          =("3" (scag 1 at))
          =("2" (scag 1 at))
      ==
    ::
    ++  is-bech32
      |=  at=tape
      ^-  ?
      ?|  =("bc1" (scag 3 at))
          =("tb1" (scag 3 at))
      ==
    --
  ::
  ++  to-script-pubkey
    ~/  %to-script-pubkey
    |=  =address
    ^-  hexb
    ?-  -.address
        %bech32
      =+  h=(from-address:bech32 +.address)
      %-  cat:byt:bcu
      :~  1^0x0
          1^wid.h
          h
      ==
      ::
        %base58
      =/  h=hexb  [21 `@ux`+.address]
      =+  lead-byt=dat:(take:byt:bcu 1 h)
      =/  version-network=[bipt network]
        ?:  =(0x0 lead-byt)   [%44 %main]
        ?:  =(0x6f lead-byt)  [%44 %testnet]
        ?:  =(0x5 lead-byt)   [%49 %main]
        ?:  =(0xc4 lead-byt)  [%49 %testnet]
        ~|("Invalid base58 address: {<+.address>}" !!)
      %-  cat:byt:bcu
      ?:  ?=(%44 -.version-network)
        :~  3^0x76.a914
            (drop:byt:bcu 1 h)
            2^0x88ac
        ==
      :~  2^0xa914
          (drop:byt:bcu 1 h)
          1^0x87
      ==
    ==
  --
::
::  +txu: transaction utility core
::   - primarily used for calculating txids
::   - ignores signatures in inputs
::
++  txu
  ~%  %bitcoin-lib-txu  ..overhead-weight  ~
  =,  bcu
  |%
  ++  en
    |%
    ++  input
      |=  i=input:tx
      ^-  hexb
      %-  cat:byt
      :~  (flip:byt txid.i)
          (flip:byt 4^pos.i)
          ?~  script-sig.i  1^0x0
        %-  cat:byt
        ~[(en:csiz wid.u.script-sig.i) u.script-sig.i]
          (flip:byt sequence.i)
      ==
    ::
    ++  output
      |=  o=output:tx
      ^-  hexb
      %-  cat:byt
      :~  (flip:byt 8^value.o)
          1^wid.script-pubkey.o
          script-pubkey.o
      ==
    --
  ::
  ++  de
    |%
    ++  nversion
      |=  b=hexb
      ^-  [nversion=@ud rest=hexb]
      :-  dat:(flip:byt (take:byt 4 b))
      (drop:byt 4 b)
    ::
    ++  segwit
      |=  b=hexb
      ^-  [segwit=(unit @ud) rest=hexb]
      ?.  =(1^0x0 (take:byt 1 b))
        [~ b]
      :-  [~ dat:(take:byt 2 b)]
      (drop:byt 2 b)
    ::
    ++  script-sig
      |=  b=hexb
      ^-  [sig=hexb rest=hexb]
      =^  siglen=hexb  b  (de:csiz b)
      :-  (take:byt dat.siglen b)
      (drop:byt dat.siglen b)
    ::
    ++  sequence
      |=  b=hexb
      ^-  [seq=hexb rest=hexb]
      [(flip:byt (take:byt 4 b)) (drop:byt 4 b)]
    ::
    ++  inputs
      |=  b=hexb
      ^-  [is=(list input:tx) rest=hexb]
      |^
      =|  acc=(list input:tx)
      =^  count  b  (dea:csiz b)
      |-
      ?:  =(0 count)  [acc b]
      =^  i  b  (input b)
      $(acc (snoc acc i), count (dec count))
      ::
      ++  input
        |=  b=hexb
        ^-  [i=input:tx rest=hexb]
        =/  txid  (flip:byt (take:byt 32 b))
        =/  pos   dat:(flip:byt (take:byt 4 (drop:byt 32 b)))
        =^  sig=hexb  b  (script-sig (drop:byt 36 b))
        =^  seq=hexb  b  (sequence b)
        :_  b
        [txid pos seq ?:((gth wid.sig 0) `sig ~) ~ 0]
      --
    ::
    ++  outputs
      |=  b=hexb
      ^-  [os=(list output:tx) rest=hexb]
      =|  acc=(list output:tx)
      =^  count  b  (dea:csiz b)
      |-
      ?:  =(0 count)  [acc b]
      =/  value  (flip:byt (take:byt 8 b))
      =^  scriptlen  b  (dea:csiz (drop:byt 8 b))
      %=  $
          acc  %+  snoc  acc
               :-  (take:byt scriptlen b)
               dat.value
          b  (drop:byt scriptlen b)
          count  (dec count)
      ==
    --
  ::  +basic-encode: encodes data in a format suitable for hashing
  ::
  ++  basic-encode
    |=  =data:tx
    ^-  hexb
    %-  cat:byt
    %-  zing
    :~  ~[(flip:byt 4^nversion.data)]
        ~[(en:csiz (lent is.data))]
        (turn is.data input:en)
        ~[(en:csiz (lent os.data))]
        (turn os.data output:en)
        ~[(flip:byt 4^locktime.data)]
    ==
  ++  get-id
    |=  =data:tx
    ^-  hexb
    %-  flip:byt
    %-  dsha256
    (basic-encode data)
  ::
  ++  decode
    |=  b=hexb
    ^-  data:tx
    =^  nversion  b
      (nversion:de b)
    =^  segwit  b
      (segwit:de b)
    =^  inputs  b
      (inputs:de b)
    =^  outputs  b
      (outputs:de b)
    =/  locktime=@ud
      dat:(take:byt 4 (flip:byt b))
    [inputs outputs locktime nversion segwit]
  --
--
