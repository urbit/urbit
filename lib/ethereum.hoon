::
/-  ethereum
=,  ^ethereum
=,  keccak:crypto
=,  mimes:html
::
|%
::
::  making calls to nodes
::
::  see also the json rpc api spec:
::  https://ethereum.gitbooks.io/frontier-guide/content/rpc.html
::
++  json-request
  =,  eyre
  |=  [url=purl jon=json]
  ^-  hiss
  :^  url  %post
    %-  ~(gas in *math)
    ~['Content-Type'^['application/json']~]
  (some (as-octt (en-json:html jon)))
::
++  batch-read-request
  |=  req=(list proto-read-request)
  ^-  json
  a+(turn req read-request)
::
++  read-request
  |=  proto-read-request
  ^-  json
  %+  request-to-json  id
  :+  %eth-call
    ^-  call
    [~ to ~ ~ ~ `tape`(encode-call function arguments)]
  [%label %latest]
::
++  request-to-json
  =,  enjs:format
  |=  [riq=(unit @t) req=request]
  ^-  json
  %-  pairs
  =;  r=[met=@t pas=(list json)]
    :*  jsonrpc+s+'2.0'
        method+s+met.r
        params+a+pas.r
        ::TODO  would just jamming the req noun for id be a bad idea?
        ?~  riq  ~
        [id+s+u.riq]~
    ==
  ?-  -.req
      %eth-block-number
    ['eth_blockNumber' ~]
  ::
      %eth-call
    :-  'eth_call'
    :~  (eth-call-to-json cal.req)
        (block-to-json deb.req)
    ==
  ::
      %eth-new-filter
    :-  'eth_newFilter'
    :_  ~
    :-  %o  %-  ~(gas by *(map @t json))
    =-  (murn - same)
    ^-  (list (unit (pair @t json)))
    :~  ?~  fro.req  ~
        `['fromBlock' (block-to-json u.fro.req)]
      ::
        ?~  tob.req  ~
        `['toBlock' (block-to-json u.tob.req)]
      ::
        ::TODO  fucking tmi
        ?:  =(0 (lent adr.req))  ~
        :+  ~  'address'
        ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
        :-  %a
        (turn adr.req (cork address-to-hex tape))
      ::
        ?~  top.req  ~
        :^  ~  'topics'  %a
        (turn `(list octs)`top.req :(cork render-hex-bytes prefix-hex tape))
    ==
  ::
      %eth-get-filter-logs
    ['eth_getFilterLogs' (tape (num-to-hex fid.req)) ~]
  ::
      %eth-get-filter-changes
    ['eth_getFilterChanges' (tape (num-to-hex fid.req)) ~]
  ==
::
++  eth-call-to-json
  =,  enjs:format
  |=  cal=call
  ^-  json
  :-  %o  %-  ~(gas by *(map @t json))
  =-  (murn - same)
  ^-  (list (unit (pair @t json)))
  :~  ?~  from.cal  ~
      `['from' (tape (address-to-hex u.from.cal))]
    ::
      `['to' (tape (address-to-hex to.cal))]
    ::
      ?~  gas.cal  ~
      `['gas' (tape (num-to-hex u.gas.cal))]
    ::
      ?~  gas-price.cal  ~
      `['gasPrice' (tape (num-to-hex u.gas-price.cal))]
    ::
      ?~  value.cal  ~
      `['value' (tape (num-to-hex u.value.cal))]
    ::
      ?~  data.cal  ~
      `['data' (tape data.cal)]
  ==
::
++  block-to-json
  |=  dob=block
  ^-  json
  ?-  -.dob
    %number   s+(crip '0' 'x' ((d-co:co 1) n.dob))
    %label    s+l.dob
  ==
::
++  num-to-hex
  |=  n=@ud
  ^-  tape
  %-  prefix-hex
  (render-hex-bytes (as-octs n))
::
++  address-to-hex
  |=  a=address
  ^-  tape
  %-  prefix-hex
  (render-hex-bytes 20 `@`a)
::
++  prefix-hex
  |=  a=tape
  ^-  tape
  ['0' 'x' a]
::
::  parsing responses from nodes
::
++  parse-eth-new-filter-res
  |=  j=json
  ^-  @ud
  ?>  ?=(%s -.j)
  (hex-to-num p.j)
::
++  parse-eth-get-filter-changes-res
  =,  dejs:format
  |=  j=json
  =-  ((ar -) j)
  |=  cha=json
  ^-  filter-change
  =-  ((ot -) cha)
  :~  =-  [type+(cu - so)]
      |=  m=@t
      ?.  =(m 'mined')  ~
      =-  `((ot -) cha)  ::TODO  not sure if elegant or hacky.
      :~  'logIndex'^(cu hex-to-num so)
          'transactionIndex'^(cu hex-to-num so)
          'blockHash'^(cu hex-to-num so)
          'blockNumber'^(cu hex-to-num so)
      ==
    ::
      address+(cu hex-to-num so)
      data+so
    ::
      ::TODO  doesn't account for the anonymous event case, which has no hash.
      =-  topics+(cu - (ar so))
      |=  r=(list @t)
      ?>  ?=([@t *] r)
      [(hex-to-num i.r) t.r]
  ==
::
++  hex-to-num
  |=  a=@t
  (rash (rsh 3 2 a) hex)
::
::  decoding
::
::  for details on encoding, see below.
::
++  decode-results
  :>  rex:  string of hex bytes with leading 0x.
  |*  [rex=@t tys=(list etyp)]
  (decode-arguments (rsh 3 2 rex) tys)
::
++  decode-arguments
  |*  [res=@t tys=(list etyp)]
  =|  win=@ud
  =/  wos=(list @t)  (rip 9 res)
  =<  (decode-from 0 tys)
  |%
  ++  decode-from
    |*  [win=@ud tys=(list etyp)]
    ?~  tys  !!
    =-  ?~  t.tys  dat
        [dat $(win nin, tys t.tys)]
    (decode-one win ~[i.tys])
  ::
  ++  decode-one
    ::NOTE  we take (list etyp) even though we only operate on
    ::      a single etyp as a workaround for urbit/arvo#673
    |*  [win=@ud tys=(list etyp)]
    =-  [nin dat]=-  ::NOTE  ^= regular form broken
    ?~  tys  !!
    =*  typ  i.tys
    =+  wor=(snag win wos)
    ?+  typ
      ~|  [%unsupported-type typ]
      !!
    ::
        ?(%address %bool %uint)  ::  %int %real %ureal
      :-  +(win)
      ?-  typ
        %address  `@ux`(rash wor hex)
        %uint     `@ud`(rash wor hex)
        %bool     =(1 (rash wor hex))
      ==
    ::
        %string
      =+  $(tys ~[%bytes])
      ~!  -
      [nin (trip (swp 3 q.dat))]
    ::
        %bytes
      :-  +(win)
      ::  find the word index of the actual data.
      =/  lic=@ud  (div (rash wor hex) 32)
      ::  learn the bytelength of the data.
      =/  len=@ud  (rash (snag lic wos) hex)
      (decode-bytes-n +(lic) len)
    ::
        [%bytes-n *]
      :-  (add win +((div (dec n.typ) 32)))
      (decode-bytes-n win n.typ)
    ::
        [%array *]
      :-  +(win)
      ::  find the word index of the actual data.
      =.  win  (div (rash wor hex) 32)
      ::  read the elements from their location.
      %-  tail
      %^  decode-array-n  ~[t.typ]  +(win)
      (rash (snag win wos) hex)
    ::
        [%array-n *]
      (decode-array-n ~[t.typ] win n.typ)
    ==
  ::
  ++  decode-bytes-n
    |=  [fro=@ud bys=@ud]
    ^-  octs
    ::  parse {bys} bytes from {fro}.
    =-  [bys (rash - hex)]
    %^  end  3  (mul 2 bys)
    %+  can  9
    %+  turn
      (swag [fro +((div (dec bys) 32))] wos)
    |=(a=@t [1 a])
  ::
  ++  decode-array-n
    ::NOTE  we take (list etyp) even though we only operate on
    ::      a single etyp as a workaround for urbit/arvo#673
    =|  res=(list)
    ~&  %watch-out--arrays-without-typeinfo
    |*  [tys=(list etyp) fro=@ud len=@ud]
    ^-  [@ud (list)]
    ?~  tys  !!
    ?:  =(len 0)  [fro (flop `(list)`res)]
    =+  (decode-one fro ~[i.tys])  ::  [nin=@ud dat=*]
    $(res ^+(res [dat res]), fro nin, len (dec len))
  --
::
::  encoding
::
::  ABI spec used for reference:
::  https://ethereum.gitbooks.io/frontier-guide/content/abi.html
::
++  encode-call
  |=  call-data
  ^-  tape
  ::TODO  should this check to see if the data matches the function signature?
  =-  :(weld "0x" - (encode-args arguments))
  %+  scag  8
  (render-hex-bytes 32 (keccak-256 (as-octs function)))
::
++  encode-args
  :>  encode list of arguments.
  ::
  |=  das=(list data)
  ^-  tape
  (encode-data [%array-n das])
::
++  encode-data
  :>  encode typed data into ABI bytestring.
  ::
  |=  dat=data
  ^-  tape
  ?+  -.dat
    ~|  [%unsupported-type -.dat]
    !!
  ::
      %array-n
    ::  enc(X) = head(X[0]) ... head(X[k-1]) tail(X[0]) ... tail(X[k-1])
    ::  where head and tail are defined for X[i] being of a static type as
    ::  head(X[i]) = enc(X[i]) and tail(X[i]) = "" (the empty string), or as
    ::  head(X[i]) = enc(len(head(X[0])..head(X[k-1]) tail(X[0])..tail(X[i-1])))
    ::  and tail(X[i]) = enc(X[i]) otherwise.
    ::
    ::  so: if it's a static type, data goes in the head. if it's a dynamic
    ::  type, a reference goes into the head and data goes into the tail.
    ::
    ::  in the head, we first put a placeholder where references need to go.
    =+  hol=(reap 64 'x')
    =/  hes=(list tape)
      %+  turn  p.dat
      |=  d=data
      ?.  (is-dynamic-type d)  ^$(dat d)
      hol
    =/  tas=(list tape)
      %+  turn  p.dat
      |=  d=data
      ?.  (is-dynamic-type d)  ""
      ^$(dat d)
    ::  once we know the head and tail, we can fill in the references in head.
    =-  (weld nes `tape`(zing tas))
    ^-  [@ud nes=tape]
    =+  led=(lent (zing hes))
    %+  roll  hes
    |=  [t=tape i=@ud nes=tape]
    :-  +(i)
    ::  if no reference needed, just put the data.
    ?.  =(t hol)  (weld nes t)
    ::  calculate byte offset of data we need to reference.
    =/  ofs/@ud
      =-  (div - 2)       ::  two hex digits per byte.
      %+  add  led        ::  count head, and
      %-  lent  %-  zing  ::  count all tail data
      (scag i tas)        ::  preceding ours.
    =+  ref=^$(dat [%uint ofs])
    ::  shouldn't hit this unless we're sending over 2gb of data?
    ~|  [%weird-ref-lent (lent ref)]
    ?>  =((lent ref) (lent hol))
    (weld nes ref)
  ::
      %array  ::  where X has k elements (k is assumed to be of type uint256):
    ::  enc(X) = enc(k) enc([X[1], ..., X[k]])
    ::  i.e. it is encoded as if it were an array of static size k, prefixed
    ::  with the number of elements.
    %+  weld  $(dat [%uint (lent p.dat)])
    $(dat [%array-n p.dat])
  ::
      %bytes-n
    ::  enc(X) is the sequence of bytes in X padded with zero-bytes to a length
    ::  of 32.
    ::  Note that for any X, len(enc(X)) is a multiple of 32.
    (pad-to-multiple (render-hex-bytes p.dat) 64 %right)
  ::
      %bytes  ::  of length k (which is assumed to be of type uint256)
    ::  enc(X) = enc(k) pad_right(X), i.e. the number of bytes is encoded as a
    ::  uint256 followed by the actual value of X as a byte sequence, followed
    ::  by the minimum number of zero-bytes such that len(enc(X)) is a multiple
    ::  of 32.
    %+  weld  $(dat [%uint p.p.dat])
    $(dat [%bytes-n p.dat])
  ::
      %string
    ::  enc(X) = enc(enc_utf8(X)), i.e. X is utf-8 encoded and this value is
    ::  interpreted as of bytes type and encoded further. Note that the length
    ::  used in this subsequent encoding is the number of bytes of the utf-8
    ::  encoded string, not its number of characters.
    $(dat [%bytes (lent p.dat) (swp 3 (crip p.dat))])
  ::
      %uint
    ::  enc(X) is the big-endian encoding of X, padded on the higher-order
    ::  (left) side with zero-bytes such that the length is a multiple of 32
    ::  bytes.
    (pad-to-multiple (render-hex-bytes (as-octs p.dat)) 64 %left)
  ::
      %bool
    ::  as in the uint8 case, where 1 is used for true and 0 for false
    $(dat [%uint ?:(p.dat 1 0)])
  ::
      %address
    ::  as in the uint160 case
    $(dat [%uint `@ud`p.dat])
  ==
::
++  is-dynamic-type
  |=  a=data
  ?.  ?=(%array-n -.a)
    ?=(?(%string %bytes %array) -.a)
  &(!=((lent p.a) 0) (lien p.a is-dynamic-type))
::
::
++  render-hex-bytes
  :>  atom to string of hex bytes without 0x prefix and dots.
  |=  a=octs
  ^-  tape
  ((x-co:co (mul 2 p.a)) q.a)
::
++  pad-to-multiple
  |=  [wat=tape mof=@ud wer=?(%left %right)]
  =+  len=(lent wat)
  =+  tad=(reap (sub mof (mod len mof)) '0')
  %-  weld
  ?:(?=(%left wer) [tad wat] [wat tad])
--
