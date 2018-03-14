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
::  see also the json rpc api spec
::  https://ethereum.gitbooks.io/frontier-guide/content/rpc.html
::
++  json-request
  =,  eyre
  |=  [url=purl jon=json]
  ^-  hiss
  :^  url  %post
    (malt ~[content-type+['application/x-www-form-urlencoded']~])
  (some (as-octt (en-json:html jon)))
::
++  batch-read-request
  |=  req=(list [@ux @t (list data)])
  ^-  json
  a+(turn req read-request)
::
++  read-request
  |=  [adr=@ux fun=@t das=(list data)]
  ^-  json
  %-  request-to-json
  :+  %eth-call
    [~ `@`adr ~ ~ ~ `tape`(encode-call fun das)]
  [%label %latest]
::
++  batch-request-to-json
  |=  req=(list request)
  ^-  json
  a+(turn req request-to-json)
::
++  request-to-json
  =,  enjs:format
  |=  req=request
  ^-  json
  %-  pairs
  =;  cal=[m=@t p=(list json)]
    :~  jsonrpc+s+'2.0'
        ::TODO  allow to self-set, because batch requests.
        id+s+'use wire to id response in hoon'
        method+s+m.cal
        params+a+p.cal
    ==
  ?+  -.req  ~|([%unsupported-request -.req] !!)
      %eth-block-number
    ['eth_blockNumber' ~]
  ::
      %eth-call
    :-  'eth_call'
    :~  (eth-call-to-json cal.req)
        (default-block-to-json deb.req)
    ==
  ==
::
++  eth-call-to-json
  |=  cal=call
  ^-  json
  :-  %o  %-  ~(gas by *(map @t json))
  =-  (murn - same)
  ^-  (list (unit (pair @t json)))
  :~  ?~  from.cal  ~
      `['from' s+(crip (render-hex-bytes 20 `@`u.from.cal))]
    ::
      `['to' s+(crip (render-hex-bytes 20 `@`to.cal))]
    ::
      ?~  gas.cal  ~
      `['gas' n+(crip ((d-co:co 0) u.gas.cal))]
    ::
      ?~  gas-price.cal  ~
      `['gasPrice' n+(crip ((d-co:co 0) u.gas-price.cal))]
    ::
      ?~  value.cal  ~
      `['value' n+(crip ((d-co:co 0) u.value.cal))]
    ::
      ?~  data.cal  ~
      `['data' s+(crip data.cal)]
  ==
::
++  default-block-to-json
  |=  dob=default-block
  ^-  json
  ?-  -.dob
    %quantity   n+(crip ((d-co:co 0) n.dob))
    %label      s+l.dob
  ==
::
::  decoding
::
::  for details on encoding, see below.
::
++  decode-results
  :>  rex:  string of hex bytes with leading 0x.
  |=  [rex=@t tys=(list etyp)]
  (decode-arguments (rsh 3 2 rex) tys)
::
++  decode-arguments
  :>  res:  string of hex bytes without leading 0x.
  |=  [res=@t tys=(list etyp)]
  :>  wos:  input as list of 32 byte words in hex.
  =/  wos=(list @t)  (rip 9 res)
  :>  win:  index of the word to process.
  =|  win=@ud
  :>  das:  resulting data, should match {tys}.
  =|  das=(list data)
  |-  ^+  das
  ?~  tys  (flop `(list data)`das)
  =*  typ  i.tys
  ?:  (gte win (lent wos))  ~|(%insufficient-data !!)
  =-  $(das [dat das], win nin, tys t.tys)
  ::=<  (decode-next win i.tys)  ::TODO  urbit/arvo#673
  |^  ::|%  ++  decode-next  |=  [win=@ud typ=etyp]
    ^-  [nin=@ud dat=data]
    =+  wor=(snag win wos)
    ?+  typ
      ~|  [%unsupported-type typ]
      !!
    ::
        ?(%address %bool %uint)  ::  %int %real %ureal
      :-  +(win)
      ?-  typ
        %address  address+(rash wor hex)
        %uint     uint+(rash wor hex)
        %bool     bool+=(0 (rash wor hex))
      ==
    ::
        %string
      =+  $(typ %bytes)
      ?>  ?=(%bytes -.dat)
      [nin %string (trip (swp 3 q.p.dat))]
    ::
        %bytes
      :+  +(win)  %bytes
      ::  find the word index of the actual data.
      =/  lic=@ud  (div (rash wor hex) 32)
      ::  learn the bytelength of the data.
      =/  len=@ud  (rash (snag lic wos) hex)
      (decode-bytes-n +(lic) len)
    ::
        [%bytes-n *]
      :-  (add win +((div n.typ 32)))
      [%bytes-n (decode-bytes-n win n.typ)]
    ::
        [%array *]
      :+  +(win)  %array
      ::  find the word index of the actual data.
      =.  win  (div (rash wor hex) 32)
      ::  read the elements from their location.
      ::TODO  see ++decode-array-n
      :: %^  decode-array-n  t.typ  +(win)
      :: (rash (snag win wos) hex)
      =/  len=@ud  (rash (snag win wos) hex)
      =.  win  +(win)
      =|  els=(list data)
      |-  ^+  els
      ?:  =(len 0)  (flop els)
      =+  ^$(typ t.typ, win win)
      $(els [dat els], win nin, len (dec len))
    ::
        [%array-n *]
      ::TODO  see ++decode-array-n
      :: (decode-array-n t.typ win n.typ)
      =|  els=(list data)
      |-
      ?:  =(n.typ 0)  [win %array-n (flop els)]
      ~&  [%arr-left n.typ win]
      =+  ^$(typ t.typ, win win)
      $(els [dat els], win nin, n.typ (dec n.typ))
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
      (swag [fro +((div bys 32))] wos)
    |=(a=@t [1 a])
  ::
  ::TODO  uncomment and use once urbit/arvo#673 gets resolved/a workaround.
  :: ++  decode-array-n
  ::   |=  [typ=etyp fro=@ud len=@ud]
  ::   =|  els=(list data)
  ::   |-  ^+  els
  ::   ?:  =(len 0)  (flop els)
  ::   =+  (decode-next typ fro)
  ::   $(els [dat els], fro nin, len (dec len))
  --
::
::  encoding
::
::  ABI spec used for reference:
::  https://ethereum.gitbooks.io/frontier-guide/content/abi.html
::
++  encode-call
  |=  [fun=@t das=(list data)]
  ^-  tape
  ::TODO  should this check to see if the data matches the function signature?
  =-  (weld - (encode-args das))
  %+  scag  8
  (render-hex-bytes 32 (keccak-256 (as-octs fun)))
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
