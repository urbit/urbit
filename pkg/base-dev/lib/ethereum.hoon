::  ethereum: utilities
::
=,  ethereum-types
|%
::  deriving and using ethereum keys
::
++  key
  |%
  ++  address-from-pub
    =,  keccak:crypto
    |=  pub=@
    %+  end  [3 20]
    %+  keccak-256  64
    (rev 3 64 pub)
  ::
  ++  address-from-prv
    (cork pub-from-prv address-from-pub)
  ::
  ++  pub-from-prv
    =,  secp256k1:secp:crypto
    |=  prv=@
    %-  serialize-point
    (priv-to-pub prv)
  ::
  ++  sign-transaction
    =,  crypto
    |=  [tx=transaction:rpc pk=@]
    ^-  @ux
    ::  hash the raw transaction data
    =/  hash=@
      =/  dat=@
        %-  encode-atoms:rlp
        ::  with v=chain-id, r=0, s=0
        tx(chain-id [chain-id.tx 0 0 ~])
      =+  wid=(met 3 dat)
      %-  keccak-256:keccak
      [wid (rev 3 wid dat)]
    ::  sign transaction hash with private key
    =+  (ecdsa-raw-sign:secp256k1:secp hash pk)
    ::  complete transaction is raw data, with r and s
    ::  taken from the signature, and v as per eip-155
    %-  encode-atoms:rlp
    tx(chain-id [:(add (mul chain-id.tx 2) 35 v) r s ~])
  --
::
::  rlp en/decoding
::NOTE  https://github.com/ethereum/wiki/wiki/RLP
::
++  rlp
  |%
  ::NOTE  rlp encoding doesn't really care about leading zeroes,
  ::      but because we need to disinguish between no-bytes zero
  ::      and one-byte zero (and also empty list) we end up with
  ::      this awful type...
  +$  item
    $%  [%l l=(list item)]
        [%b b=byts]
    ==
  ::  +encode-atoms: encode list of atoms as a %l of %b items
  ::
  ++  encode-atoms
    |=  l=(list @)
    ^-  @
    %+  encode  %l
    %+  turn  l
    |=(a=@ b+[(met 3 a) a])
  ::
  ++  encode
    |=  in=item
    |^  ^-  @
        ?-  -.in
            %b
          ?:  &(=(1 wid.b.in) (lte dat.b.in 0x7f))
            dat.b.in
          =-  (can 3 ~[b.in [(met 3 -) -]])
          (encode-length wid.b.in 0x80)
        ::
            %l
          =/  out=@
            %+  roll  l.in
            |=  [ni=item en=@]
            (cat 3 (encode ni) en)
          %^  cat  3  out
          (encode-length (met 3 out) 0xc0)
        ==
    ::
    ++  encode-length
      |=  [len=@ off=@]
      ?:  (lth len 56)  (add len off)
      =-  (cat 3 len -)
      :(add (met 3 len) off 55)
    --
  ::  +decode-atoms: decode expecting a %l of %b items, producing atoms within
  ::
  ++  decode-atoms
    |=  dat=@
    ^-  (list @)
    =/  i=item  (decode dat)
    ~|  [%unexpected-data i]
    ?>  ?=(%l -.i)
    %+  turn  l.i
    |=  i=item
    ~|  [%unexpected-list i]
    ?>  ?=(%b -.i)
    dat.b.i
  ::
  ++  decode
    |=  dat=@
    ^-  item
    =/  bytes=(list @)  (flop (rip 3 dat))
    =?  bytes  ?=(~ bytes)  ~[0]
    |^  item:decode-head
    ::
    ++  decode-head
      ^-  [done=@ud =item]
      ?~  bytes
        ~|  %rlp-unexpected-end
        !!
      =*  byt  i.bytes
      ::  byte in 0x00-0x79 range encodes itself
      ::
      ?:  (lte byt 0x79)
        :-  1
        [%b 1^byt]
      ::  byte in 0x80-0xb7 range encodes string length
      ::
      ?:  (lte byt 0xb7)
        =+  len=(sub byt 0x80)
        :-  +(len)
        :-  %b
        len^(get-value 1 len)
      ::  byte in 0xb8-0xbf range encodes string length length
      ::
      ?:  (lte byt 0xbf)
        =+  led=(sub byt 0xb7)
        =+  len=(get-value 1 led)
        :-  (add +(led) len)
        :-  %b
        len^(get-value +(led) len)
      ::  byte in 0xc0-f7 range encodes list length
      ::
      ?:  (lte byt 0xf7)
        =+  len=(sub byt 0xc0)
        :-  +(len)
        :-  %l
        %.  len
        decode-list(bytes (slag 1 `(list @)`bytes))
      ::  byte in 0xf8-ff range encodes list length length
      ::
      ?:  (lte byt 0xff)
        =+  led=(sub byt 0xf7)
        =+  len=(get-value 1 led)
        :-  (add +(led) len)
        :-  %l
        %.  len
        decode-list(bytes (slag +(led) `(list @)`bytes))
      ~|  [%rip-not-bloq-3 `@ux`byt]
      !!
    ::
    ++  decode-list
      |=  rem=@ud
      ^-  (list item)
      ?:  =(0 rem)  ~
      =+  ^-  [don=@ud =item]  ::TODO  =/
        decode-head
      :-  item
      %=  $
        rem    (sub rem don)
        bytes  (slag don bytes)
      ==
    ::
    ++  get-value
      |=  [at=@ud to=@ud]
      ^-  @
      (rep 3 (flop (swag [at to] bytes)))
    --
  --
::
::  abi en/decoding
::NOTE  https://solidity.readthedocs.io/en/develop/abi-spec.html
::
++  abi
  =>  |%
      ::  solidity types. integer bitsizes ignored
      ++  etyp
        $@  $?  ::  static
                %address  %bool
                %int      %uint
                %real     %ureal
                ::  dynamic
                %bytes    %string
            ==
        $%  ::  static
            [%bytes-n n=@ud]
            ::  dynamic
            [%array-n t=etyp n=@ud]
            [%array t=etyp]
        ==
      ::
      ::  solidity-style typed data. integer bitsizes ignored
      ++  data
        $%  [%address p=address]
            [%string p=tape]
            [%bool p=?]
            [%int p=@sd]
            [%uint p=@ud]
            [%real p=@rs]
            [%ureal p=@urs]
            [%array-n p=(list data)]
            [%array p=(list data)]
            [%bytes-n p=octs]  ::TODO  just @, because context knows length?
            [%bytes p=octs]
        ==
      --
  =,  mimes:html
  |%
  ::  encoding
  ::
  ++  encode-args
    ::  encode list of arguments.
    ::
    |=  das=(list data)
    ^-  tape
    (encode-data [%array-n das])
  ::
  ++  encode-data
    ::  encode typed data into ABI bytestring.
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
      ::  head(X[i]) = enc(len( head(X[0])..head(X[k-1])
      ::                        tail(X[0])..tail(X[i-1]) ))
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
      =/  ofs=@ud
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
      ::  enc(X) is the sequence of bytes in X padded with zero-bytes to a
      ::  length of 32.
      ::  Note that for any X, len(enc(X)) is a multiple of 32.
      ~|  [%bytes-n-too-long max=32 actual=p.p.dat]
      ?>  (lte p.p.dat 32)
      (pad-to-multiple (render-hex-bytes p.dat) 64 %right)
    ::
        %bytes  ::  of length k (which is assumed to be of type uint256)
      ::  enc(X) = enc(k) pad_right(X), i.e. the number of bytes is encoded as a
      ::  uint256 followed by the actual value of X as a byte sequence, followed
      ::  by the minimum number of zero-bytes such that len(enc(X)) is a
      ::  multiple of 32.
      %+  weld  $(dat [%uint p.p.dat])
      (pad-to-multiple (render-hex-bytes p.dat) 64 %right)
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
  ::  decoding
  ::
  ++  decode-topics  decode-arguments
  ::
  ++  decode-results
    ::  rex:  string of hex bytes with leading 0x.
    |*  [rex=@t tys=(list etyp)]
    =-  (decode-arguments - tys)
    %^  rut  9
      (rsh [3 2] rex)
    (curr rash hex)
  ::
  ++  decode-arguments
    |*  [wos=(list @) tys=(list etyp)]
    =/  wos=(list @)  wos  ::  get rid of tmi
    =|  win=@ud
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
          %address  `@ux`wor
          %uint     `@ud`wor
          %bool     =(1 wor)
        ==
      ::
          %string
        =+  $(tys ~[%bytes])
        [nin (trip (swp 3 q.dat))]
      ::
          %bytes
        :-  +(win)
        ::  find the word index of the actual data.
        =/  lic=@ud  (div wor 32)
        ::  learn the bytelength of the data.
        =/  len=@ud  (snag lic wos)
        (decode-bytes-n +(lic) len)
      ::
          [%bytes-n *]
        :-  (add win +((div (dec n.typ) 32)))
        (decode-bytes-n win n.typ)
      ::
          [%array *]
        :-  +(win)
        ::  find the word index of the actual data.
        =.  win  (div wor 32)
        ::  read the elements from their location.
        %-  tail
        %^  decode-array-n  ~[t.typ]  +(win)
        (snag win wos)
      ::
          [%array-n *]
        (decode-array-n ~[t.typ] win n.typ)
      ==
    ::
    ++  decode-bytes-n
      |=  [fro=@ud bys=@ud]
      ^-  octs
      ::  parse {bys} bytes from {fro}.
      :-  bys
      %+  rsh
        :-  3
        =+  (mod bys 32)
        ?:(=(0 -) - (sub 32 -))
      %+  rep  8
      %-  flop
      =-  (swag [fro -] wos)
      +((div (dec bys) 32))
    ::
    ++  decode-array-n
      ::NOTE  we take (list etyp) even though we only operate on
      ::      a single etyp as a workaround for urbit/arvo#673
      ::NOTE  careful! produces lists without type info
      =|  res=(list)
      |*  [tys=(list etyp) fro=@ud len=@ud]
      ^-  [@ud (list)]
      ?~  tys  !!
      ?:  =(len 0)  [fro (flop `(list)`res)]
      =+  (decode-one fro ~[i.tys])  ::  [nin=@ud dat=*]
      $(res ^+(res [dat res]), fro nin, len (dec len))
    --
  --
::
::  communicating with rpc nodes
::NOTE  https://github.com/ethereum/wiki/wiki/JSON-RPC
::
++  rpc
  ::  types
  ::
  =>  =,  abi
      =,  format
      |%
      ::  raw call data
      ++  call-data
        $:  function=@t
            arguments=(list data)
        ==
      ::
      ::  raw transaction data
      +$  transaction
        $:  nonce=@ud
            gas-price=@ud
            gas=@ud
            to=address
            value=@ud
            data=@ux
            chain-id=@ux
        ==
      ::
      ::  ethereum json rpc api
      ::
      ::  supported requests.
      ++  request
        $%  [%eth-block-number ~]
            [%eth-call cal=call deb=block]
            $:  %eth-new-filter
                fro=(unit block)
                tob=(unit block)
                adr=(list address)
                top=(list ?(@ux (list @ux)))
            ==
            [%eth-get-block-by-number bon=@ud txs=?]
            [%eth-get-filter-logs fid=@ud]
            $:  %eth-get-logs
                fro=(unit block)
                tob=(unit block)
                adr=(list address)
                top=(list ?(@ux (list @ux)))
            ==
            $:  %eth-get-logs-by-hash
                has=@
                adr=(list address)
                top=(list ?(@ux (list @ux)))
            ==
            [%eth-get-filter-changes fid=@ud]
            [%eth-get-transaction-count adr=address =block]
            [%eth-get-transaction-receipt txh=@ux]
            [%eth-send-raw-transaction dat=@ux]
        ==
      ::
      ::TODO  clean up & actually use
      ++  response
        $%  ::TODO
            [%eth-new-filter fid=@ud]
            [%eth-get-filter-logs los=(list event-log)]
            [%eth-get-logs los=(list event-log)]
            [%eth-get-logs-by-hash los=(list event-log)]
            [%eth-got-filter-changes los=(list event-log)]
            [%eth-transaction-hash haz=@ux]
        ==
      ::
      ++  event-log
        $:  ::  null for pending logs
            $=  mined  %-  unit
            $:  log-index=@ud
                transaction-index=@ud
                transaction-hash=@ux
                block-number=@ud
                block-hash=@ux
                removed=?
            ==
          ::
            address=@ux
            data=@t
            ::  event data
            ::
            ::    For standard events, the first topic is the event signature
            ::    hash. For anonymous events, the first topic is the first
            ::    indexed argument.
            ::    Note that this does not support the "anonymous event with
            ::    zero topics" case. This has dubious usability, and using
            ::    +lest instead of +list saves a lot of ?~ checks.
            ::
            topics=(lest @ux)
        ==
      ::
      ::  data for eth_call.
      ++  call
        $:  from=(unit address)
            to=address
            gas=(unit @ud)
            gas-price=(unit @ud)
            value=(unit @ud)
            data=tape
        ==
      ::
      ::  minimum data needed to construct a read call
      ++  proto-read-request
        $:  id=(unit @t)
            to=address
            call-data
        ==
      ::
      ::  block to operate on.
      ++  block
        $%  [%number n=@ud]
            [%label l=?(%earliest %latest %pending)]
        ==
      --
  ::
  ::  logic
  ::
  |%
  ++  encode-call
    |=  call-data
    ^-  tape
    ::TODO  should this check to see if the data matches the function signature?
    =-  :(weld "0x" - (encode-args arguments))
    %+  scag  8
    %+  render-hex-bytes  32
    %-  keccak-256:keccak:crypto
    (as-octs:mimes:html function)
  ::
  ::  building requests
  ::
  ++  json-request
    =,  eyre
    |=  [url=purl jon=json]
    ^-  hiss
    :^  url  %post
      %-  ~(gas in *math)
      ~['Content-Type'^['application/json']~]
    (some (as-octt (en-json:html jon)))
  ::  +light-json-request: like json-request, but for %l
  ::
  ::    TODO: Exorcising +purl from our system is a much longer term effort;
  ::    get the current output types for now.
  ::
  ++  light-json-request
    |=  [url=purl:eyre jon=json]
    ^-  request:http
    ::
    :*  %'POST'
        (crip (en-purl:html url))
        ~[['content-type' 'application/json']]
        (some (as-octt (en-json:html jon)))
    ==
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
      ::TODO  should use request-to-json:rpc:jstd,
      ::      and probably (fall riq -.req)
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
          :+  ~  'topics'
          (topics-to-json top.req)
      ==
    ::
        %eth-get-block-by-number
      :-  'eth_getBlockByNumber'
      :~  (tape (num-to-hex bon.req))
          b+txs.req
      ==
    ::
        %eth-get-filter-logs
      ['eth_getFilterLogs' (tape (num-to-hex fid.req)) ~]
    ::
        %eth-get-logs
      :-  'eth_getLogs'
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
          ?:  =(0 (lent adr.req))  ~
          :+  ~  'address'
          ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
          :-  %a
          (turn adr.req (cork address-to-hex tape))
        ::
          ?~  top.req  ~
          :+  ~  'topics'
          (topics-to-json top.req)
      ==
    ::
        %eth-get-logs-by-hash
      :-  'eth_getLogs'
      :_  ~  :-  %o
      %-  ~(gas by *(map @t json))
      =-  (murn - same)
      ^-  (list (unit (pair @t json)))
      :~  `['blockHash' (tape (transaction-to-hex has.req))]
        ::
          ?:  =(0 (lent adr.req))  ~
          :+  ~  'address'
          ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
          :-  %a
          (turn adr.req (cork address-to-hex tape))
        ::
          ?~  top.req  ~
          :+  ~  'topics'
          (topics-to-json top.req)
      ==
    ::
        %eth-get-filter-changes
      ['eth_getFilterChanges' (tape (num-to-hex fid.req)) ~]
    ::
        %eth-get-transaction-count
      :-  'eth_getTransactionCount'
      :~  (tape (address-to-hex adr.req))
          (block-to-json block.req)
      ==
    ::
        %eth-get-transaction-receipt
      ['eth_getTransactionReceipt' (tape (transaction-to-hex txh.req)) ~]
    ::
        %eth-send-raw-transaction
      ['eth_sendRawTransaction' (tape (num-to-hex dat.req)) ~]
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
      %number   s+(crip '0' 'x' ((x-co:co 1) n.dob))
      %label    s+l.dob
    ==
  ::
  ++  topics-to-json
    |=  tos=(list ?(@ux (list @ux)))
    ^-  json
    :-  %a
    =/  ttj
      ;:  cork
        (cury render-hex-bytes 32)
        prefix-hex
        tape:enjs:format
      ==
    %+  turn  tos
    |=  t=?(@ (list @))
    ?@  t
      ?:  =(0 t)  ~
      (ttj `@`t)
    a+(turn t ttj)
  ::
  ::  parsing responses
  ::
  ::TODO  ++  parse-response  |=  json  ^-  response
  ::
  ++  parse-hex-result
    |=  j=json
    ^-  @
    ?>  ?=(%s -.j)
    (hex-to-num p.j)
  ::
  ++  parse-eth-new-filter-res  parse-hex-result
  ::
  ++  parse-eth-block-number  parse-hex-result
  ::
  ++  parse-transaction-hash  parse-hex-result
  ::
  ++  parse-eth-get-transaction-count  parse-hex-result
  ::
  ++  parse-event-logs
    (ar:dejs:format parse-event-log)
  ::
  ++  parse-event-log
    =,  dejs:format
    |=  log=json
    ^-  event-log
    =-  ((ot -) log)
    :~  =-  ['logIndex'^(cu - (mu so))]
        |=  li=(unit @t)
        ?~  li  ~
        =-  `((ou -) log)  ::TODO  not sure if elegant or hacky.
        :~  'logIndex'^(un (cu hex-to-num so))
            'transactionIndex'^(un (cu hex-to-num so))
            'transactionHash'^(un (cu hex-to-num so))
            'blockNumber'^(un (cu hex-to-num so))
            'blockHash'^(un (cu hex-to-num so))
            'removed'^(uf | bo)
        ==
      ::
        address+(cu hex-to-num so)
        data+so
      ::
        =-  topics+(cu - (ar so))
        |=  r=(list @t)
        ^-  (lest @ux)
        ?>  ?=([@t *] r)
        :-  (hex-to-num i.r)
        (turn t.r hex-to-num)
    ==
  --
::
::  utilities
::TODO  give them better homes!
::
++  num-to-hex
  |=  n=@
  ^-  tape
  %-  prefix-hex
  ?:  =(0 n)
    "0"
  %-  render-hex-bytes
  (as-octs:mimes:html n)
::
++  address-to-hex
  |=  a=address
  ^-  tape
  %-  prefix-hex
  (render-hex-bytes 20 `@`a)
::
++  transaction-to-hex
  |=  h=@
  ^-  tape
  %-  prefix-hex
  (render-hex-bytes 32 h)
::
++  prefix-hex
  |=  a=tape
  ^-  tape
  ['0' 'x' a]
::
++  render-hex-bytes
  ::  atom to string of hex bytes without 0x prefix and dots.
  |=  a=octs
  ^-  tape
  ((x-co:co (mul 2 p.a)) q.a)
::
++  pad-to-multiple
  |=  [wat=tape mof=@ud wer=?(%left %right)]
  ^-  tape
  =+  len=(lent wat)
  ?:  =(0 len)  (reap mof '0')
  =+  mad=(mod len mof)
  ?:  =(0 mad)  wat
  =+  tad=(reap (sub mof mad) '0')
  %-  weld
  ?:(?=(%left wer) [tad wat] [wat tad])
::
++  hex-to-num
  |=  a=@t
  (rash (rsh [3 2] a) hex)
--
