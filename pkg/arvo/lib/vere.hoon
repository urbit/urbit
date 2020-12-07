::  runtime support code
::
=>  ..zuse  =>
|%
::
::  |dawn: pre-boot request/response de/serialization and validation
::
++  dawn
  =>  |%
      ::  +live: public network state of a ship
      ::
      +$  live  (unit [=life breach=?])
      --
  |%
  :: +come:dawn: mine a comet under a star
  ::
  ::    Randomly generates comet addresses until we find one whose parent is
  ::    in the list of supplied stars. Errors if any supplied ship
  ::    is not a star.
  ::
  ++  come
    |=  [tar=(list ship) eny=@uvJ]
    ::
    =|  stars=(set ship)
    =.  stars
      |-  ^+  stars
      ?~  tar  stars
      ::
      ~|  [%come-not-king i.tar]
      ?>  ?=(%king (clan:title i.tar))
      $(tar t.tar, stars (~(put in stars) i.tar))
    ::
    |-  ^-  seed:able:jael
    =/  cub=acru:ames  (pit:nu:crub:crypto 512 eny)
    =/  who=ship  `@`fig:ex:cub
    ::  disallow 64-bit or smaller addresses
    ::
    ?.  ?=(%pawn (clan:title who))
      $(eny +(eny))
    ?:  (~(has in stars) (^sein:title who))
      [who 1 sec:ex:cub ~]
    $(eny +(eny))
  ::  |give:dawn: produce requests for pre-boot validation
  ::
  ++  give
    =,  rpc:ethereum
    =,  abi:ethereum
    =/  tract  azimuth:contracts:azimuth
    |%
    ::  +bloq:give:dawn: Eth RPC for latest block number
    ::
    ++  bloq
      ^-  octs
      %-  as-octt:mimes:html
      %-  en-json:html
      %+  request-to-json
        `~.0
      [%eth-block-number ~]
    ::  +czar:give:dawn: Eth RPC for galaxy table
    ::
    ++  czar
      |=  boq=@ud
      ^-  octs
      %-  as-octt:mimes:html
      %-  en-json:html
      :-  %a
      %+  turn  (gulf 0 255)
      |=  gal=@
      %+  request-to-json
        `(cat 3 'gal-' (scot %ud gal))
      :+  %eth-call
        =-  [from=~ to=tract gas=~ price=~ value=~ data=-]
        (encode-call 'points(uint32)' [%uint gal]~)
      [%number boq]
    ::  +point:give:dawn: Eth RPC for ship's contract state
    ::
    ++  point
      |=  [boq=@ud who=ship]
      ^-  octs
      %-  as-octt:mimes:html
      %-  en-json:html
      %+  request-to-json
        `~.0
      :+  %eth-call
        =-  [from=~ to=tract gas=~ price=~ value=~ data=-]
        (encode-call 'points(uint32)' [%uint `@`who]~)
      [%number boq]
    ::  +turf:give:dawn: Eth RPC for network domains
    ::
    ++  turf
      |=  boq=@ud
      ^-  octs
      %-  as-octt:mimes:html
      %-  en-json:html
      :-  %a
      %+  turn  (gulf 0 2)
      |=  idx=@
      %+  request-to-json
        `(cat 3 'turf-' (scot %ud idx))
      :+  %eth-call
        =-  [from=~ to=tract gas=~ price=~ value=~ data=-]
        (encode-call 'dnsDomains(uint256)' [%uint idx]~)
      [%number boq]
    --
  ::  |take:dawn: parse responses for pre-boot validation
  ::
  ++  take
    =,  abi:ethereum
    =,  rpc:ethereum
    =,  azimuth
    =,  dejs-soft:format
    |%
    ::  +bloq:take:dawn: parse block number
    ::
    ++  bloq
      |=  rep=octs
      ^-  (unit @ud)
      =/  jon=(unit json)  (de-json:html q.rep)
      ?~  jon
        ~&([%bloq-take-dawn %invalid-json] ~)
      =/  res=(unit cord)  ((ot result+so ~) u.jon)
      ?~  res
        ~&([%bloq-take-dawn %invalid-response rep] ~)
      =/  out
        %-  mule  |.
        (hex-to-num:ethereum u.res)
      ?:  ?=(%& -.out)
        (some p.out)
      ~&([%bloq-take-dawn %invalid-block-number] ~)
    ::  +czar:take:dawn: parse galaxy table
    ::
    ++  czar
      |=  rep=octs
      ^-  (unit (map ship [=rift =life =pass]))
      =/  jon=(unit json)  (de-json:html q.rep)
      ?~  jon
        ~&([%czar-take-dawn %invalid-json] ~)
      =/  res=(unit (list [@t @t]))
        ((ar (ot id+so result+so ~)) u.jon)
      ?~  res
        ~&([%czar-take-dawn %invalid-response rep] ~)
      =/  dat=(unit (list [who=@p point:azimuth-types]))
        =-  ?:(?=(%| -.out) ~ (some p.out))
        ^=  out  %-  mule  |.
        %+  turn  u.res
        |=  [id=@t result=@t]
        ^-  [who=ship point:azimuth-types]
        =/  who  `@p`(slav %ud (rsh [3 4] id))
        :-  who
        %+  point-from-eth
          who
        :_  *deed:eth-noun
        %+  decode-results
          result
        point:eth-type
      ?~  dat
        ~&([%bloq-take-dawn %invalid-galaxy-table] ~)
      :-  ~
      %+  roll  u.dat
      |=  $:  [who=ship =point:azimuth-types]
              kyz=(map ship [=rift =life =pass])
          ==
      ^+  kyz
      ?~  net.point
        kyz
      (~(put by kyz) who [continuity-number life pass]:u.net.point)
    ::  +point:take:dawn: parse ship's contract state
    ::
    ++  point
      |=  [who=ship rep=octs]
      ^-  (unit point:azimuth)
      =/  jon=(unit json)  (de-json:html q.rep)
      ?~  jon
        ~&([%point-take-dawn %invalid-json] ~)
      =/  res=(unit cord)  ((ot result+so ~) u.jon)
      ?~  res
        ~&([%point-take-dawn %invalid-response rep] ~)
      ~?  =(u.res '0x')
        :-  'bad result from node; is azimuth address correct?'
        azimuth:contracts
      =/  out
        %-  mule  |.
        %+  point-from-eth
          who
        :_  *deed:eth-noun  ::TODO  call rights to fill
        (decode-results u.res point:eth-type)
      ?:  ?=(%& -.out)
        (some p.out)
      ~&([%point-take-dawn %invalid-point] ~)
    ::  +turf:take:dawn: parse network domains
    ::
    ++  turf
      |=  rep=octs
      ^-  (unit (list ^turf))
      =/  jon=(unit json)  (de-json:html q.rep)
      ?~  jon
        ~&([%turf-take-dawn %invalid-json] ~)
      =/  res=(unit (list [@t @t]))
        ((ar (ot id+so result+so ~)) u.jon)
      ?~  res
        ~&([%turf-take-dawn %invalid-response rep] ~)
      =/  dat=(unit (list (pair @ud ^turf)))
        =-  ?:(?=(%| -.out) ~ (some p.out))
        ^=  out  %-  mule  |.
        %+  turn  u.res
        |=  [id=@t result=@t]
        ^-  (pair @ud ^turf)
        :-  (slav %ud (rsh [3 5] id))
        =/  dom=tape
          (decode-results result [%string]~)
        =/  hot=host:eyre
          (scan dom thos:de-purl:html)
        ?>(?=(%& -.hot) p.hot)
      ?~  dat
        ~&([%turf-take-dawn %invalid-domains] ~)
      :-  ~
      =*  dom  u.dat
      :: sort by id, ascending, removing duplicates
      ::
      =|  tuf=(map ^turf @ud)
      |-  ^-  (list ^turf)
      ?~  dom
        %+  turn
          %+  sort  ~(tap by tuf)
          |=([a=(pair ^turf @ud) b=(pair ^turf @ud)] (lth q.a q.b))
        head
      =?  tuf  !(~(has by tuf) q.i.dom)
        (~(put by tuf) q.i.dom p.i.dom)
      $(dom t.dom)
    --
  ::  +veri:dawn: validate keys, life, discontinuity, &c
  ::
  ++  veri
    |=  [=seed:able:jael =point:azimuth =live]
    ^-  (unit error=term)
    =/  rac  (clan:title who.seed)
    =/  cub  (nol:nu:crub:crypto key.seed)
    ?-  rac
        %pawn
      ::  a comet address is the fingerprint of the keypair
      ::
      ?.  =(who.seed `@`fig:ex:cub)
        `%key-mismatch
      ::  a comet can never be breached
      ::
      ?^  live
        `%already-booted
      ::  a comet can never be re-keyed
      ::
      ?.  ?=(%1 lyf.seed)
        `%invalid-life
      ~
    ::
        %earl
      ~
    ::
        *
      ::  on-chain ships must be launched
      ::
      ?~  net.point
        `%not-keyed
      =*  net  u.net.point
      ::  boot keys must match the contract
      ::
      ?.  =(pub:ex:cub pass.net)
        ~&  [%key-mismatch pub:ex:cub pass.net]
        `%key-mismatch
      ::  life must match the contract
      ::
      ?.  =(lyf.seed life.net)
        `%life-mismatch
      ::  the boot life must be greater than and discontinuous with
      ::  the last seen life (per the sponsor)
      ::
      ?:  ?&  ?=(^ live)
              ?|  ?=(%| breach.u.live)
                  (lte life.net life.u.live)
          ==  ==
        `%already-booted
      ::  produce the sponsor for vere
      ::
      ~?  !has.sponsor.net
        [%no-sponsorship-guarantees-from who.sponsor.net]
      ~
    ==
  ::  +sponsor:dawn: retreive sponsor from point
  ::
  ++  sponsor
    |=  [who=ship =point:azimuth]
    ^-  (each ship error=term)
    ?-    (clan:title who)
        %pawn  [%& (^sein:title who)]
        %earl  [%& (^sein:title who)]
        %czar  [%& (^sein:title who)]
        *
      ?~  net.point
        [%| %not-booted]
      ?.  has.sponsor.u.net.point
        [%| %no-sponsor]
      [%& who.sponsor.u.net.point]
    ==
  --
::
::TODO  include from lib/ethereum and lib/azimuth instead
::
++  ethereum
  =,  ethereum-types
  |%
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
        ::  ethereum json rpc api
        ::
        ::  supported requests.
        ++  request
          $%  [%eth-block-number ~]
              [%eth-call cal=call deb=block]
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
    --
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
::
++  azimuth
  =,  ethereum-types
  =,  azimuth-types
  =>  |%
      +$  complete-ship
        $:  state=point
            history=(list diff-point)  ::TODO  maybe block/event nr?  ::  newest first
            keys=(map life pass)
        ==
      ::
      ++  fleet  (map @p complete-ship)
      ::
      ++  eth-type
        |%
        ++  point
          :~  [%bytes-n 32]   ::  encryptionKey
              [%bytes-n 32]   ::  authenticationKey
              %bool           ::  hasSponsor
              %bool           ::  active
              %bool           ::  escapeRequested
              %uint           ::  sponsor
              %uint           ::  escapeRequestedTo
              %uint           ::  cryptoSuiteVersion
              %uint           ::  keyRevisionNumber
              %uint           ::  continuityNumber
          ==
        ++  deed
          :~  %address        ::  owner
              %address        ::  managementProxy
              %address        ::  spawnProxy
              %address        ::  votingProxy
              %address        ::  transferProxy
          ==
        --
      ::
      ++  eth-noun
        |%
        +$  point
          $:  encryption-key=octs
              authentication-key=octs
              has-sponsor=?
              active=?
              escape-requested=?
              sponsor=@ud
              escape-to=@ud
              crypto-suite=@ud
              key-revision=@ud
              continuity-number=@ud
          ==
        +$  deed
          $:  owner=address
              management-proxy=address
              spawn-proxy=address
              voting-proxy=address
              transfer-proxy=address
          ==
        --
      ::
      ::  #  constants
      ::
      ::  contract addresses
      ++  contracts  mainnet-contracts
      ++  mainnet-contracts
        |%
        ::  azimuth: data contract
        ::
        ++  azimuth
          0x223c.067f.8cf2.8ae1.73ee.5caf.ea60.ca44.c335.fecb
        ::
        ++  ecliptic
          0x6ac0.7b7c.4601.b5ce.11de.8dfe.6335.b871.c7c4.dd4d
        ::
        ++  linear-star-release
          0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
        ::
        ++  conditional-star-release
          0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
        ::
        ++  delegated-sending
          0xf790.8ab1.f1e3.52f8.3c5e.bc75.051c.0565.aeae.a5fb
        ::
        ::  launch: block number of azimuth deploy
        ::
        ++  launch  6.784.800
        ::
        ::  public: block number of azimuth becoming independent
        ::
        ++  public  7.033.765
        --
      ::
      ::  Testnet contract addresses
      ::
      ++  ropsten-contracts
        |%
        ++  azimuth
          0x308a.b6a6.024c.f198.b57e.008d.0ac9.ad02.1988.6579
        ::
        ++  ecliptic
          0x8b9f.86a2.8921.d9c7.05b3.113a.755f.b979.e1bd.1bce
        ::
        ++  linear-star-release
          0x1f8e.dd03.1ee4.1474.0aed.b39b.84fb.8f2f.66ca.422f
        ::
        ++  conditional-star-release
          0x0
        ::
        ++  delegated-sending
          0x3e8c.a510.354b.c2fd.bbd6.1502.52d9.3105.c9c2.7bbe
        ::
        ++  launch  4.601.630
        ++  public  launch
        --
      --
  ::
  ::  logic
  ::
  |%
  ++  pass-from-eth
    |=  [enc=octs aut=octs sut=@ud]
    ^-  pass
    %^  cat  3  'b'
    ?.  &(=(1 sut) =(p.enc 32) =(p.aut 32))
      (cat 8 0 0)
    (cat 8 q.aut q.enc)
  ::
  ++  point-from-eth
    |=  [who=@p point:eth-noun deed:eth-noun]
    ^-  point
    ::
    ::  ownership
    ::
    :+  :*  owner
            management-proxy
            voting-proxy
            transfer-proxy
        ==
      ::
      ::  network state
      ::
      ?.  active  ~
      :-  ~
      :*  key-revision
        ::
          (pass-from-eth encryption-key authentication-key crypto-suite)
        ::
          continuity-number
        ::
          [has-sponsor `@p`sponsor]
        ::
          ?.  escape-requested  ~
          ``@p`escape-to
      ==
    ::
    ::  spawn state
    ::
    ?.  ?=(?(%czar %king) (clan:title who))  ~
    :-  ~
    :*  spawn-proxy
        ~  ::TODO  call getSpawned to fill this
    ==
  --
--
::
=/  pit  !>(.)
=>  |%
    ++  load  _[~ ..load]                                   ::   +4
    ++  peek  _~                                            ::  +22
    ++  poke  _[~ ..poke]                                   ::  +23
    ++  wish                                                ::  +10
      |=  txt=*
      q:(slap pit (ream ;;(@t txt)))
    --
::
|=(* .(+> +:(poke +<)))
