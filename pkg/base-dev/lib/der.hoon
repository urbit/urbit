/-  asn1
::  |der: distinguished encoding rules for ASN.1
::
::    DER is a tag-length-value binary encoding for ASN.1, designed
::    so that there is only one (distinguished) valid encoding for an
::    instance of a type.
::
|%
::  +en:der: encode +spec:asn1 to +octs (kindof)
::
++  en
  =<  |=  a=spec:asn1
      ^-  [len=@ud dat=@ux]
      =/  b  ~(ren raw a)
      [(lent b) (rep 3 b)]
  |%
  ::  +raw:en:der: door for encoding +spec:asn1 to list of bytes
  ::
  ++  raw
    |_  pec=spec:asn1
    ::  +ren:raw:en:der: render +spec:asn1 to tag-length-value bytes
    ::
    ++  ren
      ^-  (list @D)
      =/  a  lem
      [tag (weld (len a) a)]
    ::  +tag:raw:en:der: tag byte
    ::
    ++  tag
      ^-  @D
      ?-  pec
        [%int *]   2
        [%bit *]   3
        [%oct *]   4
        [%nul *]   5
        [%obj *]   6
        [%seq *]  48              :: constructed: (con 0x20 16)
        [%set *]  49              :: constructed: (con 0x20 17)
        [%con *]  ;:  con
                    0x80                    :: context-specifc
                    ?:(imp.bes.pec 0 0x20)  :: implicit?
                    (dis 0x1f tag.bes.pec)  :: 5 bits of custom tag
                  ==
      ==
    ::  +lem:raw:en:der: element bytes
    ::
    ++  lem
      ^-  (list @D)
      ?-  pec
        ::  unsigned only, interpreted as positive-signed and
        ::  rendered in big-endian byte order. negative-signed would
        ::  be two's complement
        ::
        [%int *]  =/  a  (flop (rip 3 int.pec))
                  ?~  a  [0 ~]
                  ?:((lte i.a 127) a [0 a])
        ::  padded to byte-width, must be already byte-aligned
        ::
        [%bit *]  =/  a  (rip 3 bit.pec)
                  =/  b  ~|  %der-invalid-bit
                      ?.  =(0 (mod len.pec 8))
                        ~|(%der-invalid-bit-alignment !!)
                      (sub (div len.pec 8) (lent a))
                  [0 (weld a (reap b 0))]
        ::  padded to byte-width
        ::
        [%oct *]  =/  a  (rip 3 oct.pec)
                  =/  b  ~|  %der-invalid-oct
                      (sub len.pec (lent a))
                  (weld a (reap b 0))
        ::
        [%nul *]  ~
        [%obj *]  (rip 3 obj.pec)
        ::
        [%seq *]  %-  zing
                  |-  ^-  (list (list @))
                  ?~  seq.pec  ~
                  :-  ren(pec i.seq.pec)
                  $(seq.pec t.seq.pec)
        ::  presumed to be already deduplicated and sorted
        ::
        [%set *]  %-  zing
                  |-  ^-  (list (list @))
                  ?~  set.pec  ~
                  :-  ren(pec i.set.pec)
                  $(set.pec t.set.pec)
        ::  already constructed
        ::
        [%con *]  con.pec
      ==
    ::  +len:raw:en:der: length bytes
    ::
    ++  len
      |=  a=(list @D)
      ^-  (list @D)
      =/  b  (lent a)
      ?:  (lte b 127)
        [b ~]                :: note: big-endian
      [(con 0x80 (met 3 b)) (flop (rip 3 b))]
    --
  --
::  +de:der: decode atom to +spec:asn1
::
++  de
  |=  [len=@ud dat=@ux]
  ^-  (unit spec:asn1)
  :: XX refactor into +parse
  =/  a  (rip 3 dat)
  =/  b  ~|  %der-invalid-len
      (sub len (lent a))
  (rust `(list @D)`(weld a (reap b 0)) parse)
::  +parse:der: DER parser combinator
::
++  parse
  =<  ^-  $-(nail (like spec:asn1))
      ;~  pose
        (stag %int (bass 256 (sear int ;~(pfix (tag 2) till))))
        (stag %bit (sear bit (boss 256 ;~(pfix (tag 3) till))))
        (stag %oct (boss 256 ;~(pfix (tag 4) till)))
        (stag %nul (cold ~ ;~(plug (tag 5) (tag 0))))
        (stag %obj (^boss 256 ;~(pfix (tag 6) till)))
        (stag %seq (sear recur ;~(pfix (tag 48) till)))
        (stag %set (sear recur ;~(pfix (tag 49) till)))
        (stag %con ;~(plug (sear context next) till))
      ==
  |%
  ::  +tag:parse:der: parse tag byte
  ::
  ++  tag
    |=(a=@D (just a))
  ::  +int:parse:der: sear unsigned big-endian bytes
  ::
  ++  int
    |=  a=(list @D)
    ^-  (unit (list @D))
    ?~  a  ~
    ?:  ?=([@ ~] a)  `a
    ?.  =(0 i.a)  `a
    ?.((gth i.t.a 127) ~ `t.a)
  ::  +bit:parse:der: convert bytewidth to bitwidth
  ::
  ++  bit
    |=  [len=@ud dat=@ux]
    ^-  (unit [len=@ud dat=@ux])
    ?.  =(0 (end 3 dat))  ~
    :+  ~
      (mul 8 (dec len))
    (rsh 3 dat)
  ::  +recur:parse:der: parse bytes for a list of +spec:asn1
  ::
  ++  recur
    |=(a=(list @) (rust a (star parse)))
  ::  +context:parse:der: decode context-specific tag byte
  ::
  ++  context
    |=  a=@D
    ^-  (unit bespoke:asn1)
    ?.  =(1 (cut 0 [7 1] a))  ~
    :+  ~
      =(1 (cut 0 [5 1] a))
    (dis 0x1f a)
  ::  +boss:parse:der: shadowed to count as well
  ::
  ::    Use for parsing +octs more broadly?
  ::
  ++  boss
    |*  [wuc=@ tyd=rule]
    %+  cook
      |=  waq=(list @)
      :-  (lent waq)
      (reel waq |=([p=@ q=@] (add p (mul wuc q))))
    tyd
  ::  +till:parse:der: parser combinator for len-prefixed bytes
  ::
  ::  advance until
  ::
  ++  till
    |=  tub=nail
    ^-  (like (list @D))
    ?~  q.tub
      (fail tub)
    ::  fuz: first byte - length, or length of the length
    ::
    =*  fuz  i.q.tub
    ::  nex: offset of value bytes from fuz
    ::  len: length of value bytes
    ::
    =/  [nex=@ len=@]
      ::  faz: meaningful bits in fuz
      ::
      =/  faz  (end [0 7] fuz)
      ?:  =(0 (cut 0 [7 1] fuz))
        [0 faz]
      [faz (rep 3 (flop (scag faz t.q.tub)))]
    ?:  ?&  !=(0 nex)
            !=(nex (met 3 len))
        ==
      (fail tub)
    ::  zuf: value bytes
    ::
    =/  zuf  (swag [nex len] t.q.tub)
    ?.  =(len (lent zuf))
      (fail tub)
    ::  zaf:  product nail
    ::
    =/  zaf  [p.p.tub (add +(nex) q.p.tub)]
    [zaf `[zuf zaf (slag (add nex len) t.q.tub)]]
  --
--

