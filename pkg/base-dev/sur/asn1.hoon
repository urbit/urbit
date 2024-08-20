::  |asn1: small selection of types and constants for ASN.1
::
::    A minimal representation of some basic ASN.1 types,
::    created to support PKCS keys, digests, and cert requests.
::
^?
|%
::  +bespoke:asn1: context-specific, generic ASN.1 tag type
::
::    Note that *explicit* implies *constructed* (ie, bit 5 is set in DER).
::
+$  bespoke
  ::  imp: & is implicit, | is explicit
  ::  tag: 5 bits for the custom tag number
  ::
  [imp=? tag=@ud]
::  +spec:asn1: minimal representations of basic ASN.1 types
::
+$  spec
  $%  ::  %int: arbitrary-sized, unsigned integers
      ::
      ::    Unsigned integers, represented as having a positive sign.
      ::    Negative integers would be two's complement in DER,
      ::    but we don't need them.
      ::
      [%int int=@u]
      ::  %bit: very minimal support for bit strings
      ::
      ::    Specifically, values must already be padded and byte-aligned.
      ::    len: bitwidth
      ::    bit: data
      ::
      [%bit len=@ud bit=@ux]
      ::  %oct: octets in little-endian byte order
      ::
      ::    len: bytewidth
      ::    bit: data
      ::
      [%oct len=@ud oct=@ux]
      ::  %nul: fully supported!
      ::
      [%nul ~]
      ::  %obj: object identifiers, pre-packed
      ::
      ::    Object identifiers are technically a sequence of integers,
      ::    represented here in their already-encoded form.
      ::
      [%obj obj=@ux]
      ::  %seq: a list of specs
      ::
      [%seq seq=(list spec)]
      ::  %set: a logical set of specs
      ::
      ::    Implemented here as a list for the sake of simplicity.
      ::    must be already deduplicated and sorted!
      ::
      [%set set=(list spec)]
      ::  %con: context-specific
      ::
      ::    General support for context-specific tags.
      ::    bes: custom tag number, implicit or explicit
      ::    con: already-encoded bytes
      ::
      [%con bes=bespoke con=(list @D)]
  ==
::  |obj:asn1: constant object ids, pre-encoded
::
++  obj
  ^?
  |%                                                ::    rfc4055
  ++  sha-256      0x1.0204.0365.0148.8660          ::  2.16.840.1.101.3.4.2.1
  ++  rsa          0x1.0101.0df7.8648.862a          ::  1.2.840.113549.1.1.1
  ++  rsa-sha-256  0xb.0101.0df7.8648.862a          ::  1.2.840.113549.1.1.11
                                                    ::    rfc2985
  ++  csr-ext      0xe.0901.0df7.8648.862a          ::  1.2.840.113549.1.9.14
                                                    ::    rfc3280
  ++  sub-alt      0x11.1d55                        ::  2.5.29.17
  --
--

