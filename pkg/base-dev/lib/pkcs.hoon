/-  asn1
/+  primitive-rsa, der
=*  rsa  primitive-rsa
::::  %/lib/pkcs
|%
::  +rs256: RSA signatures over a sha-256 digest
::
++  rs256
  |_  k=key:rsa
  ::  +emsa:rs256: message digest
  ::
  ::    Padded, DER encoded sha-256 hash (EMSA-PKCS1-v1_5).
  ::
  ++  emsa
    |=  m=byts
    =/  emlen  (met 3 n.pub.k)
    =/  pec=spec:asn1
      :~  %seq
          [%seq [%obj sha-256:obj:asn1] [%nul ~] ~]
          [%oct 32 (shay wid.m dat.m)]
      ==
    ::  note: this asn.1 digest is rendered raw here, as we require
    ::  big-endian bytes, and the product of +en:der is little-endian
    ::
    =/  t=(list @D)  ~(ren raw:en:der pec)
    =/  tlen=@ud  (lent t)
    ?:  (lth emlen (add 11 tlen))
      ~|(%emsa-too-short !!)
    =/  ps=(list @D)
      (reap (sub emlen (add 3 tlen)) 0xff)
    (rep 3 (flop (weld [0x0 0x1 ps] [0x0 t])))
  ::  +sign:rs256: sign message
  ::
  ::    An RSA signature is the primitive decryption of the message hash.
  ::
  ++  sign
    |=(m=byts (de:rsa (emsa m) k))
  ::  +verify:rs256: verify signature
  ::
  ::    RSA signature verification confirms that the primitive encryption
  ::    of the signature matches the message hash.
  ::
  ++  verify
    |=  [s=@ m=byts]
    =((emsa m) (en:rsa s k))
  --
::  |pem: generic PEM implementation (rfc7468)
::
::    PEM is the base64 encoding of DER encoded data, with BEGIN and
::    END labels indicating some type.
::
++  pem
  |%
  ::  +en:pem: PEM encode
  ::
  ++  en
    |=  [lab=@t len=@ud der=@ux]
    ^-  wain
    :: XX validate label?
    :-  (rap 3 ['-----BEGIN ' lab '-----' ~])
    =/  a  (en:base64:mimes:html len `@`der)
    |-  ^-  wain
    ?~  a
      [(rap 3 ['-----END ' lab '-----' ~]) ~]
    [(end [3 64] a) $(a (rsh [3 64] a))]
  ::  +de:pem: PEM decode
  ::
  ++  de
    |=  [lab=@t mep=wain]
    ^-  (unit [len=@ud der=@ux])
    =/  a  (sub (lent mep) 2)
    ?~  mep  ~
    :: XX validate label?
    ?.  =((rap 3 ['-----BEGIN ' lab '-----' ~]) i.mep)  ~
    ?.  =((rap 3 ['-----END ' lab '-----' ~]) (snag a t.mep))  ~
    ^-  (unit [@ @])
    (de:base64:mimes:html (rap 3 (scag a t.mep)))
  --
::  |pkcs1: RSA asymmetric cryptography (rfc3447)
::
++  pkcs1
  |%
  ::  |spec:pkcs1: ASN.1 specs for RSA keys
  ::
  ++  spec
    |%
    ::  |en:spec:pkcs1: ASN.1 encoding for RSA keys
    ::
    ++  en
      |%
      ::  +pass:en:spec:pkcs1: encode public key to ASN.1
      ::
      ++  pass
        |=  k=key:rsa
        ^-  spec:asn1
        [%seq [%int n.pub.k] [%int e.pub.k] ~]
      ::  +ring:en:spec:pkcs1: encode private key to ASN.1
      ::
      ++  ring
        |=  k=key:rsa
        ^-  spec:asn1
        ~|  %rsa-need-ring
        ?>  ?=(^ sek.k)
        :~  %seq
            [%int 0]
            [%int n.pub.k]
            [%int e.pub.k]
            [%int d.u.sek.k]
            [%int p.u.sek.k]
            [%int q.u.sek.k]
            [%int (mod d.u.sek.k (dec p.u.sek.k))]
            [%int (mod d.u.sek.k (dec q.u.sek.k))]
            [%int (~(inv fo p.u.sek.k) q.u.sek.k)]
        ==
      --
    ::  |de:spec:pkcs1: ASN.1 decoding for RSA keys
    ::
    ++  de
      |%
      ::  +pass:de:spec:pkcs1: decode ASN.1 public key
      ::
      ++  pass
        |=  a=spec:asn1
        ^-  (unit key:rsa)
        ?.  ?=([%seq [%int *] [%int *] ~] a)
          ~
        =*  n  int.i.seq.a
        =*  e  int.i.t.seq.a
        `[[n e] ~]
      ::  +ring:de:spec:pkcs1: decode ASN.1 private key
      ::
      ++  ring
        |=  a=spec:asn1
        ^-  (unit key:rsa)
        ?.  ?=([%seq *] a)  ~
        ?.  ?=  $:  [%int %0]
                    [%int *]
                    [%int *]
                    [%int *]
                    [%int *]
                    [%int *]
                    *
                ==
            seq.a
          ~
        =*  n  int.i.t.seq.a
        =*  e  int.i.t.t.seq.a
        =*  d  int.i.t.t.t.seq.a
        =*  p  int.i.t.t.t.t.seq.a
        =*  q  int.i.t.t.t.t.t.seq.a
        `[[n e] `[d p q]]
      --
    --
  ::  |der:pkcs1: DER encoding for RSA keys
  ::
  ::    En(coding) and de(coding) for public (pass) and private (ring) keys.
  ::
  ++  der
    |%
    ++  en
      |%
      ++  pass  |=(k=key:rsa (en:^der (pass:en:spec k)))
      ++  ring  |=(k=key:rsa (en:^der (ring:en:spec k)))
      --
    ++  de
      |%
      ++  pass  |=([len=@ud dat=@ux] `(unit key:rsa)`(biff (de:^der len dat) pass:de:spec))
      ++  ring  |=([len=@ud dat=@ux] `(unit key:rsa)`(biff (de:^der len dat) ring:de:spec))
      --
    --
  ::  |pem:pkcs1: PEM encoding for RSA keys
  ::
  ::    En(coding) and de(coding) for public (pass) and private (ring) keys.
  ::
  ++  pem
    |%
    ++  en
      |%
      ++  pass  |=(k=key:rsa (en:^pem 'RSA PUBLIC KEY' (pass:en:der k)))
      ++  ring  |=(k=key:rsa (en:^pem 'RSA PRIVATE KEY' (ring:en:der k)))
      --
    ++  de
      |%
      ++  pass  |=(mep=wain (biff (de:^pem 'RSA PUBLIC KEY' mep) pass:de:der))
      ++  ring  |=(mep=wain (biff (de:^pem 'RSA PRIVATE KEY' mep) ring:de:der))
      --
    --
  --
::  |pkcs8: asymmetric cryptography (rfc5208, rfc5958)
::
::    RSA-only for now.
::
++  pkcs8
  |%
  ::  |spec:pkcs8: ASN.1 specs for asymmetric keys
  ::
  ++  spec
    |%
    ++  en
      |%
      ::  +pass:spec:pkcs8: public key ASN.1
      ::
      ::    Technically not part of pkcs8, but standardized later in
      ::    the superseding RFC. Included here for symmetry.
      ::
      ++  pass
        |=  k=key:rsa
        ^-  spec:asn1
        :~  %seq
            [%seq [[%obj rsa:obj:asn1] [%nul ~] ~]]
            =/  a=[len=@ud dat=@ux]
              (pass:en:der:pkcs1 k)
            [%bit (mul 8 len.a) dat.a]
        ==
      ::  +ring:spec:pkcs8: private key ASN.1
      ::
      ++  ring
        |=  k=key:rsa
        ^-  spec:asn1
        :~  %seq
            [%int 0]
            [%seq [[%obj rsa:obj:asn1] [%nul ~] ~]]
            [%oct (ring:en:der:pkcs1 k)]
        ==
      --
    ::  |de:spec:pkcs8: ASN.1 decoding for asymmetric keys
    ::
    ++  de
      |%
      ::  +pass:de:spec:pkcs8: decode public key ASN.1
      ::
      ++  pass
        |=  a=spec:asn1
        ^-  (unit key:rsa)
        ?.  ?=([%seq [%seq *] [%bit *] ~] a)
          ~
        ?.  ?&  ?=([[%obj *] [%nul ~] ~] seq.i.seq.a)
                =(rsa:obj:asn1 obj.i.seq.i.seq.a)
            ==
          ~
        (pass:de:der:pkcs1 (div len.i.t.seq.a 8) bit.i.t.seq.a)
      ::  +ring:de:spec:pkcs8: decode private key ASN.1
      ::
      ++  ring
        |=  a=spec:asn1
        ^-  (unit key:rsa)
        ?.  ?=([%seq [%int %0] [%seq *] [%oct *] ~] a)
          ~
        ?.  ?&  ?=([[%obj *] [%nul ~] ~] seq.i.t.seq.a)
                =(rsa:obj:asn1 obj.i.seq.i.t.seq.a)
            ==
          ~
        (ring:de:der:pkcs1 [len oct]:i.t.t.seq.a)
      --
    --
  ::  |der:pkcs8: DER encoding for asymmetric keys
  ::
  ::    En(coding) and de(coding) for public (pass) and private (ring) keys.
  ::    RSA-only for now.
  ::
  ++  der
    |%
    ++  en
      |%
      ++  pass  |=(k=key:rsa `[len=@ud dat=@ux]`(en:^der (pass:en:spec k)))
      ++  ring  |=(k=key:rsa `[len=@ud dat=@ux]`(en:^der (ring:en:spec k)))
      --
    ++  de
      |%
      ++  pass  |=([len=@ud dat=@ux] `(unit key:rsa)`(biff (de:^der len dat) pass:de:spec))
      ++  ring  |=([len=@ud dat=@ux] `(unit key:rsa)`(biff (de:^der len dat) ring:de:spec))
      --
    --
  ::  |pem:pkcs8: PEM encoding for asymmetric keys
  ::
  ::    En(coding) and de(coding) for public (pass) and private (ring) keys.
  ::    RSA-only for now.
  ::
  ++  pem
    |%
    ++  en
      |%
      ++  pass  |=(k=key:rsa (en:^pem 'PUBLIC KEY' (pass:en:der k)))
      ++  ring  |=(k=key:rsa (en:^pem 'PRIVATE KEY' (ring:en:der k)))
      --
    ++  de
      |%
      ++  pass  |=(mep=wain (biff (de:^pem 'PUBLIC KEY' mep) pass:de:der))
      ++  ring  |=(mep=wain (biff (de:^pem 'PRIVATE KEY' mep) ring:de:der))
      --
    --
  --
::  |pkcs10: certificate signing requests (rfc2986)
::
::    Only implemented for RSA keys with subject-alternate names.
::
++  pkcs10
  =>  |%
      ::  +csr:pkcs10: certificate request
      ::
      +$  csr  [key=key:rsa hot=(list turf)]
      --
  |%
  ::  |spec:pkcs10: ASN.1 specs for certificate signing requests
  ::
  ++  spec
    |%
    ::  +en:spec:pkcs10: ASN.1 encoding for certificate signing requests
    ::
    ++  en
      |=  csr
      ^-  spec:asn1
      |^  =/  dat=spec:asn1  (info key hot)
          :~  %seq
              dat
              [%seq [[%obj rsa-sha-256:obj:asn1] [%nul ~] ~]]
              :: big-endian signature bits
              ::
              ::   the signature bitwidth is definitionally the key length
              ::
              :+  %bit
                (met 0 n.pub.key)
              (swp 3 (~(sign rs256 key) (en:^der dat)))
          ==
      ::  +info:en:spec:pkcs10: certificate request info
      ::
      ++  info
        |=  csr
        ^-  spec:asn1
        :~  %seq
            [%int 0]
            [%seq ~]
            (pass:en:spec:pkcs8 key)
            :: explicit, context-specific tag #0 (extensions)
            ::
            :+  %con
              `bespoke:asn1`[| 0]
            %~  ren
              raw:en:^der
            :~  %seq
                [%obj csr-ext:obj:asn1]
                :~  %set
                    :~  %seq
                        :~  %seq
                            [%obj sub-alt:obj:asn1]
                            [%oct (en:^der (san hot))]
        ==  ==  ==  ==  ==
      ::  +san:en:spec:pkcs10: subject-alternate-names
      ::
      ++  san
        |=  hot=(list turf)
        ^-  spec:asn1
        :-  %seq
        %+  turn  hot
        :: implicit, context-specific tag #2 (IA5String)
        :: XX sanitize string?
        |=(=turf [%con `bespoke:asn1`[& 2] (trip (en-turf:html turf))])
      --
    ::  |de:spec:pkcs10: ASN.1 decoding for certificate signing requests
    ++  de  !!
    --
  ::  |der:pkcs10: DER encoding for certificate signing requests
  ::
  ++  der
    |%
    ++  en  |=(a=csr `[len=@ud der=@ux]`(en:^der (en:spec a)))
    ++  de  !! ::|=(a=@ `(unit csr)`(biff (de:^der a) de:spec))
    --
  ::  |pem:pkcs10: PEM encoding for certificate signing requests
  ::
  ++  pem
    |%
    ++  en  |=(a=csr (en:^pem 'CERTIFICATE REQUEST' (en:der a)))
    ++  de  !! ::|=(mep=wain (biff (de:^pem 'CERTIFICATE REQUEST' mep) de:der))
    --
  --
--

