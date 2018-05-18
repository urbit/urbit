/+  tester
=,  eyre
::
::::  libraries
::
|%                                                      ::  +base64
++  base64                                              ::  [pad? url-safe?]
  =+  [pad=& url=|]                                     ::
  |%                                                    ::  +en:base64
  ++  en                                                ::  encode base64
    |=  tig=@
    ^-  cord
    =/  poc  (~(dif fo 3) 0 (met 3 tig))
    =/  pad  (lsh 3 poc (swp 3 tig))
    =/  cha
      ?:  url
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    %-  crip
    %-  flop
    %+  weld
      ?.(^pad ~ (reap poc '='))
    %+  slag  poc
    |-  ^-  tape
    ?~  pad  ~
    =/  d  (end 3 3 pad)
    :*  (cut 3 [(cut 0 [0 6] d) 1] cha)
        (cut 3 [(cut 0 [6 6] d) 1] cha)
        (cut 3 [(cut 0 [12 6] d) 1] cha)
        (cut 3 [(cut 0 [18 6] d) 1] cha)
        $(pad (rsh 3 3 pad))
    ==
  ::                                                    ::  +de:base64
  ++  de                                                ::  decode base64
    |=  a=cord
    ^-  (unit @)
    %+  rush  a
    %+  cook  (cury swp 3)
    %+  bass  64
    %+  cook  welp
    ;~  plug
      %-  plus
      ;~  pose
        (cook |=(a=@ (sub a 'A')) (shim 'A' 'Z'))
        (cook |=(a=@ (sub a 'G')) (shim 'a' 'z'))
        (cook |=(a=@ (add a 4)) (shim '0' '9'))
        (cold 62 (just ?:(url '-' '+')))
        (cold 63 (just ?:(url '_' '/')))
      ==
      ?:  pad
        (stun 0^2 (cold %0 tis))
      =/  b  (~(dif fo 4) 0 (met 3 a))
      (cold (reap b %0) (easy ~))
    ==
  --
::
++  en-base64url                                        ::  padding omitted
  ~(en base64 | &)                                      ::  per rfc7515
::
++  de-base64url
  ~(de base64 | &)
::
++  asn1
  |%                                                    ::  +spec:asn1
  +=  spec                                              ::
    $%  [%int p=@u]                                     ::  unsigned
        [%bit p=@ux]                                    ::  pad yerself
        [%oct p=@ux]                                    ::
        [%nul ~]                                        ::
        [%obj p=@ux]                                    ::  pack yerself
        [%seq p=(list spec)]                            ::
        [%set p=(list spec)]                            ::  sort yerself
        $:  %con                                        ::  construct yerself
            p=[p=? q=@udC]                              ::    [primitive? tag]
            q=(list @)                                  ::    bytes
    ==  ==
  ::                                                    ::  +obj:asn1
  ++  obj                                               ::  oid constants
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
::                                                      ::  +der
++  der                                                 ::  DER ASN.1
  |%                                                    ::  +en:der
  ++  en                                                ::  encode to atom
    =<  |=(a=spec:asn1 `@ux`(rep 3 ~(ren raw a)))
    |%                                                  ::  +raw:en:der
    ++  raw                                             ::  encode to bytes
      |_  pec=spec:asn1
      ::                                                ::  +ren:raw:en:der
      ++  ren                                           ::  tag-length-value
        ^-  (list @)
        =/  a  lem
        [tag (weld (len a) a)]
      ::                                                ::  +tag:raw:en:der
      ++  tag                                           ::  type tag
        ^-  @
        ?-  pec
          [%int *]   2
          [%bit *]   3
          [%oct *]   4
          [%nul *]   5
          [%obj *]   6
          [%seq *]  48    :: (con 0x20 16)
          [%set *]  49    :: (con 0x20 17)
          [%con *]  :(con q.p.pec 0x80 ?:(p.p.pec 0 0x20))
        ==
      ::                                                ::  +lem:raw:en:der
      ++  lem                                           ::  element bytes
        ^-  (list @)
        ?-  pec
          [%int *]  =/  a  (flop (rip 3 p.pec))   :: note: big-endian
                    ?~  a  [0 ~]
                    ?:((lte i.a 127) a [0 a])
          ::
          [%bit *]  [0 (rip 3 p.pec)]            :: XX padding
          [%oct *]  (rip 3 p.pec)
          [%nul *]  ~
          [%obj *]  (rip 3 p.pec)
          ::
          [%seq *]  %-  zing
                    |-  ^-  (list (list @))
                    ?~  p.pec  ~
                    :-  ren(pec i.p.pec)
                    $(p.pec t.p.pec)
          ::
          [%set *]  %-  zing                     :: XX tap/sort
                    |-  ^-  (list (list @))
                    ?~  p.pec  ~
                    :-  ren(pec i.p.pec)
                    $(p.pec t.p.pec)
          ::
          [%con *]  q.pec
        ==
      ::                                                ::  +len:raw:en:der
      ++  len                                           ::  length bytes
        |=  a=(list @)
        ^-  (list @)
        =/  b  (lent a)
        ?:  (lte b 127)
          [b ~]                :: note: big-endian
        [(con 0x80 (met 3 b)) (flop (rip 3 b))]
      --
    --
  ::                                                    ::  +de:der
  ++  de                                                ::  decode to spec
    =<  |=(a=@ `(unit spec:asn1)`(rush a decode))
    |%                                                  ::  +decode:de:der
    ++  decode                                          ::  DER parser
      %+  cook  |*(a=* `spec:asn1`a)                    ::  XX rename
      :: ^-  $-(nail (like spec:asn1))
      ;~  pose
        (stag %int (bass 256 (sear int ;~(pfix (tag 2) till))))
        (stag %bit (boss 256 (cook tail ;~(pfix (tag 3) till)))) :: XX test
        (stag %oct (boss 256 ;~(pfix (tag 4) till)))
        (stag %nul (cold ~ ;~(plug (tag 5) (tag 0))))
        (stag %obj (boss 256 ;~(pfix (tag 6) till)))
        (stag %seq (sear recur ;~(pfix (tag 48) till)))
        (stag %set (sear recur ;~(pfix (tag 49) till)))
        (stag %con ;~(plug (sear context next) till))
      ==
    ::                                                  ::  +tag:de:der
    ++  tag                                             ::  tag byte
      |=(a=@ (just a))
    ::                                                  ::  +int:de:der
    ++  int                                             ::  @u big-endian
      |=  a=(list @)
      ^-  (unit (list @))
      ?~  a  ~
      ?:  ?=([@ ~] a)  `a
      ?.  =(0 i.a)  `a
      ?.((gth i.t.a 127) ~ `t.a)
    ::
    ++  recur
      |=(a=(list @) (rust a (star decode))) :: XX plus? curr?
    ::                                                  ::  +context:de:der
    ++  context                                         ::  context-specific tag
      |=  a=@
      ^-  (unit [? @udC])
      ?.  =(1 (cut 0 [7 1] a))  ~
      :+  ~
        =(1 (cut 0 [5 1] a))
      (dis 0x1f a)
    ::                                                  ::  +till:de:der
    ++  till                                            ::  len-prefixed bytes
      |=  tub/nail
      ^-  (like (list @D))
      ?~  q.tub
        (fail tub)
      =*  fuz  i.q.tub
      =+  ^-  [nex=@ len=@]
        =/  faz  (end 0 7 fuz)
        ?:  =(0 (cut 0 [7 1] fuz))
          [0 faz]
        [faz (rep 3 (flop (scag faz t.q.tub)))]
      ?:  ?&  !=(0 nex)
              !=(nex (met 3 len))
          ==
        (fail tub)
      =/  zuf  (swag [nex len] t.q.tub)
      ?.  =(len (lent zuf))
        (fail tub)
      =/  zaf  [p.p.tub (add +(nex) q.p.tub)]
      [zaf `[zuf zaf (slag (add nex len) t.q.tub)]]
    --
  --
::                                                      ::  +rsa
++  rsa                                                 ::  textbook RSA
  |%                                                    ::  unpadded!
  +=  key
    $:  pub=[n=@ux e=@ux]
        sek=(unit [d=@ux p=@ux q=@ux])
    ==
  ::                                                    ::  +elcm:rsa
  ++  elcm                                              ::  carmichael totient
    |=  [a=@ b=@]
    (div (mul a b) d:(egcd a b))
  ::                                                    ::  +new-key:rsa
  ++  new-key                                           ::
    =/  e  `@ux`65.537
    |=  [wid=@ eny=@]
    ^-  key
    =/  diw  (rsh 0 1 wid)
    =/  p=@ux  (ramp:number diw [3 5 ~] eny)
    =/  q=@ux  (ramp:number diw [3 5 ~] +(eny))
    =/  n=@ux  (mul p q)
    =/  d=@ux  (~(inv fo (elcm (dec p) (dec q))) e)
    [[n e] `[d p q]]
  ::                                                    ::  +en:rsa
  ++  en                                                ::  unpadded RSA encrypt
    |=  [m=@ k=key]
    ~|  %rsa-len
    ?>  (lte (met 0 m) (met 0 n.pub.k))
    (~(exp fo n.pub.k) e.pub.k m)
  ::                                                    ::  +de:rsa
  ++  de                                                ::  unpadded RSA decrypt
    |=  [m=@ k=key]
    :: XX assert rsa-len here too?
    ~|  %rsa-need-ring
    ?>  ?=(^ sek.k)
    =/  fu  (fu:number p.u.sek.k q.u.sek.k)
    (out.fu (exp.fu d.u.sek.k (sit.fu m)))
  --
::                                                      ::  +rs256
++  rs256                                               ::  RSA sha-256 digest
  |_  k=key:rsa
  ::                                                    ::  +emsa:rs256
  ++  emsa                                              ::  EMSA-PKCS1-v1_5
    |=  m=@
    =/  emlen  (met 3 n.pub.k)
    =/  pec=spec:asn1
      :~  %seq
          [%seq [%obj sha-256:obj:asn1] [%nul ~] ~]
          [%oct (shax m)]
      ==
    =/  t=(list @)  ~(ren raw:en:der pec)
    =/  tlen  (lent t)
    ?:  (lth emlen (add 11 tlen))
      ~|(%emsa-too-short !!)
    =/  ps  (reap (sub emlen (add 3 tlen)) 0xff)
    %+  rep  3
    (flop (weld [0x0 0x1 ps] [0x0 t]))  :: note: big-endian
  ::                                                    ::  +sign:rs256
  ++  sign  |=(m=@ (de:rsa (emsa m) k))                 ::
  ::                                                    ::  +verify:rs256
  ++  verify                                            ::
    |=  [s=@ m=@]
    =((emsa m) (en:rsa s k))
  --
::                                                      ::  +pem
++  pem                                                 ::  rfc7468
  |%                                                    ::  +en:pem
  ++  en                                                ::  PEM encode
    |=  [lab=@t der=@ux]
    ^-  wain
    :: XX validate label?
    :-  (rap 3 ['-----BEGIN ' lab '-----' ~])
    =/  a  (en:base64 der)
    |-  ^-  wain
    ?~  a
      [(rap 3 ['-----END ' lab '-----' ~]) ~]
    [(end 3 64 a) $(a (rsh 3 64 a))]
  ::                                                     ::  +de:pem
  ++  de                                                 ::  PEM decode
    |=  [lab=@t mep=wain]
    ^-  (unit @ux)
    =/  a  (sub (lent mep) 2)
    ?~  mep  ~
    :: XX validate label?
    ?.  =((rap 3 ['-----BEGIN ' lab '-----' ~]) i.mep)  ~
    ?.  =((rap 3 ['-----END ' lab '-----' ~]) (snag a t.mep))  ~
    (de:base64 (rap 3 (scag a t.mep)))
  --
::                                                      ::  +pkcs1
++  pkcs1                                               ::  RSA asym crypto
  |%                                                    ::  rfc3447
  ++  spec
    |%
    ++  pass                                            ::  +pass:spec:pkcs1
      |=  k=key:rsa                                     ::  public key ASN.1
      ^-  spec:asn1
      [%seq [%int n.pub.k] [%int e.pub.k] ~]
    ::
    ++  ring                                            ::  +ring:spec:pkcs1
      |=  k=key:rsa                                     ::  private key ASN.1
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
  ::                                                    ::  +der:pkcs1
  ++  der                                               ::
    |%                                                  ::  +en:der:pkcs1
    ++  en                                              ::
      |%
      ++  pass  |=(k=key:rsa `@ux`(en:^der (pass:spec k)))
      ++  ring  |=(k=key:rsa `@ux`(en:^der (ring:spec k)))
      --
    ++  de                                              ::  +de:der:pkcs1
      |%
      ++  pass
        |=  a=@
        ^-  (unit key:rsa)
        =/  b  (de:^der a)
        ?~  b  ~
        ?.  ?=([%seq [%int *] [%int *] ~] u.b)
          ~
        =*  n  p.i.p.u.b
        =*  e  p.i.t.p.u.b
        `[[n e] ~]
      ::
      ++  ring
        |=  a=@
        ^-  (unit key:rsa)
        =/  b  (de:^der a)
        ?~  b  ~
        ?.  ?=([%seq *] u.b)  ~
        ?.  ?=  $:  [%int %0]
                    [%int *]
                    [%int *]
                    [%int *]
                    [%int *]
                    [%int *]
                    *
                ==
            p.u.b
          ~
        =*  n  p.i.t.p.u.b
        =*  e  p.i.t.t.p.u.b
        =*  d  p.i.t.t.t.p.u.b
        =*  p  p.i.t.t.t.t.p.u.b
        =*  q  p.i.t.t.t.t.t.p.u.b
        `[[n e] `[d p q]]
      --
    --
  ::
  ++  pem
    |%
    ++  en                                              ::  +en:pem:pkcs1
      |%
      ++  pass  |=(k=key:rsa (en:^pem 'RSA PUBLIC KEY' (pass:en:der k)))
      ++  ring  |=(k=key:rsa (en:^pem 'RSA PRIVATE KEY' (ring:en:der k)))
      --
    ++  de                                              ::  +de:pem:pkcs1
      |%
      ++  pass  |=(mep=wain (biff (de:^pem 'RSA PUBLIC KEY' mep) pass:de:der))
      ++  ring  |=(mep=wain (biff (de:^pem 'RSA PRIVATE KEY' mep) ring:de:der))
      --
    --
  --
::                                                      ::  +pkcs8
++  pkcs8                                               ::  asym crypto
  |%                                                    ::  rfc5208
  ++  spec
    |%                                                  ::  +pass:spec:pkcs8
    ++  pass                                            ::  public key
      |=  k=key:rsa
      ^-  spec:asn1
      :~  %seq
          [%seq [[%obj rsa:obj:asn1] [%nul ~] ~]]
          [%bit (pass:en:der:pkcs1 k)]
      ==
    ::
    ++  ring  !!
    --
  ::                                                    ::  +der:pkcs8
  ++  der                                               ::
    |%                                                  ::  +en:der:pkcs8
    ++  en                                              ::
      |%
      ++  pass  |=(k=key:rsa `@ux`(en:^der (pass:spec k)))
      ++  ring  !! ::|=(k=key:rsa `@ux`(en:^der (ring:spec k)))
      --
    ::                                                  ::  +de:der:pkcs8
    ++  de                                              ::
      |%
      ++  pass
        |=  a=@
        ^-  (unit key:rsa)
        =/  b  (de:^der a)
        ?~  b  ~
        ?.  ?=([%seq [%seq *] [%bit *] ~] u.b)
          ~
        ?.  ?&  ?=([[%obj *] [%nul ~] ~] p.i.p.u.b)
                =(rsa:obj:asn1 p.i.p.i.p.u.b)
            ==
          ~
        (pass:de:der:pkcs1 p.i.t.p.u.b)
      ::
      ++  ring  !!
      --
    --
  ::                                                    ::  +pem:pkcs8
  ++  pem                                               ::
    |%                                                  ::  +en:pem:pkcs8
    ++  en                                              ::
      |%
      ++  pass  |=(k=key:rsa (en:^pem 'PUBLIC KEY' (pass:en:der k)))
      ++  ring  !! ::|=(k=key:rsa (en:^pem 'PUBLIC KEY' (ring:en:der k)))
      --
    ::                                                  ::  +de:pem:pkcs8
    ++  de                                              ::
      |%
      ++  pass  |=(mep=wain (biff (de:^pem 'PUBLIC KEY' mep) pass:de:der))
      ++  ring  !! ::|=(mep=wain (biff (de:^pem 'PRIVATE KEY' mep) ring:de:der))
      --
    --
  --
::                                                      ::  +pkcs10
++  pkcs10                                              ::  certificate request
  =>  |%                                                ::  rfc2986
      +=  csr  [key=key:rsa hot=(list (list @t))]
      --
  |%
  ++  spec
    |%                                                  ::  +host:spec:pkcs10
    ++  host                                            ::  subject-alt names
      |=  hot=(list (list @t))
      ^-  spec:asn1
      :-  %seq
      %+  turn  hot
      |=(h=(list @t) [%con [& 2] (rip 3 (en-host h))])
    ::                                                  ::  +cert:spec:pkcs10
    ++  cert                                            ::  cert request info
      |=  csr                                           ::  XX rename
      ^-  spec:asn1
      :~  %seq
          [%int 0]
          [%seq ~]
          (pass:spec:pkcs8 key)
          :+  %con  [| 0]
          =-  ~(ren raw:en:^der -)
          :~  %seq
              [%obj csr-ext:obj:asn1]
              :~  %set
                  :~  %seq
                      :~  %seq
                          [%obj sub-alt:obj:asn1]
                          [%oct (en:^der (host hot))]
      ==  ==  ==  ==  ==
    ::                                                  ::  +sign:spec:pkcs10
    ++  sign                                            ::  signed request
      |=  csr
      ^-  spec:asn1
      =/  cer  (cert key hot)
      :~  %seq
          cer
          [%seq [[%obj rsa-sha-256:obj:asn1] [%nul ~] ~]]
          [%bit (swp 3 (~(sign rs256 key) (en:^der cer)))]  :: big-endian
      ==
    --
  ::                                                    ::  +der:pkcs10
  ++  der
    |%
    ++  en  |=(a=csr `@ux`(en:^der (sign:spec a)))
    ++  de  !!
    --
  ::                                                    ::  +pem:pkcs10
  ++  pem
    |%
    ++  en  |=(a=csr (en:^pem 'CERTIFICATE REQUEST' (en:der a)))
    ++  de  !! :: |=(mep=wain (biff (de:^pem 'CERTIFICATE REQUEST' mep) de:der))
    --
  --
::                                                    ::  +en-json-sort
++  en-json-sort                                      ::  json w/ sorted keys
  |^  |=([sor=$-(^ ?) val=json] (apex val sor ""))    ::  XX rename
  ::                                                  ::  ++apex:en-json:html
  ++  apex
    =,  en-json:html
    |=  {val/json sor/$-(^ ?) rez/tape}
    ^-  tape
    ?~  val  (weld "null" rez)
    ?-    -.val
        $a
      :-  '['
      =.  rez  [']' rez]
      !.
      ?~  p.val  rez
      |-
      ?~  t.p.val  ^$(val i.p.val)
      ^$(val i.p.val, rez [',' $(p.val t.p.val)])
   ::
        $b  (weld ?:(p.val "true" "false") rez)
        $n  (weld (trip p.val) rez)
        $s
      :-  '"'
      =.  rez  ['"' rez]
      =+  viz=(trip p.val)
      !.
      |-  ^-  tape
      ?~  viz  rez
      =+  hed=(jesc i.viz)
      ?:  ?=({@ $~} hed)
        [i.hed $(viz t.viz)]
      (weld hed $(viz t.viz))
   ::
        $o
      :-  '{'
      =.  rez  ['}' rez]
      =/  viz
        %+  sort  ~(tap by p.val)
        |=((pair) (sor (head p) (head q)))
      ?~  viz  rez
      !.
      |-  ^+  rez
      ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
      =.  rez  [',' $(viz t.viz)]
      ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
    ==
  --
::                                                      ::  +jwk
++  jwk                                                 ::  json web keys
  |%                                                    ::  rfc7517
  ++  en
    |%                                                  ::  +pass:en:jwk
    ++  pass                                            ::  encode public key
      |=  k=key:rsa
      ^-  json
      :-  %o  %-  my  :~
        kty+s+'RSA'
        n+s+(en-base64url (swp 3 n.pub.k))      :: note: big-endian
        e+s+(en-base64url (swp 3 e.pub.k))
      ==
    ::                                                  ::  +ring:en:jwk
    ++  ring                                            ::  encode private key
      |=  k=key:rsa
      ^-  json
      ~|  %rsa-need-ring
      ?>  ?=(^ sek.k)
      :-  %o  %-  my  :~
        kty+s+'RSA'
        n+s+(en-base64url (swp 3 n.pub.k))
        e+s+(en-base64url (swp 3 e.pub.k))
        d+s+(en-base64url (swp 3 d.u.sek.k))
        p+s+(en-base64url (swp 3 p.u.sek.k))
        q+s+(en-base64url (swp 3 q.u.sek.k))
      ==
    --
  ++  de
    |%                                                  ::  +pass:de:jwk
    ++  pass                                            ::  decode public key
      =,  dejs-soft:format
      %+  ci
        |=  [kty=@t n=(unit @) e=(unit @)]
        ^-  (unit key:rsa)
        =/  swp  (cury swp 3)
        =/  pub  (both (bind n swp) (bind e swp))
        ?~(pub ~ `[u.pub ~])
      %-  ot  :~
        kty+(su (jest 'RSA'))
        n+(cu de-base64url so)
        e+(cu de-base64url so)
      ==
    ::                                                  ::  +ring:de:jwk
    ++  ring                                            ::  decode private key
      =,  dejs-soft:format
      %+  ci
        |=  $:  kty=@t
                n=(unit @)
                e=(unit @)
                d=(unit @)
                p=(unit @)
                q=(unit @)
            ==
        ^-  (unit key:rsa)
        =/  swp  (cury swp 3)
        =/  pub  (both (bind n swp) (bind e swp))
        =/  sek  :(both (bind d swp) (bind p swp) (bind q swp))
        ?:(|(?=(~ pub) ?=(~ sek)) ~ `[u.pub sek])
      %-  ot  :~
        kty+(su (jest 'RSA'))
        n+(cu de-base64url so)
        e+(cu de-base64url so)
        d+(cu de-base64url so)
        p+(cu de-base64url so)
        q+(cu de-base64url so)
      ==
    --
  ::                                                    ::  +thumb:jwk
  ++  thumb                                             ::  thumbprint
    |%                                                  ::  rfc7638
    ++  ring  !!
    ++  pass
      |=  k=key:rsa
      (en-base64url (shax (crip (en-json-sort aor (pass:en k)))))
    --
  --
::                                                      ::  +jws
++  jws                                                 ::  json web signature
  |%                                                    ::  rfc7515
  ++  sign                                              ::  +sign:jws
    |=  [k=key:rsa pro=json lod=json]                   ::  flattened signature
    |^  ^-  json
        =.  pro  header
        =/  protect=cord  (encode pro)
        =/  payload=cord  (encode lod)
        :-  %o  %-  my  :~
          protected+s+protect
          payload+s+payload
          signature+s+(sign protect payload)
        ==
    ::
    ++  header
      ?>  ?=([%o *] pro)
      ^-  json
      [%o (~(put by p.pro) %alg s+'RS256')]
    ::
    ++  encode
      |=  jon=json
      (en-base64url (crip (en-json-sort aor jon)))
    ::
    ++  sign
      |=  [protect=cord payload=cord]
      %-  en-base64url
      (swp 3 (~(sign rs256 k) (rap 3 ~[protect '.' payload])))
    --
  ::
  ++  verify  !!
  --
::                                                    ::  +eor
++  eor                                               ::  explicit order
  |=  [com=$-([@ @] ?) lit=(list)]
  |=  [a=* b=*]
  ^-  ?
  (fall (bind (both (find ~[a] lit) (find ~[b] lit)) com) |)
::                                                    ::  +en-host
++  en-host                                           ::  rendor host
  |=  hot=(list @t)
  ^-  @t
  =|  out=(list @t)
  ?>  ?=(^ hot)
  |-  ^-  @t
  ?~  t.hot
    (rap 3 [i.hot out])
  $(out ['.' i.hot out], hot t.hot)
::
:::: acme api response json reparsers
::
++  grab
  =,  dejs:format
  |%
  ++  json-purl  (su auri:de-purl:html)
  ::
  ++  directory
    %-  ot
    :~  ['newAccount' json-purl]
        ['newNonce' json-purl]
        ['newOrder' json-purl]
        ['revokeCert' json-purl]
        ['keyChange' json-purl]
    ==
  ::
  ++  acct
    %-  ot
    :~  ['id' no]
        ['createdAt' so] :: XX (su iso-8601)
        ['status' so]
        :: ignore key, contact, initialIp
    ==
  ::
  ++  order
    %-  ot
    :~  ['authorizations' (ar json-purl)]
        ['finalize' json-purl]
        ['expires' so] :: XX (su iso-8601)
        ['status' so]
    ==
  ::
  ++  finalizing-order
    %-  ot
    :~  ['expires' so] :: XX (su iso-8601)
        ['status' so]
    ==
  ::
  ++  final-order
    %-  ot
    :~  ['expires' so] :: XX (su iso-8601)
        ['status' so]
        ['certificate' json-purl]
    ==
  ::
  ++  auth
    =>  |%
        ++  iden
          |=  [typ=@t hot=host]
          ?>(&(?=(%dns typ) ?=([%& *] hot)) p.hot)
        ::
        ++  trial
          |=  a=(list [typ=@t sas=@t url=purl tok=@t])
          ^+  ?>(?=(^ a) i.a)
          =/  b
            (skim a |=([typ=@t *] ?=(%http-01 typ)))
          ?>(?=(^ b) i.b)
        --
    %-  ot
    :~  ['identifier' (cu iden (ot type+so value+(su thos:de-purl:html) ~))]
        ['status' so]
        ['expires' so] :: XX (su iso-8601)
        ['challenges' (cu trial (ar challenge))]
    ==
  ::
  ++  challenge
    %-  ot
    :~  ['type' so]
        ['status' so]
        ['url' json-purl]
        ['token' so]
    ==
  ::
  ++  error
    %-  ot
    :~  ['type' so]
        ['detail' so]
    ==
  --
--
::
::::  acme state
::
|%
+=  move  [bone card]                                   ::
+=  card                                                ::
  $%  [%hiss wire [~ ~] %httr %hiss hiss:eyre]          ::
      [%well wire path (unit mime)]                     ::
  ==                                                    ::
+=  directory                                           ::  ACME v2
  $:  reg/purl                                          ::  newAccount
      non/purl                                          ::  newNonce
      der/purl                                          ::  newOrder
      rev/purl                                          ::  revokeCert
      rek/purl                                          ::  keyChange
  ==                                                    ::
+=  acct  [key=key:rsa reg=(unit [wen=@t kid=@t])]      ::  account XX wen=@da
+=  turf  (list @t)                                     ::  domain
+=  trial                                               ::  challenge
  $%  [%http cal=purl tok=@t sas=?(%recv %pend %auth)]  ::    http-only
  ==                                                    ::
+=  auth                                                ::  authorization
  $%  [%0 aut=purl]                                     ::    received
      [%1 aut=purl dom=turf cal=trial]                  ::    in-progress
  ==                                                    ::
+=  order                                               ::  certificate order
  $%  [%0 dom=(list turf)]                              ::    initialized
      [%1 dom=(list turf) exp=@t der=purl fin=purl aut=(map @ud auth)] :: XX exp=@da
      [%2 dom=(list turf) exp=@t der=purl fin=purl key=key:rsa csr=@ux]      ::    cert requested
  ==                                                    ::
+=  config  [key=key:rsa exp=@da cer=wain]              ::  finalized config
+=  history                                             ::  isn't over
  $:  act=(list acct)                                   ::
      rod=(list order)                                  ::
      fig=(list (pair (list turf) config))              ::
  ==                                                    ::
+=  acme                                                ::
  $:  bas=purl                                          ::  service base url
      dir=directory                                     ::  service urls
      act=acct                                          ::  service account
      non=@t                                            ::  nonce from last
      rod=(map @ud order)                               ::  active orders
      cey=key:rsa                                       ::  cert key
      liv=(map (list turf) config)                      ::  active config
      hit=history                                       ::  a foreign country
  ==                                                    ::
--                                                      ::
::
::::  acme app
::
=|  mov=(list move)
|_  [bow=bowl:gall acme]
::
++  this  .               :: XX #712
::
++  emit
  |=  car=card
  ~&  [%emit car]
  this(mov [[ost.bow car] mov])
::
++  emil
  |=  rac=(list card)
  q:(spin rac this |=([a=card b=_this] [~ (emit:b a)]))
::
++  abet
  [(flop mov) this(mov ~)]
::
++  request
  |=  [wir=wire url=purl bod=(unit json)]
  |^  ^-  card
      [%hiss wir [~ ~] %httr %hiss url moth]
  ::
  ++  moth
    ?~  bod
      [%get ~ ~]
    [%post (my content-type+['application/jose+json' ~] ~) `body]
  ::
  ++  body
    ?>  ?=(^ bod)
    ^-  octs
    =;  pro=json
      (as-octt:mimes:html (en-json:html (sign:jws key.act pro u.bod)))
    :-  %o  %-  my  :~
      nonce+s+non
      url+s+(crip (en-purl:html url))
      ?^  reg.act
        kid+s+kid.u.reg.act
      jwk+(pass:en:jwk key.act)
    ==
  --
::
++  directory
  (emit (request /acme/dir/(scot %p our.bow) bas ~))
::
++  nonce
  |=  nex=wire
  ^+  this
  ?>  ?=([%next *] nex)
  (emit (request (weld `wire`/acme/non `wire`nex) non.dir ~))
::
++  register
  %-  emit(reg.act ~)
  %^  request  /acme/reg/(scot %p our.bow)
    reg.dir
  `[%o (my [['termsOfServiceAgreed' b+&] ~])]
::
++  new-order
  ^+  this
  ?~  reg.act
    this          :: XX pending registration assumed
  %-  emil
  %+  turn
    (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %0 *] a)))
  |=  [i=@ud rod=order]
  ^-  card
  ?>  ?=([%0 *] rod)
  %^  request  /acme/der/(scot %ud i)
    der.dir
  :-  ~
  ^-  json
  :-  %o  %-  my  :~
    :-  %identifiers
    :-  %a
    %+  turn
      dom.rod
    |=(a=turf [%o (my type+s+'dns' value+s+(en-host a) ~)])
  ==
::
++  authorize
  ^+  this
  %-  emil
  =/  aut=(list (trel @ud @ud purl))
    %-  zing
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %1 *] a)))
    |=  [ider=@ud rod=order]
    ?>  ?=([%1 *] rod)
    %+  turn
      ~(tap by aut.rod)
    |=  [i=@ud aut=auth]
    ?>  ?=([%0 *] aut)
    [i ider aut.aut]
  %+  turn  aut
  |=  [i=@ud ider=@ud aut=purl]
  ^-  card
  (request /acme/aut/(scot %ud i)/der/(scot %ud ider) aut ~)
::
++  save-challenge
  ^+  this
  %-  emil
  =/  cal=(list (trel @ud @ud trial))
    %-  zing
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %1 *] a)))
    |=  [ider=@ud rod=order]
    ?>  ?=([%1 *] rod)
    %+  turn
      ~(tap by aut.rod)
    |=  [i=@ud aut=auth]
    ?>  ?=([%1 *] aut)
    [i ider cal.aut]
  %+  turn  cal
  |=  [i=@ud ider=@ud cal=trial]
  ^-  card
  :^    %well
      /acme/wel/(scot %ud i)/der/(scot %ud ider)
    /acme-challenge/[tok.cal]
  :+  ~
    /text/plain
  %-  as-octs:mimes:html
  (rap 3 [tok.cal '.' (pass:thumb:jwk key.act) ~])
::
++  test-challenge
  ^+  this
  %-  emil
  =/  cal=(list [@ud @ud turf trial])
    %-  zing
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %1 *] a)))
    |=  [ider=@ud rod=order]
    ?>  ?=([%1 *] rod)
    %+  turn
      ~(tap by aut.rod)
    |=  [i=@ud aut=auth]
    ?>  ?=([%1 *] aut)
    [i ider dom.aut cal.aut]
  %+  turn  cal
  |=  [i=@ud ider=@ud dom=turf cal=trial]
  ^-  card
  =/  wir=wire  /acme/tcal/(scot %ud i)/der/(scot %ud ider)
  =/  pat=path  /'.well-known'/acme-challenge/[tok.cal]
  =/  pul=purl  [[| ~ [%& dom]] [~ pat] ~]
  (request wir pul ~)
::
++  finalize-challenge
  ^+  this
  %-  emil
  =/  cal=(list (trel @ud @ud purl))
    %-  zing
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %1 *] a)))
    |=  [ider=@ud rod=order]
    ?>  ?=([%1 *] rod)
    %+  turn
      ~(tap by aut.rod)
    |=  [i=@ud aut=auth]
    ?>  ?=([%1 *] aut)
    [i ider cal.cal.aut]
  %+  turn  cal
  |=  [i=@ud ider=@ud cal=purl]
  ^-  card
  =/  wir=wire  /acme/cal/(scot %ud i)/der/(scot %ud ider)
  (request wir cal `[%o ~])
::
++  finalize-order
  ^+  this
  %-  emil
  =/  csr=(list (trel @ud purl @ux))
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %2 *] a)))
    |=  [i=@ud rod=order]
    ?>  ?=([%2 *] rod)
    [i fin.rod csr.rod]
  %+  turn  csr
  |=  [i=@ud fin=purl csr=@ux]
  ^-  card
  %^    request
      /acme/fin/(scot %ud i)
    fin
  `[%o (my csr+s+(en-base64url csr) ~)]
::
++  poll-order
  ^+  this
  %-  emil
  =/  url=(list (pair @ud purl))
    %+  turn
      (skim ~(tap by rod) |=(a=[@ud order] ?=([@ %2 *] a)))
    |=  [i=@ud rod=order]
    ?>  ?=([%2 *] rod)
    [i der.rod]
  %+  turn  url
  |=  [i=@ud der=purl]
  ^-  card
  (request /acme/por/(scot %ud i) der ~)
::
++  sigh-httr
  |=  [wir=wire rep=httr:eyre]
  ^-  (quip move _this)
  ~&  [wir rep]
  ?>  ?=([%acme ^] wir)
  =/  ron  (skim q.rep |=((pair @t @t) ?=(%replay-nonce p)))
  =?  non  ?=(^ ron)  q.i.ron
  ?.  ?=(%2 (div p.rep 100))
    ~&  %lack-of-success
    ?:  ?=(%tcal i.t.wir)
      abet:test-challenge  :: XX sleep?
    =/  bod=[typ=@t det=@t]
      (error:grab (need (de-json:html q:(need r.rep))))
    ?:  =('urn:ietf:params:acme:error:badNonce' typ.bod)
      ?.  ?=(?(%reg %der %aut %cal %fin) i.t.wir)
        ~&(unrecoverable-bad-nonce+wir abet:this)
      abet:(nonce /next/[i.t.wir])
    :: XX challenge is not pending
    :: XX order can't be finalized
    abet:this
  ~|  sigh-fail+rep
  ?+  i.t.wir  !!
      %dir
    =<  abet:(nonce /next/reg)
    =/  bod=^directory
      (directory:grab (need (de-json:html q:(need r.rep))))
    this(dir bod)
  ::
      %non
    =<  abet
    ?.  ?=([%next ^] t.t.wir)
      ~&(unrecognized-nonce-wire+wir this)
    =*  nex  i.t.t.t.wir
    ?+  nex  ~&(unknown-nonce-next+nex this)
      %reg  register
      %der  new-order
      %aut  authorize
      %cal  finalize-challenge
      %fin  finalize-order
    ==
  ::
      %reg
    =<  abet:new-order
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    =/  wen=@t              :: XX @da
      ?~  r.rep
        (scot %da now.bow)
      =/  bod=[id=@t wen=@t sas=@t]
        (acct:grab (need (de-json:html q.u.r.rep)))
      ?>  ?=(%valid sas.bod)
      wen.bod
    this(reg.act `[wen loc])
  ::
      %der
    =<  abet:authorize
    =/  i=@ud  (slav %ud (head t.t.wir))
    =/  der=order  (~(got by rod) i)
    ?>  ?=([%0 *] der)
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    =/  url=purl  (need (de-purl:html loc))
    =/  bod=[aut=(list purl) fin=purl exp=@t sas=@t]
      (order:grab (need (de-json:html q:(need r.rep))))
    =/  aut  %-  ~(gas by *(map @ud auth))
             (spun aut.bod |=([a=purl b=@ud] [[b %0 a] +(b)]))
    =/  dor=order  [%1 dom.der exp.bod url fin.bod aut]
    this(rod (~(put by rod) i dor))
  ::
      %aut
    =<  abet:test-challenge:save-challenge
    ?>  ?=([@ %der @ *] t.t.wir)
    =/  i  (slav %ud i.t.t.wir)
    =/  ider  (slav %ud i.t.t.t.t.wir)
    =/  der=order  (~(got by rod) ider)
    ?>  ?=([%1 *] der)
    =/  aut=auth  (~(got by aut.der) i)
    ?>  ?=([%0 *] aut)
    =/  bod=[dom=turf sas=@t exp=@t cal=[typ=@t sas=@t url=purl tok=@t]]
      (auth:grab (need (de-json:html q:(need r.rep))))
    =/  cal=trial
      [%http url.cal.bod tok.cal.bod %recv] :: XX parse tok?
    =/  tau=auth  [%1 aut.aut dom.bod cal]
    =.  der  der(aut (~(put by aut.der) i tau))
    this(rod (~(put by rod) ider der))
  ::
      %tcal
    :: XX check content type and response body
    abet:finalize-challenge
  ::
      %cal
    =<  abet:finalize-order
    ?>  ?=([@ %der @ *] t.t.wir)
    =/  i  (slav %ud i.t.t.wir)
    =/  ider  (slav %ud i.t.t.t.t.wir)
    =/  der=order  (~(got by rod) ider)
    ?>  ?=([%1 *] der)
    =/  aut=auth  (~(got by aut.der) i)
    ?>  ?=([%1 *] aut)
    :: XX 204 assuming pending?
    :: =/  bod=[typ=@t sas=@t url=purl tok=@t]
    ::   (challenge:grab (need (de-json:html q:(need r.rep))))
    :: ?>  ?=(%pending sas.bod)
    =.  sas.cal.aut  %pend
    =.  der  der(aut (~(put by aut.der) i aut))
    =.  rod  (~(put by rod) ider der)
    =/  fin=(list (pair @ud order))
      %+  skim
        ~(tap by rod)
      |=  a=[@ud order]
      ?&  ?=([@ %1 *] a)
          %+  levy
            ~(tap by aut.a)
          |=  [i=@ud aut=auth]
          ?&  ?=([%1 *] aut)
              ?=(%pend sas.cal.aut)
      ==  ==
    %=  this
      rod  %-  ~(gas by rod)
           %+  turn  fin
           |=  [i=@ud der=order]
           ^+  [i der]
           ?>  ?=([%1 *] der)
           :: =/  k=key:rsa  rekey  :: XX reuse
           =/  csr=@ux  (en:der:pkcs10 cey dom.der)
           [i %2 dom.der exp.der der.der fin.der cey csr]
      :: XX save pending authz somewhere instead of just dropping them
    ==
  ::
      %fin
    :: XX rep body missing authorizations
    :: XX finalizing-order
    :: =/  bod=[aut=(list purl) fin=purl exp=@t sas=@t]
    ::   (order:grab (need (de-json:html q:(need r.rep))))
    :: XX check status? (i don't think failures get here)
    abet:poll-order
  ::
      %por
    =/  i=@ud  (slav %ud (head t.t.wir))
    =/  raw=json
      (need (de-json:html q:(need r.rep)))
    =/  bod=[exp=@t sas=@t]
      (finalizing-order:grab raw)
    ?+  sas.bod
        ~&(poll-order-status+sas.bod abet)
      %invalid     abet          :: XX check authz, retry order?
      %pending     abet:poll-order
      %processing  abet:poll-order
      %valid       =<  abet
                   =/  bod=[exp=@t sas=@t cer=purl]
                     (final-order:grab raw)  :: XX json reparser unit
                   %-  emit                  :: XX accept hed
                   (request /acme/cer/(scot %ud i) cer.bod ~)
                   :: XX del .well-known
    ==
  ::
      %cer    :: XX send configuration to eyre
    =<  abet
    =/  i=@ud  (slav %ud (head t.t.wir))
    =/  der=order  (~(got by rod) i)
    =/  cer=wain
      (to-wain:format q:(need r.rep))
    ?>  ?=([%2 *] der)
    %=  this
      liv      (~(put by liv) dom.der [key.der *@da cer]) :: XX exp when?
      rod      (~(del by rod) i)
      rod.hit  [der rod.hit]
    ==
  ==
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a  ~&  +<+.this
         [~ this]
    %init   abet:init
    %reg    abet:register
    %order  abet:new-order
    %auth   abet:authorize
    %trial  abet:test-challenge
    %final  abet:finalize-order
    %poll   abet:poll-order
    %our    abet:(add-order /org/urbit/(crip +:(scow %p our.bow)) ~)
    %test   test
  ==
::
++  poke-path
  |=(a=path abet:new-order:(add-order a ~))
::
:: ++  prep  _[~ this]
++  prep
  |=  old=(unit acme)
  ?~  old
    abet:init
  [~ this(+<+ u.old)]
::
++  rekey                             :: XX do something about this
  |=  eny=@
  =|  i=@
  |-  ^-  key:rsa
  =/  k  (new-key:rsa 2.048 eny)
  =/  m  (met 0 n.pub.k)
  ?:  =(0 (mod m 8))  k
  ~&  [%key iter=i width=m]
  $(i +(i), eny +(eny))
::
++  init
  =/  url
    'https://acme-staging-v02.api.letsencrypt.org/directory'
  %=  directory
    bas  (need (de-purl:html url))
    act  [(rekey eny.bow) ~]
    cey  (rekey (mix eny.bow (shaz now.bow)))
  ==
  :: XX wait for DNS binding confirmation?
  :: (add-order /org/urbit/(crip +:(scow %p our.bow)) ~)
::
++  add-order
  |=  dom=(list turf)
  ^+  this
  :: XX temporarily force one at a time
  :: new-order(rod (~(put by rod) ~(wyt by rod) [%0 dom]))
  %=  new-order
    rod.hit  (weld ~(val by rod) rod.hit)
    rod      (~(put by ^+(rod ~)) ~(wyt by rod) [%0 dom])
  ==
::
++  test
  =,  tester:tester
  =/  eny  eny.bow
    :: non-deterministic for now
    :: 0vhu.gp79o.hi7at.smp8u.g5hhr.u3rff.st8ms.q4dc2.hv5ls.tp5cp.10qds.
    ::      h9bpt.vlmm7.lh375.f6u9n.krqv8.5jcml.cujkr.v1uqv.cjhe5.nplta
  |^  =/  out=tang
          ;:  weld
            test-base64
            test-asn1
            test-rsakey
            test-rsa
            test-rsapem
            test-rsa-pkcs8
            test-rs256
            test-jwk
            test-jws
            test-csr
          ==
      ?~(out abet ((slog out) abet))
  ::
  ++  test-base64
    ;:  weld
      %-  expect-eq  !>
        ['AQAB' (en-base64url 65.537)]
      %-  expect-eq  !>
        [65.537 (need (de-base64url 'AQAB'))]
      :: echo "hello" | base64
      %-  expect-eq  !>
        ['aGVsbG8K' (en:base64 'hello\0a')]
      %-  expect-eq  !>
        ['hello\0a' (need (de:base64 'aGVsbG8K'))]
      :: echo -n -e "\x01\x01\x02\x03" | base64
      %-  expect-eq  !>
        ['AQECAw==' (en:base64 (swp 3 0x101.0203))]
      %-  expect-eq  !>
        [0x302.0101 (need (de:base64 'AQECAw=='))]
    ==
  ::
  ++  test-asn1
    =/  nul=spec:asn1  [%nul ~]
    =/  int=spec:asn1  [%int 187]
    =/  obj=spec:asn1  [%obj sha-256:obj:asn1]
    =/  oct=spec:asn1  [%oct (shax 'hello\0a')]
    =/  seq=spec:asn1  [%seq [%seq obj nul ~] oct ~]
    ;:  weld
      %-  expect-eq  !>
        :-  [0x5 0x0 ~]
        ~(ren raw:en:der nul)
      %-  expect-eq  !>
        [nul (scan ~(ren raw:en:der nul) decode:de:der)]
      %-  expect-eq  !>
        :-  [0x2 0x2 0x0 0xbb ~]
        ~(ren raw:en:der int)
      %-  expect-eq  !>
        [int (scan ~(ren raw:en:der int) decode:de:der)]
      %-  expect-eq  !>
        :-  [0x6 0x9 0x60 0x86 0x48 0x1 0x65 0x3 0x4 0x2 0x1 ~]
        ~(ren raw:en:der obj)
      %-  expect-eq  !>
        [obj (scan ~(ren raw:en:der obj) decode:de:der)]
      %-  expect-eq  !>
        :-    0x420.5891.b5b5.22d5.df08.6d0f.f0b1.10fb.
          d9d2.1bb4.fc71.63af.34d0.8286.a2e8.46f6.be03
        `@ux`(swp 3 (en:der oct))
      %-  expect-eq  !>
        [oct (scan ~(ren raw:en:der oct) decode:de:der)]
      %-  expect-eq  !>
        :-  0x30.3130.0d06.0960.8648.0165.0304.0201.0500.0420.5891.b5b5.22d5.
            df08.6d0f.f0b1.10fb.d9d2.1bb4.fc71.63af.34d0.8286.a2e8.46f6.be03
        `@ux`(swp 3 (en:der seq))
      %-  expect-eq  !>
        [seq (scan ~(ren raw:en:der seq) decode:de:der)]
    ==
  ::
  ++  test-rsakey
    =/  primes=(list @)
      :~    2    3    5    7   11   13   17   19   23   29   31   37   41   43
           47   53   59   61   67   71   73   79   83   89   97  101  103  107
          109  113  127  131  137  139  149  151  157  163  167  173  179  181
          191  193  197  199  211  223  227  229  233  239  241  251  257  263
          269  271  277  281  283  293  307  311  313  317  331  337  347  349
          353  359  367  373  379  383  389  397  401  409  419  421  431  433
          439  443  449  457  461  463  467  479  487  491  499  503  509  521
          523  541  547  557  563  569  571  577  587  593  599  601  607  613
          617  619  631  641  643  647  653  659  661  673  677  683  691  701
          709  719  727  733  739  743  751
      ==
    =/  k1  (new-key:rsa 2.048 eny)
    ::
    =/  k2=key:rsa
      =/  p  0x1837.be57.1286.bf6a.3cf8.4716.634f.ef85.f947.c654.da6e.e222.
          5654.9466.0ab0.a2ef.1985.1095.e3c3.9e74.9478.e3f3.ee92.f885.ec3c.
          84c3.6b3c.9731.65f9.9d1d.f743.646f.37d7.82d8.3f4a.856c.6453.b2c8.
          28d5.d720.145e.c7ab.4ba9.a9c2.6b8e.8819.7aa8.69b3.420f.dbfa.1ddb.
          4d1a.9c2e.e25a.d4de.d351.945f.d7ca.74a4.815d.5f0e.9f44.df64.39bd
      =/  q  0xf1bc.ec8f.d238.32d9.afb8.8083.76b3.82da.6274.f56e.1b5b.662b.
          ab1b.1e01.fbd5.86c5.ba98.b246.b621.f190.2425.25ea.b39f.efa2.4fb8.
          0d6b.c3c4.460d.e7df.d2f5.6604.51e0.415b.db60.db5a.6601.16c7.46ec.
          5e67.9195.f3c9.80d3.47c5.fe24.fbfd.43c3.380a.40bd.c4f5.d65e.b93b.
          60ca.5f26.4ed7.9c64.d26d.b0fe.985d.7be3.1308.34dd.b8c5.4d7c.d8a5
      =/  n  (mul p q)
      =/  e  0x1.0001
      =/  d  (~(inv fo (elcm:rsa (dec p) (dec q))) e)
      [[n e] `[d p q]]
    ::
    |^  ^-  tang
        ;:  weld
            (check-primes k1)
            (check-primes k2)
        ==
    ++  check-primes
      =,  number
      |=  k=key:rsa
      ?>  ?=(^ sek.k)
      %+  roll  primes
      |=  [p=@ a=tang]
      ?^  a  a
      ?:  =(0 (mod n.pub.k p))
        :~  leaf+"{(scow %ux n.pub.k)}"
            :-  %leaf
            %+  weld
              "n.pub.key (prime? {(scow %f (pram n.pub.k))})"
            " divisible by {(scow %ud p)}:"
        ==
      ?:  =(0 (mod p.u.sek.k p))
        :~  leaf+"{(scow %ux p.u.sek.k)}"
            :-  %leaf
            %+  weld
              "p.u.sek.key (prime? {(scow %f (pram p.u.sek.k))})"
            " divisible by {(scow %ud p)}:"
        ==
      ?:  =(0 (mod q.u.sek.k p))
        :~  leaf+"{(scow %ux q.u.sek.k)}"
            :-  %leaf
            %+  weld
              "q.u.sek.key (prime? {(scow %f (pram q.u.sek.k))})"
            " divisible by {(scow %ud p)}:"
        ==
      ~
    --
  ::
  ++  test-rsapem
    ::  ex from https://stackoverflow.com/a/19855935
    =/  k1=key:rsa
      :-  [`@ux`187 `@ux`7]
      [~ `@ux`23 `@ux`17 `@ux`11]
    =/  kpem1=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MBwCAQACAgC7AgEHAgEXAgERAgELAgEHAgEDAgEO'
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  k2=key:rsa
      :*  [n=`@ux`16.238.973.331.713.186.433 e=`@ux`65.537]
          ~
          d=`@ux`3.298.243.342.098.580.397
          p=`@ux`4.140.273.707
          q=`@ux`3.922.198.019
      ==
    :: openssl genrsa -out private.pem 64
    =/  kpem2=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MEACAQACCQDhXGw1Gc5agQIDAQABAggtxbbYRJVDrQIFAPbHkCsCBQDpx/4DAgUA'
          '23X55QIFAIpPROsCBQC56nYF'
          '-----END RSA PRIVATE KEY-----'
      ==
    :: openssl genrsa -out private.pem 2048
    =/  kpem3=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
          'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
          'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
          'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
          'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
          'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
          '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
          'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
          'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
          'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
          '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
          'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
          'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
          'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
          'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
          'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
          'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
          'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
          'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
          '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
          'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
          '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
          'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
          'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
          'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  kpem3-pub=wain
      :~  '-----BEGIN RSA PUBLIC KEY-----'
          'MIIBCgKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5qxx2'
          'IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdXCCSG'
          '7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3HhgS'
          'iNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNeWlqT'
          '9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6BoIG'
          'I5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQAB'
          '-----END RSA PUBLIC KEY-----'
      ==
    =/  k3=key:rsa
      (need (ring:de:pem:pkcs1 kpem3))
    =/  k3-pub=key:rsa
      (need (pass:de:pem:pkcs1 kpem3-pub))
    ;:  weld
      %-  expect-eq  !>
        [kpem1 (ring:en:pem:pkcs1 k1)]
      %-  expect-eq  !>
        [k1 (need (ring:de:pem:pkcs1 kpem1))]
      %-  expect-eq  !>
        [kpem2 (ring:en:pem:pkcs1 k2)]
      %-  expect-eq  !>
        [k2 (need (ring:de:pem:pkcs1 kpem2))]
      %-  expect-eq  !>
        [kpem3 (ring:en:pem:pkcs1 k3)]
      %-  expect-eq  !>
        [kpem3-pub (pass:en:pem:pkcs1 k3)]
      %-  expect-eq  !>
        [k3-pub [pub.k3 ~]]
    ==
  ::
  ++  test-rsa-pkcs8
    =/  kpem=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
          'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
          'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
          'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
          'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
          'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
          '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
          'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
          'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
          'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
          '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
          'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
          'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
          'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
          'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
          'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
          'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
          'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
          'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
          '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
          'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
          '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
          'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
          'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
          'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  pub=wain
      :~  '-----BEGIN PUBLIC KEY-----'
          'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2jJp8dgAKy5cSzDE4D+a'
          'UbKZsQoMhIWI2IFlE+AO0GCBMig5qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8Q'
          'IKRvkZaIwnA3Lz5UUrxgh96sezdXCCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3'
          'kneo5aAKMIPyOTzcY590UTc+luQ3HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxN'
          'fBgXbV+C7/gO8mFxpkafhmgkIGNeWlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGv'
          'xvBMp3xUASlzYHiDntcB5MiOPRW6BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxw'
          'FwIDAQAB'
          '-----END PUBLIC KEY-----'
      ==
    =/  k=key:rsa
      (need (ring:de:pem:pkcs1 kpem))
    %-  expect-eq  !>
      [pub (pass:en:pem:pkcs8 k)]
  ::
  ++  test-rsa
    =/  k1=key:rsa
      =/  p  `@ux`61
      =/  q  `@ux`53
      =/  e  `@ux`17
      =/  n  (mul p q)
      =/  d  (~(inv fo (elcm:rsa (dec p) (dec q))) e)
      [[n e] `[d p q]]
    ::
    =/  k2=key:rsa
      :-  [`@ux`143 `@ux`7]
      [~ `@ux`103 `@ux`11 `@ux`13]
    ::
    :: ex from http://doctrina.org/How-RSA-Works-With-Examples.html
    =/  k3=key:rsa
      =/  p
    12.131.072.439.211.271.897.323.671.531.612.440.428.472.427.633.701.410.
       925.634.549.312.301.964.373.042.085.619.324.197.365.322.416.866.541.
       017.057.361.365.214.171.711.713.797.974.299.334.871.062.829.803.541
      =/  q
    12.027.524.255.478.748.885.956.220.793.734.512.128.733.387.803.682.075.
       433.653.899.983.955.179.850.988.797.899.869.146.900.809.131.611.153.
       346.817.050.832.096.022.160.146.366.346.391.812.470.987.105.415.233
      =/  n  (mul p q)
      =/  e  65.537
      =/  d  (~(inv fo (elcm:rsa (dec p) (dec q))) e)
      [[`@ux`n `@ux`e] `[`@ux`d `@ux`p `@ux`q]]
    =/  m3  (swp 3 'attack at dawn')
    =/  c3
      35.052.111.338.673.026.690.212.423.937.053.328.511.880.760.811.579.981.
         620.642.802.346.685.810.623.109.850.235.943.049.080.973.386.241.113.
         784.040.794.704.193.978.215.378.499.765.413.083.646.438.784.740.952.
         306.932.534.945.195.080.183.861.574.225.226.218.879.827.232.453.912.
         820.596.886.440.377.536.082.465.681.750.074.417.459.151.485.407.445.
         862.511.023.472.235.560.823.053.497.791.518.928.820.272.257.787.786
    ::
    ?>  ?=(^ sek.k1)
    ;:  weld
      %-  expect-eq  !>
        [413 d.u.sek.k1]
      %-  expect-eq  !>
        [2.790 (en:rsa 65 k1)]
      %-  expect-eq  !>
        [65 (de:rsa 2.790 k1)]
      ::
      %-  expect-eq  !>
        [48 (en:rsa 9 k2)]
      %-  expect-eq  !>
        [9 (de:rsa 48 k2)]
      ::
      %-  expect-eq  !>
        [c3 (en:rsa m3 k3)]
      %-  expect-eq  !>
        [m3 (de:rsa c3 k3)]
    ==
  ::
  ++  test-rs256
    ::  ex from https://stackoverflow.com/a/41448118
    =/  k1=key:rsa
      :*  :-  n=0xc231.1fc5.fa31.d333.a409.bb4c.e95b.20d2.1cfc.e375.3871.7256.53a2.
            8425.af6d.e97d.f202.0b23.633f.458d.f12a.6362.7121.bff4.e23c.e578.
            7e07.7898.0578.61d1.ae60.ac2f
          e=0x1.0001
          ~
          d=0xd91.6719.eb10.3e24.768a.a386.8d2b.6bd0.a26b.dcec.9cc3.f86c.25ad.
            ce33.dfdc.fb1a.4d50.3e07.3d7f.f5fd.748e.43f8.df02.a60e.d730.5314.
            3e59.1e70.8df7.2c27.93e2.2b69
          p=0xf7ef.37e6.7fa6.685a.c178.8b01.cf38.da20.ca4b.de5d.8b01.a71b.d28c.
            65b4.09c3.6e4d
          q=0xc882.5760.3fb8.a5e2.5e9d.db55.3a73.b647.a3ec.a6e9.abc6.c440.dbc7.
            05f8.2ed4.da6b
      ==
    =/  inp1  0x302.0101
    =/  exp1
      0x575c.8a41.09ed.6ea2.a708.6338.d150.a5bb.8205.142e.7785.47b5.0cc6.0198.
        6807.0243.bf49.de7c.6039.0160.e392.faca.18f4.a05d.3a7a.88a4.de86.dd99.
        f030.eb4a.a755.d7ce
    =/  emsa1
      0x1.ffff.ffff.ffff.ffff.ffff.0030.3130.0d06.0960.8648.0165.0304.0201.0500.
          0420.9184.abd2.bb31.8731.d717.e972.0572.40ea.e26c.ca20.2a8d.35db.e9d2.
          176f.5268.86a0
    =/  kpem2=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
          'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
          'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
          'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
          'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
          'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
          '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
          'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
          'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
          'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
          '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
          'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
          'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
          'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
          'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
          'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
          'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
          'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
          'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
          '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
          'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
          '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
          'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
          'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
          'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  k2=key:rsa
      (need (ring:de:pem:pkcs1 kpem2))
    =/  inp2=cord  'hello\0a'
    =/  exp2=@ux
      0x2920.bba3.cb38.bca6.3768.6345.c95e.0717.81bf.6c61.4006.6070.a7b5.e609.
        f3b4.7f48.878b.d1f8.1882.8852.1db6.b6b5.a5fd.c23b.e764.b910.5a3f.fda9.
        9d3a.e8bd.060a.ac06.58f1.487a.b50d.dee2.e161.0b74.4d3b.e6e3.7004.c721.
        4f32.5c95.ce68.a008.b1e9.788b.375f.d389.0fa4.4012.c07a.8319.a183.02d5.
        e2b8.10df.6ff7.f64a.6b85.3c7d.de80.19cf.ab6d.e588.40cb.0ea4.c436.8d8b.
        47f7.cce6.b9bf.097d.3275.c128.147a.628d.2b7c.3912.3950.ef68.87b2.180d.
        ba01.3b05.285d.3dfd.09ee.2f38.3111.9e4c.92c6.bf66.a91b.5762.3cdf.f8b7.
        8281.81a2.8324.5330.43c1.035a.56c3.71b8.eb85.e660.c3a4.28b4.8af7.c16f.
        7d7d.87cc.036d.aeb2.c757.30f5.f194.c90d.6bb4.5e5c.f95f.8e28.0fbc.5fb4.
        b21a.e6fe
    =/  exp2b64=cord
      %+  rap  3
      :~  'KSC7o8s4vKY3aGNFyV4HF4G/bGFABmBwp7XmCfO0f0iHi9H4GIKIUh22trWl/cI752S5'
          'EFo//amdOui9BgqsBljxSHq1Dd7i4WELdE075uNwBMchTzJclc5ooAix6XiLN1/TiQ+k'
          'QBLAeoMZoYMC1eK4EN9v9/ZKa4U8fd6AGc+rbeWIQMsOpMQ2jYtH98zmub8JfTJ1wSgU'
          'emKNK3w5EjlQ72iHshgNugE7BShdPf0J7i84MRGeTJLGv2apG1diPN/4t4KBgaKDJFMw'
          'Q8EDWlbDcbjrheZgw6QotIr3wW99fYfMA22ussdXMPXxlMkNa7ReXPlfjigPvF+0shrm'
          '/g=='
      ==
    =/  sig=@ux  (~(sign rs256 k2) inp2)
    ;:  weld
      %-  expect-eq  !>
        [exp1 (~(sign rs256 k1) inp1)]
      %-  expect-eq  !>
        [& (~(verify rs256 k1) exp1 inp1)]
      %-  expect-eq  !>
        [emsa1 `@ux`(~(emsa rs256 k1) inp1)]
      %-  expect-eq  !>
        [& (~(verify rs256 k2) sig inp2)]
      %-  expect-eq  !>
        [exp2 sig]
      :: save kpem2 to private.pem
      :: echo "hello" | openssl dgst -sha256 -sign private.pem | base64
      %-  expect-eq  !>
        [exp2b64 (en:base64 (swp 3 sig))]
    ==
  ::
  ++  test-jwk
    :: rfc7638 section 3.1
    =/  n
      :~  '0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2'
          'aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCi'
          'FV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65Y'
          'GjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n'
          '91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_x'
          'BniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw'
      ==
    =/  jk=json
      :-  %o  %-  my  :~
        kty+s+'RSA'
        n+s+(rap 3 n)
        e+s+'AQAB'
      ==
    =/  k  (need (pass:de:jwk jk))
    ;:  weld
      %-  expect-eq  !>
        :-  jk
        (pass:en:jwk k)
      %-  expect-eq  !>
        :-  'NzbLsXh8uDCcd-6MNwXF4W_7noWXFZAfHkxZsRGC9Xs'
        (pass:thumb:jwk k)
    ==
  ::
  ++  test-jws
    ::  rfc7515 appendix 2
    =/  pt=@t
      %+  rap  3
      :~  '4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi'
          'YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG'
          'BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc'
      ==
    =/  qt=@t
      %+  rap  3
      :~  'uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa'
          'ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA'
          '-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc'
      ==
    =/  nt=@t
      %+  rap  3
      :~  'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx'
          'HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs'
          'D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH'
          'SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV'
          'MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8'
          'NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ'
       ==
    =/  dt=@t
      %+  rap  3
      :~  'Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I'
          'jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0'
          'BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn'
          '439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT'
          'CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh'
          'BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ'
      ==
    =/  jk=json
      :-  %o  %-  my  :~
        kty+s+'RSA'
        n+s+nt
        e+s+'AQAB'
        d+s+dt
        p+s+pt
        q+s+qt
      ==
    =/  k=key:rsa  (need (ring:de:jwk jk))
    =/  hed=json  o+(my alg+s+'RS256' ~)
    =/  hedt=@t  'eyJhbGciOiJSUzI1NiJ9'
    =/  lod=json
      :-  %o  %-  my  :~
        iss+s+'joe'
        exp+n+'1300819380'
        ['http://example.com/is_root' %b &]
      ==
    =/  lodt=@t
      %+  rap  3
      :~  'eyJpc3MiOiJqb2UiLCJleHAiOjEzMDA4MTkzODAsImh0dHA'
          '6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ'
      ==
    ::  rfc example includes whitespace in json serialization
    =/  lodt-ws=@t
      %+  rap  3
      :~  'eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQo'
          'gImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ'
      ==
    =/  inp-ws=@t
      (rap 3 [hedt '.' lodt-ws ~])
    =/  exp-ws=@t
      %+  rap  3
      :~  'cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7'
          'AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4'
          'BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K'
          '0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqv'
          'hJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrB'
          'p0igcN_IoypGlUPQGe77Rw'
      ==
    =/  lod-order=(list @t)  ['iss' 'exp' 'http://example.com/is_root' ~]
    ?>  ?=(^ sek.k)
    ;:  weld
      %-  expect-eq  !>
        [jk (ring:en:jwk k)]
      %-  expect-eq  !>
        [n.pub.k `@ux`(mul p.u.sek.k q.u.sek.k)]
      %-  expect-eq  !>
        :-  d.u.sek.k
        `@ux`(~(inv fo (elcm:rsa (dec p.u.sek.k) (dec q.u.sek.k))) e.pub.k)
      %-  expect-eq  !>
        :-  hedt
        (en-base64url (crip (en-json-sort aor hed)))
      %-  expect-eq  !>
        :-  lodt
        (en-base64url (crip (en-json-sort (eor lte lod-order) lod)))
      %-  expect-eq  !>
        :-  exp-ws
        (en-base64url (swp 3 (~(sign rs256 k) inp-ws)))
    ==
  ::
  ++  test-csr
    =/  kpem=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
          'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
          'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
          'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
          'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
          'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
          '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
          'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
          'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
          'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
          '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
          'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
          'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
          'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
          'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
          'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
          'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
          'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
          'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
          '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
          'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
          '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
          'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
          'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
          'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  k=key:rsa
      (need (ring:de:pem:pkcs1 kpem))
    ::  generated with openssl, certbot style
    =/  csr-pem=wain
      :~  '-----BEGIN CERTIFICATE REQUEST-----'
          'MIICezCCAWMCAQAwADCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBANoy'
          'afHYACsuXEswxOA/mlGymbEKDISFiNiBZRPgDtBggTIoOascdiCAD1SHEouX4zi7'
          'Ux57iGWMPrNOqnm/ECCkb5GWiMJwNy8+VFK8YIferHs3VwgkhuxZ3SBbJHE/sxtN'
          'CS/nz0XZT47dVYNm95J3qOWgCjCD8jk83GOfdFE3PpbkNx4YEojRd5+WEIduHd5E'
          'tmDlKEl0Nd/mEZY8TXwYF21fgu/4DvJhcaZGn4ZoJCBjXlpak/aACEWvoDsdd7jw'
          'IPiW//JT1fWxmhahr8bwTKd8VAEpc2B4g57XAeTIjj0VugaCBiOagxUmEWQZMvXK'
          'xO1gGIKrT7YNtc78cBcCAwEAAaA2MDQGCSqGSIb3DQEJDjEnMCUwIwYDVR0RBBww'
          'GoINem9kLnVyYml0Lm9yZ4IJem9kLnVyYml0MA0GCSqGSIb3DQEBCwUAA4IBAQCu'
          'HdUqIlW8w7G7l3YxXfb0fn1HxD7zHf3QpNqDYZuDq958OhJNXYJ9EjiHBBEW5ySg'
          'e7vyaaWh6UcVIu/RYFGbyVupNVbp4aS4U1LEEiqMQ6vQQnlSt7hVMi04bhf6X6jd'
          'kkTJIB5OlmNh5z/mjvlIOyOCeitK5IMrStsUPE5F8OynMuzmBKq318LiwImO41Fu'
          'S1M6rgPnvoZeqShLxnJduMcu7awHQ0tn2FmjwBpU713/tmNjtCLYYoBxsHM2ptxd'
          'G3zcMSCDZ/CLXWoktkULdWNDED8meCKWQJxYuHjxY4JPIzIVdVN50xRcZEV7Z80l'
          'bPcFaE8op3e2hIxzqi1t'
          '-----END CERTIFICATE REQUEST-----'
      ==
    =/  hot1  /org/urbit/zod
    =/  hot2  /urbit/zod
    %-  expect-eq  !>
      :-  csr-pem
      (en:pem:pkcs10 k [hot1 hot2 ~])
  --
--

