/+  primitive-rsa, *pkcs
=*  rsa  primitive-rsa
|%
::  +en-base64url: url-safe base64 encoding, without padding
::
++  en-base64url
  ~(en base64:mimes:html | &)
::  +de-base64url: url-safe base64 decoding, without padding
::
++  de-base64url
  ~(de base64:mimes:html | &)
::  |octn: encode/decode unsigned atoms as big-endian octet stream
::
++  octn
  |%
  ++  en  |=(a=@u `octs`[(met 3 a) (swp 3 a)])
  ++  de  |=(a=octs `@u`(rev 3 p.a q.a))
  --
::  +eor: explicit sort order comparator
::
::    Lookup :a and :b in :lit, and pass their indices to :com.
::
++  eor
  |=  [com=$-([@ @] ?) lit=(list)]
  |=  [a=* b=*]
  ^-  ?
  (fall (bind (both (find ~[a] lit) (find ~[b] lit)) com) |)
::  +en-json-sort: json encoding with sorted object keys
::
::    XX move %zuse with sorting optional?
::
++  en-json-sort                                 ::  XX rename
  |^  |=([sor=$-(^ ?) val=json] (apex val sor ""))
  ::                                                  ::  ++apex:en-json:html
  ++  apex
    |=  [val=json sor=$-(^ ?) rez=tape]
    ^-  tape
    ?~  val  (weld "null" rez)
    ?-    -.val
        %a
      :-  '['
      =.  rez  [']' rez]
      !.
      ?~  p.val  rez
      |-
      ?~  t.p.val  ^$(val i.p.val)
      ^$(val i.p.val, rez [',' $(p.val t.p.val)])
    ::
        %b  (weld ?:(p.val "true" "false") rez)
        %n  (weld (trip p.val) rez)
        %s
      :-  '"'
      =.  rez  ['"' rez]
      =+  viz=(trip p.val)
      !.
      |-  ^-  tape
      ?~  viz  rez
      =+  hed=(jesc i.viz)
      ?:  ?=([@ ~] hed)
        [i.hed $(viz t.viz)]
      (weld hed $(viz t.viz))
    ::
        %o
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
  ::
  ++  jesc
    =+  utf=|=(a=@ ['\\' 'u' ((x-co 4):co a)])
    |=  a=@  ^-  tape
    ?+  a  ?:(&((gth a 0x1f) !=(a 0x7f)) [a ~] (utf a))
      %10  "\\n"
      %34  "\\\""
      %92  "\\\\"
    ==
  --
::  %/lib/jose
::
::  |jwk: json representations of cryptographic keys (rfc7517)
::
::    Url-safe base64 encoding of key parameters in big-endian byte order.
::    RSA-only for now
::
++  jwk
  |%
  ::  |en:jwk: encoding of json cryptographic keys
  ::
  ++  en
    =>  |%
        ::  +numb:en:jwk: base64-url encode big-endian number
        ::
        ++  numb  (corl en-base64url en:octn)
        --
    |%
    ::  +pass:en:jwk: json encode public key
    ::
    ++  pass
      |=  k=key:rsa
      ^-  json
      [%o (my kty+s+'RSA' n+s+(numb n.pub.k) e+s+(numb e.pub.k) ~)]
    ::  +ring:en:jwk: json encode private key
    ::
    ++  ring
      |=  k=key:rsa
      ^-  json
      ~|  %rsa-need-ring
      ?>  ?=(^ sek.k)
      :-  %o  %-  my  :~
        kty+s+'RSA'
        n+s+(numb n.pub.k)
        e+s+(numb e.pub.k)
        d+s+(numb d.u.sek.k)
        p+s+(numb p.u.sek.k)
        q+s+(numb q.u.sek.k)
      ==
    --
  ::  |de:jwk: decoding of json cryptographic keys
  ::
  ++  de
    =,  dejs-soft:format
    =>  |%
        ::  +numb:de:jwk: parse base64-url big-endian number
        ::
        ++  numb  (cu (cork de-base64url (lift de:octn)) so)
        --
    |%
    ::  +pass:de:jwk: decode json public key
    ::
    ++  pass
      %+  ci
        =/  a  (unit @ux)
        |=  [kty=@t n=a e=a]
        ^-  (unit key:rsa)
        =/  pub  (both n e)
        ?~(pub ~ `[u.pub ~])
      (ot kty+(su (jest 'RSA')) n+numb e+numb ~)
    ::  +ring:de:jwk: decode json private key
    ::
    ++  ring
      %+  ci
        =/  a  (unit @ux)
        |=  [kty=@t n=a e=a d=a p=a q=a]
        ^-  (unit key:rsa)
        =/  pub  (both n e)
        =/  sek  :(both d p q)
        ?:(|(?=(~ pub) ?=(~ sek)) ~ `[u.pub sek])
      (ot kty+(su (jest 'RSA')) n+numb e+numb d+numb p+numb q+numb ~)
    --
  ::  |thumb:jwk: "thumbprint" json-encoded key (rfc7638)
  ::
  ++  thumb
    |%
    ::  +pass:thumb:jwk: thumbprint json-encoded public key
    ::
    ++  pass
      |=  k=key:rsa
      (en-base64url 32 (shax (crip (en-json-sort aor (pass:en k)))))
    ::  +ring:thumb:jwk: thumbprint json-encoded private key
    ::
    ++  ring  !!
    --
  --
::  |jws: json web signatures (rfc7515)
::
::    Note: flattened signature form only.
::
++  jws
  |%
  ::  +sign:jws: sign json value
  ::
  ++  sign
    |=  [k=key:rsa pro=json lod=json]
    |^  ^-  json
        =.  pro  header
        =/  protect=cord  (encode pro)
        =/  payload=cord  (encode lod)
        :-  %o  %-  my  :~
          protected+s+protect
          payload+s+payload
          signature+s+(sign protect payload)
        ==
    ::  +header:sign:jws: set signature algorithm in header
    ::
    ++  header
      ?>  ?=([%o *] pro)
      ^-  json
      [%o (~(put by p.pro) %alg s+'RS256')]
    ::  +encode:sign:jws: encode json for signing
    ::
    ::    Alphabetically sort object keys, url-safe base64 encode
    ::    the serialized json.
    ::
    ++  encode
      |=  jon=json
      %-  en-base64url
      %-  as-octt:mimes:html
      (en-json-sort aor jon)
    ::  +sign:sign:jws: compute signature
    ::
    ::    Url-safe base64 encode in big-endian byte order.
    ::
    ++  sign
      |=  [protect=cord payload=cord]
      =/  msg=@t   (rap 3 ~[protect '.' payload])
      =/  sig=@ud  (~(sign rs256 k) (met 3 msg) msg)
      =/  len=@ud  (met 3 n.pub.k)
      (en-base64url len (rev 3 len sig))
    --
  ::  +verify:jws: verify signature
  ::
  ++  verify  !!
  --
--
