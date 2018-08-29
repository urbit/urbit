/-  asn1
/+  base64, der, primitive-rsa, *pkcs, tester
=,  eyre
=*  rsa  primitive-rsa
::
::::  %zuse additions
::
|%
::  +en-base64url: url-safe base64 encoding, without padding
::
++  en-base64url
  ~(en base64 | &)
::  +de-base64url: url-safe base64 decoding, without padding
::
++  de-base64url
  ~(de base64 | &)
::  |octn: encode/decode unsigned atoms as big-endian octet stream
::
++  octn
  |%
  ++  en  |=(a=@u `octs`[(met 3 a) (swp 3 a)])
  ++  de  |=(a=octs `@u`(rev 3 p.a q.a))
  --
::  +en-json-sort: json encoding with sorted object keys
::
::    to be included in %zuse, with sorting optional?
::
++  en-json-sort                                 ::  XX rename
  |^  |=([sor=$-(^ ?) val=json] (apex val sor ""))
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
::
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
      =/  sig=@ud  (~(sign rs256 k) (rap 3 ~[protect '.' payload]))
      =/  len=@ud  (met 3 n.pub.k)
      (en-base64url len (rev 3 len sig))
    --
  ::  +verify:jws: verify signature
  ::
  ++  verify  !!
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
:: |grab: acme api response json reparsers
::
++  grab
  =,  dejs:format
  |%
  :: +json-purl: parse url
  ::
  ++  json-purl  (su auri:de-purl:html)
  :: +directory: parse ACME service directory
  ::
  ++  directory
    %-  ot
    :~  ['newAccount' json-purl]
        ['newNonce' json-purl]
        ['newOrder' json-purl]
        ['revokeCert' json-purl]
        ['keyChange' json-purl]
    ==
  :: +acct: parse ACME service account
  ::
  ++  acct
    %-  ot
    :~  ['id' no]
        ['createdAt' so] :: XX (su iso-8601)
        ['status' so]
        :: ignore key, contact, initialIp
    ==
  :: +order: parse certificate order
  ::
  ++  order
    %-  ot
    :~  ['authorizations' (ar json-purl)]
        ['finalize' json-purl]
        ['expires' so] :: XX (su iso-8601)
        ['status' so]
    ==
  :: +finalizing-order: parse order in a finalizing state
  ::
  ::   XX remove once +order has optional keys
  ::
  ++  finalizing-order
    %-  ot
    :~  ['expires' so] :: XX (su iso-8601)
        ['status' so]
    ==
  :: +final-order: parse order in a finalized state
  ::
  ::   XX remove once +order has optional keys
  ::
  ++  final-order
    %-  ot
    :~  ['expires' so] :: XX (su iso-8601)
        ['status' so]
        ['certificate' json-purl]
    ==
  :: +auth: parse authorization
  ++  auth
    =>  |%
        :: +iden: parse dns identifier to +turf
        ::
        ++  iden
          |=  [typ=@t hot=host]
          ?>(&(?=(%dns typ) ?=([%& *] hot)) p.hot)
        :: +trial: transform parsed domain validation challenge
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
  :: +challenge: parse domain validation challenge
  ::
  ++  challenge
    %-  ot
    :~  ['type' so]
        ['status' so]
        ['url' json-purl]
        ['token' so]
    ==
  :: +error: parse ACME service error response
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
:: +move: output effect
::
+=  move  [bone card]
:: +card: output effect payload
::
+=  card
  $%  [%hiss wire [~ ~] %httr %hiss hiss:eyre]
      [%wait wire @da]
      [%well wire path (unit mime)]
      [%rule wire %cert (unit [wain wain])]
  ==
:: +nonce-next: next effect to emit upon receiving nonce
::
+=  nonce-next
  $?  %register
      %new-order
      %finalize-order
      %finalize-trial
  ==
:: +turf: a domain, TLD first
::
+=  turf  (list @t)
:: +acct: an ACME service account
::
+=  acct
  $:  :: key: account keypair
      ::
      key=key:rsa
      :: reg: account registration
      ::
      reg=(unit [wen=@t kid=@t])   :: XX wen=@da
  ==
:: +config: finalized configuration
::
+=  config
  $:  :: dom: domains
      ::
      dom=(set turf)
      :: key: certificate keypair
      ::
      key=key:rsa
      :: cer: signed certificate
      ::
      cer=wain
      :: exp: expiration date
      ::
      exp=@da
      :: dor: source ACME service order URL
      ::
      dor=purl
  ==
:: +trial: domain validation challenge
::
+=  trial
  $%  :: %http only for now
      $:  %http
          :: ego: ACME service challenge url
          ::
          ego=purl
          :: tok: challenge token
          ::
          tok=@t
          :: sas: challenge status
          ::
          sas=?(%recv %pend %auth)
  ==  ==
:: +auth: domain authorization
::
+=  auth
  $:  :: ego: ACME service authorization url
      ::
      ego=purl
      :: dom: domain under authorization
      ::
      dom=turf
      :: cal: domain validation challenge
      ::
      cal=trial
  ==
:: +order-auth: domain authorization state for order processing
::
+=  order-auth
  $:  :: pending: remote authorization urls
      ::
      pending=(list purl)
      :: active: authorization in progress
      ::
      active=(unit [idx=@ auth])
      :: done: finalized authorizations (XX or failed?)
      ::
      done=(list auth)
  ==
:: +order: ACME certificate order
::
+=  order
  $:  :: dom: domains
      ::
      dom=(set turf)
      :: sas: order state
      ::
      sas=$@(%wake [%rest wen=@da])
      :: exp: expiration date
      ::
      ::   XX @da once ISO-8601 parser
      ::
      exp=@t
      :: ego: ACME service order url
      ::
      ego=purl
      :: fin: ACME service order finalization url
      ::
      fin=purl
      :: key: certificate keypair
      ::
      key=key:rsa
      :: csr: DER-encoded PKCS10 certificate signing request
      ::
      csr=@ux
      :: aut: authorizations required by this order
      ::
      aut=order-auth
  ==
:: +history: archive of past ACME service interactions
::
+=  history
  $:  :: act: list of revoked account keypairs
      ::
      act=(list acct)
      :: fig: list of expired configurations
      ::
      fig=(list config)
      :: fal: list of failed order attempts
      ::
      fal=(list order)
  ==
:: +directory: ACME v2 service directory
::
+=  directory
  $:  :: reg: registration url (newAccount)
      ::
      reg=purl
      :: non: nonce creation url (newNonce)
      ::
      non=purl
      :: der: order creation url (newOrder)
      ::
      der=purl
      :: rev: certificate revocation url (revokeCert)
      ::
      rev=purl
      :: rek: account key revocation url (keyChange)
      ::
      rek=purl
  ==
:: +acme: complete app state
::
+=  acme
  $:  :: bas: ACME service root url
      ::
      bas=purl
      :: dir: ACME service directory
      ::
      dir=directory
      :: act: ACME service account
      ::
      act=acct
      :: liv: active, live configuration
      ::
      liv=(unit config)
      :: hit: ACME account history
      ::
      hit=history
      :: nonces: list of unused nonces
      ::
      nonces=(list @t)
      :: rod: active, in-progress order
      ::
      rod=(unit order)
      :: pen: pending domains for next order
      ::
      pen=(unit (set turf))
      :: cey: certificate key XX move?
      ::
      cey=key:rsa
  ==
--
::
::::  acme app
::
:: mov: list of outgoing moves for the current transaction
::
=|  mov=(list move)
::
|_  [bow=bowl:gall acme]
:: +this: self
::
::   XX Should be a +* core alias, see urbit/arvo#712
::
++  this  .
:: +emit: emit a move
::
++  emit
  |=  car=card
  ~&  [%emit car]
  this(mov [[ost.bow car] mov])
:: +abet: finalize transaction
::
++  abet
  ^-  (quip move _this)
  [(flop mov) this(mov ~)]
:: +request: generic http request
::
++  request
  |=  [wir=wire req=hiss]
  ^-  card
  [%hiss wir [~ ~] %httr %hiss req]
:: +signed-request: JWS JSON POST
::
++  signed-request
  |=  [url=purl non=@t bod=json]
  ^-  hiss
  :^  url  %post
    (my content-type+['application/jose+json' ~] ~)
  :-  ~
  ^-  octs
  =;  pro=json
    (as-octt:mimes:html (en-json:html (sign:jws key.act pro bod)))
  :-  %o  %-  my  :~
    nonce+s+non
    url+s+(crip (en-purl:html url))
    ?^  reg.act
      kid+s+kid.u.reg.act
    jwk+(pass:en:jwk key.act)
  ==
:: +bad-nonce: check if an http response is a badNonce error
::
++  bad-nonce
  |=  rep=httr
  ^-  ?
  :: XX always 400?
  ?.  =(400 p.rep)  |
  ?~  r.rep  |
  =/  jon=(unit json)  (de-json:html q.u.r.rep)
  ?~  jon  |
  :: XX unit parser, types
  =('urn:ietf:params:acme:error:badNonce' -:(error:grab u.jon))
:: |effect: send moves to advance
::
++  effect
  |%
  :: +directory: get ACME service directory
  ::
  ++  directory
    ^+  this
    (emit (request /acme/directory/(scot %p our.bow) bas %get ~ ~)) :: XX now?
  :: +nonce: get a new nonce for the next request
  ::
  ++  nonce
    |=  nex=@tas
    ~|  [%bad-nonce-next nex]
    ?>  ?=(nonce-next nex)
    ^+  this
    :: XX now?
    (emit (request /acme/nonce/next/[nex] non.dir %get ~ ~))
  :: +register: create ACME service account
  ::
  ::   Note: accepts services ToS.
  ::
  ++  register
    ^+  this
    ?~  nonces
      (nonce %register)
    %-  emit(nonces t.nonces, reg.act ~)
    %+  request
      /acme/register/(scot %p our.bow) :: XX now?
    %^  signed-request  reg.dir  i.nonces
    [%o (my [['termsOfServiceAgreed' b+&] ~])]
  :: XX rekey
  ::
  :: +new-order: create a new certificate order
  ::
  ++  new-order
    ^+  this
    ~|  %new-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=([~ ^] pen)  ~|(%no-domains !!)
    ?~  nonces
      (nonce %new-order)
    %-  emit(nonces t.nonces)
    %+  request
      /acme/new-order/(scot %da now.bow)
    %^  signed-request  der.dir  i.nonces
    :-  %o  %-  my  :~
      :-  %identifiers
      :-  %a
      %+  turn
        ~(tap in `(set turf)`u.pen)
      |=(a=turf [%o (my type+s+'dns' value+s+(join '.' a) ~)])
    ==
  :: +finalize-order: finalize completed order
  ::
  ++  finalize-order
    ^+  this
    ~|  %finalize-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(~ pending.aut.u.rod)  ~|(%pending-authz !!)
    ?.  ?=(~ active.aut.u.rod)   ~|(%active-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    ?~  nonces
      (nonce %finalize-order)
    %-  emit(nonces t.nonces)
    %+  request
      /acme/finalize-order/(scot %da now.bow)
    %^  signed-request  fin.u.rod  i.nonces
    [%o (my csr+s+(en-base64url (met 3 csr.u.rod) `@`csr.u.rod) ~)]
  :: +check-order: check completed order for certificate availability
  ::
  ++  check-order
    ^+  this
    ~|  %check-order-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(~ pending.aut.u.rod)  ~|(%pending-authz !!)
    ?.  ?=(~ active.aut.u.rod)   ~|(%active-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    (emit (request /acme/check-order/(scot %da now.bow) ego.u.rod %get ~ ~))
  :: +certificate: download PEM-encoded certificate
  ::
  ++  certificate
    |=  url=purl
    ^+  this
    ~|  %certificate-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    =/  hed  (my accept+['applicate/x-pem-file' ~] ~)
    (emit (request /acme/certificate/(scot %da now.bow) url %get hed ~))
  :: +install: tell %eyre about our certificate
  ::
  ++  install
    ^+  this
    ~|  %install-effect-fail
    ?>  ?=(^ liv)
    =/  key=wain  (ring:en:pem:pkcs8 key.u.liv)
    (emit %rule /install %cert `[key `wain`cer.u.liv])
  :: +get-authz: get next ACME service domain authorization object
  ::
  ++  get-authz
    ^+  this
    ~|  %get-authz-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ pending.aut.u.rod)  ~|(%no-pending-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    %-  emit
    (request /acme/get-authz/(scot %da now.bow) i.pending.aut.u.rod %get ~ ~)
  :: XX check/finalize-authz ??
  ::
  :: +save-trial: save ACME domain validation challenge to /.well-known/
  ::
  ++  save-trial
    ^+  this
    ~|  %save-trial-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ active.aut.u.rod)  ~|(%no-active-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    =*  aut  u.active.aut.u.rod
    %-  emit
    :^    %well
        :: XX idx in wire?
        /acme/save-trial/(scot %da now.bow)
      /acme-challenge/[tok.cal.aut]
    :+  ~
      /text/plain
    %-  as-octs:mimes:html
    (rap 3 [tok.cal.aut '.' (pass:thumb:jwk key.act) ~])
  :: +test-trial: confirm that ACME domain validation challenge is available
  ::
  ++  test-trial
    ^+  this
    ~|  %test-trial-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ active.aut.u.rod)  ~|(%no-active-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    =*  aut  u.active.aut.u.rod
    =/  pat=path  /'.well-known'/acme-challenge/[tok.cal.aut]
    :: note: requires port 80, just as the ACME service will
    =/  url=purl  [[sec=| por=~ hos=[%& dom.aut]] [ext=~ pat] hed=~]
    :: =/  url=purl  [[sec=| por=`8.081 hos=[%& /localhost]] [ext=~ pat] hed=~]
    :: XX idx in wire?
    (emit (request /acme/test-trial/(scot %da now.bow) url %get ~ ~))
  :: +finalize-trial: notify ACME service that challenge is ready
  ::
  ++  finalize-trial
    ^+  this
    ~|  %finalize-trial-effect-fail
    ?.  ?=(^ reg.act)  ~|(%no-account !!)
    ?.  ?=(^ rod)      ~|(%no-active-order !!)
    ?.  ?=(^ active.aut.u.rod)  ~|(%no-active-authz !!)
    :: XX revisit wrt rate limits
    ?>  ?=(%wake sas.u.rod)
    =*  aut  u.active.aut.u.rod
    ?~  nonces
      (nonce %finalize-trial)
    %-  emit(nonces t.nonces)
    %+  request
      :: XX idx in wire?
      /acme/finalize-trial/(scot %da now.bow)
    :: empty object included for signature
    (signed-request ego.cal.aut i.nonces [%o ~])
  ::  XX delete-trial?
  ::
  :: +retry: retry effect after timeout
  ::
  ++  retry
    |=  [wir=wire wen=@da]
    :: XX validate wire and date
    (emit %wait [%acme wir] wen)
  --
:: |event: accept event, emit next effect(s)
::
::   XX should these next effects be triggered at call sites instead?
::
++  event
  |%
  :: +directory: accept ACME service directory, trigger registration
  ::
  ++  directory
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      :: XX never happened yet, wat do?
      ~&  [%directory-fail rep]
      this
    =.  dir  (directory:grab (need (de-json:html q:(need r.rep))))
    ?~(reg.act register:effect this)
  :: +nonce: accept new nonce and trigger next effect
  ::
  ::   Nonce has already been saved in +sigh-httr. The next effect
  ::   is specified in the wire.
  ::
  ++  nonce
    |=  [wir=wire rep=httr]
    ^+  this
    ~|  [%unrecognized-nonce-wire wir]
    ?>  &(?=(^ wir) ?=([%next ^] t.wir))
    =*  nex  i.t.t.wir
    ~|  [%unknown-nonce-next nex]
    ?>  ?=(nonce-next nex)
    ?.  =(204 p.rep)
      :: cttp i/o timeout, always retry
      :: XX set timer to backoff?
      ?:  =(504 p.rep)  (nonce:effect nex)
      :: XX never happened yet, retry nonce anyway?
      ::
      ~&([%nonce-fail wir rep] this)
    ?-  nex
      %register        register:effect
      %new-order       new-order:effect
      %finalize-order  finalize-order:effect
      %finalize-trial  finalize-trial:effect
    ==
  :: +register: accept ACME service registration
  ::
  ++  register
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(201 p.rep)
      ::XX 204?
      ?:  (bad-nonce rep)
        (nonce:effect %register)
      :: XX retry immediately or backoff?
      ~&  [%register-fail wir rep]
      this
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    =/  wen=@t              :: XX @da
      ?~  r.rep
        (scot %da now.bow)
      =/  bod=[id=@t wen=@t sas=@t]
        (acct:grab (need (de-json:html q.u.r.rep)))
      ?>  ?=(%valid sas.bod)
      wen.bod
    =.  reg.act  `[wen loc]
    ?~(pen this new-order:effect)
  :: XX rekey
  ::
  ::  +new-order: order created, begin processing authorizations
  ::
  ++  new-order
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(201 p.rep)
      ?:  (bad-nonce rep)
        (nonce:effect %new-order)
      :: XX retry immediately or backoff?
      :: XX possible 204?
      ~&  [%new-order-fail wir rep]
      this
    :: XX delete order if not?
    ?>  ?=(^ pen)
    =/  loc=@t
      q:(head (skim q.rep |=((pair @t @t) ?=(%location p))))
    =/  ego=purl  (need (de-purl:html loc))
    :: XX add parser output types
    :: XX parse identifiers, confirm equal to pending domains
    :: XX check status
    =/  bod=[aut=(list purl) fin=purl exp=@t sas=@t]
      (order:grab (need (de-json:html q:(need r.rep))))
    :: XX maybe generate key here?
    =/  csr=@ux  +:(en:der:pkcs10 cey ~(tap in u.pen))
    =/  dor=order
      [dom=u.pen sas=%wake exp.bod ego fin.bod cey csr [aut.bod ~ ~]]
    get-authz:effect(rod `dor, pen ~)
  :: +finalize-order: order finalized, poll for certificate
  ::
  ++  finalize-order
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      ?:  (bad-nonce rep)
        (nonce:effect %finalize-order)
      ~&  [%finalize-order-fail wir rep]
      ?>  ?=(^ rod)
      :: XX get the failure reason
      this(rod ~, fal.hit [u.rod fal.hit])
    ?>  ?=(^ rod)
    :: XX rep body missing authorizations, need flexible/separate parser
    :: XX finalizing-order
    :: =/  bod=[aut=(list purl) fin=purl exp=@t sas=@t]
    ::   (order:grab (need (de-json:html q:(need r.rep))))
    :: XX check status? (i don't think failures get here)
    check-order:effect
  ::  +check-order: check if certificate is ready for finalized order
  ::
  ++  check-order
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      :: XX retry immediately? backoff?
      ~&  [%check-order-fail wir rep]
      this
    ?>  ?=(^ rod)
    =/  raw=json
      (need (de-json:html q:(need r.rep)))
    =/  bod=[exp=@t sas=@t]
      (finalizing-order:grab raw)
    ?+  sas.bod
      ~&  [%check-order-status-unknown sas.bod]
      this
    ::
        %invalid
      ~&  [%check-order-fail %invalid wir rep]
      :: XX check authz for debug info
      :: XX send notification somehow?
      :: XX start over with new order?
      this
    ::
        %pending
      check-order:effect
    ::
        %processing
      check-order:effect
    ::
        %valid
      :: XX json reparser unit
      =/  bod=[exp=@t sas=@t cer=purl]
        (final-order:grab raw)
      :: XX update order state
      :: XX =< delete-trial
      (certificate:effect cer.bod)
    ==
  ::
  :: +certificate: accept PEM-encoded certificate
  ::
  ++  certificate
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      :: XX retry immediately? backoff?
      ~&  [%certificate-fail wir rep]
      this
    ?>  ?=(^ rod)
    =/  cer=wain  (to-wain:format q:(need r.rep))
    =/  fig=config
      :: XX expiration date
      [dom.u.rod key.u.rod cer (add now.bow ~d90) ego.u.rod]
    =?  fig.hit  ?=(^ liv)  [u.liv fig.hit]
    :: XX set renewal timer
    install:effect(liv `fig, rod ~)
  :: +get-authz: accept ACME service authorization object
  ::
  ++  get-authz
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      :: XX retry immediately? backoff?
      ~&  [%get-authz-fail wir rep]
      this
    ?>  ?=(^ rod)
    ?>  ?=(^ pending.aut.u.rod)
    :: XX parser types
    =/  bod=[dom=turf sas=@t exp=@t cal=[typ=@t sas=@t ego=purl tok=@t]]
      (auth:grab (need (de-json:html q:(need r.rep))))
    =/  cal=trial
       :: XX parse token to verify url-safe base64?
      [%http ego.cal.bod tok.cal.bod %recv]
    :: XX check that URLs are the same
   =/  tau=auth  [i.pending.aut.u.rod dom.bod cal]
    :: XX get idx from wire instead?
    =/  idx=@ud  +((lent done.aut.u.rod))
    =/  rod-aut=order-auth
      %=  aut.u.rod
        pending  t.pending.aut.u.rod
        active   `[idx tau]
      ==
    =<  test-trial:effect
    save-trial:effect(aut.u.rod rod-aut)
  :: XX check/finalize-authz ??
  ::
  :: +test-trial: accept response from challenge test
  ::
  ::   Note that +save-trail:effect has no corresponding event.
  ::
  ++  test-trial
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      :: XX count retries, backoff
      ~&  [%test-trial-fail wir rep]
      (retry:effect /test-trial (add now.bow ~m10))
    ?>  ?=(^ rod)
    ?>  ?=(^ active.aut.u.rod)
    :: XX check content type and response body
    finalize-trial:effect
  :: +finalize-trial:
  ::
  ++  finalize-trial
    |=  [wir=wire rep=httr]
    ^+  this
    ?.  =(200 p.rep)
      ?:  (bad-nonce rep)
        (nonce:effect %finalize-trial)
      :: XX retry? or cancel order?
      :: XX 204 assume pending?
      ~&  [%finalize-trial-fail wir rep]
      :: XX handle "challenge is not pending"
      this
    ?>  ?=(^ rod)
    ?>  ?=(^ active.aut.u.rod)
    =*  aut  u.active.aut.u.rod
    =/  bod=[typ=@t sas=@t url=purl tok=@t]
      (challenge:grab (need (de-json:html q:(need r.rep))))
    :: XX check for other possible values in 200 response
    :: note: may have already been validated
    ?>  ?=(?(%pending %valid) sas.bod)
    =/  rod-aut=order-auth
      aut.u.rod(active ~, done [+.aut(sas.cal %pend) done.aut.u.rod])
    ?~  pending.aut.u.rod
      finalize-order:effect(aut.u.rod rod-aut)
    get-authz:effect(aut.u.rod rod-aut)
  ::  XX delete-trial?
  ::
  :: +retry: retry effect after timeout
  ::
  ++  retry
    |=  wir=wire
    ^+  this
    ?+  wir
        ~&(unknown-retry+wir this)
      :: XX do the needful
      [%directory ~]  directory:effect
      [%test-trial ~]  test-trial:effect
    ==
  --
:: +sigh-tang: handle http request failure
::
++  sigh-tang
  |=  [wir=wire saw=tang]
  ^-  (quip move _this)
  ~&  [%sigh-tang wir]
  :: XX take evasive action
  [((slog saw) ~) this]
:: +sigh-recoverable-error: handle http rate-limit response
::
++  sigh-recoverable-error
  |=  [wir=wire %429 %rate-limit lim=(unit @da)]
  ^-  (quip move _this)
  ~&  [%sigh-recoverable wir lim]
  :: XX retry
  [~ this]
:: +sigh-httr: accept http response
::
++  sigh-httr
  |=  [wir=wire rep=httr]
  ^-  (quip move _this)
  ~&  [wir rep]
  ?>  ?=([%acme ^] wir)
  :: add nonce to pool, if present
  =/  nonhed  (skim q.rep |=((pair @t @t) ?=(%replay-nonce p)))
  =?  nonces  ?=(^ nonhed)  [q.i.nonhed nonces]
  =<  abet
  ~|  [%sigh-fail wir rep]
  %.  [t.wir rep]
  ?+  i.t.wir
      ~&([%unknown-wire i.t.wir] !!)
    %directory       directory:event
    %nonce           nonce:event
    %register        register:event
    :: XX rekey
    %new-order       new-order:event
    %finalize-order  finalize-order:event
    %check-order     check-order:event
    %certificate     certificate:event
    %get-authz       get-authz:event
    :: XX check/finalize-authz ??
    %test-trial      test-trial:event
    %finalize-trial  finalize-trial:event
    ::  XX delete-trial?
  ==
:: +wake: timer wakeup event
::
++  wake
  |=  [wir=wire ~]
  ^-  (quip move _this)
  ~&  [%wake wir]
  ?>  ?=([%acme *] wir)
  abet:(retry:event t.wir)
:: +poke-acme-order: create new order for a set of domains
::
++  poke-acme-order
  |=  a=(set turf)
  ~&  [%poke-acme a]
  abet:(add-order a)
:: +poke-noun: for debugging
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  =<  abet
  ?+  a
      ~&(+<+.this this)
    %dbug  ~&  [%private (ring:en:pem:pkcs1 key.act)]
           ~&  [%public (pass:en:pem:pkcs1 key.act)]
           this
    %init   init
    %reg    register:effect
    %order  new-order:effect
    %auth   get-authz:effect
    %trial  test-trial:effect
    %final  finalize-order:effect
    %poll   check-order:effect
    %our    (add-order (sy /org/urbit/(crip +:(scow %p our.bow)) ~))
    %rule   install:effect
    %fake   fake
    %none   none
    %test   test
  ==
++  none
  ^+  this
  (emit %rule /uninstall %cert ~)
++  fake
  ^+  this
  =/  key=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEpAIBAAKCAQEAisQPzzmGWNZSNNAwY59XrqK/bU0NKNZS2ETOiJeSpzPAHYl+'
        'c39V96/QUR0tra2zQI4QD6kpMYX/7R5nwuvsA4o7ypfYupNrlzLPThCKEHpZomDD'
        '0Bb3T8u7YGrMjEX5cOmZIU2T/iy4GK/wWuBIy2TEp/0J+RoSCIr8Df/A7GIM8bwn'
        'v23Vq0kE2xBqqaT5LjvuQoXfiLJ42F33DDno9lVikKEyt55D/08rH41KpXvn3tWZ'
        '46tZK6Ds7Zr1hEV1LbDx1CXDzQ6gKObBe54DWDV3h7TJhr0BSW68dFJhro7Y60Ns'
        'zTcFqY1RC9F0ePtsnKGFzMOe/U+fPvsGe2oWvwIDAQABAoIBACCf19ewfpWETe98'
        'wuOpIsQ8HyVjaCShvvh5tNUITcJhuFk5ajFdTqjc/O0VHxgmLm6O99e2vaiXCISH'
        'EX4SWXq7lTMcYCf9YN47Y+HGoa8eFNTIS0ExJRPtojAY695O1UZmpUnfI1wux1mG'
        'g8vZz0OCfXnBVAbsyjCX/IqOBp2MVzfMyMuaF/oQ2xiX4AZ1hDIMDpUTGw7OKX15'
        'JAUlTZUhzifmijPg1gViD8Lf5w42nlwYPC5j6wWKpJSx76CNUxLdJAaaZb3QYE96'
        'zu/jOCdy25sPHIux3XTdV6fqZ2iTvt31+bcnSAvmbDpmcujsZPVRXRu5OO/0xBh6'
        'GGlTLAECgYEAwSyNkbNk0mBRxet68IW02wXYaxIEVUWqhSeE2MGaXg4h9VSgh83q'
        '7wly0ujy9Sj79aF2frkpMbIoeeGIOTIYI4RCYuBKx+/NNWFoggu4UK5xOMr0dfQK'
        '2Ggr2agUH3KExvOpAW3rvWzepLl8ppySLNipLcFQHOJ0kxwPd2ig3Z8CgYEAt+WM'
        'JoW9dLxUu/zTih5Dacubl+fnnm8BsypKmv88mzcqEVwXOo6Z6bmlw0NeWxmlwHu7'
        'vs+XQ8MDUDvQvIul8sFagZk7RvWcXTlaHtPQ1D8/ztrg5d58TwxpwXshBytfR6NA'
        'tIZa+tNvzQF5AKVlB+lZEWF6E6FoI5NmGDAZ8uECgYB4FV4cCMzQCphK1Muj4TpA'
        'PS3/wT94Usph4+Mta4yuk1KA047HXTaCSflbKvx9cnDOjQTAWhJFll6bBZxNEdr3'
        'mSw7kvppt6R1Xow861Q0s3wmteOpv39Ob9Nyho2bzvDDTIzvGonFQ3xUIgpe+E3W'
        'GwlwLA/FJPEa0gK7VAtMOQKBgQCgcPtX2LM0l+Ntp+V/yWuTb/quC7w+tCbNhAZX'
        'OHxOB1ECmFAD3MpX6oq+05YM8VF1n/5rOX6Ftiy74ZP6C/Sa2Sr3ixL2k+76PsFr'
        'x+2YYB5xgPFaXEQkS3YxQhXMxYB5ZetcFSRnVfVi7Pf/Ik4FGweEbIEvg1DySPV4'
        'AO+CwQKBgQCFnjHsFeNZVvtiL2wONT6osjRCpMvaUiVecMW9oUBtjpLHI2gQr7+4'
        'dvCm2Sj7uq9OWO0rBz1px/kI+ONjhwsFPLK5v8hyVDoIE791Qg3qAY1a6JOXRl9P'
        '6TBc3dQ2qUVqt8gi9RLCDFJU18Td6La4mkJSP5YrioGtwUJow0F07Q=='
        '-----END RSA PRIVATE KEY-----'
    ==

  =/  cert=wain
    :~  '-----BEGIN CERTIFICATE-----'
        'MIIF8jCCBNqgAwIBAgITAPrPc8Udwmv5dJ+hx2Uh+gZF1TANBgkqhkiG9w0BAQsF'
        'ADAiMSAwHgYDVQQDDBdGYWtlIExFIEludGVybWVkaWF0ZSBYMTAeFw0xODA3MDMx'
        'ODAyMTZaFw0xODEwMDExODAyMTZaMB8xHTAbBgNVBAMTFHpvZC5keW5kbnMudXJi'
        'aXQub3JnMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAisQPzzmGWNZS'
        'NNAwY59XrqK/bU0NKNZS2ETOiJeSpzPAHYl+c39V96/QUR0tra2zQI4QD6kpMYX/'
        '7R5nwuvsA4o7ypfYupNrlzLPThCKEHpZomDD0Bb3T8u7YGrMjEX5cOmZIU2T/iy4'
        'GK/wWuBIy2TEp/0J+RoSCIr8Df/A7GIM8bwnv23Vq0kE2xBqqaT5LjvuQoXfiLJ4'
        '2F33DDno9lVikKEyt55D/08rH41KpXvn3tWZ46tZK6Ds7Zr1hEV1LbDx1CXDzQ6g'
        'KObBe54DWDV3h7TJhr0BSW68dFJhro7Y60NszTcFqY1RC9F0ePtsnKGFzMOe/U+f'
        'PvsGe2oWvwIDAQABo4IDIjCCAx4wDgYDVR0PAQH/BAQDAgWgMB0GA1UdJQQWMBQG'
        'CCsGAQUFBwMBBggrBgEFBQcDAjAMBgNVHRMBAf8EAjAAMB0GA1UdDgQWBBTokXAU'
        'vPwcrbkLxcVBCNNQ588pfjAfBgNVHSMEGDAWgBTAzANGuVggzFxycPPhLssgpvVo'
        'OjB3BggrBgEFBQcBAQRrMGkwMgYIKwYBBQUHMAGGJmh0dHA6Ly9vY3NwLnN0Zy1p'
        'bnQteDEubGV0c2VuY3J5cHQub3JnMDMGCCsGAQUFBzAChidodHRwOi8vY2VydC5z'
        'dGctaW50LXgxLmxldHNlbmNyeXB0Lm9yZy8wHwYDVR0RBBgwFoIUem9kLmR5bmRu'
        'cy51cmJpdC5vcmcwgf4GA1UdIASB9jCB8zAIBgZngQwBAgEwgeYGCysGAQQBgt8T'
        'AQEBMIHWMCYGCCsGAQUFBwIBFhpodHRwOi8vY3BzLmxldHNlbmNyeXB0Lm9yZzCB'
        'qwYIKwYBBQUHAgIwgZ4MgZtUaGlzIENlcnRpZmljYXRlIG1heSBvbmx5IGJlIHJl'
        'bGllZCB1cG9uIGJ5IFJlbHlpbmcgUGFydGllcyBhbmQgb25seSBpbiBhY2NvcmRh'
        'bmNlIHdpdGggdGhlIENlcnRpZmljYXRlIFBvbGljeSBmb3VuZCBhdCBodHRwczov'
        'L2xldHNlbmNyeXB0Lm9yZy9yZXBvc2l0b3J5LzCCAQIGCisGAQQB1nkCBAIEgfME'
        'gfAA7gB1ALDMg+Wl+X1rr3wJzChJBIcqx+iLEyxjULfG/SbhbGx3AAABZGGGG6QA'
        'AAQDAEYwRAIgJHrIawVea5/++wteocdbt1QUBxysW7uJqYgvnOWOQMgCIGRlioyE'
        'vzunUm/HZre3fF2jBsJr45C1tz5FTe/cYQwmAHUA3Zk0/KXnJIDJVmh9gTSZCEmy'
        'Sfe1adjHvKs/XMHzbmQAAAFkYYYjLQAABAMARjBEAiAWovIKERYeNbJlAKvNorwn'
        'RnSFP0lJ9sguwcpbcsYJ1gIgRJxTolkMOr0Fwq62q4UYnpREY8zu4hiL90Mhntky'
        'EwYwDQYJKoZIhvcNAQELBQADggEBAMYxvA+p4Qj0U23AHAe61W3+M6T1M0BfrGE2'
        'jJCaq4c3d7b9NEN1qFJHl8t/+Z/7RHUIzbm4CIOZynSM8mBxg2NgXymvXQkRrrBo'
        'fhO9u8Yxizx4+KOtiigt9JBVlpyCm6I9uifM+7rZYh45s2IkfDBPKd+M1tfIUOne'
        'YgUt1YguEkM2xqRG16JyHA0Xwn6mn+4pWiTdfNzlqol6vyGT7WfIvmV7cdGoYKjB'
        'wOt/g1wWMTwhSWBCVqCyn+f2rl8u3wbXrIUeRng2ryNVXO03nukTp7OLN3HUO6PR'
        'hC4NdS4o2geBNZr8RJiORtCelDaJprY7lhh2MFzVpsodc2eB5sQ='
        '-----END CERTIFICATE-----'
        ''
        '-----BEGIN CERTIFICATE-----'
        'MIIEqzCCApOgAwIBAgIRAIvhKg5ZRO08VGQx8JdhT+UwDQYJKoZIhvcNAQELBQAw'
        'GjEYMBYGA1UEAwwPRmFrZSBMRSBSb290IFgxMB4XDTE2MDUyMzIyMDc1OVoXDTM2'
        'MDUyMzIyMDc1OVowIjEgMB4GA1UEAwwXRmFrZSBMRSBJbnRlcm1lZGlhdGUgWDEw'
        'ggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDtWKySDn7rWZc5ggjz3ZB0'
        '8jO4xti3uzINfD5sQ7Lj7hzetUT+wQob+iXSZkhnvx+IvdbXF5/yt8aWPpUKnPym'
        'oLxsYiI5gQBLxNDzIec0OIaflWqAr29m7J8+NNtApEN8nZFnf3bhehZW7AxmS1m0'
        'ZnSsdHw0Fw+bgixPg2MQ9k9oefFeqa+7Kqdlz5bbrUYV2volxhDFtnI4Mh8BiWCN'
        'xDH1Hizq+GKCcHsinDZWurCqder/afJBnQs+SBSL6MVApHt+d35zjBD92fO2Je56'
        'dhMfzCgOKXeJ340WhW3TjD1zqLZXeaCyUNRnfOmWZV8nEhtHOFbUCU7r/KkjMZO9'
        'AgMBAAGjgeMwgeAwDgYDVR0PAQH/BAQDAgGGMBIGA1UdEwEB/wQIMAYBAf8CAQAw'
        'HQYDVR0OBBYEFMDMA0a5WCDMXHJw8+EuyyCm9Wg6MHoGCCsGAQUFBwEBBG4wbDA0'
        'BggrBgEFBQcwAYYoaHR0cDovL29jc3Auc3RnLXJvb3QteDEubGV0c2VuY3J5cHQu'
        'b3JnLzA0BggrBgEFBQcwAoYoaHR0cDovL2NlcnQuc3RnLXJvb3QteDEubGV0c2Vu'
        'Y3J5cHQub3JnLzAfBgNVHSMEGDAWgBTBJnSkikSg5vogKNhcI5pFiBh54DANBgkq'
        'hkiG9w0BAQsFAAOCAgEABYSu4Il+fI0MYU42OTmEj+1HqQ5DvyAeyCA6sGuZdwjF'
        'UGeVOv3NnLyfofuUOjEbY5irFCDtnv+0ckukUZN9lz4Q2YjWGUpW4TTu3ieTsaC9'
        'AFvCSgNHJyWSVtWvB5XDxsqawl1KzHzzwr132bF2rtGtazSqVqK9E07sGHMCf+zp'
        'DQVDVVGtqZPHwX3KqUtefE621b8RI6VCl4oD30Olf8pjuzG4JKBFRFclzLRjo/h7'
        'IkkfjZ8wDa7faOjVXx6n+eUQ29cIMCzr8/rNWHS9pYGGQKJiY2xmVC9h12H99Xyf'
        'zWE9vb5zKP3MVG6neX1hSdo7PEAb9fqRhHkqVsqUvJlIRmvXvVKTwNCP3eCjRCCI'
        'PTAvjV+4ni786iXwwFYNz8l3PmPLCyQXWGohnJ8iBm+5nk7O2ynaPVW0U2W+pt2w'
        'SVuvdDM5zGv2f9ltNWUiYZHJ1mmO97jSY/6YfdOUH66iRtQtDkHBRdkNBsMbD+Em'
        '2TgBldtHNSJBfB3pm9FblgOcJ0FSWcUDWJ7vO0+NTXlgrRofRT6pVywzxVo6dND0'
        'WzYlTWeUVsO40xJqhgUQRER9YLOLxJ0O6C8i0xFxAMKOtSdodMB3RIwt7RFQ0uyt'
        'n5Z5MqkYhlMI3J1tPRTp1nEt9fyGspBOO05gi148Qasp+3N+svqKomoQglNoAxU='
        '-----END CERTIFICATE-----'
    ==
  =/  k=key:rsa  (need (ring:de:pem:pkcs1 key))
  =/  k8=wain  (ring:en:pem:pkcs8 k)
  (emit %rule /install %cert `[k8 cert])
:: +poke-path: for debugging
::
++  poke-path
  |=(a=path abet:(add-order (sy a ~)))
::
:: ++  prep  _[~ this]
++  prep
  |=  old=(unit acme)
  ^-  (quip move _this)
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  rekey                             :: XX do something about this
  |=  eny=@
  =|  i=@
  |-  ^-  key:rsa
  =/  k  (new-key:rsa 2.048 eny)
  =/  m  (met 0 n.pub.k)
  :: ?:  =(0 (mod m 8))  k
  ?:  =(2.048 m)  k
  ~&  [%key iter=i width=m]
  $(i +(i), eny +(eny))
::
++  init
  =/  url
    'https://acme-staging-v02.api.letsencrypt.org/directory'
  =<  (retry:effect /directory +(now.bow))
  %=  this
    bas  (need (de-purl:html url))
    act  [(rekey eny.bow) ~]
    cey  (rekey (mix eny.bow (shaz now.bow)))
  ==
::
++  add-order
  |=  dom=(set turf)
  ^+  this
  ?:  ?=(?(%earl %pawn) (clan:title our.bow))
    this
  :: set pending order
  ::
  =.  pen  `dom
  :: archive active order if exists
  ::
  ::   XX we may have pending moves out for this order
  ::   put dates in wires, check against order creation date?
  ::   or re-use order-id?
  ::
  =?  fal.hit  ?=(^ rod)  [u.rod fal.hit]
  =.  rod  ~
  :: if registered, create order
  ::
  ?^  reg.act
    new-order:effect
  :: if initialized, defer
  ::
  ?.(=(act *acct) this init)
::
++  test
  =,  tester:tester
  =/  eny  eny.bow
    :: non-deterministic for now
    :: 0vhu.gp79o.hi7at.smp8u.g5hhr.u3rff.st8ms.q4dc2.hv5ls.tp5cp.10qds.
    ::      h9bpt.vlmm7.lh375.f6u9n.krqv8.5jcml.cujkr.v1uqv.cjhe5.nplta
  |^  =/  out=tang
          ;:  weld
            test-jwk
            test-jws
            test-jws-2
          ==
      ?~(out this ((slog out) this))
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
        (en-base64url (as-octt:mimes:html (en-json-sort aor hed)))
      %-  expect-eq  !>
        :-  lodt
        (en-base64url (as-octt:mimes:html (en-json-sort (eor lte lod-order) lod)))
      %-  expect-eq  !>
        :-  exp-ws
        (en-base64url (en:octn (~(sign rs256 k) inp-ws)))
    ==
  ::
  ++  test-jws-2
    :: captured from an in-the-wild failure
    :: relevant sha-256 has a significant leading zero
    :: which was not being captured in the asn.1 digest ...
    =/  kpem=wain
      :~  '-----BEGIN RSA PRIVATE KEY-----'
          'MIIEogIBAAKCAQEAkmWLu+9gyzCbrGAHTFE6Hs7CtVQofONmpnhmE7JQkmdS+aph'
          'WwZQfp9p6RU6vSoBaPXD96uqMXhvoOXz9/Ub5TRwLmQzfHZdksfU3pEZ8qFMikZU'
          'p5v+CyBnLq9YR0VXN+/JVatmYb1hhC1k101X9m+IU3DR3U+kyCZnXuOd10xVX05H'
          '0pXl+nI25bZyMJFnz1Xfw1rTnhtU/w7bgCWYdMii5jLkl5zfoY2gulpPu7QeYa4K'
          '3fTqklDNFK7kQQ1l4O3461fbSO0cnG4t8Vk3026ageA54+Qx8O8UDi8k18Z1NF+B'
          'pbPUZn55/InuZ8iGyHBZ4GRFIPG0iOdWM7gHCwIDAQABAoIBAAMQN/9SS6MJMULq'
          'CsXHxyl5sHtXa/BgWLHP+j2/FtRX++EkR0s+ln2FobZa+l5Q9m4Ljn5PbqSMAFfM'
          'Y6u0hNyj9om04oOl8bILl4Vcvqgp51oFvAEGOW15/o69+6bS3aBx7cqwfnsivInr'
          'nIXDvHcyey3kh9WCKNx3rxNVgfuTCkw0+K2qXkMTh2c3Iz2efR2f78qbNWQcBe1+'
          's83fABafxACYuXzfOYoO01GBCJnHrmXxJVePLXwxLkLeJHOQJQgPnagVbUH4kbUp'
          'OLd9h1dOVYKpyVaxbQiAH3U/ekOXCCv18a47/PQSbueolzSzMzwVPSZdf+88lzuq'
          'ZZyDXDECgYEAk5zt4cO7X+8IIeNXx8/2pztT9WmC1kqw4RtInoVXm62K1B0pPndW'
          'm0nMVFEDuSwdn61G5amlaOT0dTFHlMFydC9H+1L5PMK7d+6ArSeAtMWoUhz+jkcO'
          'B9KoMfZ9CtP2r5589zDGir8kaY8Fia5Z7TohpJDidmuumgDabl+qH+kCgYEA/eP6'
          'lIGVHF8EIrfewjLM+8i1RE/hzItOpegrwDUVeYfZlPM59xUyC9REdgvmnTssxPcL'
          '2+EB11wvcImSPLuwN0kXUkh9qZUkr9hvYlikALNH1f8WhCJ0kT6pUeA7LbjU4/bM'
          'fsgcOh1POW2piIMERl1TuNRZg7JdKuCJKax3qtMCgYB2dxcifOc/0qIAMGgeX/Rf'
          'ueljp03tlPvnbPIW5oSs19X27YBQNY44Cj4F3Q7T6WfM4k9nuYKacEUQWIBODgJA'
          '5EEsniaQcOfrFGoIjQ9qBMdVPxe8L6I+/P0nO96Wdg4gW12HNIniiAw8+x9Co75f'
          '+KtPW0ekKj9yMQUcV4I9IQKBgE06bruDmzbRFDH3WjQaPc4M5E6OOfH9IgRHVh+W'
          'Rhz8nMu5HJWzBdEhVV3PCuwi1uBnAV112RiIOwnxXuFIejam7ggics8Fxe4TWPZC'
          'Xki0QBKxEElLLcgMlnaITZf/1AovxU5/Uk6/IZ0nZV1X9RHuS4w6U6xCsiJbwH1D'
          'r/bvAoGAV/Vx+Z2BD7QhmHofu98OMW6EGSjWMgOI4iXdcQ80Urz9akHkOM4KGojq'
          'UDobbxxkJt1K5Dzux+vnp1siiIkcLdVdtMzqo7KcKYWonMqZmppNqIFCXQHscCRD'
          'r6f1TIjlurYrazLAkRsmjE5uYM13/E1UdxplWSkdCbivIWqoqTM='
          '-----END RSA PRIVATE KEY-----'
      ==
    =/  k=key:rsa
      (need (ring:de:pem:pkcs1 kpem))
    =/  kid=@t
      'https://acme-staging-v02.api.letsencrypt.org/acme/acct/6336694'
    =/  non=@t
      'a5Pwh6GcuqRSvHTQouW96XNg3iiMORMkBf_wSLOf0M4'
    =/  url=purl
      :-  [sec=%.y por=~ hot=[%.y p=/org/letsencrypt/api/acme-staging-v02]]
      :_  query=~
      :-  ext=~
      %+  weld
        /acme/challenge
      /'efJn0ywfjIi3M7yT-6H8Mdq85R2LnI8XsTG3DaaY8Gc'/'138087558'
    =/  protected-header=json
      :-  %o  %-  my  :~
        nonce+s+non
        url+s+(crip (en-purl:html url))
        kid+s+kid
      ==
    =/  bod=json
      [%o ~]
    =/  exp=json
      =/  payload=@t  'e30'
      =/  protected=@t
        %+  rap  3
        :~  'eyJhbGci'
            'OiJSUzI1NiIsImtpZCI6Imh0dHBzOi8vYWNtZS1zdGFnaW5nLXYwMi5hcGkubGV0c2'
            'VuY3J5cHQub3JnL2FjbWUvYWNjdC82MzM2Njk0Iiwibm9uY2UiOiJhNVB3aDZHY3Vx'
            'UlN2SFRRb3VXOTZYTmczaWlNT1JNa0JmX3dTTE9mME00IiwidXJsIjoiaHR0cHM6Ly'
            '9hY21lLXN0YWdpbmctdjAyLmFwaS5sZXRzZW5jcnlwdC5vcmcvYWNtZS9jaGFsbGVu'
            'Z2UvZWZKbjB5d2ZqSWkzTTd5VC02SDhNZHE4NVIyTG5JOFhzVEczRGFhWThHYy8xMz'
            'gwODc1NTgifQ'
        ==
      =/  signature=@t
        %+  rap  3
        :~  'cukOS_KIWTolvORyJoIu5eejdLoFi6xpd06Y6nW565zFMKZi44BepsWIZXw4yxYjxs'
            '8xFdoKOxtXhBS5BT0mbkHSUGokAPTUiF5b1wjm00ZiKRYwnIotizsLPzHAJKwhMlFs'
            'x6oAu25mmremBgnNtVD_cskQBbkTBgiTL6alrkrmwxlP2gSqyX6uEO-UCY71QB_xYj'
            '4IOoX2k0jdXJevXDAJSUWfs5cZkm8Ug_q4GVTRWhZmFHMnMzonmCC4Ui7nDa9oKJH5'
            'Npyn74FCcqbz111AK-Aul1dNhz3ojE1VOk3eVjH69lSGsaMleYR5fi60Jdc5ZbpPPy'
            't-CZRp1F0k6w'
        ==
      [%o (my payload+s+payload protected+s+protected signature+s+signature ~)]
    %-  expect-eq  !>
      :-  exp
      (sign:jws k protected-header bod)
  --
--

