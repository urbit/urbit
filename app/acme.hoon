/-  asn1
/+  base64, der, primitive-rsa, *pkcs, *jose
=,  eyre
=*  rsa  primitive-rsa
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
::
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
--

