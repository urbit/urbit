/+  tester
=,  tester:tester
=,  purl:eyre
::
::::  libraries
::
|%
++  rsa                                                 ::  unpadded!
  |%
  +=  key  [p=@ux q=@ux n=@ux e=@ux d=@ux]
  ++  elcm
    |=  [a=@ b=@]
    (div (mul a b) d:(egcd a b))
  ::
  ++  new-key
    |=  [wid=@ eny=@]
    ^-  key
    =/  diw  (rsh 0 1 wid)
    =/  p=@ux  (ramp:number diw [3 5 ~] eny)
    =/  q=@ux  (ramp:number diw [3 5 ~] +(eny))
    =/  n=@ux  (mul p q)
    =/  e  `@ux`65.537
    =/  d=@ux  (~(inv fo (elcm (dec p) (dec q))) e)
    [p q n e d]
  ::
  ++  en
    |=  [m=@ k=key]
    ~|  %rsa-len
    ?>  (lte (met 0 m) (met 0 n.k))
    (~(exp fo n.k) e.k m)
  ::
  ++  de
    |=  [m=@ k=key]
    :: XX assert rsa-len here too?
    =/  fu  (fu:number p.k q.k)
    (out.fu (exp.fu d.k (sit.fu m)))
  --
::
++  rs256
  |_  k=key:rsa
  ++  sign  |=(m=@ (de:rsa (shax m) k))
  ++  verify
    |=  [s=@ m=@]
    =((shax m) (en:rsa s k))
  --
::
++  en-json-sort                                      ::  print json
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
++  en-base64url                                           :: rfc4648 section-5
  |=  tig=@                                             :: padding omitted
  ^-  tape                                              :: per rfc7515
  =/  poc  (~(dif fo 3) 0 (met 3 tig))
  =/  pad  (lsh 3 poc (swp 3 tig))
  =/  cha  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
  %-  flop
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
::
++  de-base64url
  =-  |=(a/cord (rash a fel))
  =<  fel=(cook |~(a/@ `@t`(swp 3 a)) (bass 64 .))
  =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
  ^=  siw
  ;~  pose
     (cook |=(a/@ (sub a 'A')) (shim 'A' 'Z'))
     (cook |=(a/@ (sub a 'G')) (shim 'a' 'z'))
     (cook |=(a/@ (add a 4)) (shim '0' '9'))
     (cold 62 (just '-'))
     (cold 63 (just '_'))
   ==
::
++  jwk
  |_  k=key:rsa
  ++  pass
    ^-  json
    :-  %o  %-  my  :~
      kty+s+'RSA'
      n+s+(crip (en-base64url n.k))
      e+s+(crip (en-base64url e.k))
    ==
  ++  ring  !!
  --
++  thumbprint
  |=  jon=json
  :: XX restrict keys to canonical set
  (en-base64url (shax `@`(crip `tape`(en-json-sort aor jon))))
::
++  from-json
  =,  dejs:format
  =/  json-purl  (su auri:de-purl:html)
  %-  ot
  :~  ['newAccount' json-purl]
      ['newNonce' json-purl]
      ['newOrder' json-purl]
      ['revokeCert' json-purl]
      ['keyChange' json-purl]
  ==
--
::
::::  acme state
::
|%
+=  move  [bone card]
+=  card  $%  [%hiss wire [~ ~] %httr %hiss hiss:eyre]
          ==
::
+=  nonce  @t
::
+=  directory                                           ::  ACME v2
  $:  reg/purl                                          ::  newAccount
      non/purl                                          ::  newNonce
      der/purl                                          ::  newOrder
      rev/purl                                          ::  revokeCert
      rek/purl                                          ::  keyChange
  ==
::
+=  state
  $:  dir=directory
      non=nonce
      kid=(unit @t)
      key=(unit key:rsa)
  ==
--
::
::::  acme app
::
=/  url=tape  "https://acme-staging-v02.api.letsencrypt.org/directory"
=/  bas=purl  (scan url auri:de-purl:html)
=|  mov=(list move)
|_  [bow=bowl:gall state]
::
++  this  .               :: XX #712
::
++  emit
  |=  car=card
  this(mov [[ost.bow car] mov])
::
++  abet
  [(flop mov) this(mov ~)]
::
++  jws-body
  |=  [url=purl bod=json]
  ^-  octs
  =*  enc  (corl en-base64url (corl crip (cury en-json-sort aor)))
  =/  payload=tape  (enc bod)
  =/  protect=tape
    %-  enc
    :-  %o  %-  my  :~
      alg+s+'RS256'
      nonce+s+non
      url+s+(crip (en-purl:html url))
      ?^  kid
        kid+s+u.kid
      ?>  ?=(^ key)
      jwk+~(pass jwk u.key) 
    ==
  %-  (corl as-octt:mimes:html en-json:html)
  ^-  json
  :-  %o  %-  my  :~
    protected+s+(crip protect)
    payload+s+(crip payload)
    :+  %signature  %s
    %-  crip  %-  en-base64url
    ?>  ?=(^ key)
    %-  ~(sign rs256 u.key)
    (crip :(welp protect "." payload))
  == 
::  
++  request
  |=  [wir=wire url=purl bod=(unit json)]
  =/  lod
    ?~  bod
      [%get ~ ~]
    =/  hed  (my content-type+['application/jose+json' ~] ~)
    [%post hed `(jws-body url u.bod)]
  (emit [%hiss wir [~ ~] %httr %hiss url lod])
::
++  initialize
  =?  key  ?=(~ key)  `(new-key:rsa 2.048 eny.bow)
  (request /acme/init/(scot %p our.bow) bas ~)
::
++  nonce
  (request /acme/non/(scot %p our.bow) non.dir ~)
::
++  register
  %^  request  /acme/reg/(scot %p our.bow)
    reg.dir
  `[%o (my [['termsOfServiceAgreed' b+&] ~])]
::
++  authorize
  :+  request
    der.dir ::aut.dir
  ^-  json
  :-  %o  %-  my  :~
    resource+s+'new-authz'
    :-  %identifier
    :-  %o  %-  my  :~
      type+s+'dns'
      value+s+(crip (welp +:(scow %p our.bow) ".urbit.org"))
    ==
  ==
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a  ~&  +<+.this
         [~ this]
    %init  abet:initialize
    %test  test
  ==
::
++  sigh-httr
  |=  [wir=wire rep=httr:eyre]
  ^-  (quip move _this)
  ~&  [wir rep]
  ?>  ?=([%acme ^] wir)
  ?.  ?=(%2 (div p.rep 100))
    ~&  %lack-of-success
    [~ +>]
  ?+  i.t.wir  !!
      %init
    =<  abet:nonce
    this(dir (from-json (need (de-json:html q:(need r.rep)))))
      ::
      %non
    =<  abet:register
    this(non q:(head (skim q.rep |=((pair @t @t) ?=(%replay-nonce p)))))
  ==
::
++  prep  _[~ this]
::
++  test
  =/  eny  eny.bow
    :: non-deterministic for now
    :: 0vhu.gp79o.hi7at.smp8u.g5hhr.u3rff.st8ms.q4dc2.hv5ls.tp5cp.10qds.
    ::      h9bpt.vlmm7.lh375.f6u9n.krqv8.5jcml.cujkr.v1uqv.cjhe5.nplta
  |^  =/  out=wall 
          ;:  weld
            testen-base64url
            testrsakey
            testrsa
            testjwkthumbprint
            testjws
          ==
      ?~  out  abet
      ((slog (turn `wall`out |=(a=tape leaf+a))) abet)
  ::
  ++  testen-base64url
    =/  jon  '{"iss":"joe","exp":1300819380,"http://example.com/is_root":true}'
    ;:  weld
      %-  expect-eq  !>
        :-  "AQAB"
        (en-base64url 65.537)
      %-  expect-eq  !>
        ~&  (en-base64:mimes:html jon)
        :-  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ"
        (en-base64url jon)
    ==
  ::
  ++  testrsakey
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
    =/  k2
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
      (key:rsa [p q n e d])
    ::
    |^  ^-  wall
        ;:  weld
            (check-primes k1)
            (check-primes k2)
        ==
    ++  check-primes
      =,  number
      |=  k=key:rsa
      %+  roll  primes
      |=  [p=@ a=wall]
      ?^  a  a
      ?:  =(0 (mod n.k p))
        :~  "n.key (prime? {(scow %f (pram n.k))}) divisible by {(scow %ud p)}:"
            "{(scow %ux n.k)}"
        ==
      ?:  =(0 (mod p.k p))
        :~  "p.key (prime? {(scow %f (pram p.k))}) divisible by {(scow %ud p)}:"
            "{(scow %ux p.k)}"
        ==
      ?:  =(0 (mod q.k p))
        :~  "q.key (prime? {(scow %f (pram q.k))}) divisible by {(scow %ud p)}:"
            "{(scow %ux q.k)}"
        ==
      ~
    --
  ::
  ++  testrsa
    =/  k1
      =/  p  61
      =/  q  53
      =/  e  17
      =/  n  (mul p q)
      =/  d  (~(inv fo (elcm:rsa (dec p) (dec q))) e)
      (key:rsa [p q n e d])
    ::
    =/  k2  (key:rsa [11 13 143 7 103])
    ::
    =/  k3
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
      (key:rsa [p q n e d])
    =/  m3  (swp 3 'attack at dawn')
    =/  c3
      35.052.111.338.673.026.690.212.423.937.053.328.511.880.760.811.579.981.
         620.642.802.346.685.810.623.109.850.235.943.049.080.973.386.241.113.
         784.040.794.704.193.978.215.378.499.765.413.083.646.438.784.740.952.
         306.932.534.945.195.080.183.861.574.225.226.218.879.827.232.453.912.
         820.596.886.440.377.536.082.465.681.750.074.417.459.151.485.407.445.
         862.511.023.472.235.560.823.053.497.791.518.928.820.272.257.787.786
    ::
    ;:  weld
      %-  expect-eq  !>
        [413 d.k1]
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
  --
  ::
  ++  testjwkthumbprint
    =/  n=(list @t)
      :~  '0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2'
          'aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCi'
          'FV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65Y'
          'GjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n'
          '91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_x'
          'BniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw'
      ==
    =/  k=json
      :-  %o  %-  my  :~
        e+s+'AQAB'
        kty+s+'RSA'
        n+s+(reel n (cury cat 3))
      ==
    ::
    %-  expect-eq  !>
      :-  "NzbLsXh8uDCcd-6MNwXF4W_7noWXFZAfHkxZsRGC9Xs"
      (thumbprint k)
  ++  testjws
    ^-  wall
    =/  pt=@t
      %+  reel
        ^-  (list @t)
        :~  '4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi'
            'YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG'
            'BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc'
        ==
      (cury cat 3)
    =/  qt=@t
      %+  reel
        ^-  (list @t)
        :~  'uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa'
            'ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA'
            '-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc'
        ==
      (cury cat 3)
    =/  nt=@t
      %+  reel
        ^-  (list @t)
        :~  'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx'
            'HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs'
            'D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH'
            'SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV'
            'MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8'
            'NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ'
         ==
      (cury cat 3)
    =/  dt=@t
      %+  reel
        ^-  (list @t)
        :~  'Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I'
            'jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0'
            'BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn'
            '439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT'
            'CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh'
            'BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ'
        ==
      (cury cat 3)
    =/  p  (de-base64url pt)
    =/  q  (de-base64url qt)
    =/  n  (de-base64url nt)
    =/  d  (de-base64url dt)
    =/  e  65.537
    =/  k  (key:rsa [p q n e d])
    =/  hed=json  o+(my alg+s+'RS256' ~)
    =/  lod=json
      :-  %o  %-  my  :~
        iss+s+'joe'
        exp+n+'1300819380'
        ['http://example.com/is_root' %b &]
      ==
    =/  lod-order
      =/  keys=(list @t)  ['iss' 'exp' 'http://example.com/is_root' ~]
      |=  [a=* b=*]
      =/  fa  (find ~[a] keys)
      =/  fb  (find ~[b] keys)
      ?~  fa  |
      ?~  fb  |
      (lte u.fa u.fb)
    ;:  weld
      %-  expect-eq  !>
        :-  "eyJhbGciOiJSUzI1NiJ9"
        (en-base64url (crip (en-json-sort aor hed)))
      %-  expect-eq  !>
        :-  "eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ"
        (en-base64url (crip (en-json-sort lod-order lod)))
    ==
--

