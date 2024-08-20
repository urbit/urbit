::  runtime support code
::
/+  ethereum, azimuth
=>  [ethereum=ethereum azimuth=azimuth ..zuse]  =>
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
    |-  ^-  seed:jael
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
    |%
    ::  +czar:give:dawn: Eth RPC for galaxy table
    ::
    ++  czar
      ^-  octs
      %-  as-octs:mimes:html
      %-  en:json:html
      :-  %a
      %+  turn  (gulf 0 255)
      |=  gal=@
      %+  request-to-json
        (cat 3 'gal-' (scot %ud gal))
      :-  'getPoint'
      (~(put by *(map @t json)) 'ship' s+(scot %p gal))
    ::  +point:give:dawn: Eth RPC for ship's contract state
    ::
    ++  point
      |=  who=ship
      ^-  octs
      %-  as-octs:mimes:html
      %-  en:json:html
      %+  request-to-json
        ~.
      :-  'getPoint'
      (~(put by *(map @t json)) 'ship' s+(scot %p who))
    ::  +turf:give:dawn: Eth RPC for network domains
    ::
    ++  turf
      ^-  octs
      %-  as-octs:mimes:html
      %-  en:json:html
      %+  request-to-json
        'turf'
      ['getDns' ~]
    ::  +request-to-json:give:dawn: internally used for request generation
    ::
    ::NOTE  we could import this from /lib/json/rpc, but adding that as a
    ::      dependency seems a bit unclean
    ::
    ++  request-to-json
      |=  [id=@t method=@t params=(map @t json)]
      ^-  json
      %-  pairs:enjs:format
      :~  jsonrpc+s+'2.0'
          id+s+id
          method+s+method
          params+o+params
      ==
    --
  ::  |take:dawn: parse responses for pre-boot validation
  ::
  ++  take
    =,  abi:ethereum
    =,  rpc:ethereum
    =,  azimuth
    =,  dejs-soft:format
    |%
    ::  +czar:take:dawn: parse galaxy table
    ::
    ++  czar
      |=  rep=octs
      ^-  (unit (map ship [=rift =life =pass]))
      =/  jon=(unit json)  (de:json:html q.rep)
      ?~  jon
        ~&([%czar-take-dawn %invalid-json] ~)
      =/  res=(unit (list [@t @ud @ud @]))
        %.  u.jon
        =,  dejs-soft:format
        =-  (ar (ot id+so result+(ot network+- ~) ~))
        %-  ot
        :~  :-  'rift'  (su dim:ag)
            :-  'keys'  (ot 'life'^(su dim:ag) ~)
            :-  'keys'  %+  cu  pass-from-eth:azimuth
                        %-  ot
                        :~  'crypt'^(cu (lead 32) (su ;~(pfix (jest '0x') hex)))
                            'auth'^(cu (lead 32) (su ;~(pfix (jest '0x') hex)))
                            'suite'^(su dim:ag)
                        ==
        ==
      ?~  res
        ~&([%czar-take-dawn %incomplete-json] ~)
      :-  ~
      %+  roll  u.res
      |=  $:  [id=@t deet=[=rift =life =pass]]
              kyz=(map ship [=rift =life =pass])
          ==
      ^+  kyz
      ?:  =(0 life.deet)
        kyz
      %+  ~(put by kyz)
        (slav %ud (rsh [3 4] id))
      deet
    ::  +point:take:dawn: parse ship's contract state
    ::
    ++  point
      |=  [who=ship rep=octs]
      ^-  (unit point:azimuth)
      ~!  *point:azimuth
      =/  jon=(unit json)  (de:json:html q.rep)
      ?~  jon
        ~&([%point-take-dawn %invalid-json] ~)
      =-  ?~  res
            ~&([%point-take-dawn %incomplete-json] ~)
          =,  u.res
          %-  some
          :+  own
            ?:  =(0 life)  ~
            `[life pass rift sponsor ~]  ::NOTE  escape unknown ::TODO could be!
          ?.  (gth who 0xffff)  ~
          `[spawn ~]  ::NOTE  spawned unknown
      ^-  $=  res
          %-  unit
          $:  [spawn=@ own=[@ @ @ @]]
              [=rift =life =pass sponsor=[? ship]]
          ==
      %.  u.jon
      =,  dejs-soft:format
      =-  (ot result+- ~)
      %-  ot
      :~  :-  'ownership'
          %-  ot
          |^  :~  'spawnProxy'^address
                  'owner'^address
                  'managementProxy'^address
                  'votingProxy'^address
                  'transferProxy'^address
              ==
          ::
          ++  address
            (ot 'address'^(cu hex-to-num:ethereum so) ~)
          --
        ::
          :-  'network'
          %-  ot
          ::TODO  dedupe with +czar
          :~  'rift'^(su dim:ag)
              'keys'^(ot 'life'^(su dim:ag) ~)
            ::
              :-  'keys'
              %+  cu  pass-from-eth:azimuth
              %-  ot
              :~  'crypt'^(cu (lead 32) (su ;~(pfix (jest '0x') hex)))
                  'auth'^(cu (lead 32) (su ;~(pfix (jest '0x') hex)))
                  'suite'^(su dim:ag)
              ==
            ::
              ::TODO  inconsistent @p string
              'sponsor'^(ot 'has'^bo 'who'^ni ~)
            ::
              ::TODO  escape
              ::TODO  what if escape or sponsor not present? possible?
          ==
      ==
    ::  +turf:take:dawn: parse network domains
    ::
    ++  turf
      |=  rep=octs
      ^-  (unit (list ^turf))
      =/  jon=(unit json)  (de:json:html q.rep)
      ?~  jon
        ~&([%turf-take-dawn %invalid-json] ~)
      =/  res=(unit (list @t))
        ((ot result+(ar so) ~) u.jon)
      ?~  res
        ~&([%turf-take-dawn %invalid-response] ~)
      ::  remove duplicates, parse into turfs
      ::
      =-  `doz
      %+  roll  u.res
      |=  [dom=@t doh=(set @t) doz=(list ^turf)]
      ?:  (~(has in doh) dom)  [doh doz]
      :-  (~(put in doh) dom)
      =/  hot=host:eyre
        (rash dom thos:de-purl:html)
      ?.  ?=(%& -.hot)  doz
      (snoc doz p.hot)
    --
  ::  +veri:dawn: validate keys, life, discontinuity, &c
  ::
  ++  veri
    |=  [=ship =feed:jael =point:azimuth =live]
    ^-  (each seed:jael (lest error=term))
    |^  ?@  -.feed
          ?^  err=(test feed)  |+[u.err ~]
          &+feed
        ?>  ?=([%1 ~] -.feed)
        =|  errs=(list term)
        |-
        ?~  kyz.feed
          |+?~(errs [%no-key ~] errs)
        =/  =seed:jael  [who [lyf key ~]:i.kyz]:feed
        ?~  err=(test seed)
          &+seed
        =.  errs  (snoc errs u.err)
        $(kyz.feed t.kyz.feed)
    ::
    ++  test
      |=  =seed:jael
      ^-  (unit error=term)
      ?.  =(ship who.seed)  `%not-our-key
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
    --
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
