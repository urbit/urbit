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
    |=  [=seed:jael =point:azimuth =live]
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
