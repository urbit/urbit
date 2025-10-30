/-  dns
/+  strandio, libdns=dns
=*  card  card:agent:gall
|%
+$  state  state-2
+$  any-state
 $~  *state
 $%  state-2
     state-1
     state-0
 ==
+$  state-2  [%2 =mass-timer]
+$  state-1  [%1 =mass-timer]
+$  state-0  [%0 hoc=(map bone session-0)]
+$  session-0
  $:  say=*
      mud=*
      =mass-timer
  ==
::
+$  mass-timer  [way=wire nex=@da tim=@dr]
::
++  state-0-to-1
  |=  s=state-0
  ^-  state-1
  [%1 mass-timer:(~(got by hoc.s) 0)]
::
++  state-1-to-2
  |=  s=state-1
  ^-  state-2
  [%2 +.s]
--
|=  [=bowl:gall sat=state]
=|  moz=(list card)
|%
++  this  .
+$  state      ^state      ::  proxy
+$  any-state  ^any-state  ::  proxy
++  abet  [(flop moz) sat]
++  flog  |=(=flog:dill (emit %pass /di %arvo %d %flog flog))
++  emit  |=(card this(moz [+< moz]))
::  +emil: emit multiple cards
::
++  emil
  |=  caz=(list card)
  ^+  this
  ?~(caz this $(caz t.caz, this (emit i.caz)))
::
++  on-init
  (poke-serve [~ /who] %base /gen/who/hoon ~)
::
++  on-load
  |=  [hood-version=@ud old=any-state]
  =<  abet
  =?  old   ?=(%0 -.old)  (state-0-to-1 old)
  =?  this  ?=(%1 -.old)
    (emil -:(poke-serve [~ /who] %base /gen/who/hoon ~))
  =?  old   ?=(%1 -.old)  (state-1-to-2 old)
  ?>  ?=(%2 -.old)
  this(sat old)
::
++  poke-rekey                                        ::  rotate private keys
  |=  des=@t
  =/  fud=(unit feed:jael)
    %+  biff
      (bind (slaw %uw des) cue)
    (soft feed:jael)
  =<  abet
  ?~  fud
    ~&  %invalid-private-key
    this
  =/  fed  (need fud)
  ?@  -.fed
    ?.  =(our.bowl who.fed)
      ~&  [%wrong-private-key-ship who.fed]
      this
    (emit %pass / %arvo %j %rekey lyf.fed key.fed)
  ?.  =(our.bowl who.fed)
    ~&  [%wrong-private-key-ship who.fed]
    this
  =/  kyz=(list [lyf=life key=ring])
    ?:  ?=([%1 ~] -.fed)  kyz.fed
    kyz.fed
  =|  caz=(list card)
  %-  emil
  |-
  ?~  kyz  (flop caz)
  %=  $
    kyz  t.kyz
    caz  [[%pass / %arvo %j %rekey i.kyz] caz]
  ==
::
++  ames-secret
  ^-  @t
  =;  pax  (crip +:<.^(@p %j pax)>)
  /(scot %p our.bowl)/code/(scot %da now.bowl)/(scot %p our.bowl)
::
++  poke-sec-atom
  |=  [hot=host:eyre dat=@]
  ?>  ?=(%& -.hot)
  =.  p.hot  (scag 2 p.hot)      :: ignore subdomain
  =.  dat  (scot %uw (en:cyf:cric:crypto ames-secret dat))
  =-  abet:(emit %pass /write %arvo %c %info -)
  =/  byk=path  (en-beam byk.bowl(r da+now.bowl) ~)
  =+  .^(=tube:clay cc+(welp byk /mime/atom))
  =/  =cage  atom+(tube !>([/ (as-octs:mimes:html dat)]))
  (foal:space:userlib :(welp byk sec+p.hot /atom) cage)
::
++  poke-moon                                        ::  rotate moon keys
  |=  sed=(unit [=ship =udiff:point:jael])
  =<  abet
  ?~  sed
    this
  (emit %pass / %arvo %j %moon u.sed)
::
++  poke-moon-breach
  |=  =ship
  ?>  ?|  =(our src):bowl
          =(src.bowl ship)
      ==
  abet:(emit %pass /helm/moon-breach/(scot %p ship) %arvo %b %wait now.bowl)
::
++  take-wake-moon-breach
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog %take-wake-moon-breach-fail u.error)
    abet
  ?>  ?=([@ ~] way)
  =/  =ship  (slav %p i.way)
  ?>  =(%earl (clan:title ship))
  ?>  =(our.bowl (sein:title our.bowl now.bowl ship))
  =/  =rift
    +(.^(rift j+/(scot %p our.bowl)/rift/(scot %da now.bowl)/(scot %p ship)))
  abet:(emit %pass / %arvo %j %moon ship *id:block:jael %rift rift %.n)
::
++  poke-code
  |=  act=?(~ %reset)
  =<  abet
  ?~  act
    this
  (emit %pass / %arvo %j %step ~)
::
++  poke-mass
  |=  ~  =<  abet
  (emit %pass /heft %arvo %d %flog %heft ~)
::
++  poke-automass
  |=  recur=@dr
  =.  mass-timer.sat
    [/helm/automass (add now.bowl recur) recur]
  abet:(emit %pass way.mass-timer.sat %arvo %b %wait nex.mass-timer.sat)
::
++  poke-cancel-automass
  |=  ~
  abet:(emit %pass way.mass-timer.sat %arvo %b %rest nex.mass-timer.sat)
::
++  poke-meld
  |=  ~  =<  abet
  (emit %pass /pack %arvo %d %flog %meld ~)
::
++  poke-pack
  |=  ~  =<  abet
  (emit %pass /pack %arvo %d %flog %pack ~)
::
++  poke-pans
  |=  pans=(list note-arvo)
  ?~  pans  abet
  =.  this  (emit %pass /helm/pans %arvo i.pans)
  $(pans t.pans)
::
++  poke-pass
  |=  =note-arvo  =<  abet
  (emit %pass /helm/pass %arvo note-arvo)
::
++  take-wake-automass
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog u.error)
    ~&  %helm-wake-automass-fail
    abet
  =.  nex.mass-timer.sat  (add now.bowl tim.mass-timer.sat)
  =<  abet
  %-  emil
  :~  [%pass /heft %arvo %d %flog %crud %hax-heft ~]
      [%pass way.mass-timer.sat %arvo %b %wait nex.mass-timer.sat]
  ==
::
++  poke-send-hi
  |=  [her=ship mes=@]  =<  abet
  %-  emit
  :*  %pass  /helm/hi/(scot %p her)
      %agent  [her %hood]  %poke
      %helm-hi  !>(mes)
  ==
::
++  poke-send-ahoy
  |=  [her=ship test=? force=?]  =<  abet
  =/  =wire
    :+  %helm  %ahoy
    ?.(test /(scot %p her) /test/(scot %p her))
  =/  =path  ?:(test /test/mesa-1 /mesa-1)
  ::  before migrating, test if we can migrate, regress, and check that there
  ::  are not flows in a weird state. if we don't crash, send the %ahoy $plea
  ::
  =^  mate-moves  sat  
    ?.  force  `sat  
    (poke-mass-mate `her test=%.y)
  =^  ahoy-moves  sat  abet:(emit %pass wire %arvo %a %plea her %$ path %ahoy ~)
  (emil (weld mate-moves ahoy-moves))
::
++  poke-mass-mate
  |=  [ship=(unit ship) dry=?]
  =/  =wire
    :+  %helm  %mate
    ?.(dry ~ /test)
  abet:(emit %pass wire %arvo %a %mate ship dry)
::
++  poke-mass-rege
  |=  [ship=(unit ship) dry=?]
  =/  =wire
    :+  %helm  %rege
    ?.(dry ~ /test)
  abet:(emit %pass wire %arvo %a %rege ship dry)
::
++  take-ahoy
  |=  [way=wire error=(unit error:ames)]
  ?:  ?=([%test @ *] way)
    ?~  error
      ~&  >   %migration-test-worked
      ~&  >>  %test-local-migration
      abet:(emit %pass /helm/migrate %arvo %a %mate (slaw %p i.t.way) dry=%.y)
    %-  (slog %take-ahoy-test-failed u.error)
    abet
  ?>  ?=([@ ~] way)
  ?~  error
      ~&  >   %remote-migration-worked
      ~&  >>  %try-local-migration
    abet:(emit %pass /helm/migrate %arvo %a %mate (slaw %p i.way) dry=%.n)
  ~&  >>>  %ahoy-crash
  ::  XX retry?
  ::
  %-  (slog %take-ahoy-failed u.error)
  abet
  :: abet:(emit %pass `wire`[%helm %ahoy-crash way] %arvo %b %wait (add now.bowl ~s30)) :: XX exp backoff?
::
++  take-ahoy-crash
  |=  [way=wire error=(unit tang)]
  ?>  ?=([@ ~] way)
  ?~  error
    (poke-send-ahoy (slav %p i.way) | force=&)
  ~&  >>>  %ahoy-wake-crash
  ::  XX retry?
  ::
  %-  (slog %take-ahoy-wake-crash u.error)
  abet:(emit %pass `wire`[%helm %ahoy-crash way] %arvo %b %wait (add now.bowl ~s30)) :: XX exp backoff?
::
++  poke-send-rege
  |=  [her=ship test=?]  =<  abet
  =/  =wire
    :+  %helm  %rege
    ?.(test /(scot %p her) /test/(scot %p her))
  =/  =path  ?:(test /test/ames /ames)
  ::  before regressing, test if we can regress, migrate, and check that there
  ::  are not flows in a weird state. if we don't crash, send the %back $plea
  ::
  =^  rege-moves  sat  (poke-mass-rege `her test=%.y)
  =^  back-moves  sat  abet:(emit %pass wire %arvo %a %plea her %$ path %back ~)
  (emil (weld rege-moves back-moves))
::
++  take-rege
  |=  [way=wire error=(unit error:ames)]
  ?:  ?=([%test @ *] way)
    ?~  error
      ~&  >   %rege-test-worked
      ~&  >>  %test-local-rege
      abet:(emit %pass /helm/migrate %arvo %a %rege (slaw %p i.t.way) dry=%.y)
    %-  (slog %take-rege-test-failed u.error)
    abet
  ?>  ?=([@ ~] way)
  ?~  error
      ~&  >   %remote-regress-worked
      ~&  >>  %try-local-rege
    abet:(emit %pass /helm/migrate %arvo %a %rege (slaw %p i.way) dry=%.n)
  ~&  >>>  %rege-crash
  ::  XX retry?
  ::
  %-  (slog %take-rege-failed u.error)
  abet
  :: abet:(emit %pass `wire`[%helm %ahoy-crash way] %arvo %b %wait (add now.bowl ~s30)) :: XX exp backoff?
::
++  poke-hi
  |=  mes=@t  =<  abet
  ~|  %poke-hi-fail
  ?:  =(%fail mes)
    ~&  %poke-hi-fail
    !!
  =+  size=(met 3 mes)
  =+  hash=`@ux`(mug mes)
  %+  flog  %text
  "< {<src.bowl>}: {?:((gth size 100) <[hash=hash size=size]> (trip mes))}"
::
++  poke-ames-prod
  |=  ships=(list ship)
  abet:(emit %pass /helm/prod %arvo %a %prod ships)
::
++  poke-ames-snub
  |=  snub=[form=?(%allow %deny) ships=(list ship)]
  abet:(emit %pass /helm/snub %arvo %a %snub snub)
::
++  poke-atom
  |=  ato=@
  =+  len=(scow %ud (met 3 ato))
  =+  gum=(scow %p (mug ato))
  =<  abet
  (flog %text "< {<src.bowl>}: atom: {len} bytes, mug {gum}")
::
++  coup-hi
  |=  [pax=path cop=(unit tang)]  =<  abet
  ?>  ?=([@t ~] pax)
  (flog %text "hi {(trip i.pax)} {?~(cop "" "un")}successful")
::
++  poke-trim
  |=  pri=@ud  =<  abet
  (emit %pass /pack %arvo %d %flog %crop pri)
::
++  poke-verb                                         ::  toggle verbose
  |=  ~  =<  abet
  (flog %verb ~)
::
++  poke-ames-sift
  |=  ships=(list ship)  =<  abet
  (emit %pass /helm %arvo %a %sift ships)
::
++  poke-ames-verb
  |=  veb=(list verb:ames)  =<  abet
  (emit %pass /helm %arvo %a %spew veb)
::
++  poke-gall-sift
  |=  dudes=(list dude:gall)  =<  abet
  (emit %pass /helm %arvo %g %sift dudes)
::
++  poke-gall-verb
  |=  veb=(list verb:gall)  =<  abet
  (emit %pass /helm %arvo %g %spew veb)
::
++  poke-gall-lave
  |=  [dry=? subs=(list [?(%g %a) ship term duct])]  =<  abet
  ?:  dry  this
  (emit %pass /helm %arvo %g %lave subs)
::
++  poke-eyre-lave
  |=  [dry=? subs=(list [%g ship term duct])]  =<  abet
  ?:  dry  this
  (emit %pass /helm %arvo %g %lave subs)
::
++  poke-ames-wake
  |=  ~  =<  abet
  (emit %pass /helm %arvo %a %stir '')
::
++  poke-ames-kroc
  |=  [dry=? bones=(list [ship bone])]  =<  abet
  ?:  dry  this
  (emit %pass /helm %arvo %a %kroc bones)
::
++  poke-ames-cong
  |=  cong=[msg=@ud mem=@ud]  =<  abet
  (emit %pass /helm %arvo %a %cong cong)
::
++  poke-serve
  |=  [=binding:eyre =generator:eyre]  =<  abet
  (emit %pass /helm/serv %arvo %e %serve binding generator)
::
++  poke-cors-approve
  |=  =origin:eyre
  =<  abet
  (emit %pass /helm/cors/approve %arvo %e %approve-origin origin)
::
++  poke-cors-reject
  |=  =origin:eyre
  =<  abet
  (emit %pass /helm/cors/reject %arvo %e %reject-origin origin)
::
++  poke-doff
  |=  [dude=(unit dude:gall) ship=(unit ship)]
  =<  abet
  (emit %pass /helm/doff %arvo %g %doff dude ship)
::
++  poke-dns-config
  |=  [addr=(each address:dns @t) collector=dock self-check=? reset=?]
  =<  abet
  =;  =shed:khan
    (emit %pass /helm/dns-config %arvo %k %lard %base shed)
  |^  ^-  shed:khan
  =/  m  (strand:rand ,vase)
  ;<  ip=@if        bind:m  get-ip
  ;<  ~             bind:m  (check-ip ip)
  ;<  =binding:dns  bind:m  (binding-request ip)
  ;<  ~             bind:m  (check-domain turf.binding)
  %-  (slog 'dns: installing domain' ~)
  ;<  ~             bind:m  (install-domain turf.binding reset)
  %-  (slog 'dns: acme will handle any necessary certificate configuration' ~)
  (pure:m !>(~))
  ::
  ++  install-domain
    |=  [=turf reset=?]
    =/  m  (strand:rand ,~)
    ^-  form:m
    ?.  reset
      (send-raw-card:strandio %pass / %arvo %e %rule %turf %put turf)
    (send-raw-card:strandio %pass / %arvo %e %rule %turf %new (silt turf ~))
  ::
  ++  check-domain
    |=  =turf
    =/  m  (strand:rand ,~)
    ^-  form:m
    ?.  self-check
      %-  (slog 'dns: skipping domain self-check' ~)
      (pure:m ~)
    %-  (slog 'dns: checking if domain is bound and accessible' ~)
    ;<  ~  bind:m  (check-loop [%& turf] 30)
    %-  (slog 'dns: domain is bound and accessible' ~)
    (pure:m ~)
  ::
  ++  check-ip
    |=  if=@if
    =/  m  (strand:rand ,~)
    ^-  form:m
    ?.  self-check
      %-  (slog 'dns: skipping IP self-check' ~)
      (pure:m ~)
    %-  (slog 'dns: checking if IP accessible on port 80' ~)
    ;<  ~  bind:m  (check-loop [%| if] 2)
    %-  (slog 'dns: IP is accessible on port 80' ~)
    (pure:m ~)
  ::
  ++  check-loop
    |=  [=host:eyre max=@ud]
    =/  m  (strand:rand ,~)
    ^-  form:m
    =/  =hiss:eyre
      =/  url=purl:eyre
        [[sec=| por=~ host] [ext=~ path=/~/name] query=~]
      [url %get ~ ~]
    =/  try=@ud  0
    |-  ^-  form:m
    =*  loop  $
    ?:  =(try max)
      ?:  ?=([%& *] host)
        (strand-fail:rand %dns-domain-check-fail ~)
      (strand-fail:rand %dns-ip-check-fail ~)
    ;<  ~                     bind:m  (sleep:strandio ?:(=(try 0) ~s0 ~s30))
    ;<  rep=(unit httr:eyre)  bind:m  (hiss-request:strandio hiss)
    ?:  ?&  ?=(^ rep)
            |(=(200 p.u.rep) =(307 p.u.rep) =(301 p.u.rep))
        ==
      (pure:m ~)
    ?.  ?|  ?=(~ rep)
            =(504 p.u.rep)
        ==
      ?:  ?=([%& *] host)
        (strand-fail:rand %dns-domain-check-fail ~)
      (strand-fail:rand %dns-ip-check-fail ~)
    =/  msg=tang
      ?:  =(max +(try))
        ~
      =/  target=@t
        ?:  ?=([%& *] host)
          (en-turf:html p.host)
        (crip (tail (scow %if p.host)))
      :~  %+  rap  3
          :~  'dns: self-check for '
              target
              ' failed. Retrying in 30s ('
              (crip (a-co:co +(try)))
              '/'
              (crip (a-co:co max))
              ')'
      ==  ==
    %-  (slog msg)
    loop(try +(try))
  ::
  ++  binding-request
    |=  if=@if
    =/  m  (strand:rand ,binding:dns)
    ^-  form:m
    ;<  our=@p  bind:m  get-our:strandio
    %-  (slog (cat 3 'dns: sending DNS request to ' (scot %p p.collector)) ~)
    ;<  ~       bind:m  (watch:strandio /response collector /(scot %p our))
    ;<  ~       bind:m  (poke:strandio collector %dns-address !>([%if if]))
    %-  (slog (cat 3 'dns: awaiting response from ' (scot %p p.collector)) ~)
    ;<  =cage   bind:m  (take-fact:strandio /response)
    ?>  ?=(%dns-binding p.cage)
    =/  =binding:dns  !<(binding:dns q.cage)
    ?:  !=(if if.address.binding)
      (strand-fail:rand %dns-ip-mismatch ~)
    %-  %-  slog
        :~  %+  rap  3
            :~  'dns: '
                (scot %p p.collector)
                ' confirmed DNS entry from '
                (en-turf:html turf.binding)
                ' to '
                (crip (tail (scow %if if.address.binding)))
        ==  ==
    (pure:m binding)
  ::
  ++  get-ip
    =/  m  (strand:rand ,@if)
    ^-  form:m
    ;<  our=@p  bind:m  get-our:strandio
    ?.  ?=(?(%king %duke) (clan:title our))
      (strand-fail:rand %dns-rank-error ~)
    ?:  ?=(%& -.addr)
      ?:  (reserved:eyre if.p.addr)
        (strand-fail:rand %dns-ip-reserved ~)
      (pure:m if.p.addr)
    =/  =request:http
      [%'GET' p.addr ~ ~]
    %-  (slog 'dns: trying to discover our IP address' ~)
    ;<  ~  bind:m  (send-request:strandio request)
    ;<  res=client-response:iris  bind:m
      %+  (set-timeout:strandio client-response:iris)
      ~s20  take-client-response:strandio
    ?>  ?=([%finished *] res)
    ?.  =(200 status-code.response-header.res)
      %+  strand-fail:rand  %dns-ip-bad-status
      [leaf+(a-co:co status-code.response-header.res) ~]
    ?~  full-file.res
      (strand-fail:rand %dns-ip-no-body ~)
    =/  parsed=(unit @if)
      %+  rush  q.data.u.full-file.res
      =+  tod=(sear |=(a=@ `(unit @)`?:((gth a 255) ~ `a)) (ape:ag ted:ab))
      %+  ifix  [(star ace) (star ;~(pose ace (just '\0a')))]
      (bass 256 ;~(plug tod (stun [3 3] ;~(pfix dot tod))))
    ?~  parsed
      =/  v6=(unit)
        %+  rush  q.data.u.full-file.res
        =+  tod=(bass 16 ;~(plug six:ab (stun [0 3] six:ab)))
        %+  ifix  [(star ace) (star ;~(pose ace (just '\0a')))]
        (bass 65.536 ;~(plug tod (stun [7 7] ;~(pfix col tod))))
      ?^  v6
        (strand-fail:rand %dns-ip-ipv6 ~)
      (strand-fail:rand %dns-ip-parse-fail ~)
    ?:  (reserved:eyre u.parsed)
      (strand-fail:rand %dns-ip-got-reserved ~)
    %-  (slog (cat 3 'dns: discovered IP address ' q.data.u.full-file.res) ~)
    (pure:m u.parsed)
  --
::
++  take-arow-dns-config
  |=  res=(avow:khan cage)
  ?:  ?=(%& -.res)  abet
  =;  =tang
    ((slog tang) abet)
  ?.  ?=(^ tang.p.res)
    ~[(rap 3 'dns: %' mote.p.res ' error!' ~)]
  ?+    i.tang.p.res
      ?.  ?=(@ i.tang.p.res)
        ~[(rap 3 'dns: %' mote.p.res ' error!' ~)]
      ~[(rap 3 'dns: %' i.tang.p.res ' error!' ~)]
  ::
      %dns-domain-check-fail
    :~  'dns: %dns-domain-check-fail error!'
        'dns: Domain self-check failed, your urbit cannot reach itself on'
        'dns: port 80. DNS update propagation may have been slow, and you'
        'dns: can try again later, or else something is misconfigured. If'
        'dns: you have a strange setup with a reverse-proxy or your router'
        'dns: does not support NAT hairpinning and you are sure it is fine,'
        'dns: you can try run this again and skip self-checks.'
    ==
  ::
      %dns-ip-check-fail
    :~  'dns: %dns-ip-check-fail error!'
        'dns: IP self-check failed, your urbit cannot reach itself on port 80.'
        'dns: Something may be misconfigured. If you have a strange setup with'
        'dns: a reverse-proxy or your router does not support NAT hairpinning'
        'dns: and you are sure it is fine, you can try run this again and skip'
        'dns: self-checks.'
    ==
  ::
      %dns-ip-mismatch
    :~  'dns: %dns-ip-mismatch error!'
        'dns: The remote DNS collector app confirmed a binding for a'
        'dns: different IP address than the one requested. This should not'
        'dns: happen.'
    ==
  ::
      %dns-rank-error
    :~  'dns: %dns-rank-error error!'
        'dns: Only Planets and Stars may use this service.'
    ==
  ::
      %dns-ip-reserved
    :~  'dns: %dns-ip-reserved error!'
        'dns: you have tried to bind a reserved IP address. The IP address'
        'dns: must be a normal public IPv4 address. If this has happened'
        'dns: with automatic IP address discovery, you may need to specify'
        'dns: your IP address explicitly.'
    ==
  ::
      %dns-ip-bad-status
    :~  'dns: %dns-ip-bad-status error!'
        'dns: The attempt at discovering our IP address failed. We received an'
        (cat 3 'dns: HTTP error code of' ?>(?=([@ ~] tang.p.res) i.tang.p.res))
        'dns: You may wish to try again and explicitly specify the IP address.'
    ==
  ::
      %dns-ip-no-body
    :~  'dns: %dns-ip-no-body error!'
        'dns: The attempt at discovering our IP address failed. We received an'
        'dns: HTTP response with an empty body. You may wish to try again and'
        'dns: explicitly specify the IP address.'
    ==
  ::
      %dns-ip-ipv6
    :~  'dns: %dns-ip-ipv6 error!'
        'dns: The IP address discovered was an IPv6 address. Only IPv4'
        'dns: addresses are currently supported. You may wish to try again'
        'dns: and explicitly specify the IP address.'
    ==
  ::
      %dns-ip-parse-fail
    :~  'dns: %dns-ip-parse-fail error!'
        'dns: The attempt at discovering our IP address failed because the'
        'dns: response was malformed. You may wish to try again and explicitly'
        'dns: specify the IP address.'
    ==
  ::
      %dns-ip-got-reserved
    :~  'dns: %dns-ip-got-reserved error!'
        'dns: The attempt at discovering our IP address failed because the'
        'dns: IP we received is in a reserved range. You may wish to try again'
        'dns: and explicitly specify the IP address.'
    ==
  ::
      %http-request-cancelled
    :~  'dns: %http-request-cancelled error!'
        'dns: The attempt at discovering our IP address failed because the'
        'dns: HTTP request failed. You may wish to try again and explicitly'
        'dns: specify the IP address.'
    ==
  ::
      %timeout
    :~  'dns: %timeout error!'
        'dns: The attempt at discovering our IP address failed because the'
        'dns: request timed out. You may wish to try again and explicitly'
        'dns: specify the IP address.'
    ==
  ==
::
++  poke
  |=  [=mark =vase]
  ?>  ?|  ?=(%helm-hi mark)
          ?=(%helm-moon-breach mark)
          =(our src):bowl
      ==
  ?+  mark  ~|([%poke-helm-bad-mark mark] !!)
    %helm-ames-prod        =;(f (f !<(_+<.f vase)) poke-ames-prod)
    %helm-ames-snub        =;(f (f !<(_+<.f vase)) poke-ames-snub)
    %helm-ames-sift        =;(f (f !<(_+<.f vase)) poke-ames-sift)
    %helm-ames-verb        =;(f (f !<(_+<.f vase)) poke-ames-verb)
    %helm-ames-wake        =;(f (f !<(_+<.f vase)) poke-ames-wake)
    %helm-ames-kroc        =;(f (f !<(_+<.f vase)) poke-ames-kroc)
    %helm-ames-cong        =;(f (f !<(_+<.f vase)) poke-ames-cong)
    %helm-atom             =;(f (f !<(_+<.f vase)) poke-atom)
    %helm-automass         =;(f (f !<(_+<.f vase)) poke-automass)
    %helm-cancel-automass  =;(f (f !<(_+<.f vase)) poke-cancel-automass)
    %helm-code             =;(f (f !<(_+<.f vase)) poke-code)
    %helm-cors-approve     =;(f (f !<(_+<.f vase)) poke-cors-approve)
    %helm-cors-reject      =;(f (f !<(_+<.f vase)) poke-cors-reject)
    %helm-doff             =;(f (f !<(_+<.f vase)) poke-doff)
    %helm-gall-sift        =;(f (f !<(_+<.f vase)) poke-gall-sift)
    %helm-gall-verb        =;(f (f !<(_+<.f vase)) poke-gall-verb)
    %helm-gall-lave        =;(f (f !<(_+<.f vase)) poke-gall-lave)
    %helm-eyre-lave        =;(f (f !<(_+<.f vase)) poke-eyre-lave)
    %helm-hi               =;(f (f !<(_+<.f vase)) poke-hi)
    %helm-pans             =;(f (f !<(_+<.f vase)) poke-pans)
    %helm-mass             =;(f (f !<(_+<.f vase)) poke-mass)
    %helm-meld             =;(f (f !<(_+<.f vase)) poke-meld)
    %helm-moon             =;(f (f !<(_+<.f vase)) poke-moon)
    %helm-moon-breach      =;(f (f !<(_+<.f vase)) poke-moon-breach)
    %helm-pack             =;(f (f !<(_+<.f vase)) poke-pack)
    %helm-pass             =;(f (f !<(_+<.f vase)) poke-pass)
    %helm-rekey            =;(f (f !<(_+<.f vase)) poke-rekey)
    %helm-send-hi          =;(f (f !<(_+<.f vase)) poke-send-hi)
    %helm-send-ahoy        =;(f (f !<(_+<.f vase)) poke-send-ahoy)
    %helm-mass-mate        =;(f (f !<(_+<.f vase)) poke-mass-mate)
    %helm-send-rege        =;(f (f !<(_+<.f vase)) poke-send-rege)
    %helm-mass-rege        =;(f (f !<(_+<.f vase)) poke-mass-rege)
    %helm-serve            =;(f (f !<(_+<.f vase)) poke-serve)
    %helm-trim             =;(f (f !<(_+<.f vase)) poke-trim)
    %helm-verb             =;(f (f !<(_+<.f vase)) poke-verb)
    %helm-write-sec-atom   =;(f (f !<(_+<.f vase)) poke-sec-atom)
    %helm-dns-config       =;(f (f !<(_+<.f vase)) poke-dns-config)
  ==
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+    wire  ~|([%helm-bad-take-agent wire -.sign] !!)
      [%hi *]
    ?>  ?=(%poke-ack -.sign)
    (coup-hi t.wire p.sign)
  ==
::
++  take-bound
  |=  [wir=wire success=? binding=binding:eyre]  =<  abet
  (flog %text "bound: {<success>}")
::
++  take-arvo
  |=  [=wire =sign-arvo]
  ?+  wire  ~|([%helm-bad-take-wire wire +<.sign-arvo] !!)
    [%dns-config ~]   %-  take-arow-dns-config
                      ?>(?=(%arow +<.sign-arvo) +>.sign-arvo)
    [%automass *]     %+  take-wake-automass  t.wire
                      ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
    [%serv *]         %+  take-bound  t.wire
                      ?>(?=(%bound +<.sign-arvo) +>.sign-arvo)
    [%moon-breach *]  %+  take-wake-moon-breach  t.wire
                      ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
    [%ahoy *]         %+  take-ahoy  t.wire
                      ?>(?=(%done +<.sign-arvo) +>.sign-arvo)
    [%rege *]         %+  take-rege  t.wire
                      ?>(?=(%done +<.sign-arvo) +>.sign-arvo)
    [%ahoy-crash *]   %+  take-ahoy-crash  t.wire
                      ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
    [%pass *]         abet
  ==
--
