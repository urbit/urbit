::
:: moves and state
::
|%
+=  move  (pair bone card)
+=  poke  $%  [%dns-bind for=ship him=ship target]
              [%dns-bond for=ship him=ship turf]
          ==
+=  card  $%  [%tend wire ~]
              [%poke wire dock poke]
          ==
:: +turf: a domain, TLD first
::
+=  turf  (list @t)
:: +authority: responsibility for a DNS zone
::
+=  authority
  $:  :: dom: authority over a domain
      ::
      dom=turf
      :: zon: DNS zone name
      ::
      zon=@t
      :: pro: DNS provider (gcloud only for now)
      ::
      pro=%gcloud
  ==
:: +target: a ship is bound to a ...
::
+=  target
  $%  :: %direct: an A record
      [%direct %if p=@if]
      :: %indirect: a CNAME
      [%indirect p=ship]
  ==
:: +bound: an established binding, plus history
::
+=  bound
  $:  :: wen: established
      ::
      wen=@da
      :: cur: current target
      ::
      cur=target
      :: hit: historical targets
      ::
      hit=(list (pair @da target))
  ==
:: +nameserver: a b s o l u t e  p o w e r
::
+=  nameserver
  $:  aut=authority
      pen=(map ship target)
      bon=(map ship bound)
  ==
:: +relay: a good parent keeps track
::
+=  relay
  $:  wen=@da
      wer=(unit @if)
      bon=?
      tar=target
  ==
:: +state: complete app state
::
+=  state
  $:  :: dom: the set of our bindings
      ::
      dom=(set turf)
      :: per: per-dependent ips &c
      ::
      per=(map ship relay)
      :: nem: authoritative state
      ::
      nem=(unit nameserver)
  ==     
--
::
|_  [bow=bowl:gall state]
++  this  .
::
++  poke-noun
  |=  a=*
  ::^-  (quip move _this)
  ~&  +<+:this
  [~ this]
::
++  poke-dns-authority                                ::  configure
  |=  aut=authority
  ^-  (quip move _this)
  ~|  %authority-reset-wat-do
  ?<  ?=(^ nem)
  abet:(init:bind aut)
::
++  poke-dns-bind                                     ::  bind or forward
  |=  [for=ship him=ship tar=target]
  ^-  (quip move _this)
  ~&  [%bind src=src.bow him=him tar=tar]
  =^  zom=(list move)  ..this
  ^-  (quip move _this)
    abet:(~(forward tell him ~) tar)
  ?~  nem
    [zom this]
  :: XX =^ nest-fail
  =/  zam=(quip move _this)
    abet:(~(create bind u.nem) him tar)
  [(weld zom -.zam) +.zam]
::
:: ++  coup-dns-bind                                     ::  retry?
::   |=  [wir=wire saw=(unit tang)]
::   ~&  [%coup-bind +<]
::   ?~  saw
::     [~ this]
::   ?>  ?=(^ wir)
::   ?-  i.wir
::     %forward  !!  :: re-forward?
::     %bind     !!  :: rebind?
::     *  ~&(coup-dns-bind+wir [~ this])
::   ==
::
++  poke-dns-bond                                    ::  confirm or forward
  |=  [for=ship him=ship dom=turf]
  ^-  (quip move _this)
  ?<  =(for him)
  ~&  [%bound +<]
  ?:  =(our.bow him)
    :: XX notify eyre/hood/acme etc
    ~&  [%bound-us dom]
    :-  ~
    this(dom (~(put in ^dom) dom))
  ?:  =(our.bow for)
    ~&  [%bound-him him dom]
    =<  abet
    (~(bake tell [him (~(get by per) him)]) dom)
  ~&  [%strange-bond +<]
  [~ this]
::
++  rove                                              ::  hear lane change
  |=  [wir=wire p=ship q=lane:ames]
  ^-  (quip move _this)
  ?.  =(our.bow (sein:title p))  :: XX check will
    ~&  [%rove-false p]
    [~ this]
  ~&  [%rove wir p q]
  ::  XX assert that we intend to be listening?
  =<  abet
  (~(rove tell [p (~(get by per) p)]) q)
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?^  old
    [~ this(+<+ u.old)]
  ?:  ?=(?(%czar %king) (clan:title our.bow))
    abet:tend:tell
  [~ this]
::
::  acting as zone authority
::
++  bind                                              ::  nameserver
  =|  moz=(list move)
  |_  nam=nameserver
  ++  this  .
  ::
  ++  abet
    ^-  (quip move _^this)
    [(flop moz) ^this(nem `nam)]
  ::
  ++  emit
    |=  a=card
    ^+  this
    this(moz [[ost.bow a] moz])
  ::
  ++  init
    |=  aut=authority
    ::  XX confirm credentials
    ::  XX confirm zone
    this(nam [aut ~ ~])
  ::
  ++  create
    |=  [him=ship tar=target]
    =|  for=@p :: XX
    %-  emit
    :*  %poke
        /foward/bound/(scot %p him)/for/(scot %p for)
        [for %dns]
        [%dns-bond him for *turf]
    ==
  ::
  ++  confirm
    |=  him=ship
    this :: XX
  --
::
::  acting as planet parent or relay
::
++  tell                                              ::  relay
  =|  moz=(list move)
  |_  [him=ship rel=(unit relay)]
  ++  this  .
  ::
  ++  abet
    ^-  (quip move _^this)
    :-  (flop moz)
    ?~  rel
      ^this
    ^this(per (~(put by per) him u.rel))
  ::
  ++  emit
    |=  a=card
    ^+  this
    this(moz [[ost.bow a] moz])
  ::
  ++  tend                                            ::  listen
    ^+  this
    (emit [%tend /tend ~])
  ::
  ++  rove                                            ::  hear
    |=  lan=lane:ames
    ^+  this
    =/  adr=(unit @if)
      ?.(?=([%if *] lan) ~ `r.lan)
    =/  tar=target
      ?:  ?|  ?=(~ adr)
              ?=(%duke (clan:title him))
          ==
        [%indirect our.bow]
      [%direct %if u.adr]
    =/  ler=relay
      [now.bow adr | tar]
    ?.  ?|  ?=(~ rel)
            !=(tar tar.u.rel)
        ==
      this
    :: we may be an authority, so we poke ourselves
    =/  wir=wire
      /bind/(scot %p him)/for/(scot %p our.bow)
    %-  emit(rel `ler)
    [%poke wir [our.bow %dns] %dns-bind our.bow him tar]
  ::
  ++  bake                                            ::  bound
    |=  dom=turf
    ^+  this
    ?>  ?=(^ rel)
    =.  bon.u.rel  &
    :: XX save domain?
    :: XX notify ship?
    this
  ::
  ++  forward                                         ::  on to parent
    |=  tar=target
    ^+  this
    ?:  ?=(%~zod our.bow)  ::  ~zod don't forward
      ~&  [%zod-no-forward him tar]
      this
    =/  to=ship
      ?-  (clan:title our.bow)
        %czar  ~zod
        *      (sein:title our.bow)
      ==
    =/  wir=wire
      /forward/bind/(scot %p him)/for/(scot %p src.bow)
    %-  emit  :: XX for
    [%poke wir [to %dns] %dns-bind src.bow him tar]
  --
--
