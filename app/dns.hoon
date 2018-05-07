::
:: moves and state
::
|%
+=  move  (pair bone card)
+=  poke  $%  [%dns-bind ship target]
              [%dns-bound ship ship turf]
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
  |=  [him=ship tar=target]
  ^-  (quip move _this)
  ~&  [%bind src=src.bow him=him tar=tar]
  ?:  ?=(^ nem)
    :: XX bind & forward?
    abet:(~(create bind u.nem) him tar)
  abet:(~(forward tell him ~) tar)
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
++  poke-dns-bound                                    ::  confirm or forward
  |=  [him=ship for=ship dom=turf]
  ^-  (quip move _this)
  ~&  [%bound +<]
  ?.  =(our.bow for)
    abet:(backward:tell him for dom)
  =<  abet
  (~(bake tell [him (~(get by per) him)]) dom)
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
  ++  init
    |=  aut=authority
    ::  XX confirm credentials
    ::  XX confirm zone
    this(nam [aut ~ ~])
  ::
  ++  create
    |=  [him=ship tar=target]
    this :: XX
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
    =/  ip=(unit @if)
      ?.(?=([%if *] lan) ~ `r.lan)
    =.  rel  `[now.bow ip |]
    %-  emit
    :*  %poke
        /bind/(scot %p him)/for/(scot %p our.bow)
        [(sein:title our.bow) %dns]
        %dns-bind
        [him ?~(ip [%indirect our.bow] [%direct %if u.ip])]
    ==
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
    %-  emit
    :*  %poke
        /foward/bind/(scot %p him)/for/(scot %p src.bow)
        [to %dns]
        [%dns-bind him tar]
    ==
  ::
  ++  backward                                        ::  relay binding ack
    |=  [him=ship for=ship dom=turf]
    ^+  this
    %-  emit
    :*  %poke
        /foward/bound/(scot %p him)/for/(scot %p for)
        [for %dns]
        [%dns-bound him for dom]
    ==
  --
--
