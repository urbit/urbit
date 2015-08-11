section 3bI, Arvo models
========================

### `++acru`

    ++  acru                                                ::  asym cryptosuite
              $_  ^?  |%                                    ::  opaque object

Cryptosuite interface, see %ames documentation

    ~zod/main=> `acru`crua
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>
    ~zod/main=> `acru`crub
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>
    ~zod/main=> *acru
    <6?guc 243.nxm 41.spz 374.iqw 100.rip 1.ypj %164>

### `++as`

              ++  as  ^?                                    ::  asym ops
                |%  ++  seal  |=([a=pass b=@ c=@] _@)       ::  encrypt to a

XX document

### `++seal`

    ++  seal                                                ::  auth conversation
              $:  whu=(unit ship)                           ::  client identity
                  pul=purl                                  ::  destination url
                  wit=?                                     ::  wait for partner
                  foy=(unit ,[p=ship q=hole])               ::  partner to notify
                  pus=(unit ,@ta)                           ::  password
              ==                                            ::

XX document

### `++sign`

                    ++  sign  |=([a=@ b=@] _@)              ::  certify as us

XX document

### `++sure`

                    ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate from us

XX document

### `++tear`

                    ++  tear  |=  [a=pass b=@]              ::  accept from a 
                              *(unit ,[p=@ q=@])            ::

XX document

### `++de`

              ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft

XX document

### `++dy`

              ++  dy  |+([a=@ b=@] _@)                      ::  symmetric de, hard

XX document

### `++en`

              ++  en  |+([a=@ b=@] _@)                      ::  symmetric en

XX document

### `++ex`

              ++  ex  ^?                                    ::  export
                |%  ++  fig  _@uvH                          ::  fingerprint

XX document

### `++fig`


    XX document

    ###++pac

    ```
                    ++  pac  _@uvG                          ::  default passcode
    ```

    XX document

    ###++pub

    ```
                    ++  pub  *pass                          ::  public key
    ```

    XX document

    ###++sec

    ```
                    ++  sec  *ring                          ::  private key
    ```

    XX document

    ###++nu

    ```
              ++  nu  ^?                                    ::  reconstructors
                 |%  ++  pit  |=([a=@ b=@] ^?(..nu))        ::  from [width seed]
    ```

    XX document

    ###++pit

XX document

### `++nol`

                     ++  nol  |=(a=@ ^?(..nu))              ::  from naked ring

XX document

### `++com`

                     ++  com  |=(a=@ ^?(..nu))              ::  from naked pass

XX document

### `++aeon`

    ++  aeon  ,@ud                                          ::

Clay revision number

### `++agon`

    ++  agon  (map ,[p=ship q=desk] ,[p=@ud q=@ud r=waks])  ::  mergepts

See %clay doc

    ~zod/main=> *agon
    {}

### `++ankh`

    ++  ankh                                                ::  fs node (new)
              $:  p=cash                                    ::  recursive hash
                  q=(unit ,[p=cash q=*])                    ::  file
                  r=(map ,@ta ankh)                         ::  folders
              ==                                            ::

State at path

See also ++ze, %clay documentation

### `++ankz`

    ++  ankz  ,[p=@ (map ,@ta ankz)]                        ::  trimmed ankh

XX document

### `++apex`

    ++  apex  ,[p=@uvI q=(map ,@ta ,@uvI) r=(map ,@ta ,~)]  ::  node report (old)

XX document

### `++ares`

    ++  ares  (unit ,[p=term q=(list tank)])                ::  possible error

Failure cause: unknown, or machine-readable term and stack trace.

    ~zod/main=> `ares`~
    ~
    ~zod/main=> `ares`[~ %syntax-error leaf/"[1 27]" ~]
    [~ [p=%syntax-error q=~[[%leaf p="[1 27]"]]]]
    ~zod/main=> 

### `++ball`

    ++  ball  ,@uw                                          ::  statement payload

XX document

### `++bait`

    ++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec

XX document

### `++bath`

    ++  bath                                                ::  convo per client
              $:  sop=shed                                  ::  not stalled
                  raz=(map path race)                       ::  statements inbound
                  ryl=(map path rill)                       ::  statements outbound
              ==                                            ::

XX document

### `++beam`

    ++  beam  ,[[p=ship q=desk r=case] s=path]              ::  global name

See section 2dF, %clay documentation

    ~zod/try=> (need (tome %/bin))
    [[p=~zod q=%try r=[%da p=~2014.11.3..17.30.07..ca8f]] s=/bin]

### `++beak`

    ++  beak  ,[p=ship q=desk r=case]                       ::  garnish with beak

Global root

### `++bird`

    ++  bird                                                ::  packet in travel
              $:  gom=soap                                  ::  message identity
                  mup=@ud                                   ::  pktno in msg
                  nux=@ud                                   ::  xmission count
                  lys=@da                                   ::  last sent
                  pac=rock                                  ::  packet data
              ==                                            ::

XX document

### `++blob`

    ++  blob  $%  [%delta p=lobe q=lobe r=udon]             ::  delta on q
                  [%direct p=lobe q=* r=umph]               ::
                  [%indirect p=lobe q=* r=udon s=lobe]      ::
              ==                                            ::

Stored data, see ++ze

### `++boat`

    ++  boat  ,[(list slip) tart]                           ::  user stage

XX deprecated

### `++boon`

    ++  boon                                                ::  fort output
              $%  [%beer p=ship q=@uvG]                     ::  gained ownership
                  [%cake p=sock q=soap r=coop s=duct]       ::  e2e message result
                  [%coke p=sock q=soap r=cape s=duct]       ::  message result
                  [%mead p=lane q=rock]                     ::  accept packet
                  [%milk p=sock q=soap r=*]                 ::  accept message
                  [%mulk p=sock q=soap r=*]                 ::  e2e pass message
                  [%ouzo p=lane q=rock]                     ::  transmit packet
                  [%wine p=sock q=tape]                     ::  notify user
              ==                                            ::

See %ford documentation

### `++bowl`

    ++  bowl  ,[p=(list gift) q=(unit boat)]                ::  app product

XX deprecated

### `++bray`

    ++  bray  ,[p=life q=(unit life) r=ship s=@da]          ::  our parent us now

Ship identity. See %ames documentation

### `++brow`

    ++  brow  ,[p=@da q=@tas]                               ::  browser version

XX unused?

### `++buck`

    ++  buck  ,[p=mace q=will]                              ::  all security data

XX document

### `++cake`

    ++  cake  ,[p=sock q=skin r=@]                          ::  top level packet

XX document

### `++cape`

    ++  cape                                                ::  end-to-end result
              $?  %good                                     ::  delivered
                  %dead                                     ::  rejected
              ==                                            ::

XX document

### `++cart`

    ++  cart  ,[p=cash q=cash]                              ::  hash change

XX document

### `++care`

    ++  care  ?(%u %v %w %x %y %z)                          ::  clay submode

XX document

### `++case`

    ++  case                                                ::  ship desk case spur
              $%  [%da p=@da]                               ::  date
                  [%tas p=@tas]                             ::  label
                  [%ud p=@ud]                               ::  number
              ==                                            ::

Access by absolute date, term label, or revision number. See %clay
documentation

### `++cash`

    ++  cash  ,@uvH                                         ::  ankh hash

XX document

### `++chum`

    ++  chum  ,@uvI                                         ::  hashed passcode

XX document

### `++clot`

    ++  clot                                                ::  symmetric record
              $:  yed=(unit ,[p=hand q=code])               ::  outbound
                  heg=(map hand code)                       ::  proposed
                  qim=(map hand code)                       ::  inbound
              ==                                            ::

XX document

### `++coal`

    ++  coal  ,*                                            ::  untyped vase

XX document

### `++code`

    ++  code  ,@uvI                                         ::  symmetric key

XX document

### `++cone`

    ++  cone                                                ::  reconfiguration
              $%  [& p=twig]                                ::  transform
                  [| p=(list ,@tas)]                        ::  alter
              ==                                            ::

XX document

### `++chum`

    ++  chum  ,@uvI                                         ::  hashed passcode

XX document

### `++claw`

    ++  claw                                                ::  startup chain
              $:  joy=(unit coal)                           ::  local context
                  ran=(unit coal)                           ::  arguments
                  pux=(unit path)                           ::  execution path
                  jiv=(unit coal)                           ::  app configuration
                  kyq=(unit coal)                           ::  app customization
                  gam=(unit coal)                           ::  app image
              ==                                            ::

XX document

### `++clip`

    ++  clip  (each ,@if ,@is)                              ::  client IP

See %eyre documentation.

    ~zod/try=> `clip`[%& .127.0.0.1]
    [%.y p=.127.0.0.1]
    ~zod/try=> `clip`[%| .12.0.0.0.342d.d24d.0.0]
    [%.n p=.12.0.0.0.342d.d24d.0.0]
    ~zod/try=> `clip`[%| .0.0.0.1]
    ! type-fail
    ! exit

### `++coal`

    ++  coal  ,*                                            ::  untyped vase

XX document

### `++code`

    ++  code  ,@uvI                                         ::  symmetric key

XX document

### `++cone`

    ++  cone                                                ::  reconfiguration
              $%  [& p=twig]                                ::  transform
                  [| p=(list ,@tas)]                        ::  alter
              ==                                            ::

XX document

### `++coop`

    ++  coop  (unit ares)                                   ::  e2e ack

XX document

### `++corn`

    ++  corn                                                ::  flow by server
              $:  hen=duct                                  ::  admin channel
                  nys=(map flap bait)                       ::  packets incoming
                  olz=(map flap cape)                       ::  packets completed
                  wab=(map ship bath)                       ::  relationship
              ==                                            ::

XX document

### `++cred`

    ++  cred                                                ::  credential
              $:  hut=hoot                                  ::  client host
                  aut=(jug ,@tas ,@t)                       ::  client identities
                  orx=oryx                                  ::  CSRF secret
                  acl=(unit ,@t)                            ::  accept-language
                  cip=(each ,@if ,@is)                      ::  client IP
                  cum=(map ,@tas ,*)                        ::  custom dirt
              ==                                            ::

XX document

### `++cuff`

    ++  cuff                                                ::  permissions
              $:  p=(unit (set monk))                       ::  readers
                  q=(set monk)                              ::  authors
              ==                                            ::

XX document

### `++deed`

    ++  deed  ,[p=@ q=step r=?]                             ::  sig, stage, fake?

XX document

### `++dome`

    ++  dome                                                ::  project state
              $:  ang=agon                                  ::  pedigree
                  ank=ankh                                  ::  state
                  let=@ud                                   ::  top id
                  hit=(map ,@ud tako)                       ::  changes by id
                  lab=(map ,@tas ,@ud)                      ::  labels
              ==                                            ::

XX document

### `++dore`

    ++  dore                                                ::  foreign contact
              $:  wod=road                                  ::  connection to
                  wyl=will                                  ::  inferred mirror
                  caq=clot                                  ::  symmetric key state
              ==                                            ::

XX document

### `++dove`

    ++  dove  ,[p=@ud q=(map ,@ud ,@)]                      ::  count hash 13-blocks

XX document

### `++epic`

    ++  epic                                                ::  FCGI parameters
              $:  qix=(map ,@t ,@t)                         ::  query
                  ced=cred                                  ::  client credentials
                  bem=beam                                  ::  original path
                  but=path                                  ::  ending
                  nyp=path                                  ::  request model
              ==                                            ::

XX document

### `++flap`

    ++  flap  ,@uvH                                         ::  network packet id

XX document

### `++flow`

    ++  flow                                                ::  packet connection
              $:  rtt=@dr                                   ::  decaying avg rtt
                  wid=@ud                                   ::  logical wdow msgs
              ==                                            ::

XX document

### `++fort`

    ++  fort                                                ::  formal state
              $:  %0                                        ::  version
                  gad=duct                                  ::  client interface
                  hop=@da                                   ::  network boot date
                  ton=town                                  ::  security
                  zac=(map ship corn)                       ::  flows by server
              ==                                            ::

XX document

### `++frog`

    ++  frog  ,[p=@da q=nori]                               ::  time and change

XX document

### `++gank`

    ++  gank  (each vase (list tank))                       ::  abstract result

XX document

### `++gift`

    ++  gift                                                ::  one-way effect
              $%  [%$ p=vase]                               ::  trivial output
                  [%cc p=(unit case)]                       ::  change case
                  [%ck p=@tas]                              ::  change desk
                  [%cs p=path]                              ::  change spur
                  [%de p=@ud q=tank]                        ::  debug/level
                  [%ex p=(unit vase) q=lath]                ::  exec/patch
                ::[%fd p=vase]                              ::  fundamental down
                ::[%fo p=vase]                              ::  fundamental forward
                ::[%fu p=vase]                              ::  fundamental up
                  [%ha p=tank]                              ::  single error
                  [%ho p=(list tank)]                       ::  multiple error
                  [%la p=tank]                              ::  single statement
                  [%lo p=(list tank)]                       ::  multiple statement
                  [%mu p=type q=(list)]                     ::  batch emit
                  [%mx p=(list gift)]                       ::  batch gift
                  [%ok p=@ta q=nori]                        ::  save changes
                  [%og p=@ta q=mizu]                        ::  save direct
                  [%sc p=(unit skit)]                       ::  stack library
                  [%sp p=(list lark)]                       ::  spawn task(s)
                  [%sq p=ship q=@tas r=path s=*]            ::  send request
                  [%sr p=ship q=path r=*]                   ::  send response
                  [%te p=(list ,@t)]                        ::  dump lines
                  [%th p=@ud q=love]                        ::  http response
                  [%tq p=path q=hiss]                       ::  http request
                  [%va p=@tas q=(unit vase)]                ::  set/clear variable
                  [%xx p=curd]                              ::  return card
                  [%xy p=path q=curd]                       ::  push card
                  [%xz p=[p=ship q=term] q=ship r=mark s=zang]
                  [%zz p=path q=path r=curd]                ::
              ==                                            ::

XX document

### `++zang`

    ++  zang                                                ::  XX evil hack
              $%  [%backlog p=path q=?(%da %dr %ud) r=@]    ::
                  [%hola p=path]                            ::
                  $:  %mess  p=path                         ::
                    $=  q                                   ::
                  $%  [%do p=@t]                            ::  act
                      [%exp p=@t q=tank]                    ::  code
                      [%say p=@t]                           ::  speak
                  ==  ==                                    ::
                  [%tint p=ship]                            ::
              ==                                            ::

XX document

### `++gilt`

    ++  gilt  ,[@tas *]                                     ::  presumed gift

XX document

### `++gens`

    ++  gens  ,[p=lang q=gcos]                              ::  general identity

XX document

### `++germ`

    ++  germ  ?(%init %fine %that %this %mate %meld)        ::  merge style

XX document

### `++gcos`

    ++  gcos                                                ::  id description
              $%  [%czar ~]                                 ::  8-bit ship
                  [%duke p=what]                            ::  32-bit ship
                  [%earl p=@t]                              ::  64-bit ship
                  [%king p=@t]                              ::  16-bit ship
                  [%pawn p=(unit ,@t)]                      ::  128-bit ship
              ==                                            ::

XX document

### `++goad`

    ++  goad                                                ::  common note
              $%  [%eg p=riot]                              ::  simple result
                  [%gr p=mark q=*]                          ::  gall rush/rust
                  [%hp p=httr]                              ::  http response
                  ::  [%ht p=@ud q=scab r=cred s=moth]          ::  http request
                  [%it p=~]                                 ::  interrupt event
                  [%lq p=ship q=path r=*]                   ::  client request
                  [%ly p=newt q=tape]                       ::  lifecycle event
                  [%ow p=cape]                              ::  one-way reaction
                  [%rt p=(unit)]                            ::  roundtrip response
                  [%up p=@t]                                ::  prompt response
                  [%wa ~]                                   ::  alarm
              ==                                            ::

XX document

### `++goal`

    ++  goal                                                ::  app request
              $%  [%$ p=type]                               ::  open for input
                  [%do p=vase q=vase]                       ::  call gate sample
                  [%eg p=kite]                              ::  single request
                  [%es p=ship q=desk r=rave]                ::  subscription
                  [%gr ~]                                   ::  gall response
                  [%ht p=(list rout)]                       ::  http server
                  [%hp ~]                                   ::  http response
                  [%lq p=@tas]                              ::  listen for service
                  [%ow ~]                                   ::  one-way reaction
                  [%rt ~]                                   ::  roundtrip response
                  [%up p=prod]                              ::  user prompt
                  [%wa p=@da]                               ::  alarm
              ==                                            ::

XX document

### `++govt`

    ++  govt  path                                          ::  country/postcode

XX document

### `++hand`

    ++  hand  ,@uvH                                         ::  hash of code

XX document

### `++hart`

    ++  hart  ,[p=? q=(unit ,@ud) r=host]                   ::  http sec/port/host

XX document

### `++hate`

    ++  hate  ,[p=purl q=@p r=moth]                         ::  semi-cooked request

XX document

### `++heir`

    ++  heir  ,[p=@ud q=mess r=(unit love)]                 ::  status/headers/data

XX document

### `++hiss`

    ++  hiss  ,[p=purl q=moth]                              ::  outbound request

XX document

### `++hist`

    ++  hist  ,[p=@ud q=(list ,@t)]                         ::  depth texts

XX document

### `++hole`

    ++  hole  ,@t                                           ::  session identity

XX document

### `++hoot`

    ++  hoot  ,[p=? q=(unit ,@ud) r=host]                   ::  secure/port/host

XX document

### `++hort`

    ++  hort  ,[p=(unit ,@ud) q=host]                       ::  http port/host

XX document

### `++host`

    ++  host  $%([& p=(list ,@t)] [| p=@if])                ::  http host

XX document

### `++httq`

    ++  httq                                                ::  raw http request
              $:  p=meth                                    ::  method
                  q=@t                                      ::  unparsed url
                  r=(list ,[p=@t q=@t])                     ::  headers
                  s=(unit octs)                             ::  body
              ==                                            ::

XX document

### `++httr`

    ++  httr  ,[p=@ud q=mess r=(unit octs)]                 ::  raw http response

XX document

### `++httx`

    ++  httx                                                ::  encapsulated http
              $:  p=?                                       ::  https?
                  q=clip                                    ::  source IP
                  r=httq                                    ::
              ==                                            ::

XX document

### `++kite`

    ++  kite  ,[p=care q=case r=ship s=desk t=spur]         ::  parsed global name

XX document

### `++json`

    ++  json                                                ::  normal json value
              $|  ~                                         ::  null
              $%  [%a p=(list json)]                        ::  array
                  [%b p=?]                                  ::  boolean
                  [%o p=(map ,@t json)]                     ::  object
                  [%n p=@ta]                                ::  number
                  [%s p=@ta]                                ::  string
              ==                                            ::

XX document

### `++jsot`

    ++  jsot                                                ::  strict json top
              $%  [%a p=(list json)]                        ::  array
                  [%o p=(map ,@t json)]                     ::  object
              ==                                            ::

XX document

### `++lamb`

    ++  lamb                                                ::  short path
              $%  [& p=@tas]                                ::  auto
                  [| p=twig]                                ::  manual
              ==                                            ::

XX document

### `++lane`

    ++  lane                                                ::  packet route
              $%  [%if p=@da q=@ud r=@if]                   ::  IP4/public UDP/addr
                  [%is p=@ud q=(unit lane) r=@is]           ::  IPv6 w/alternates
                  [%ix p=@da q=@ud r=@if]                   ::  IPv4 provisional
              ==                                            ::

XX document

### `++lang`

    ++  lang  ,@ta                                          ::  IETF lang as code

XX document

### `++lark`

    ++  lark  ,[p=(unit ,@tas) q=lawn]                      ::  parsed command

XX document

### `++lass`

    ++  lass  ?(%0 %1 %2)                                   ::  power increment

XX document

### `++lath`

    ++  lath  $%                                            ::  pipeline stage
                  [%0 p=lass q=lamb r=(list cone) s=twig]   ::  command
                  [%1 p=twig]                               ::  generator
                  [%2 p=twig]                               ::  filter
              ==                                            ::

XX document

### `++lawn`

    ++  lawn  (list lath)                                   ::

XX document

### `++lice`

    ++  lice  ,[p=ship q=buck]                              ::  full license

XX document

### `++life`

    ++  life  ,@ud                                          ::  regime number

XX document

### `++lint`

    ++  lint  (list rock)                                   ::  fragment array

XX document

### `++lobe`

    ++  lobe  ,@                                            ::  blob ref

XX document

### `++love`

    ++  love  $%                                            ::  http response
                  [%ham p=manx]                             ::  html node
                  [%mid p=mite q=octs]                      ::  mime-typed data
                  [%raw p=httr]                             ::  raw http response
                  [%wan p=wain]                             ::  text lines
                  [%zap p=@ud q=(list tank)]                ::  status/error
              ==                                            ::

XX document

### `++luge`

    ++  luge  ,[p=mark q=*]                                 ::  fully typed content

XX document

### `++maki`

    ++  maki  ,[p=@ta q=@ta r=@ta s=path]                   ::

XX document

### `++mace`

    ++  mace  (list ,[p=life q=ring])                       ::  private secrets

XX document

### `++marv`

    ++  marv  ?(%da %tas %ud)                               ::  release form

XX document

### `++math`

    ++  math  (map ,@t (list ,@t))                          ::  semiparsed headers

XX document

### `++meal`

    ++  meal                                                ::  payload
              $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
                  [%buck p=coop q=flap r=@dr]               ::  e2e ack
                  [%bond p=life q=path r=@ud s=*]           ::  message
                  [%bund p=life q=path r=@ud s=*]           ::  e2e message
                  [%carp p=@ q=@ud r=@ud s=flap t=@]        ::  skin/inx/cnt/hash
                  [%fore p=ship q=(unit lane) r=@]          ::  forwarded packet
              ==                                            ::

XX document

### `++mess`

    ++  mess  (list ,[p=@t q=@t])                           ::  raw http headers

XX document

### `++meta`

    ++  meta                                                ::  path metadata
              $%  [& q=@uvI]                                ::  hash
                  [| q=(list ,@ta)]                         ::  dir
              ==                                            ::

XX document

### `++meth`

    ++  meth                                                ::  http methods
              $?  %conn                                     ::  CONNECT
                  %delt                                     ::  DELETE
                  %get                                      ::  GET
                  %head                                     ::  HEAD
                  %opts                                     ::  OPTIONS
                  %post                                     ::  POST
                  %put                                      ::  PUT
                  %trac                                     ::  TRACE
              ==                                            ::

XX document

### `++mite`

    ++  mite  (list ,@ta)                                   ::  mime type

XX document

### `++miso`

    ++  miso                                                ::  ankh delta
              $%  [%del p=*]                                ::  delete
                  [%ins p=*]                                ::  insert
                  [%mut p=udon]                             ::  mutate
              ==                                            ::

XX document

### `++mizu`

    ++  mizu  ,[p=@u q=(map ,@ud tako) r=rang]              ::  new state

XX document

### `++moar`

    ++  moar  ,[p=@ud q=@ud]                                ::  normal change range

XX document

### `++moat`

    ++  moat  ,[p=case q=case r=path]                       ::  change range

XX document

### `++mood`

    ++  mood  ,[p=care q=case r=path]                       ::  request in desk

XX document

### `++moth`

    ++  moth  ,[p=meth q=math r=(unit octs)]                ::  http operation

XX document

### `++name`

    ++  name  ,[p=@t q=(unit ,@t) r=(unit ,@t) s=@t]        ::  first mid/nick last

XX document

### `++newt`

    ++  newt  ?(%boot %kick %mess %slay %wake)              ::  lifecycle events

XX document

### `++nose`

    ++  nose                                                ::  response, kernel
              $?  [%$ p=(unit ,[p=tutu q=(list)])]          ::  standard input
                  goad                                      ::
              ==                                            ::

XX document

### `++note`

    ++  note                                                ::  response, user
              $?  [%$ p=(unit ,[p=type q=(list)])]          ::  standard input
                  [%do p=vase]                              ::  execution result
                  goad                                      ::
              ==                                            ::

XX document

### `++nori`

    ++  nori                                                ::  repository action
              $%  [& q=soba]                                ::  delta
                  [| p=@tas]                                ::  label
              ==                                            ::

XX document

### `++octs`

    ++  octs  ,[p=@ud q=@]                                  ::  octet-stream

XX document

### `++oryx`

    ++  oryx  ,@t                                           ::  CSRF secret

XX document

### `++pact`

    ++  pact  path                                          ::  routed path

XX document

### `++pail`

    ++  pail  ?(%none %warm %cold)                          ::  connection status

XX document

### `++plan`

    ++  plan  (trel view (pair ,@da (unit ,@dr)) path)      ::  subscription

XX document

### `++plea`

    ++  plea  ,[p=@ud q=[p=? q=@t]]                         ::  live prompt

XX document

### `++pork`

    ++  pork  ,[p=(unit ,@ta) q=(list ,@t)]                 ::  fully parsed url

XX document

### `++pred`

    ++  pred  ,[p=@ta q=@tas r=@ta ~]                       ::  proto-path

XX document

### `++prod`

    ++  prod  ,[p=prom q=tape r=tape]                       ::  prompt

XX document

### `++prom`

    ++  prom  ?(%text %pass %none)                          ::  format type

XX document

### `++purl`

    ++  purl  ,[p=hart q=pork r=quay]                       ::  parsed url

XX document

### `++putt`

    ++  putt                                                ::  outgoing message
              $:  ski=snow                                  ::  sequence acked/sent
                  wyv=(list rock)                           ::  packet list XX gear
              ==                                            ::

XX document

### `++pyre`

    ++  pyre                                                ::  cascade stash
              $:  p=(map ,[p=path q=path r=coal] coal)      ::  by path
                  q=(map ,[p=path q=@uvI r=coal] coal)      ::  by source hash
                  r=(map ,[p=* q=coal] coal)                ::  by (soft) twig
              ==                                            ::

XX document

### `++quay`

    ++  quay  (list ,[p=@t q=@t])                           ::  parsed url query

XX document

### `++quri`

    ++  quri                                                ::  request-uri
              $%  [& p=purl]                                ::  absolute
                  [| p=pork q=quay]                         ::  relative
              ==                                            ::

XX document

### `++race`

    ++  race                                                ::  inbound stream
              $:  did=@ud                                   ::  filled sequence
                  dod=?                                     ::  not processing
                  bum=(map ,@ud ares)                       ::  nacks
                  mis=(map ,@ud ,[p=cape q=lane r=flap s=(unit)]) ::  misordered
              ==                                            ::

XX document

### `++rank`

    ++  rank  ?(%czar %king %duke %earl %pawn)              ::  ship width class

XX document

### `++rang`

    ++  rang  $:  hut=(map tako yaki)                       ::
                  lat=(map lobe blob)                       ::
              ==                                            ::

XX document

### `++rant`

    ++  rant                                                ::  namespace binding
              $:  p=[p=care q=case r=@tas]                  ::  clade release book
                  q=path                                    ::  spur
                  r=*                                       ::  data
              ==                                            ::

XX document

### `++rave`

    ++  rave                                                ::  general request
              $%  [& p=mood]                                ::  single request
                  [| p=moat]                                ::  change range
              ==                                            ::

XX document

### `++rill`

    ++  rill                                                ::  outbound stream
              $:  sed=@ud                                   ::  sent
                  san=(map ,@ud duct)                       ::  outstanding
              ==                                            ::

XX document

### `++riot`

    ++  riot  (unit rant)                                   ::  response/complete

XX document

### `++road`

    ++  road                                                ::  secured oneway route
              $:  exp=@da                                   ::  expiration date
                  lun=(unit lane)                           ::  route to friend
                  lew=will                                  ::  will of friend
              ==                                            ::

XX document

### `++rock`

    ++  rock  ,@uvO                                         ::  packet

XX document

### `++rout`

    ++  rout  ,[p=(list host) q=path r=oryx s=path]         ::  http route (new)

XX document

### `++rump`

    ++  rump  ,[p=care q=case r=@tas s=path]                ::  relative path

XX document

### `++saba`

    ++  saba  ,[p=ship q=@tas r=moar s=dome]                ::  patch/merge

XX document

### `++sack`

    ++  sack  ,[p=ship q=ship]                              ::  incoming [our his]

XX document

### `++sufi`

    ++  sufi                                                ::  domestic host
              $:  hoy=(list ship)                           ::  hierarchy
                  val=wund                                  ::  private keys
                  law=will                                  ::  server will
                  seh=(map hand ,[p=ship q=@da])            ::  key cache
                  hoc=(map ship dore)                       ::  neighborhood
              ==                                            ::

XX document

### `++salt`

    ++  salt  ,@uv                                          ::  entropy

XX document

### `++seal`

    ++  seal                                                ::  auth conversation
              $:  whu=(unit ship)                           ::  client identity
                  pul=purl                                  ::  destination url
                  wit=?                                     ::  wait for partner
                  foy=(unit ,[p=ship q=hole])               ::  partner to notify
                  pus=(unit ,@ta)                           ::  password
              ==                                            ::

XX document

### `++sect`

    ++  sect  ?(%black %blue %red %orange %white)           ::  banner

XX document

### `++shed`

    ++  shed                                                ::  packet flow
              $:  $:  rtt=@dr                               ::  smoothed rtt
                      rto=@dr                               ::  retransmit timeout
                      rtn=(unit ,@da)                       ::  next timeout
                      rue=(unit ,@da)                       ::  last heard from
                  ==                                        ::
                  $:  nus=@ud                               ::  number sent
                      nif=@ud                               ::  number live
                      nep=@ud                               ::  next expected
                      caw=@ud                               ::  logical window
                      cag=@ud                               ::  congest thresh
                  ==                                        ::
                  $:  diq=(map flap ,@ud)                   ::  packets sent
                      pyz=(map soup ,@ud)                   ::  message/unacked
                      puq=(qeu ,[p=@ud q=soul])             ::  packet queue
                  ==                                        ::
              ==                                            ::

XX document

### `++skit`

    ++  skit  ,[p=(unit ,@ta) q=(list ,@ta) r=(list ,@ta)]  ::  tracking path

XX document

### `++skin`

    ++  skin  ?(%none %open %fast %full)                    ::  encoding stem

XX document

### `++slip`

    ++  slip  ,[p=path q=goal]                              ::  traceable request

XX document

### `++snow`

    ++  snow  ,[p=@ud q=@ud r=(set ,@ud)]                   ::  window exceptions

XX document

### `++soap`

    ++  soap  ,[p=[p=life q=life] q=path r=@ud]             ::  statement id

XX document

### `++soup`

    ++  soup  ,[p=path q=@ud]                               ::  new statement id

XX document

### `++soul`

    ++  soul                                                ::  packet in travel
              $:  gom=soup                                  ::  message identity
                  nux=@ud                                   ::  xmission count
                  liv=?                                     ::  deemed live
                  lys=@da                                   ::  last sent
                  pac=rock                                  ::  packet data
              ==                                            ::

XX document

### `++soba`

    ++  soba  ,[p=cart q=(list ,[p=path q=miso])]           ::  delta

XX document

### `++sock`

    ++  sock  ,[p=ship q=ship]                              ::  outgoing [from to]

XX document

### `++spur`

    ++  spur  path                                          ::  ship desk case spur

XX document

### `++step`

    ++  step  ,[p=bray q=gens r=pass]                       ::  identity stage

XX document

### `++tako`

    ++  tako  ,@                                            ::  yaki ref

XX document

### `++tart`

    ++  tart  $+([@da path note] bowl)                      ::  process core

XX document

### `++taxi`

    ++  taxi  ,[p=lane q=rock]                              ::  routed packet

XX document

### `++tick`

    ++  tick  ,@ud                                          ::  process id

XX document

### `++toro`

    ++  toro  ,[p=@ta q=nori]                               ::  general change

XX document

### `++town`

    ++  town                                                ::  all security state
              $:  lit=@ud                                   ::  imperial modulus
                  any=@                                     ::  entropy
                  urb=(map ship sufi)                       ::  all keys and routes
                  fak=?                                     ::
              ==                                            ::

XX document

### `++tube`

    ++  tube  ,[p=@ta q=@ta r=@ta s=path]                   ::  canonical path

XX document

### `++tutu`

    ++  tutu  ,*                                            ::  presumed type

XX document

### `++yaki`

    ++  yaki  ,[p=(list tako) q=(map path lobe) r=tako t=@da] ::  commit

XX document

### `++view`

    ++  view  ?(%u %v %w %x %y %z)                          ::  view mode

XX document

### `++waks`

    ++  waks  (map path woof)                               ::  list file states

XX document

### `++what`

    ++  what                                                ::  logical identity
              $%  [%anon ~]                                 ::  anonymous
                  [%lady p=whom]                            ::  female person ()
                  [%lord p=whom]                            ::  male person []
                  [%punk p=sect q=@t]                       ::  opaque handle ""
              ==                                            ::

XX document

### `++whom`

    ++  whom  ,[p=@ud q=govt r=sect s=name]                 ::  year/govt/id

XX document

### `++woof`

    ++  woof  $|  %know                                     ::  udon transform
                  [%chan (list $|(@ud [p=@ud q=@ud]))]      ::

XX document

### `++wund`

    ++  wund  (list ,[p=life q=ring r=acru])                ::  mace in action

XX document

### `++will`

    ++  will  (list deed)                                   ::  certificate

XX document

### `++worm`

    ++  worm  ,*                                            ::  vase of tart

XX document

### `++zuse`

    ++  zuse  %314                                          ::  hoon/zuse kelvin
    --

XX document
