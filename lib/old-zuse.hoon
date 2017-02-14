::  Shims from cc-release
::
::::  /hoon/oldzuse/lib
  ::
|%
++  dope        !!                                      ::  XX deprecated
++  fu          fu:number                               ::  modulo (mul p q)
++  aes         aes:crypto                              ::  aes, all sizes
++  crua        crua:crypto                             ::  cryptosuite A (RSA)
++  bruw        bruw:suite:crypto                       ::  create keypair
++  haul        haul:suite:crypto                       ::  activate public key
++  weur        weur:suite:crypto                       ::  activate secret key
++  trua        trua:test:crypto                        ::  test rsa
++  crub        crub:crypto                             ::  cryptosuite B (Ed)
++  trub        trub:test:crypto                        ::  test crub
++  hmac        hmac:crypto                             ::  HMAC-SHA1
++  dawn        dawn:chrono:userlib                     ::  Jan 1 weekday
++  daws        daws:chrono:userlib                     ::  date weekday
++  deal        deal:chrono:userlib                     ::  to leap sec time
++  lead        lead:chrono:userlib                     ::  from leap sec time
++  dust        dust:chrono:userlib                     ::  print UTC format
++  stud        stud:chrono:userlib                     ::  parse UTC format
++  unt         unt:chrono:userlib                      ::  UGT to UTC time
++  yu          yu:chrono:userlib                       ::  UTC format constants
++  moon        en-mite:mimes:html                      ::  mime type to text
++  perk        perk.is                                 ::  parse cube fork 
++  poja        en-json:html                            ::  parse JSON
++  pojo        de-json:html                            ::  print json
++  poxo        en-xml:html                             ::  node to tape
++  poxa        de-xml:html                             ::  xml parser
++  jo          dejs-soft:format                        ::  json reparser
++  joba        frond:enjs:format                       ::  object from k-v pair
++  jobe        pairs:enjs:format                       ::  object from k-v list
++  jape        tape:enjs:format                        ::  string from tape
++  jone        numb:enjs:format                        ::  number from unsigned
++  jode        time:enjs:format                        ::  ms timestamp
++  jesc        jesc:en-json:html                       ::  escape for json
++  scanf       scanf:userlib                           ::  formatted scan
++  parsf       parsf:scanf:userlib                     ::  make parser from:
++  taco        as-octs:mimes:html                      ::  atom to octstream
++  tact        as-octt:mimes:html                      ::  tape to octstream
++  tell        of-wall:format                          ::  wall to octstream
++  feel        feel:space:userlib                      ::  simple file write
++  file        file:space:userlib                      ::  simple file load
++  foal        foal:space:userlib                      ::  high-level write
++  fray        fray:space:userlib                      ::  high-level delete
++  furl        furl:space:userlib                      ::  unify changes
++  tame        !!                                      ::  XX deprecated
++  tome        de-beam:format                          ::  parse path to beam
++  tope        en-beam:format                          ::  beam to path
++  deft        deft:de-purl:html                       ::  parse url extension
++  fuel        fuel:html                               ::  parse fcgi
++  sifo        en-base64:mimes:html                    ::  64-bit encode
++  ofis        de-base64:mimes:html                    ::  64-bit decode
++  dray        dray:wired                              ::  load tuple into path
++  raid        raid:wired                              ::  demand path odors
++  read        read:wired                              ::  parse odored path
++  urle        en-urlt:html                            ::  URL encode
++  urld        de-urlt:html                            ::  URL decode
++  earn        en-purl:html                            ::  purl to tape
++  earf        apix:en-purl:html                       ::  purf to tape
++  earl        !!                                      ::  XX deprecated
++  urlp        de-purl:html                            ::  url+header parser
++  epur        de-purl:html                            ::  url+header parser
++  clan        clan:title                              ::  ship to rank
++  cite        cite:title                              ::  ship short name
++  glam        glam:title                              ::  carrier names
++  gnom        gnom:title                              ::  ship display name
++  gnow        gnow:title                              ::  display gcos
++  pale        pale:pubsub:userlib                     ::  filter peers
++  prix        prix:pubsub:userlib                     ::  filter gate
++  prey        prey:pubsub:userlib                     ::  prefix 
++  hunt        !!                                      ::  YY in hoon with dif abi
++  mole        !!                                      ::  XX deprecated
++  myle        !!                                      ::  XX deprecated
++  pack        !!                                      ::  YY in eyre privately
++  puck        !!                                      ::  YY in eyre privately
++  saxo        saxo:title                              ::  autocanon
++  sein        sein:title                              ::  autoboss
++  team        team:title                              ::  our / our moon
++  acru        acru:ames                               ::  asym cryptosuite
++  aeon        aeon:clay                               ::  revision
++  agon        !!                                      ::  XX deprecated
++  ankh        ankh:clay                               ::  fs node (new)
++  apex        !!                                      ::  XX deprecated
++  ares        ares.is                                 ::  possible error
++  bale        bale:eyre                               ::  driver state
++  iden        !!                                      ::  username
++  sec-move    sec-move:eyre                           ::  driver effect
++  ball        !!                                      ::  XX deprecated
++  bait        bait:ames                               ::  fmt nrecvd spec
++  bath        !!                                      ::  XX deprecated
++  beam        beam:clay                               ::  global name
++  beak        beak:clay                               ::  garnish with beak
++  bird        !!                                      ::  XX deprecated
++  bitt        bitt:gall                               ::  incoming subs
++  blob        blob:clay                               ::  fs blob
++  boat        boat:gall                               ::  outgoing subs
++  boon        boon:ames                               ::  fort output
++  bowl        bowl:gall                               ::  standard app state
++  bray        bray:ames                               ::  our parent us now
++  brow        !!                                      ::  XX deprecated
++  buck        buck:ames                               ::  all security data
++  cake        cake:ames                               ::  top level packet
++  cape        cape:ames                               ::  end-to-end result
++  care        care:clay                               ::  clay submode
++  case        case:clay                               ::  ship desk case spur
++  chum        !!                                      ::  XX depreacted
++  clot        clot:ames                               ::  symmetric record
++  claw        !!                                      ::  XX depreacted
++  clip        clip:eyre                               ::  client IP
++  coal        !!                                      ::  XX depreacted
++  code        code:xmas                               ::  symmetric key
++  cone        !!                                      ::  XX depreacted
++  coop        coop.is                                 ::  e2e ack
++  corn        !!                                      ::  XX depreacted
++  cred        cred:eyre                               ::  credential
++  deco        deco:dill                               ::  text decoration
++  deed        deed:pki:jael                           ::  sig stage fake?
++  dome        dome:clay                               ::  project state
++  dore        dore:ames                               ::  foreign contact
++  dove        dove:ames                               ::  count hash 13-blocks
++  epic        epic:eyre                               ::  FCGI parameters
++  flap        flap:ames                               ::  network packet id
++  flow        flow:ames                               ::  packet connection
++  fort        !!                                      ::  XX deprecated
++  gank        !!                                      ::  XX deprecated
++  gilt        !!                                      ::  YY declare locally
++  gens        gens:ames                               ::  general identity
++  germ        germ:clay                               ::  merge style
++  gcos        gcos:ames                               ::  id description
++  govt        govt:ames                               ::  country+postcode
++  hand        hand:ames                               ::  hash of code
++  hart        hart:eyre                               ::  http sec+port+host
++  hate        hate:eyre                               ::  semi-cooked request
++  heir        heir:eyre                               ::  status+headers+data
++  hiss        hiss:eyre                               ::  outbound request
++  hist        !!                                      ::  XX deprecated
++  hole        hole:eyre                               ::  session identity
++  hort        hort:eyre                               ::  http port+host
++  host        host:eyre                               ::  http host
++  hoke        hoke:eyre                               ::  local host
++  httq        httq:eyre                               ::  raw http request
++  httr        httr:eyre                               ::  raw http response
++  httx        httx:eyre                               ::  encapsulated http
++  kite        !!                                      ::  XX deprecated
++  json        json.is                                 ::  normal json value
++  lamb        !!                                      ::  XX deprecated
++  lane        lane:xmas                               ::  packet route
++  lang        lang:ames                               ::  IETF lang as code
++  lark        !!                                      ::  parsed command
++  lass        !!                                      ::  power increment
++  lath        !!                                      ::  pipeline stage
++  lawn        !!                                      ::  pipeline (??)
++  lice        lice:ames                               ::  full license
++  life        life:xmas                               ::  regime number
++  lint        !!                                      ::  fragment array
++  lobe        lobe:clay                               ::  blob ref
++  love        love:eyre                               ::  http response
++  maki        maki:clay                               ::  XX dead?
++  mace        mace:ames                               ::  private secrets
++  math        math:eyre                               ::  semiparsed headers
++  meal        meal:ames                               ::  payload
++  mess        mess:eyre                               ::  raw http headers
++  meta        !!                                      ::  path metadata
++  meth        meth:eyre                               ::  http methods
++  mime        mime.is                                 ::  mimetyped data
++  mite        mite:eyre                               ::  mime type
++  miso        miso:clay                               ::  ankh delta
++  misu        misu:clay                               ::  computed delta
++  mizu        mizu:clay                               ::  new state
++  moar        moar:clay                               ::  normal change range
++  moat        moat:clay                               ::  change range
++  mood        mood:clay                               ::  request in desk
++  moth        moth:eyre                               ::  http operation
++  name        name:ames                               ::  first mid+nick last
++  newt        !!                                      ::  XX deprecated
++  nori        nori:clay                               ::  repository action
++  nuri        nuri:clay                               ::  repository action
++  octs        octs:eyre                               ::  octet-stream
++  oryx        oryx:eyre                               ::  CSRF secret
++  page        page:clay                               ::  untyped cage
++  pail        !!                                      ::  XX deprecated?
++  plan        !!                                      ::  XX deprecated?
++  plea        !!                                      ::  XX deprecated?
++  plop        plop:clay                               ::  unvalidated blob
++  pork        pork:eyre                               ::  fully parsed url
++  pred        !!                                      ::  XX deprecated?
++  prod        !!                                      ::  XX deprecated?
++  prom        !!                                      ::  XX deprecated?
++  purf        purf:eyre                               ::  url with fragment
++  purl        purl:eyre                               ::  parsed url
++  putt        putt:ames                               ::  outgoing message
++  pyre        !!                                      ::  YY moved to clay
++  quay        quay:eyre                               ::  parsed url query
++  quri        quri:eyre                               ::  request-uri
++  race        race:ames                               ::  inbound stream
++  rank        rank:ames                               ::  ship width class
++  rang        rang:clay                               ::  object store
++  rand        !!                                      ::  YY moved to clay
++  rant        rant:clay                               ::  namespace binding
++  rave        rave:clay                               ::  general request
++  rill        rill:ames                               ::  outbound stream
++  riot        riot:clay                               ::  response+complete
++  road        road:ames                               ::  secured oneway route
++  rock        rock:ames                               ::  packet
++  rout        rout:eyre                               ::  http route (new)
++  rump        rump:clay                               ::  relative path
++  saba        saba:clay                               ::  patch+merge
++  sack        sack.is                                 ::  incoming [our his]
++  sufi        sufi:ames                               ::  domestic host
++  salt        !!                                      ::  XX deprecated
++  seal        !!                                      ::  XX deprecated
++  sect        sect:ames                               ::  banner
++  shed        shed:ames                               ::  packet flow
++  skit        !!                                      ::  XX deprecated
++  skin        skin:ames                               ::  encoding stem
++  snow        snow:ames                               ::  window exceptions
++  soap        soap:ames                               ::  statement id
++  soup        soup:ames                               ::  new statement id
++  soul        soul:ames                               ::  packet in travel
++  soba        soba:clay                               ::  delta
++  sock        sock.is                                 ::  outgoing [from to]
++  spur        spur.is                                 ::  ship desk case spur
++  step        step:ames                               ::  identity stage
++  stub        stub:dill                               ::  styled tuba
++  stye        stye:dill                               ::  decos/bg/fg
++  styl        styl:dill                               ::  text style
++  styx        styx:dill                               ::  styled text
++  suba        suba:clay                               ::  delta
++  tako        tako:clay                               ::  yaki ref
++  tick        tick:ames                               ::  process id
++  tint        tint:dill                               ::  text color
++  toro        toro:clay                               ::  general change
++  town        town:ames                               ::  all security state
++  tube        !!                                      ::  canonical path
++  tutu        !!                                      ::  XX deprecated
++  yaki        yaki:clay                               ::  XX deprecated
++  view        view:clay                               ::  view mode
++  waks        !!                                      ::  XX deprecated
++  what        what:ames                               ::  logical identity
++  whom        whom:ames                               ::  year+govt+id
++  woof        !!                                      ::  XX deprecated
++  wund        wund:ames                               ::  mace in action
++  will        will:pki:jael                           ::  certificate
++  zuse        ^zuse                                   ::  hoon+zuse kelvin
++  kiss-ames   task:able:ames                          ::  in request ->$
++  gift-ames   gift:able:ames                          ::  out result <-$
++  kiss-behn   task:able:behn                          ::  in request ->$
++  gift-behn   gift:able:behn                          ::  out result <-$
++  khan        khan:clay                               ::  XX dead
++  mode        mode:clay                               ::  update info?
++  riff        riff:clay                               ::  request+desist
++  kiss-clay   task:able:clay                          ::  in request ->$
++  gift-clay   gift:able:clay                          ::  out result <-$
++  blew        blew:dill                               ::  columns rows
++  belt        belt:dill                               ::  old belt
++  blit        blit:dill                               ::  old blit
++  dill-belt   dill-belt:dill                          ::  new belt
++  dill-blit   dill-blit:dill                          ::  new blit
++  flog        flog:dill                               ::  sent to %dill
++  gill        gill:gall                               ::  general contact
++  kiss-dill   task:able:dill                          ::  in request ->$
++  gift-dill   gift:able:dill                          ::  out result <-$
++  gram        gram:eyre                               ::  inter-ship message
++  kiss-eyre   task:able:eyre                          ::  in request ->$
++  gift-eyre   gift:able:eyre                          ::  out result <-$
++  hood        hood:ford                               ::  assembly plan
++  hoof        hoof:ford                               ::  resource reference
++  hoop        hoop:ford                               ::  source in hood
++  hops        hops:ford                               ::  XX late-bound path
++  horn        horn:ford                               ::  resource tree
++  milk        milk:ford                               ::  sourced silk
++  silk        silk:ford                               ::  construction layer
++  bilk        bilk:ford                               ::  sourced request
++  gage        gage:ford                               ::  recursive cage+tang
++  kiss-ford   task:able:ford                          ::  in request ->$
++  gift-ford   gift:able:ford                          ::  out result <-$
++  club        club:gall                               ::  agent action
++  cuft        cuft:gall                               ::  internal gift
++  culm        culm:gall                               ::  config action
++  cush        cush:gall                               ::  internal kiss
++  dude        dude:gall                               ::  server identity
++  scup        scup:gall                               ::  autoupdate
++  well        well:gall                               ::  autostartable app
++  suss        suss:gall                               ::  config report
++  kiss-gall   task:able:gall                          ::  incoming request
++  gift-gall   gift:able:gall                          ::  outgoing result
++  gift-arvo   gift-arvo                               ::  out result <-$
++  kiss-arvo   task-arvo                               ::  in request ->$
++  note-arvo   note-arvo                               ::  out request $->
++  sign-arvo   sign-arvo                               ::  in result $<-
--
