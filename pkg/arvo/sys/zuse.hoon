::                                                      ::  /van/zuse
::                                                      ::  %reference/1
::  %zuse: arvo library.
::
::  %zuse is two nested cores: the first for models
::  (data structures), the second for engines (functions
::  or classes).
::
::  each of these stages is split into cores for each of
::  arvo's nine major vanes (kernel modules).  these are:
::
::      - %ames: networking         (rhymes with "games")
::      - %behn: scheduling         ("bane")
::      - %clay: revision control   ("play")
::      - %dill: console            ("pill")
::      - %eyre: http server        ("fair")
::      - %ford: build              ("lord")
::      - %gall: application        ("ball")
::      - %iris: http client        ("virus")
::      - %jael: security           ("jail")
::
::  with %zuse in your core, the engines of any vane are
::  available at `engine:vane`.  the models (molds) are
::  available at `mold:^vane`.
::
::  every model or engine in %zuse is attached to some
::  vane, but any vane can use it (in its namespace),
::  as can any normal user-level code.
::
::  it's important to keep %zuse minimal.  models and
::  engines not used outside a vane should stay inside
::  that vane.
~%  %zuse  +>  ~
=>
::                                                      ::  ::
::::                                                    ::  ::  (1) models
  ::                                                    ::  ::
|%
::  #  %misc
::
::  miscellaneous systems types
::+|
++  ares  (unit {p/term q/(list tank)})                 ::  possible error
::  +capped-queue: a +qeu with a maximum number of entries
::
++  capped-queue
  |*  item-type=mold
  $:  queue=(qeu item-type)
      size=@ud
      max-size=_64
  ==
::  +clock: polymorphic cache type for use with the clock replacement algorithm
::
::     The +by-clock core wraps interface arms for manipulating a mapping from
::     :key-type to :val-type. Detailed docs for this type can be found there.
::
++  clock
  |*  $:  ::  key-type: mold of keys
          ::
          key-type=mold
          ::  val-type: mold of values
          ::
          val-type=mold
      ==
    $:  lookup=(map key-type [val=val-type fresh=@ud])
        queue=(qeu key-type)
        size=@ud
        max-size=_2.048
        depth=_1
    ==
::
++  coop  (unit ares)                                   ::  possible error
++  life  @ud                                           ::  ship key revision
++  rift  @ud                                           ::  ship continuity
++  mime  {p/mite q/octs}                               ::  mimetyped data
::
::
::    TODO: Rename to +mime once the current +mime and +mite are gone. The
::
++  octs  {p/@ud q/@t}                                  ::  octet-stream
++  sock  {p/ship q/ship}                               ::  outgoing [our his]
::+|
::
++  roof  (room vase)                                   ::  namespace
++  room                                                ::  either namespace
  |*  vase/mold                                         ::  vase or maze
  $-  $:  ref/*                                         ::  reference type
          lyc/(unit (set ship))                         ::  leakset
          car/term                                      ::  perspective
          bem/beam                                      ::  path
      ==                                                ::
  %-  unit                                              ::  ~: unknown
  %-  unit                                              ::  ~ ~: invalid
  (cask vase)                                           ::  marked cargo
::
++  turf  (list @t)                                     ::  domain, tld first
::                                                      ::
::::                      ++jstd                        ::  json standards structures
  ::                                                    ::::
++  jstd
  |%
  ++  rpc
    |%
    +$  request
      $:  id=@t
          method=@t
          params=request-params
      ==
    ::
    +$  request-params
      $%  [%list (list json)]
          [%object (list (pair @t json))]
      ==
    +$  response
      $~  [%fail *httr:eyre]
      $%  [%result id=@t res=json]
          [%error id=@t code=@t message=@t]  ::TODO  data?
          [%fail hit=httr:eyre]
          [%batch bas=(list response)]
      ==
    --
  --
::                                                      ::::
::::                      ++ethereum-types                ::  eth surs for jael
  ::                                                    ::::
++  ethereum-types
  |%
  ::  ethereum address, 20 bytes.
  ::
  ++  address  @ux
  ::  event location
  ::
  +=  event-id  [block=@ud log=@ud]
  ::
  ++  events  (set event-id)
  --
::                                                      ::::
::::                      ++azimuth-types                 ::  az surs for jael
  ::                                                    ::::
++  azimuth-types
  =,  ethereum-types
  |%
  ++  point
    $:  ::  ownership
        ::
        $=  own
        $:  owner=address
            management-proxy=address
            voting-proxy=address
            transfer-proxy=address
        ==
      ::
        ::  networking
        ::
        $=  net
        %-  unit
        $:  =life
            =pass
            continuity-number=@ud
            sponsor=[has=? who=@p]
            escape=(unit @p)
        ==
      ::
        ::  spawning
        ::
        $=  kid
        %-  unit
        $:  spawn-proxy=address
            spawned=(set @p)  ::TODO  sparse range, pile, see old jael ++py
        ==
    ==
  ::
  +=  dnses  [pri=@t sec=@t ter=@t]
  ::
  ++  diff-azimuth
    $%  [%point who=@p dif=diff-point]
        [%dns dnses]
    ==
  ::
  ++  diff-point
    $%  [%full new=point]                           ::
        [%owner new=address]                        ::  OwnerChanged
        [%activated who=@p]                         ::  Activated
        [%spawned who=@p]                           ::  Spawned
        [%keys =life =pass]                         ::  ChangedKeys
        [%continuity new=@ud]                       ::  BrokeContinuity
        [%sponsor new=[has=? who=@p]]               ::  EscapeAcc/LostSpons
        [%escape new=(unit @p)]                     ::  EscapeReq/Can
        [%management-proxy new=address]             ::  ChangedManagementPro
        [%voting-proxy new=address]                 ::  ChangedVotingProxy
        [%spawn-proxy new=address]                  ::  ChangedSpawnProxy
        [%transfer-proxy new=address]               ::  ChangedTransferProxy
    ==
  --
::  +vane-task: general tasks shared across vanes
::
+$  vane-task
  $~  [%born ~]
  $%  ::  i/o device replaced (reset state)
      ::
      [%born ~]
      ::  error report
      ::
      [%crud p=@tas q=(list tank)]
      ::  boot completed (XX legacy)
      ::
      [%init p=ship]
      ::  trim state (in response to memory pressure)
      ::
      [%trim p=@ud]
      ::  kernel upgraded
      ::
      [%vega ~]
      ::  produce labeled state (for memory measurement)
      ::
      [%wegh ~]
      ::  receive message via %ames
      ::
      ::    TODO: move .vane from $plea to here
      ::
      [%plea =ship =plea:ames]
  ==
::                                                      ::::
::::                      ++http                        ::
  ::                                                    ::::
::  http: shared representations of http concepts
::
++  http  ^?
  |%
  ::  +header-list: an ordered list of http headers
  ::
  +$  header-list
    (list [key=@t value=@t])
  ::  +method: exhaustive list of http verbs
  ::
  +$  method
    $?  %'CONNECT'
        %'DELETE'
        %'GET'
        %'HEAD'
        %'OPTIONS'
        %'POST'
        %'PUT'
        %'TRACE'
    ==
  ::  +request: a single http request
  ::
  +$  request
    $:  ::  method: http method
        ::
        method=method
        ::  url: the url requested
        ::
        ::    The url is not escaped. There is no escape.
        ::
        url=@t
        ::  header-list: headers to pass with this request
        ::
        =header-list
        ::  body: optionally, data to send with this request
        ::
        body=(unit octs)
    ==
  ::  +response-header: the status code and header list on an http request
  ::
  ::    We separate these away from the body data because we may not wait for
  ::    the entire body before we send a %progress to the caller.
  ::
  +$  response-header
    $:  ::  status: http status code
        ::
        status-code=@ud
        ::  headers: http headers
        ::
        headers=header-list
    ==
  ::  +http-event: packetized http
  ::
  ::    Urbit treats Earth's HTTP servers as pipes, where Urbit sends or
  ::    receives one or more %http-events. The first of these will always be a
  ::    %start or an %error, and the last will always be %cancel or will have
  ::    :complete set to %.y to finish the connection.
  ::
  ::    Calculation of control headers such as 'Content-Length' or
  ::    'Transfer-Encoding' should be performed at a higher level; this structure
  ::    is merely for what gets sent to or received from Earth.
  ::
  +$  http-event
    $%  ::  %start: the first packet in a response
        ::
        $:  %start
            ::  response-header: first event information
            ::
            =response-header
            ::  data: data to pass to the pipe
            ::
            data=(unit octs)
            ::  whether this completes the request
            ::
            complete=?
        ==
        ::  %continue: every subsequent packet
        ::
        $:  %continue
            ::  data: data to pass to the pipe
            ::
            data=(unit octs)
            ::  complete: whether this completes the request
            ::
            complete=?
        ==
        ::  %cancel: represents unsuccessful termination
        ::
        [%cancel ~]
    ==
  ::  +get-header: returns the value for :header, if it exists in :header-list
  ::
  ++  get-header
    |=  [header=@t =header-list]
    ^-  (unit @t)
    ::
    ?~  header-list
      ~
    ::
    ?:  =(key.i.header-list header)
      `value.i.header-list
    ::
    $(header-list t.header-list)
  ::  +set-header: sets the value of an item in the header list
  ::
  ::    This adds to the end if it doesn't exist.
  ::
  ++  set-header
    |=  [header=@t value=@t =header-list]
    ^-  ^header-list
    ::
    ?~  header-list
      ::  we didn't encounter the value, add it to the end
      ::
      [[header value] ~]
    ::
    ?:  =(key.i.header-list header)
      [[header value] t.header-list]
    ::
    [i.header-list $(header-list t.header-list)]
  ::  +delete-header: removes the first instance of a header from the list
  ::
  ++  delete-header
    |=  [header=@t =header-list]
    ^-  ^header-list
    ::
    ?~  header-list
      ~
    ::  if we see it in the list, remove it
    ::
    ?:  =(key.i.header-list header)
      t.header-list
    ::
    [i.header-list $(header-list t.header-list)]
  ::  +simple-payload: a simple, one event response used for generators
  ::
  +$  simple-payload
    $:  ::  response-header: status code, etc
        ::
        =response-header
        ::  data: the data returned as the body
        ::
        data=(unit octs)
    ==
  --
::                                                      ::::
::::                      ++ames                          ::  (1a) network
  ::                                                    ::::
++  ames  ^?
  |%
  ::                                                    ::
  ::::                  ++able:ames                     ::  (1a1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ::  $task: job for ames
    ::
    ::    Messaging Tasks
    ::
    ::    %hear: packet from unix
    ::    %hole: report that packet handling crashed
    ::    %heed: track peer's responsiveness; gives %clog if slow
    ::    %jilt: stop tracking peer's responsiveness
    ::    %plea: request to send message
    ::
    ::    System and Lifecycle Tasks
    ::
    ::    %born: process restart notification
    ::    %crud: crash report
    ::    %init: vane boot
    ::    %sift: limit verbosity to .ships
    ::    %spew: set verbosity toggles
    ::    %vega: kernel reload notification
    ::    %wegh: request for memory usage report
    ::
    +$  task
      $%  [%hear =lane =blob]
          [%hole =lane =blob]
          [%heed =ship]
          [%jilt =ship]
          $>(%plea vane-task)
      ::
          $>(%born vane-task)
          $>(%crud vane-task)
          $>(%init vane-task)
          [%sift ships=(list ship)]
          [%spew veb=(list verb)]
          $>(%vega vane-task)
          $>(%wegh vane-task)
      ==
    ::  $gift: effect from ames
    ::
    ::    Messaging Gifts
    ::
    ::    %boon: response message from remote ship
    ::    %clog: notify vane that %boon's to peer are backing up locally
    ::    %done: notify vane that peer (n)acked our message
    ::    %lost: notify vane that we crashed on %boon
    ::    %send: packet to unix
    ::
    ::    System and Lifecycle Gifts
    ::
    ::    %mass: memory usage report
    ::    %turf: domain report, relayed from jael
    ::
    +$  gift
      $%  [%boon payload=*]
          [%clog =ship]
          [%done error=(unit error)]
          [%lost ~]
          [%send =lane =blob]
      ::
          [%mass p=mass]
          [%turf turfs=(list turf)]
      ==
    --  ::able
  ::
  ::::                                                  ::  (1a2)
    ::
  ++  acru  $_  ^?                                      ::  asym cryptosuite
    |%                                                  ::  opaque object
    ++  as  ^?                                          ::  asym ops
      |%  ++  seal  |~({a/pass b/@} *@)                 ::  encrypt to a
          ++  sign  |~(a/@ *@)                          ::  certify as us
          ++  sure  |~(a/@ *(unit @))                   ::  authenticate from us
          ++  tear  |~({a/pass b/@} *(unit @))          ::  accept from a
      --  ::as                                          ::
    ++  de  |~({a/@ b/@} *(unit @))                     ::  symmetric de, soft
    ++  dy  |~({a/@ b/@} *@)                            ::  symmetric de, hard
    ++  en  |~({a/@ b/@} *@)                            ::  symmetric en
    ++  ex  ^?                                          ::  export
      |%  ++  fig  *@uvH                                ::  fingerprint
          ++  pac  *@uvG                                ::  default passcode
          ++  pub  *pass                                ::  public key
          ++  sec  *ring                                ::  private key
      --  ::ex                                          ::
    ++  nu  ^?                                          ::  reconstructors
      |%  ++  pit  |~({a/@ b/@} ^?(..nu))               ::  from [width seed]
          ++  nol  |~(a/ring ^?(..nu))                  ::  from ring
          ++  com  |~(a/pass ^?(..nu))                  ::  from pass
      --  ::nu                                          ::
    --  ::acru                                          ::
  ::  $address: opaque atomic transport address to or from unix
  ::
  +$  address  @uxaddress
  ::  $verb: verbosity flag for ames
  ::
  +$  verb  ?(%snd %rcv %odd %msg %ges %for %rot)
  ::  $blob: raw atom to or from unix, representing a packet
  ::
  +$  blob  @uxblob
  ::  $error: tagged diagnostic trace
  ::
  +$  error  [tag=@tas =tang]
  ::  $lane: ship transport address; either opaque $address or galaxy
  ::
  ::    The runtime knows how to look up galaxies, so we don't need to
  ::    know their transport addresses.
  ::
  +$  lane  (each @pC address)
  ::  $plea: application-level message, as a %pass
  ::
  ::    vane: destination vane on remote ship
  ::    path: internal route on the receiving ship
  ::    payload: semantic message contents
  ::
  +$  plea  [vane=@tas =path payload=*]
  --  ::ames
::                                                      ::::
::::                    ++behn                            ::  (1b) timekeeping
  ::                                                    ::::
++  behn  ^?
  |%
  ::                                                    ::
  ::::                  ++able:behn                     ::  (1b1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  [%doze p=(unit @da)]                          ::  next alarm
          [%mass p=mass]                                ::  memory usage
          [%wake error=(unit tang)]                     ::  wakeup or failed
          [%meta p=vase]
      ==
    ++  task                                            ::  in request ->$
      $~  [%vega ~]                                     ::
      $%  $>(%born vane-task)                           ::  new unix process
          $>(%crud vane-task)                           ::  error with trace
          [%rest p=@da]                                 ::  cancel alarm
          [%drip p=vase]                                ::  give in next event
          [%huck p=vase]                                ::  give back
          $>(%trim vane-task)                           ::  trim state
          $>(%vega vane-task)                           ::  report upgrade
          [%wait p=@da]                                 ::  set alarm
          [%wake ~]                                     ::  timer activate
          $>(%wegh vane-task)                           ::  report memory
      ==
    --  ::able
  --  ::behn
::                                                      ::::
::::                    ++clay                            ::  (1c) versioning
  ::                                                    ::::
++  clay  ^?
  |%
  ::                                                    ::
  ::::                  ++able:clay                     ::  (1c1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  [%boon payload=*]                             ::  ames response
          {$croz rus/(map desk {r/regs w/regs})}        ::  rules for group
          {$cruz cez/(map @ta crew)}                    ::  permission groups
          {$dirk p/@tas}                                ::  mark mount dirty
          {$ergo p/@tas q/mode}                         ::  version update
          {$hill p/(list @tas)}                         ::  mount points
          [%done error=(unit error:ames)]               ::  ames message (n)ack
          {$mass p/mass}                                ::  memory usage
          {$mere p/(each (set path) (pair term tang))}  ::  merge result
          {$note p/@tD q/tank}                          ::  debug message
          {$ogre p/@tas}                                ::  delete mount point
          {$rule red/dict wit/dict}                     ::  node r+w permissions
          {$writ p/riot}                                ::  response
          {$wris p/{$da p/@da} q/(set (pair care path))}  ::  many changes
      ==                                                ::
    ++  task                                            ::  in request ->$
      $~  [%vega ~]                                     ::
      $%  {$boat ~}                                     ::  pier rebooted
          {$cred nom/@ta cew/crew}                      ::  set permission group
          {$crew ~}                                     ::  permission groups
          {$crow nom/@ta}                               ::  group usage
          $>(%crud vane-task)                           ::  error with trace
          {$drop des/desk}                              ::  cancel pending merge
          {$info des/desk dit/nori}                     ::  internal edit
          $>(%init vane-task)                           ::  report install
          {$into des/desk all/? fis/mode}               ::  external edit
          $:  $merg                                     ::  merge desks
              des/desk                                  ::  target
              her/@p  dem/desk  cas/case                ::  source
              how/germ                                  ::  method
          ==                                            ::
          {$mont des/desk bem/beam}                     ::  mount to unix
          {$dirk des/desk}                              ::  mark mount dirty
          {$ogre pot/$@(desk beam)}                     ::  delete mount point
          {$perm des/desk pax/path rit/rite}            ::  change permissions
          $>(%trim vane-task)                           ::  trim state
          $>(%vega vane-task)                           ::  report upgrade
          {$warp wer/ship rif/riff}                     ::  internal file req
          {$werp who/ship wer/ship rif/riff}            ::  external file req
          $>(%wegh vane-task)                           ::  report memory
          $>(%plea vane-task)                           ::  ames request
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1c2)
    ::
  ++  aeon  @ud                                         ::  version number
  ++  ankh                                              ::  fs node (new)
    $~  [~ ~]
    $:  fil/(unit {p/lobe q/cage})                      ::  file
        dir/(map @ta ankh)                              ::  folders
    ==                                                  ::
  ++  beam  {{p/ship q/desk r/case} s/path}             ::  global name
  ++  beak  {p/ship q/desk r/case}                      ::  path prefix
  ++  blob                                              ::  fs blob
    $%  {$delta p/lobe q/{p/mark q/lobe} r/page}        ::  delta on q
        {$direct p/lobe q/page}                         ::  immediate
    ==                                                  ::
  ++  care  ?($d $p $t $u $v $w $x $y $z)               ::  clay submode
  ++  case                                              ::  ship desk case spur
    $%  {$da p/@da}                                     ::  date
        {$tas p/@tas}                                   ::  label
        {$ud p/@ud}                                     ::  number
    ==                                                  ::
  ++  cass  {ud/@ud da/@da}                             ::  cases for revision
  ++  coop  (unit ares)                                 ::  e2e ack
  ++  crew  (set ship)                                  ::  permissions group
  ++  dict  {src/path rul/real}                         ::  effective permission
  ++  dome                                              ::  project state
    $:  ank/ankh                                        ::  state
        let/@ud                                         ::  top id
        hit/(map @ud tako)                              ::  changes by id
        lab/(map @tas @ud)                              ::  labels
    ==                                                  ::
  ++  germ                                              ::  merge style
    $?  $init                                           ::  new desk
        $this                                           ::  ours with parents
        $that                                           ::  hers with parents
        $fine                                           ::  fast forward
        $meet                                           ::  orthogonal files
        $mate                                           ::  orthogonal changes
        $meld                                           ::  force merge
    ==                                                  ::
  ++  khan                                              ::
    $~  [~ ~]
    $:  fil/(unit (unit cage))                          ::  see ++khan-to-soba
        dir/(unit (map @ta (unit khan)))                ::
    ==                                                  ::
  ++  lobe  @uvI                                        ::  blob ref
  ++  maki  {p/@ta q/@ta r/@ta s/path}                  ::
  ++  miso                                              ::  ankh delta
    $%  {$del ~}                                        ::  delete
        {$ins p/cage}                                   ::  insert
        {$dif p/cage}                                   ::  mutate from diff
        {$mut p/cage}                                   ::  mutate from raw
    ==                                                  ::
  ++  misu                                              ::  computed delta
    $%  {$del ~}                                        ::  delete
        {$ins p/cage}                                   ::  insert
        {$dif p/lobe q/cage}                            ::  mutate from diff
    ==                                                  ::
  ++  mizu  {p/@u q/(map @ud tako) r/rang}              ::  new state
  ++  moar  {p/@ud q/@ud}                               ::  normal change range
  +$  moat  [from=case to=case =path]                   ::  change range
  ++  mode  (list {path (unit mime)})                   ::  external files
  +$  mood  [=care =case =path]                         ::  request in desk
  +$  mool  [=case paths=(set (pair care path))]        ::  requests in desk
  ++  nori                                              ::  repository action
    $%  {%& p/soba}                                     ::  delta
        {%| p/@tas}                                     ::  label
    ==                                                  ::
  ++  nuri                                              ::  repository action
    $%  {%& p/suba}                                     ::  delta
        {%| p/@tas}                                     ::  label
    ==                                                  ::
  ++  page  (cask *)                                    ::  untyped cage
  ++  plop  blob                                        ::  unvalidated blob
  ++  rang                                              ::  repository
    $:  hut/(map tako yaki)                             ::  changes
        lat/(map lobe blob)                             ::  data
    ==                                                  ::
  ++  rant                                              ::  response to request
    $:  p/{p/care q/case r/desk}                        ::  clade release book
        q/path                                          ::  spur
        r/cage                                          ::  data
    ==                                                  ::
  ++  rave                                              ::  general request
    $%  [%sing =mood]                                   ::  single request
        [%next =mood]                                   ::  await next version
        [%mult =mool]                                   ::  next version of any
        [%many track=? =moat]                           ::  track range
    ==                                                  ::
  ++  real                                              ::  resolved permissions
    $:  mod/?($black $white)                            ::
        who/(pair (set ship) (map @ta crew))            ::
    ==                                                  ::
  ++  regs  (map path rule)                             ::  rules for paths
  ++  riff  {p/desk q/(unit rave)}                      ::  request+desist
  ++  rite                                              ::  new permissions
    $%  {$r red/(unit rule)}                            ::  for read
        {$w wit/(unit rule)}                            ::  for write
        {$rw red/(unit rule) wit/(unit rule)}           ::  for read and write
    ==                                                  ::
  ++  riot  (unit rant)                                 ::  response+complete
  ++  rule  {mod/?($black $white) who/(set whom)}       ::  node permission
  ++  rump  {p/care q/case r/@tas s/path}               ::  relative path
  ++  saba  {p/ship q/@tas r/moar s/dome}               ::  patch+merge
  ++  soba  (list {p/path q/miso})                      ::  delta
  ++  suba  (list {p/path q/misu})                      ::  delta
  ++  tako  @                                           ::  yaki ref
  ++  toro  {p/@ta q/nori}                              ::  general change
  ++  unce                                              ::  change part
    |*  a/mold                                          ::
    $%  {%& p/@ud}                                      ::  skip[copy]
        {%| p/(list a) q/(list a)}                      ::  p -> q[chunk]
    ==                                                  ::
  ++  urge  |*(a/mold (list (unce a)))                  ::  list change
  ++  whom  (each ship @ta)                             ::  ship or named crew
  ++  yaki                                              ::  commit
    $:  p/(list tako)                                   ::  parents
        q/(map path lobe)                               ::  namespace
        r/tako                                          ::  self-reference
        t/@da                                           ::  date
    ==                                                  ::
  --  ::clay
::                                                      ::::
::::                    ++dill                            ::  (1d) console
  ::                                                    ::::
++  dill  ^?
  |%
  ::                                                    ::
  ::::                  ++able:dill                     ::  (1d1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  out result <-$
      $%  {$bbye ~}                                     ::  reset prompt
          {$blit p/(list blit)}                         ::  terminal output
          {$burl p/@t}                                  ::  activate url
          {$init p/@p}                                  ::  set owner
          {$logo ~}                                     ::  logout
          {$lyra p/@t q/@t}                             ::  upgrade kernel
          {$mass p/mass}                                ::  memory usage
          {$pack ~}                                     ::  compact memory
          {$veer p/@ta q/path r/@t}                     ::  install vane
          {$verb ~}                                     ::  verbose mode
      ==                                                ::
    ++  task                                            ::  in request ->$
      $~  [%vega ~]                                     ::
      $%  {$belt p/belt}                                ::  terminal input
          {$blew p/blew}                                ::  terminal config
          {$boot lit/? p/*}                             ::  weird %dill boot
          $>(%crud vane-task)                           ::  error with trace
          {$flog p/flog}                                ::  wrapped error
          {$flow p/@tas q/(list gill:gall)}             ::  terminal config
          {$hail ~}                                     ::  terminal refresh
          {$heft ~}                                     ::  memory report
          {$hook ~}                                     ::  this term hung up
          {$harm ~}                                     ::  all terms hung up
          $>(%init vane-task)                           ::  after gall ready
          {$lyra p/@t q/@t}                             ::  upgrade kernel
          {$noop ~}                                     ::  no operation
          {$pack ~}                                     ::  compact memory
          {$talk p/tank}                                ::
          {$text p/tape}                                ::
          {$veer p/@ta q/path r/@t}                     ::  install vane
          $>(%trim vane-task)                           ::  trim state
          $>(%vega vane-task)                           ::  report upgrade
          {$verb ~}                                     ::  verbose mode
          [%knob tag=term level=?(%hush %soft %loud)]   ::  error verbosity
      ==                                                ::
    --  ::able
  ::
  ::::                                                  ::  (1d2)
    ::
  ++  blew  {p/@ud q/@ud}                               ::  columns rows
  ++  belt                                              ::  old belt
    $%  {$aro p/?($d $l $r $u)}                         ::  arrow key
        {$bac ~}                                        ::  true backspace
        {$ctl p/@c}                                     ::  control-key
        {$del ~}                                        ::  true delete
        {$met p/@c}                                     ::  meta-key
        {$ret ~}                                        ::  return
        {$txt p/(list @c)}                              ::  utf32 text
    ==                                                  ::
  ++  blit                                              ::  old blit
    $%  {$bel ~}                                        ::  make a noise
        {$clr ~}                                        ::  clear the screen
        {$hop p/@ud}                                    ::  set cursor position
        {$lin p/(list @c)}                              ::  set current line
        {$mor ~}                                        ::  newline
        {$sag p/path q/*}                               ::  save to jamfile
        {$sav p/path q/@}                               ::  save to file
        {$url p/@t}                                     ::  activate url
    ==                                                  ::
  ++  deco  ?(~ $bl $br $un)                            ::  text decoration
  ++  dill-belt                                         ::  new belt
    $%  {$aro p/?($d $l $r $u)}                         ::  arrow key
        {$bac ~}                                        ::  true backspace
        {$cru p/@tas q/(list tank)}                     ::  echo error
        {$ctl p/@}                                      ::  control-key
        {$del ~}                                        ::  true delete
        {$hey ~}                                        ::  refresh
        {$met p/@}                                      ::  meta-key
        {$ret ~}                                        ::  return
        {$rez p/@ud q/@ud}                              ::  resize, cols, rows
        {$txt p/(list @c)}                              ::  utf32 text
        {$yow p/gill:gall}                              ::  connect to app
    ==                                                  ::
  ++  dill-blit                                         ::  new blit
    $%  {$bel ~}                                        ::  make a noise
        {$clr ~}                                        ::  clear the screen
        {$hop p/@ud}                                    ::  set cursor position
        {$klr p/stub}                                   ::  styled text
        {$mor p/(list dill-blit)}                       ::  multiple blits
        {$pom p/stub}                                   ::  styled prompt
        {$pro p/(list @c)}                              ::  show as cursor+line
        {$qit ~}                                        ::  close console
        {$out p/(list @c)}                              ::  send output line
        {$sag p/path q/*}                               ::  save to jamfile
        {$sav p/path q/@}                               ::  save to file
        {$url p/@t}                                     ::  activate url
    ==                                                  ::
  ++  flog                                              ::  sent to %dill
    $%  {$crud p/@tas q/(list tank)}                    ::
        {$heft ~}                                       ::
        {$lyra p/@t q/@t}                               ::  upgrade kernel
        {$pack ~}                                       ::  compact memory
        {$text p/tape}                                  ::
        {$veer p/@ta q/path r/@t}                       ::  install vane
        {$verb ~}                                       ::  verbose mode
    ==                                                  ::
  --  ::dill
::                                                      ::::
::::                    ++eyre                            ::  (1e) http-server
  ::                                                    ::::
++  eyre  ^?
  |%
  ++  able
    |%
    ++  gift
      $%  ::  set-config: configures the external http server
          ::
          ::    TODO: We need to actually return a (map (unit @t) http-config)
          ::    so we can apply configurations on a per-site basis
          ::
          [%set-config =http-config]
          ::  response: response to an event from earth
          ::
          [%response =http-event:http]
          ::  response to a %connect or %serve
          ::
          ::    :accepted is whether :binding was valid. Duplicate bindings are
          ::    not allowed.
          ::
          [%bound accepted=? =binding]
          ::  memory usage report
          ::
          [%mass p=mass]
      ==
    ::
    ++  task
      $~  [%vega ~]
      $%  ::  event failure notification
          ::
          $>(%crud vane-task)
          ::  initializes ourselves with an identity
          ::
          $>(%init vane-task)
          ::  new unix process
          ::
          $>(%born vane-task)
          ::  trim state (in response to memory pressure)
          ::
          $>(%trim vane-task)
          ::  report upgrade
          ::
          $>(%vega vane-task)
          ::  notifies us of the ports of our live http servers
          ::
          [%live insecure=@ud secure=(unit @ud)]
          ::  update http configuration
          ::
          [%rule =http-rule]
          ::  starts handling an inbound http request
          ::
          [%request secure=? =address =request:http]
          ::  starts handling an backdoor http request
          ::
          [%request-local secure=? =address =request:http]
          ::  cancels a previous request
          ::
          [%cancel-request ~]
          ::  connects a binding to an app
          ::
          [%connect =binding app=term]
          ::  connects a binding to a generator
          ::
          [%serve =binding =generator]
          ::  disconnects a binding
          ::
          ::    This must be called with the same duct that made the binding in
          ::    the first place.
          ::
          [%disconnect =binding]
          ::  memory usage request
          ::
          $>(%wegh vane-task)
      ==
    ::
    --
  ::  +binding: A rule to match a path.
  ::
  ::    A +binding is a system unique mapping for a path to match. A +binding
  ::    must be system unique because we don't want two handlers for a path;
  ::    what happens if there are two different actions for [~ /]?
  ::
  +$  binding
    $:  ::  site: the site to match.
        ::
        ::    A ~ will match the Urbit's identity site (your.urbit.org). Any
        ::    other value will match a domain literal.
        ::
        site=(unit @t)
        ::  path: matches this prefix path
        ::
        ::    /~myapp will match /~myapp or /~myapp/longer/path
        ::
        path=(list @t)
    ==
  ::  +generator: a generator on the local ship that handles requests
  ::
  ::    This refers to a generator on the local ship, run with a set of
  ::    arguments. Since http requests are time sensitive, we require that the
  ::    generator be on the current ship.
  ::
  +$  generator
    $:  ::  desk: desk on current ship that contains the generator
        ::
        =desk
        ::  path: path on :desk to the generator's hoon file
        ::
        path=(list @t)
        ::  args: arguments passed to the gate
        ::
        args=*
    ==
  :: +http-config: full http-server configuration
  ::
  +$  http-config
    $:  :: secure: PEM-encoded RSA private key and cert or cert chain
        ::
        secure=(unit [key=wain cert=wain])
        :: proxy: reverse TCP proxy HTTP(s)
        ::
        proxy=_|
        :: log: keep HTTP(s) access logs
        ::
        log=?
        :: redirect: send 301 redirects to upgrade HTTP to HTTPS
        ::
        ::   Note: requires certificate.
        ::
        redirect=?
    ==
  :: +http-rule: update configuration
  ::
  +$  http-rule
    $%  :: %cert: set or clear certificate and keypair
        ::
        [%cert cert=(unit [key=wain cert=wain])]
        :: %turf: add or remove established dns binding
        ::
        [%turf action=?(%put %del) =turf]
    ==
  ::  +address: client IP address
  ::
  +$  address
    $%  [%ipv4 @if]
        [%ipv6 @is]
        ::  [%ames @p]
    ==
  ::  +inbound-request: +http-request and metadata
  ::
  +$  inbound-request
    $:  ::  authenticated: has a valid session cookie
        ::
        authenticated=?
        ::  secure: whether this request was encrypted (https)
        ::
        secure=?
        ::  address: the source address of this request
        ::
        =address
        ::  request: the http-request itself
        ::
        =request:http
    ==
  ::
  ++  cred                                              ::  credential
    $:  hut/hart                                        ::  client host
        aut/(jug @tas @t)                               ::  client identities
        orx/oryx                                        ::  CSRF secret
        acl/(unit @t)                                   ::  accept-language
        cip/(each @if @is)                              ::  client IP
        cum/(map @tas *)                                ::  custom dirt
    ==                                                  ::
  ++  epic                                              ::  FCGI parameters
    $:  qix/(map @t @t)                                 ::  query
        ced/cred                                        ::  client credentials
        bem/beam                                        ::  original path
    ==                                                  ::
  ::
  ++  hart  {p/? q/(unit @ud) r/host}                   ::  http sec+port+host
  ++  hate  {p/purl q/@p r/moth}                        ::  semi-cooked request
  ++  hiss  {p/purl q/moth}                             ::  outbound request
  ++  host  (each turf @if)                             ::  http host
  ++  hoke  %+  each   {$localhost ~}                  ::  local host
            ?($.0.0.0.0 $.127.0.0.1)                    ::
  ++  httq                                              ::  raw http request
    $:  p/meth                                          ::  method
        q/@t                                            ::  unparsed url
        r/(list {p/@t q/@t})                            ::  headers
        s/(unit octs)                                   ::  body
    ==                                                  ::
  ++  httr  {p/@ud q/mess r/(unit octs)}                ::  raw http response
  ++  math  (map @t (list @t))                          ::  semiparsed headers
  ++  mess  (list {p/@t q/@t})                          ::  raw http headers
  ++  meth                                              ::  http methods
    $?  $conn                                           ::  CONNECT
        $delt                                           ::  DELETE
        $get                                            ::  GET
        $head                                           ::  HEAD
        $opts                                           ::  OPTIONS
        $post                                           ::  POST
        $put                                            ::  PUT
        $trac                                           ::  TRACE
    ==                                                  ::
  ++  mite  (list @ta)                                  ::  mime type
  ++  moth  {p/meth q/math r/(unit octs)}               ::  http operation
  ++  octs  {p/@ud q/@t}                                ::  octet-stream
  ++  oryx  @t                                          ::  CSRF secret
  ++  pork  {p/(unit @ta) q/(list @t)}                  ::  fully parsed url
  :: +prox: proxy notification
  ::
  ::   Used on both the proxy (ward) and upstream sides for
  ::   sending/receiving proxied-request notifications.
  ::
  +=  prox
    $:  :: por: tcp port
        ::
        por=@ud
        :: sek: secure?
        ::
        sek=?
        :: non: authentication nonce
        ::
        non=@uvJ
    ==
  ++  purf  (pair purl (unit @t))                       ::  url with fragment
  ++  purl  {p/hart q/pork r/quay}                      ::  parsed url
  ++  quay  (list {p/@t q/@t})                          ::  parsed url query
  ++  quer  |-($@(~ {p/@t q/@t t/$}))                  ::  query tree
  ++  quri                                              ::  request-uri
    $%  {%& p/purl}                                     ::  absolute
        {%| p/pork q/quay}                              ::  relative
    ==                                                  ::
  ::  +reserved: check if an ipv4 address is in a reserved range
  ::
  ++  reserved
    |=  a=@if
    ^-  ?
    =/  b  (flop (rip 3 a))
    ::  0.0.0.0/8 (software)
    ::
    ?.  ?=([@ @ @ @ ~] b)  &
    ?|  ::  10.0.0.0/8 (private)
        ::
        =(10 i.b)
        ::  100.64.0.0/10 (carrier-grade NAT)
        ::
        &(=(100 i.b) (gte i.t.b 64) (lte i.t.b 127))
        ::  127.0.0.0/8 (localhost)
        ::
        =(127 i.b)
        ::  169.254.0.0/16 (link-local)
        ::
        &(=(169 i.b) =(254 i.t.b))
        ::  172.16.0.0/12 (private)
        ::
        &(=(172 i.b) (gte i.t.b 16) (lte i.t.b 31))
        ::  192.0.0.0/24 (protocol assignment)
        ::
        &(=(192 i.b) =(0 i.t.b) =(0 i.t.t.b))
        ::  192.0.2.0/24 (documentation)
        ::
        &(=(192 i.b) =(0 i.t.b) =(2 i.t.t.b))
        ::  192.18.0.0/15 (reserved, benchmark)
        ::
        &(=(192 i.b) |(=(18 i.t.b) =(19 i.t.b)))
        ::  192.51.100.0/24 (documentation)
        ::
        &(=(192 i.b) =(51 i.t.b) =(100 i.t.t.b))
        ::  192.88.99.0/24 (reserved, ex-anycast)
        ::
        &(=(192 i.b) =(88 i.t.b) =(99 i.t.t.b))
        ::  192.168.0.0/16 (private)
        ::
        &(=(192 i.b) =(168 i.t.b))
        ::  203.0.113/24 (documentation)
        ::
        &(=(203 i.b) =(0 i.t.b) =(113 i.t.t.b))
        ::  224.0.0.0/8 (multicast)
        ::  240.0.0.0/4 (reserved, future)
        ::  255.255.255.255/32 (broadcast)
        ::
        (gte i.b 224)
    ==
  ++  rout  {p/(list host) q/path r/oryx s/path}        ::  http route (new)
  ++  user  knot                                        ::  username
  --  ::eyre
::                                                      ::::
::::                    ++ford                            ::  (1f) build
  ::                                                    ::::
::  |ford: build system vane interface
::
++  ford  ^?
  |%
  ::  |able:ford: ford's public +move interface
  ::
  ++  able  ^?
    |%
    ::  +task:able:ford: requests to ford
    ::
    +=  task
      $~  [%vega ~]
      $%  ::  %build: perform a build, either live or once
          ::
          $:  %build
              ::  live: whether we run this build live
              ::
              ::    A live build will subscribe to further updates and keep the
              ::    build around.
              ::
              live=?
              ::  plan: the schematic to build
              ::
              =schematic
          ==
          ::  %keep: reset cache sizes
          ::
          [%keep compiler-cache=@ud build-cache=@ud]
          ::  %kill: stop a build; send on same duct as original %build request
          ::
          [%kill ~]
          ::  trim state (in response to memory pressure)
          ::
          $>(%trim vane-task)
          ::  %vega: report kernel upgrade
          ::
          $>(%vega vane-task)
          ::  %wegh: produce memory usage information
          ::
          $>(%wegh vane-task)
          ::  %wipe: wipes stored builds
          ::
          [%wipe percent-to-remove=@ud]
      ==
    ::  +gift:able:ford: responses from ford
    ::
    +=  gift
      $%  ::  %mass: memory usage; response to %wegh +task
          ::
          [%mass p=mass]
          ::  %made: build result; response to %build +task
          ::
          $:  %made
              ::  date: formal date of the build
              ::
              date=@da
              ::  result: result of the build; either complete build, or error
              ::
              result=made-result
      ==  ==
    --
  ::  +made-result: the main payload for a %made +gift
  ::
  +=  made-result
    $%  ::  %complete: contains the result of the completed build
        ::
        [%complete =build-result]
        ::  %incomplete: couldn't finish build; contains error message
        ::
        [%incomplete =tang]
    ==
  ::  +disc: a desk on a ship; can be used as a beak that varies with time
  ::
  +=  disc  [=ship =desk]
  ::  +rail: a time-varying full path
  ::
  ::    This can be thought of as a +beam without a +case, which is what
  ::    would specify the time. :spur is flopped just like the +spur in a +beam.
  ::
  +=  rail  [=disc =spur]
  ::  +resource: time-varying dependency on a value from the urbit namespace
  ::
  +=  resource
    $:  ::  vane which we are querying
        ::
        vane=%c
        ::  type of request
        ::
        ::    TODO: care:clay should be cleaned up in zuse as it is a general
        ::    type, not a clay specific one.
        ::
        care=care:clay
        ::  path on which to depend, missing time, which will be filled in
        ::
        =rail
    ==
  ::  +build-result: the referentially transparent result of a +build
  ::
  ::    A +build produces either an error or a result. A result is a tagged
  ::    union of the various kinds of datatypes a build can produce. The tag
  ::    represents the sub-type of +schematic that produced the result.
  ::
  +=  build-result
    $%  ::  %error: the build produced an error whose description is :message
        ::
        [%error message=tang]
        ::  %success: result of successful +build, tagged by +schematic sub-type
        ::
        $:  %success
            $^  [head=build-result tail=build-result]
            $%  [%$ =cage]
                [%alts =build-result]
                [%bake =cage]
                [%bunt =cage]
                [%call =vase]
                [%cast =cage]
                [%core =vase]
                [%diff =cage]
                [%hood =scaffold]
                [%join =cage]
                [%list results=(list build-result)]
                [%mash =cage]
                [%mute =cage]
                [%pact =cage]
                [%path =rail]
                [%plan =vase]
                [%reef =vase]
                [%ride =vase]
                [%scry =cage]
                [%slim [=type =nock]]
                [%slit =type]
                [%vale =cage]
                [%volt =cage]
                [%walk results=(list mark-action)]
    ==  ==  ==
  ::  +mark-action: represents a single mark conversion step
  ::
  ::    In mark conversion, we want to convert from :source to :target. We also
  ::    need to keep track of what type of conversion this is. If %grab, we
  ::    want to use the definitions in the :target mark. If %grow, we want to
  ::    use the :source mark.
  ::
  +=  mark-action  [type=?(%grow %grab) source=term target=term]
  ::
  ::  +schematic: plan for building
  ::
  ++  schematic
    ::    If the head of the +schematic is a pair, it's an auto-cons
    ::    schematic. Its result will be the pair of results of its
    ::    sub-schematics.
    ::
    $^  [head=schematic tail=schematic]
    ::
    $%  ::  %$: literal value. Produces its input unchanged.
        ::
        $:  %$
            ::  literal: the value to be produced by the build
            ::
            literal=cage
        ==
        ::  %pin: pins a sub-schematic to a date
        ::
        ::    There is a difference between live builds and once builds. In
        ::    live builds, we produce results over and over again and aren't
        ::    pinned to a specifc time. In once builds, we want to specify a
        ::    specific date, which we apply recursively to any sub-schematics
        ::    contained within :schematic.
        ::
        ::    If a build has a %pin at the top level, we consider it to be a
        ::    once build. Otherwise, we consider it to be a live build. We do
        ::    this so schematics which depend on the result of a once build can
        ::    be cached, giving the client explicit control over the caching
        ::    behaviour.
        ::
        $:  %pin
            ::  date: time at which to perform the build
            ::
            date=@da
            ::  schematic: wrapped schematic of pinned time
            ::
            =schematic
        ==
        ::  %alts: alternative build choices
        ::
        ::    Try each choice in :choices, in order; accept the first one that
        ::    succeeds. Note that the result inherits the dependencies of all
        ::    failed schematics, as well as the successful one.
        ::
        $:  %alts
            ::  choices: list of build options to try
            ::
            choices=(list schematic)
        ==
        ::  %bake: run a file through a renderer
        ::
        $:  %bake
            ::  renderer: name of renderer; also its file path in ren/
            ::
            renderer=term
            ::  query-string: the query string of the renderer's http path
            ::
            query-string=coin
            ::  path-to-render: full path of file to render
            ::
            path-to-render=rail
        ==
        ::  %bunt: produce the default value for a mark
        ::
        $:  %bunt
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark; also its file path in mar/
            ::
            mark=term
        ==
        ::  %call: call a gate on a sample
        ::
        $:  %call
            ::  gate: schematic whose result is a gate
            ::
            gate=schematic
            ::  sample:  schematic whose result will be the gate's sample
            ::
            sample=schematic
        ==
        ::  %cast: cast the result of a schematic through a mark
        ::
        $:  %cast
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark; also its file path in ren/
            ::
            mark=term
            ::  input: schematic whose result will be run through the mark
            ::
            input=schematic
        ==
        ::  %core: build a hoon program from a source file
        ::
        $:  %core
            ::  source-path: clay path from which to load hoon source
            ::
            source-path=rail
        ==
        ::  %diff: produce marked diff from :first to :second
        ::
        $:  %diff
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  old: schematic producing data to be used as diff starting point
            ::
            start=schematic
            ::  new: schematic producing data to be used as diff ending point
            ::
            end=schematic
        ==
        ::  %dude: wrap a failure's error message with an extra message
        ::
        $:  %dude
            ::  error: a trap producing an error message to wrap the original
            ::
            error=tank
            ::  attempt: the schematic to try, whose error we wrap, if any
            ::
            attempt=schematic
        ==
        ::  %hood: create a +hood from a hoon source file
        ::
        $:  %hood
            ::  source-path: clay path from which to load hoon source
            ::
            source-path=rail
        ==
        ::  %join: merge two diffs into one diff; produces `~` if conflicts
        ::
        $:  %join
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of the mark to use for diffs; also file path in mar/
            ::
            mark=term
            ::  first: schematic producing first diff
            ::
            first=schematic
            ::  second: schematic producing second diff
            ::
            second=schematic
        ==
        ::  %list: performs a list of schematics, returns a list of +builds-results
        ::
        $:  %list
            ::  schematics: list of builds to perform
            ::
            schematics=(list schematic)
        ==
        ::  %mash: force a merge, annotating any conflicts
        ::
        $:  %mash
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark used in diffs; also file path in mar/
            ::
            mark=term
            ::  first: marked schematic producing first diff
            ::
            first=[=disc mark=term =schematic]
            ::  second: marked schematic producing second diff
            ::
            second=[=disc mark=term =schematic]
        ==
        ::  %mute: mutate a noun by replacing its wings with new values
        ::
        $:  %mute
            ::  subject: schematic producing the noun to mutate
            ::
            subject=schematic
            ::  mutations: axes and schematics to produce their new contents
            ::
            mutations=(list (pair wing schematic))
        ==
        ::  %pact: patch a marked noun by applying a diff
        ::
        $:  %pact
            ::  disc where in clay to load marks from
            ::
            =disc
            ::  start: schematic producing a noun to be patched
            ::
            start=schematic
            ::  diff: schematic producing the diff to apply to :start
            ::
            diff=schematic
        ==
        ::  %path: resolve a path with `-`s to a path with `/`s
        ::
        ::    Resolve +raw-path to a path containing a file, replacing
        ::    any `-`s in the path with `/`s if no file exists at the
        ::    original path. Produces an error if multiple files match,
        ::    e.g. a/b/c and a/b-c, or a/b/c and a-b/c.
        ::
        $:  %path
            ::  disc: the +disc forming the base of the path to be resolved
            ::
            =disc
            ::  prefix: path prefix under which to resolve :raw-path, e.g. lib
            ::
            prefix=@tas
            ::  raw-path: the file path to be resolved
            ::
            raw-path=@tas
        ==
        ::  %plan: build a hoon program from a preprocessed source file
        ::
        $:  %plan
            ::  path-to-render: the clay path of a file being rendered
            ::
            ::    TODO: Once we've really implemented this, write the
            ::    documentation. (This is the path that starts out as the path
            ::    of the hoon source which generated the scaffold, but can be
            ::    changed with `/:`.)
            ::
            path-to-render=rail
            ::  query-string: the query string of the http request
            ::
            query-string=coin
            ::  scaffold: preprocessed hoon source and imports
            ::
            =scaffold
        ==
        ::  %reef: produce a hoon+zuse kernel. used internally for caching
        ::
        $:  %reef
            ::  disc: location of sys/hoon/hoon and sys/zuse/hoon
            ::
            =disc
        ==
        ::  %ride: eval hoon as formula with result of a schematic as subject
        ::
        $:  %ride
            ::  formula: a hoon to be evaluated against a subject
            ::
            formula=hoon
            ::  subject: a schematic whose result will be used as subject
            ::
            subject=schematic
        ==
        ::  %same: the identity function
        ::
        ::    Functionally used to "unpin" a build for caching reasons. If you
        ::    run a %pin build, it is treated as a once build and is therefore
        ::    not cached. Wrapping the %pin schematic in a %same schematic
        ::    converts it to a live build, which will be cached due to live
        ::    build subscription semantics.
        ::
        $:  %same
            ::  schematic that we evaluate to
            ::
            =schematic
        ==
        ::  %scry: lookup a value from the urbit namespace
        ::
        $:  %scry
            ::  resource: a namespace request, with unspecified time
            ::
            ::    Schematics can only be resolved when specifying a time,
            ::    which will convert this +resource into a +scry-request.
            ::
            =resource
        ==
        ::  %slim: compile a hoon against a subject type
        ::
        $:  %slim
            ::  compile-time subject type for the :formula
            ::
            subject-type=type
            ::  formula: a +hoon to be compiled to (pair type nock)
            ::
            formula=hoon
        ==
        ::  %slit: get type of gate product
        ::
        $:  %slit
            ::  gate: a vase containing a gate
            ::
            gate=vase
            ::  sample: a vase containing the :gate's sample
            ::
            sample=vase
        ==
        ::  %vale: coerce a noun to a mark, validated
        ::
        $:  %vale
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark to use; also file path in mar/
            ::
            mark=term
            ::  input: the noun to be converted using the mark
            ::
            input=*
        ==
        ::  %volt: coerce a noun to a mark, unsafe
        ::
        $:  %volt
            ::  disc where in clay to load the mark from
            ::
            =disc
            ::  mark: name of mark to use; also file path in mar/
            ::
            mark=term
            ::  input: the noun to be converted using the mark
            ::
            input=*
        ==
        ::  %walk: finds a mark conversion path between two marks
        ::
        $:  %walk
            ::  disc in clay to load the marks from
            ::
            =disc
            ::  source: the original mark type
            ::
            source=term
            ::  target: the destination mark type
            ::
            target=term
        ==
    ==
  ::
  ::  +scaffold: program construction in progress
  ::
  ::    A source file with all its imports and requirements, which will be
  ::    built and combined into one final product.
  ::
  +=  scaffold
    $:  ::  source-rail: the file this scaffold was parsed from
        ::
        source-rail=rail
        ::  zuse-version: the kelvin version of the standard library
        ::
        zuse-version=@ud
        ::  structures: files from %/sur which are included
        ::
        structures=(list cable)
        ::  libraries: files from %/lib which are included
        ::
        libraries=(list cable)
        ::  cranes: a list of resources to transform and include
        ::
        cranes=(list crane)
        ::  sources: hoon sources, either parsed or on the filesystem
        ::
        sources=(list hoon)
    ==
  ::  +cable: a reference to something on the filesystem
  ::
  +=  cable
    $:  ::  face: the face to wrap around the imported file
        ::
        face=(unit term)
        ::  file-path: location in clay
        ::
        file-path=term
    ==
  ::  +truss: late-bound path
  ::
  ::    TODO: the +tyke data structure should be rethought, possibly as part
  ::    of this effort since it is actually a `(list (unit hoon))`, when it
  ::    only represents @tas. It should be a structure which explicitly
  ::    represents a path with holes that need to be filled in.
  ::
  +=  truss
    $:  pre=(unit tyke)
        pof=(unit [p=@ud q=tyke])
    ==
  ::  +crane: parsed rune used to include and transform resources
  ::
  ::    Cranes lifting cranes lifting cranes!
  ::
  ::    A recursive tree of Ford directives that specifies instructions for
  ::    including and transforming resources from the Urbit namespace.
  ::
  +=  crane
    $%  $:  ::  %fssg: `/~` hoon literal
            ::
            ::    `/~ <hoon>` produces a crane that evaluates arbitrary hoon.
            ::
            %fssg
            =hoon
        ==
        $:  ::  %fsbc: `/$` process query string
            ::
            ::    `/$` will call a gate with the query string supplied to this
            ::    build. If no query string, this errors.
            ::
            %fsbc
            =hoon
        ==
        $:  ::  %fsbr: `/|` first of many options that succeeds
            ::
            ::    `/|` takes a series of cranes and produces the first one
            ::    (left-to-right) that succeeds. If none succeed, it produces
            ::    stack traces from all of its arguments.
            ::
            %fsbr
            ::  choices: cranes to try
            ::
            choices=(list crane)
        ==
        $:  ::  %fsts: `/=` wrap a face around a crane
            ::
            ::    /= runs a crane (usually produced by another ford rune), takes
            ::    the result of that crane, and wraps a face around it.
            ::
            %fsts
            ::  face: face to apply
            ::
            face=term
            ::  crane: internal build step
            ::
            =crane
        ==
        $:  ::  %fsdt: `/.` null-terminated list
            ::
            ::    Produce a null-terminated list from a sequence of cranes,
            ::    terminated by a `==`.
            ::
            %fsdt
            ::  items: cranes to evaluate
            ::
            items=(list crane)
        ==
        $:  ::  %fscm: `/,` switch by path
            ::
            ::    `/,` is a switch statement, which picks a branch to evaluate
            ::    based on whether the current path matches the path in the
            ::    switch statement. Takes a sequence of pairs of (path, crane)
            ::    terminated by a `==`.
            ::
            %fscm
            ::  cases: produces evaluated crane of first +spur match
            ::
            cases=(list (pair spur crane))
        ==
        $:  ::  %fspm: `/&` pass through a series of marks
            ::
            ::    `/&` passes a crane through multiple marks, right-to-left.
            ::
            %fspm
            ::  marks: marks to apply to :crane, in reverse order
            ::
            marks=(list mark)
            =crane
        ==
        $:  ::  %fscb: `/_` run a crane on each file in the current directory
            ::
            ::    `/_` takes a crane as an argument. It produces a new crane
            ::    representing the result of mapping the supplied crane over the
            ::    list of files in the current directory. The keys in the
            ::    resulting map are the basenames of the files in the directory,
            ::    and each value is the result of running that crane on the
            ::    contents of the file.
            ::
            %fscb
            =crane
        ==
        $:  ::  %fssm: `/;` operate on
            ::
            ::    `/;` takes a hoon and a crane. The hoon should evaluate to a
            ::    gate, which is then called with the result of the crane as its
            ::    sample.
            ::
            %fssm
            =hoon
            =crane
        ==
        $:  ::  %fscl: `/:` evaluate at path
            ::
            ::    `/:` takes a path and a +crane, and evaluates the crane with
            ::    the current path set to the supplied path.
            ::
            %fscl
            ::  path: late bound path to be resolved relative to current beak
            ::
            ::    This becomes current path of :crane
            ::
            path=truss
            =crane
        ==
        $:  ::  %fskt: `/^` cast
            ::
            ::    `/^` takes a +mold and a +crane, and casts the result of the
            ::    crane to the mold.
            ::
            %fskt
            ::  mold: evaluates to a mold to be applied to :crane
            ::
            =spec
            =crane
        ==
        $:  ::  %fstr: `/*` run :crane on all files with current path as prefix
            ::
            %fstr
            =crane
        ==
        $:  ::  %fszp: `/!mark/` evaluate as hoon, then pass through mark
            ::
            %fszp
            =mark
        ==
        $:  ::  %fszy: `/mark/` passes current path through :mark
            ::
            %fszy
            =mark
    ==  ==
  ::  +result-to-cage: extract a +cage from a +build-result
  ::
  ++  result-to-cage
    |=  result=build-result
    ^-  cage
    ?:  ?=(%error -.result)
      [%tang !>(message.result)]
    ?-    -.+.result
        ^      [%noun (slop q:$(result head.result) q:$(result tail.result))]
        %$     cage.result
        %alts  $(result build-result.result)
        %bake  cage.result
        %bunt  cage.result
        %call  [%noun vase.result]
        %cast  cage.result
        %core  [%noun vase.result]
        %diff  cage.result
        %hood  [%noun !>(scaffold.result)]
        %join  cage.result
        %list  [%noun -:!>(*(list cage)) (turn results.result result-to-cage)]
        %mash  cage.result
        %mute  cage.result
        %pact  cage.result
        %path  [%noun !>(rail.result)]
        %plan  [%noun vase.result]
        %reef  [%noun vase.result]
        %ride  [%noun vase.result]
        %scry  cage.result
        %slim  [%noun !>([type nock]:result)]
        %slit  [%noun !>(type.result)]
        %vale  cage.result
        %volt  cage.result
        %walk  [%noun !>(results.result)]
    ==
  ::  +result-as-error: extracts a tang out of a made-result
  ::
  ++  made-result-as-error
    |=  result=made-result
    ^-  tang
    ?:  ?=([%incomplete *] result)
      tang.result
    ?:  ?=([%complete %error *] result)
      message.build-result.result
    ~
  --
::                                                      ::::
::::                    ++gall                            ::  (1g) extensions
  ::                                                    ::::
++  gall  ^?
  |%
  ::                                                    ::
  ::::                  ++able:gall                     ::  (1g1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    |%
    ++  gift                                            ::  outgoing result
      $%  [%boon payload=*]                             ::  ames response
          [%done error=(unit error:ames)]               ::  ames message (n)ack
          [%mass p=mass]                                ::  memory usage
          [%onto p=(each suss tang)]                    ::  about agent
          [%unto p=sign:agent]                          ::
      ==                                                ::
    ++  task                                            ::  incoming request
      $~  [%vega ~]                                     ::
      $%  [$conf p=dock q=dock]                         ::  configure app
          [$deal p=sock q=term r=deal]                  ::  full transmission
          [%goad force=? agent=(unit dude)]             ::  rebuild agent(s)
          [%sear =ship]                                 ::  clear pending queues
          $>(%init vane-task)                           ::  set owner
          $>(%trim vane-task)                           ::  trim state
          $>(%vega vane-task)                           ::  report upgrade
          $>(%plea vane-task)                           ::  network request
          [%wash ~]                                     ::  clear caches
          $>(%wegh vane-task)                           ::  report memory
      ==                                                ::
    --  ::able
  ++  bitt  (map duct (pair ship path))                 ::  incoming subs
  ++  boat                                              ::  outgoing subs
    %+  map  ,[=wire =ship =term]                       ::
    ,[acked=? =path]                                    ::
  ++  bowl                                              ::  standard app state
          $:  $:  our=ship                              ::  host
                  src=ship                              ::  guest
                  dap=term                              ::  agent
              ==                                        ::
              $:  wex=boat                              ::  outgoing subs
                  sup=bitt                              ::  incoming subs
              ==                                        ::
              $:  act=@ud                               ::  change number
                  eny=@uvJ                              ::  entropy
                  now=@da                               ::  current time
                  byk=beak                              ::  load source
          ==  ==                                        ::
  ++  dude  term                                        ::  server identity
  ++  gill  (pair ship term)                            ::  general contact
  ++  scar                                              ::  opaque duct
    $:  p=@ud                                           ::  bone sequence
        q=(map duct bone)                               ::  by duct
        r=(map bone duct)                               ::  by bone
    ==                                                  ::
  ++  suss  (trel dude @tas @da)                        ::  config report
  ++  well  (pair desk term)                            ::
  ++  neat
    $%  [%arvo =note-arvo]
        [%agent [=ship name=term] =deal]
    ==
  +$  deal
    $%  [%raw-poke =mark =noun]
        task:agent
    ==
  ::
  ::  +agent: app core
  ::
  ++  agent
    =<  form
    |%
    +$  step  (quip card form)
    +$  card  (wind note gift)
    +$  note
      $%  [%arvo =note-arvo]
          [%agent [=ship name=term] =task]
      ==
    +$  task
      $%  [%watch =path]
          [%watch-as =mark =path]
          [%leave ~]
          [%poke =cage]
          [%poke-as =mark =cage]
      ==
    +$  gift
      $%  [%fact paths=(list path) =cage]
          [%kick paths=(list path) ship=(unit ship)]
          [%watch-ack p=(unit tang)]
          [%poke-ack p=(unit tang)]
      ==
    +$  sign
      $%  [%poke-ack p=(unit tang)]
          [%watch-ack p=(unit tang)]
          [%fact =cage]
          [%kick ~]
      ==
    ++  form
      $_  ^|
      |_  bowl
      ++  on-init
        *(quip card _^|(..on-init))
      ::
      ++  on-save
        *vase
      ::
      ++  on-load
        |~  old-state=vase
        *(quip card _^|(..on-init))
      ::
      ++  on-poke
        |~  [mark vase]
        *(quip card _^|(..on-init))
      ::
      ++  on-watch
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-leave
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-peek
        |~  path
        *(unit (unit cage))
      ::
      ++  on-agent
        |~  [wire sign]
        *(quip card _^|(..on-init))
      ::
      ++  on-arvo
        |~  [wire sign-arvo]
        *(quip card _^|(..on-init))
      ::
      ++  on-fail
        |~  [term tang]
        *(quip card _^|(..on-init))
      --
    --
  --  ::gall
::  %iris http-client interface
::
++  iris  ^?
  |%
  ++  able
    |%
    ::  +gift: effects the client can emit
    ::
    ++  gift
      $%  ::  %request: outbound http-request to earth
          ::
          ::    TODO: id is sort of wrong for this interface; the duct should
          ::    be enough to identify which request we're talking about?
          ::
          [%request id=@ud request=request:http]
          ::  %cancel-request: tell earth to cancel a previous %request
          ::
          [%cancel-request id=@ud]
          ::  %response: response to the caller
          ::
          [%http-response =client-response]
          ::  memory usage report
          ::
          [%mass p=mass]
      ==
    ::
    ++  task
      $~  [%vega ~]
      $%  ::  event failure notification
          ::
          $>(%crud vane-task)
          ::  system started up; reset open connections
          ::
          $>(%born vane-task)
          ::  trim state (in response to memory pressure)
          ::
          $>(%trim vane-task)
          ::  report upgrade
          ::
          $>(%vega vane-task)
          ::  fetches a remote resource
          ::
          [%request =request:http =outbound-config]
          ::  cancels a previous fetch
          ::
          [%cancel-request ~]
          ::  receives http data from outside
          ::
          [%receive id=@ud =http-event:http]
          ::  memory usage request
          ::
          $>(%wegh vane-task)
      ==
    --
  ::  +client-response: one or more client responses given to the caller
  ::
  +$  client-response
    $%  ::  periodically sent as an update on the duct that sent %fetch
        ::
        $:  %progress
            ::  http-response-header: full transaction header
            ::
            ::    In case of a redirect chain, this is the target of the
            ::    final redirect.
            ::
            =response-header:http
            ::  bytes-read: bytes fetched so far
            ::
            bytes-read=@ud
            ::  expected-size: the total size if response had a content-length
            ::
            expected-size=(unit @ud)
            ::  incremental: data received since the last %http-progress
            ::
            incremental=(unit octs)
        ==
        ::  final response of a download, parsed as mime-data if successful
        ::
        [%finished =response-header:http full-file=(unit mime-data)]
        ::  canceled by the runtime system
        ::
        [%cancel ~]
    ==
  ::  mime-data: externally received but unvalidated mimed data
  ::
  +$  mime-data
    [type=@t data=octs]
  ::  +outbound-config: configuration for outbound http requests
  ::
  +$  outbound-config
    $:  ::  number of times to follow a 300 redirect before erroring
        ::
        ::    Common values for this will be 3 (the limit most browsers use), 5
        ::    (the limit recommended by the http standard), or 0 (let the
        ::    requester deal with 300 redirects).
        ::
        redirects=_5
        ::  number of times to retry before failing
        ::
        ::    When we retry, we'll automatically try to use the 'Range' header
        ::    to resume the download where we left off if we have the
        ::    'Accept-Range: bytes' in the original response.
        ::
        retries=_3
    ==
  ::  +to-httr: adapts to old eyre interface
  ::
  ++  to-httr
    |=  [header=response-header:http full-file=(unit mime-data)]
    ^-  httr:eyre
    ::
    =/  data=(unit octs)
      ?~(full-file ~ `data.u.full-file)
    ::
    [status-code.header headers.header data]
  --
::                                                      ::::
::::                    ++jael                          ::  (1h) security
  ::                                                    ::::
++  jael  ^?
  |%
  ::                                                    ::
  ::::                  ++able:jael                     ::  (1h1) arvo moves
    ::                                                  ::::
  ++  able  ^?
    =,  pki
    |%
    +$  public-keys-result
      $%  [%full points=(map ship point)]
          [%diff who=ship =diff:point]
          [%breach who=ship]
      ==
    ::                                                  ::
    ++  gift                                            ::  out result <-$
      $%  [%init p=ship]                                ::  report install unix
          [%mass p=mass]                                ::  memory usage report
          [%done error=(unit error:ames)]               ::  ames message (n)ack
          [%boon payload=*]                             ::  ames response
          [%private-keys =life vein=(map life ring)]    ::  private keys
          [%public-keys =public-keys-result]            ::  ethereum changes
          [%turf turf=(list turf)]                      ::  domains
      ==                                                ::
    ::  +seed: private boot parameters
    ::
    +$  seed  [who=ship lyf=life key=ring sig=(unit oath:pki)]
    ::
    +=  task                                            ::  in request ->$
      $~  [%vega ~]                                     ::
      $%  [%dawn dawn-event]                            ::  boot from keys
          [%fake =ship]                                 ::  fake boot
          [%listen whos=(set ship) =source]             ::  set ethereum source
          ::TODO  %next for generating/putting new private key
          [%meet =ship =life =pass]                     ::  met after breach
          [%moon =ship =udiff:point]                    ::  register moon keys
          [%nuke whos=(set ship)]                       ::  cancel tracker from
          [%private-keys ~]                             ::  sub to privates
          [%public-keys ships=(set ship)]               ::  sub to publics
          [%rekey =life =ring]                          ::  update private keys
          $>(%trim vane-task)                           ::  trim state
          [%turf ~]                                     ::  view domains
          $>(%vega vane-task)                           ::  report upgrade
          $>(%wegh vane-task)                           ::  memory usage request
          $>(%plea vane-task)                           ::  ames request
      ==                                                ::
    ::
    +$  dawn-event
      $:  =seed
          spon=(list [=ship point:azimuth-types])
          czar=(map ship [=rift =life =pass])
          turf=(list turf)
          bloq=@ud
          node=(unit purl:eyre)
      ==
    ::
    ++  block
      =<  block
      |%
      +$  hash    @uxblockhash
      +$  number  @udblocknumber
      +$  id      [=hash =number]
      +$  block   [=id =parent=hash]
      --
    ::
    ::  Azimuth points form a groupoid, where the objects are all the
    ::  possible values of +point and the arrows are the possible values
    ::  of (list point-diff).  Composition of arrows is concatenation,
    ::  and you can apply the diffs to a +point with +apply.
    ::
    ::  It's simplest to consider +point as the coproduct of three
    ::  groupoids, Rift, Keys, and Sponsor.  Recall that the coproduct
    ::  of monoids is the free monoid (Kleene star) of the coproduct of
    ::  the underlying sets of the monoids.  The construction for
    ::  groupoids is similar.  Thus, the objects of the coproduct are
    ::  the product of the objects of the underlying groupoids.  The
    ::  arrows are a list of a sum of the diff types of the underlying
    ::  groupoids.  Given an arrow=(list diff), you can project to the
    ::  underlying arrows with +skim filtering on the head of each diff.
    ::
    ::  The identity element is ~.  Clearly, composing this with any
    ::  +diff gives the original +diff.  Since this is a category,
    ::  +compose must be associative (true, because concatenation is
    ::  associative).  This is a groupoid, so we must further have that
    ::  every +point-diff has an inverse.  These are given by the
    ::  +inverse operation.
    ::
    ++  point
      =<  point
      |%
      +$  point
        $:  =rift
            =life
            keys=(map life [crypto-suite=@ud =pass])
            sponsor=(unit @p)
        ==
      ::
      +$  key-update  [=life crypto-suite=@ud =pass]
      ::
      ::  Invertible diffs
      ::
      +$  diffs  (list diff)
      +$  diff
        $%  [%rift from=rift to=rift]
            [%keys from=key-update to=key-update]
            [%spon from=(unit @p) to=(unit @p)]
        ==
      ::
      ::  Non-invertible diffs
      ::
      +$  udiffs  (list [=ship =udiff])
      +$  udiff
        $:  =id:block
        $%  [%rift =rift]
            [%keys key-update]
            [%spon sponsor=(unit @p)]
            [%disavow ~]
        ==  ==
      ::
      ++  udiff-to-diff
        |=  [=a=udiff =a=point]
        ^-  (unit diff)
        ?-    +<.a-udiff
            %disavow  ~|(%udiff-to-diff-disavow !!)
            %spon     `[%spon sponsor.a-point sponsor.a-udiff]
            %rift
          ?.  (gth rift.a-udiff rift.a-point)
            ~
          ~?  !=(rift.a-udiff +(rift.a-point))
            [%udiff-to-diff-skipped-rift a-udiff a-point]
          `[%rift rift.a-point rift.a-udiff]
        ::
            %keys
          ?.  (gth life.a-udiff life.a-point)
            ~
          ~?  !=(life.a-udiff +(life.a-point))
            [%udiff-to-diff-skipped-life a-udiff a-point]
          :^  ~  %keys
            [life.a-point (~(gut by keys.a-point) life.a-point *[@ud pass])]
          [life crypto-suite pass]:a-udiff
        ==
      ::
      ++  inverse
        |=  diffs=(list diff)
        ^-  (list diff)
        %-  flop
        %+  turn  diffs
        |=  =diff
        ^-  ^diff
        ?-  -.diff
          %rift  [%rift to from]:diff
          %keys  [%keys to from]:diff
          %spon  [%spon to from]:diff
        ==
      ::
      ++  compose
        (bake weld ,[(list diff) (list diff)])
      ::
      ++  apply
        |=  [diffs=(list diff) =a=point]
        (roll diffs (apply-diff a-point))
      ::
      ++  apply-diff
        |=  a=point
        |:  [*=diff a-point=a]
        ^-  point
        ?-    -.diff
            %rift
          ?>  =(rift.a-point from.diff)
          a-point(rift to.diff)
        ::
            %keys
          ?>  =(life.a-point life.from.diff)
          ?>  =((~(get by keys.a-point) life.a-point) `+.from.diff)
          %_  a-point
            life  life.to.diff
            keys  (~(put by keys.a-point) life.to.diff +.to.diff)
          ==
        ::
            %spon
          ?>  =(sponsor.a-point from.diff)
          a-point(sponsor to.diff)
        ==
      --
    --                                                  ::
  ::                                                    ::
  ::::                                                  ::
    ::                                                  ::
  +$  source  (each ship term)
  +$  source-id  @udsourceid
  ::
  ::  +state-eth-node: state of a connection to an ethereum node
  ::
  +$  state-eth-node                                    ::  node config + meta
    $:  top-source-id=source-id
        sources=(map source-id source)
        sources-reverse=(map source source-id)
        default-source=source-id
        ship-sources=(map ship source-id)
        ship-sources-reverse=(jug source-id ship)
    ==                                                  ::
  ::                                                    ::
  ::::                  ++pki:jael                      ::  (1h2) certificates
    ::                                                  ::::
  ++  pki  ^?
    |%
    ::TODO  update to fit azimuth-style keys
    ::  the urbit meta-certificate (++will) is a sequence
    ::  of certificates (++cert).  each cert in a will
    ::  revokes and replaces the previous cert.  the
    ::  version number of a ship is a ++life.
    ::
    ::  the deed contains an ++arms, a definition
    ::  of cosmetic identity; a semi-trusted parent,
    ::  which signs the initial certificate and provides
    ::  routing services; and a dirty bit.  if the dirty
    ::  bit is set, the new life of this ship may have
    ::  lost information that the old life had.
    ::
    ++  hand  @uvH                                      ::  128-bit hash
    ++  mind  {who/ship lyf/life}                       ::  key identifier
    ++  name  (pair @ta @t)                             ::  ascii / unicode
    ++  oath  @                                         ::  signature
    --  ::  pki
  --  ::  jael
::
++  gift-arvo                                           ::  out result <-$
  $~  [%init ~zod]
  $%  gift:able:ames
      gift:able:behn
      gift:able:clay
      gift:able:dill
      gift:able:eyre
      gift:able:ford
      gift:able:gall
      gift:able:iris
      gift:able:jael
  ==
++  task-arvo                                           ::  in request ->$
  $%  task:able:ames
      task:able:clay
      task:able:behn
      task:able:dill
      task:able:eyre
      task:able:ford
      task:able:gall
      task:able:iris
      task:able:jael
  ==
++  note-arvo                                           ::  out request $->
  $~  [%b %wake ~]
  $%  {$a task:able:ames}
      {$b task:able:behn}
      {$c task:able:clay}
      {$d task:able:dill}
      [%e task:able:eyre]
      {$f task:able:ford}
      {$g task:able:gall}
      [%i task:able:iris]
      {$j task:able:jael}
      {@tas $meta vase}
  ==
++  sign-arvo                                           ::  in result $<-
  $%  {$a gift:able:ames}
      $:  $b
          $%  gift:able:behn
              [%writ riot:clay]
              $>(%mere gift:able:clay)
              $>(%unto gift:able:gall)
          ==
      ==
      {$c gift:able:clay}
      {$d gift:able:dill}
      {$f gift:able:ford}
      [%e gift:able:eyre]
      {$g gift:able:gall}
      [%i gift:able:iris]
      {$j gift:able:jael}
  ==
::  $unix-task: input from unix
::
+$  unix-task                                           ::  input from unix
  $~  [%wake ~]
  $%  ::  %dill: keyboard input
      ::
      $>(%belt task:able:dill)
      ::  %dill: configure terminal (resized)
      ::
      $>(%blew task:able:dill)
      ::  %clay: new process
      ::
      $>(%boat task:able:clay)
      ::  %behn/%eyre/%iris: new process
      ::
      $>(%born vane-task)
      ::  %eyre: cancel request
      ::
      [%cancel-request ~]
      ::  any vane: error report
      ::
      $>(%crud vane-task)
      ::  %dill: reset terminal configuration
      ::
      $>(%hail task:able:dill)
      ::  %ames: hear packet
      ::
      $>(%hear task:able:ames)
      ::  %dill: hangup
      ::
      $>(%hook task:able:dill)
      ::  %clay: external edit
      ::
      $>(%into task:able:clay)
      ::  %eyre: learn ports of live http servers
      ::
      $>(%live task:able:eyre)
      ::  %iris: hear (partial) http response
      ::
      $>(%receive task:able:iris)
      ::  %eyre: starts handling an inbound http request
      ::
      $>(%request task:able:eyre)
      ::  %eyre: starts handling an backdoor http request
      ::
      $>(%request-local task:able:eyre)
      ::  %behn: wakeup
      ::
      $>(%wake task:able:behn)
  ==
--  ::
::                                                      ::  ::
::::                                                    ::  ::  (2) engines
  ::                                                    ::  ::
|%
::                                                      ::::
::::                      ++number                      ::  (2a) number theory
  ::                                                    ::::
++  number  ^?
  |%
  ::                                                    ::  ++fu:number
  ++  fu                                                ::  modulo (mul p q)
    |=  a/{p/@ q/@}
    =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
    |%
    ::                                                  ::  ++dif:fu:number
    ++  dif                                             ::  subtract
      |=  {c/{@ @} d/{@ @}}
      [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
    ::                                                  ::  ++exp:fu:number
    ++  exp                                             ::  exponent
      |=  {c/@ d/{@ @}}
      :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
      (~(exp fo q.a) (mod c (dec q.a)) +.d)
    ::                                                  ::  ++out:fu:number
    ++  out                                             ::  garner's formula
      |=  c/{@ @}
      %+  add  +.c
      %+  mul  q.a
      %+  ~(pro fo p.a)  b
      (~(dif fo p.a) -.c (~(sit fo p.a) +.c))
    ::                                                  ::  ++pro:fu:number
    ++  pro                                             ::  multiply
      |=  {c/{@ @} d/{@ @}}
      [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
    ::                                                  ::  ++sum:fu:number
    ++  sum                                             ::  add
      |=  {c/{@ @} d/{@ @}}
      [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
    ::                                                  ::  ++sit:fu:number
    ++  sit                                             ::  represent
      |=  c/@
      [(mod c p.a) (mod c q.a)]
    --  ::fu
  ::                                                    ::  ++pram:number
  ++  pram                                              ::  rabin-miller
    |=  a/@  ^-  ?
    ?:  ?|  =(0 (end 0 1 a))
            =(1 a)
            =+  b=1
            |-  ^-  ?
            ?:  =(512 b)
              |
            ?|(=+(c=+((mul 2 b)) &(!=(a c) =(a (mul c (div a c))))) $(b +(b)))
        ==
      |
    =+  ^=  b
        =+  [s=(dec a) t=0]
        |-  ^-  {s/@ t/@}
        ?:  =(0 (end 0 1 s))
          $(s (rsh 0 1 s), t +(t))
        [s t]
    ?>  =((mul s.b (bex t.b)) (dec a))
    =+  c=0
    |-  ^-  ?
    ?:  =(c 64)
      &
    =+  d=(~(raw og (add c a)) (met 0 a))
    =+  e=(~(exp fo a) s.b d)
    ?&  ?|  =(1 e)
            =+  f=0
            |-  ^-  ?
            ?:  =(e (dec a))
              &
            ?:  =(f (dec t.b))
              |
            $(e (~(pro fo a) e e), f +(f))
        ==
        $(c +(c))
    ==
  ::                                                    ::  ++ramp:number
  ++  ramp                                              ::  make r-m prime
    |=  {a/@ b/(list @) c/@}  ^-  @ux                   ::  {bits snags seed}
    =>  .(c (shas %ramp c))
    =+  d=*@
    |-
    ?:  =((mul 100 a) d)
      ~|(%ar-ramp !!)
    =+  e=(~(raw og c) a)
    ?:  &((levy b |=(f/@ !=(1 (mod e f)))) (pram e))
      e
    $(c +(c), d (shax d))
  ::                                                    ::  ++curt:number
  ++  curt                                              ::  curve25519
    |=  {a/@ b/@}
    =>  %=    .
            +
          =>  +
          =+  =+  [p=486.662 q=(sub (bex 255) 19)]
              =+  fq=~(. fo q)
              [p=p q=q fq=fq]
          |%
          ::                                            ::  ++cla:curt:number
          ++  cla                                       ::
            |=  raw/@
            =+  low=(dis 248 (cut 3 [0 1] raw))
            =+  hih=(con 64 (dis 127 (cut 3 [31 1] raw)))
            =+  mid=(cut 3 [1 30] raw)
            (can 3 [[1 low] [30 mid] [1 hih] ~])
          ::                                            ::  ++sqr:curt:number
          ++  sqr                                       ::
            |=(a/@ (mul a a))
          ::                                            ::  ++inv:curt:number
          ++  inv                                       ::
            |=(a/@ (~(exp fo q) (sub q 2) a))
          ::                                            ::  ++cad:curt:number
          ++  cad                                       ::
            |=  {n/{x/@ z/@} m/{x/@ z/@} d/{x/@ z/@}}
            =+  ^=  xx
                ;:  mul  4  z.d
                  %-  sqr  %-  abs:si
                  %+  dif:si
                    (sun:si (mul x.m x.n))
                  (sun:si (mul z.m z.n))
                ==
            =+  ^=  zz
                ;:  mul  4  x.d
                  %-  sqr  %-  abs:si
                  %+  dif:si
                    (sun:si (mul x.m z.n))
                  (sun:si (mul z.m x.n))
                ==
            [(sit.fq xx) (sit.fq zz)]
          ::                                            ::  ++cub:curt:number
          ++  cub                                       ::
            |=  {x/@ z/@}
            =+  ^=  xx
                %+  mul
                  %-  sqr  %-  abs:si
                  (dif:si (sun:si x) (sun:si z))
                (sqr (add x z))
            =+  ^=  zz
                ;:  mul  4  x  z
                  :(add (sqr x) :(mul p x z) (sqr z))
                ==
            [(sit.fq xx) (sit.fq zz)]
          --  ::
        ==
    =+  one=[b 1]
    =+  i=253
    =+  r=one
    =+  s=(cub one)
    |-
    ?:  =(i 0)
      =+  x=(cub r)
      (sit.fq (mul -.x (inv +.x)))
    =+  m=(rsh 0 i a)
    ?:  =(0 (mod m 2))
       $(i (dec i), s (cad r s one), r (cub r))
    $(i (dec i), r (cad r s one), s (cub s))
  ::                                                    ::  ++ga:number
  ++  ga                                                ::  GF (bex p.a)
    |=  a/{p/@ q/@ r/@}                                 ::  dim poly gen
    =+  si=(bex p.a)
    =+  ma=(dec si)
    =>  |%
        ::                                              ::  ++dif:ga:number
        ++  dif                                         ::  add and sub
          |=  {b/@ c/@}
          ~|  [%dif-ga a]
          ?>  &((lth b si) (lth c si))
          (mix b c)
        ::                                              ::  ++dub:ga:number
        ++  dub                                         ::  mul by x
          |=  b/@
          ~|  [%dub-ga a]
          ?>  (lth b si)
          ?:  =(1 (cut 0 [(dec p.a) 1] b))
            (dif (sit q.a) (sit (lsh 0 1 b)))
          (lsh 0 1 b)
        ::                                              ::  ++pro:ga:number
        ++  pro                                         ::  slow multiply
          |=  {b/@ c/@}
          ?:  =(0 b)
            0
          ?:  =(1 (dis 1 b))
            (dif c $(b (rsh 0 1 b), c (dub c)))
          $(b (rsh 0 1 b), c (dub c))
        ::                                              ::  ++toe:ga:number
        ++  toe                                         ::  exp+log tables
          =+  ^=  nu
              |=  {b/@ c/@}
              ^-  (map @ @)
              =+  d=*(map @ @)
              |-
              ?:  =(0 c)
                d
              %=  $
                c  (dec c)
                d  (~(put by d) c b)
              ==
          =+  [p=(nu 0 (bex p.a)) q=(nu ma ma)]
          =+  [b=1 c=0]
          |-  ^-  {p/(map @ @) q/(map @ @)}
          ?:  =(ma c)
            [(~(put by p) c b) q]
          %=  $
            b  (pro r.a b)
            c  +(c)
            p  (~(put by p) c b)
            q  (~(put by q) b c)
          ==
        ::                                              ::  ++sit:ga:number
        ++  sit                                         ::  reduce
          |=  b/@
          (mod b (bex p.a))
        --  ::
    =+  toe
    |%
    ::                                                  ::  ++fra:ga:number
    ++  fra                                             ::  divide
      |=  {b/@ c/@}
      (pro b (inv c))
    ::                                                  ::  ++inv:ga:number
    ++  inv                                             ::  invert
      |=  b/@
      ~|  [%inv-ga a]
      =+  c=(~(get by q) b)
      ?~  c  !!
      =+  d=(~(get by p) (sub ma u.c))
      (need d)
    ::                                                  ::  ++pow:ga:number
    ++  pow                                             ::  exponent
      |=  {b/@ c/@}
      =+  [d=1 e=c f=0]
      |-
      ?:  =(p.a f)
        d
      ?:  =(1 (cut 0 [f 1] b))
        $(d (pro d e), e (pro e e), f +(f))
      $(e (pro e e), f +(f))
    ::                                                  ::  ++pro:ga:number
    ++  pro                                             ::  multiply
      |=  {b/@ c/@}
      ~|  [%pro-ga a]
      =+  d=(~(get by q) b)
      ?~  d  0
      =+  e=(~(get by q) c)
      ?~  e  0
      =+  f=(~(get by p) (mod (add u.d u.e) ma))
      (need f)
    --  ::ga
  --  ::number
::                                                      ::::
::::                      ++crypto                      ::  (2b) cryptography
  ::                                                    ::::
++  crypto  ^?
  =,  ames
  =,  number
  |%
  ::                                                    ::
  ::::                    ++aes:crypto                  ::  (2b1) aes, all sizes
    ::                                                  ::::
  ++  aes    !.
    ~%  %aes  ..is  ~
    |%
    ::                                                  ::  ++ahem:aes:crypto
    ++  ahem                                            ::  kernel state
      |=  {nnk/@ nnb/@ nnr/@}
      =>
        =+  =>  [gr=(ga 8 0x11b 3) few==>(fe .(a 5))]
            [pro=pro.gr dif=dif.gr pow=pow.gr ror=ror.few]
        =>  |%                                          ::
            ++  cipa  $_  ^?                            ::  AES params
              |%
              ++  co  *{p/@ q/@ r/@ s/@}                ::  column coefficients
              ++  ix  |~(a/@ *@)                        ::  key index
              ++  ro  *{p/@ q/@ r/@ s/@}                ::  row shifts
              ++  su  *@                                ::  s-box
              --  ::cipa
            --  ::
        |%
        ::                                              ::  ++pen:ahem:aes:
        ++  pen                                         ::  encrypt
          ^-  cipa
          |%
          ::                                            ::  ++co:pen:ahem:aes:
          ++  co                                        ::  column coefficients
            [0x2 0x3 1 1]
          ::                                            ::  ++ix:pen:ahem:aes:
          ++  ix                                        ::  key index
            |~(a/@ a)
          ::                                            ::  ++ro:pen:ahem:aes:
          ++  ro                                        ::  row shifts
            [0 1 2 3]
          ::                                            ::  ++su:pen:ahem:aes:
          ++  su                                        ::  s-box
            0x16bb.54b0.0f2d.9941.6842.e6bf.0d89.a18c.
              df28.55ce.e987.1e9b.948e.d969.1198.f8e1.
              9e1d.c186.b957.3561.0ef6.0348.66b5.3e70.
              8a8b.bd4b.1f74.dde8.c6b4.a61c.2e25.78ba.
              08ae.7a65.eaf4.566c.a94e.d58d.6d37.c8e7.
              79e4.9591.62ac.d3c2.5c24.0649.0a3a.32e0.
              db0b.5ede.14b8.ee46.8890.2a22.dc4f.8160.
              7319.5d64.3d7e.a7c4.1744.975f.ec13.0ccd.
              d2f3.ff10.21da.b6bc.f538.9d92.8f40.a351.
              a89f.3c50.7f02.f945.8533.4d43.fbaa.efd0.
              cf58.4c4a.39be.cb6a.5bb1.fc20.ed00.d153.
              842f.e329.b3d6.3b52.a05a.6e1b.1a2c.8309.
              75b2.27eb.e280.1207.9a05.9618.c323.c704.
              1531.d871.f1e5.a534.ccf7.3f36.2693.fdb7.
              c072.a49c.afa2.d4ad.f047.59fa.7dc9.82ca.
              76ab.d7fe.2b67.0130.c56f.6bf2.7b77.7c63
          --
        ::                                              ::  ++pin:ahem:aes:
        ++  pin                                         ::  decrypt
          ^-  cipa
          |%
          ::                                            ::  ++co:pin:ahem:aes:
          ++  co                                        ::  column coefficients
            [0xe 0xb 0xd 0x9]
          ::                                            ::  ++ix:pin:ahem:aes:
          ++  ix                                        ::  key index
            |~(a/@ (sub nnr a))
          ::                                            ::  ++ro:pin:ahem:aes:
          ++  ro                                        ::  row shifts
            [0 3 2 1]
          ::                                            ::  ++su:pin:ahem:aes:
          ++  su                                        ::  s-box
            0x7d0c.2155.6314.69e1.26d6.77ba.7e04.2b17.
              6199.5383.3cbb.ebc8.b0f5.2aae.4d3b.e0a0.
              ef9c.c993.9f7a.e52d.0d4a.b519.a97f.5160.
              5fec.8027.5910.12b1.31c7.0788.33a8.dd1f.
              f45a.cd78.fec0.db9a.2079.d2c6.4b3e.56fc.
              1bbe.18aa.0e62.b76f.89c5.291d.711a.f147.
              6edf.751c.e837.f9e2.8535.ade7.2274.ac96.
              73e6.b4f0.cecf.f297.eadc.674f.4111.913a.
              6b8a.1301.03bd.afc1.020f.3fca.8f1e.2cd0.
              0645.b3b8.0558.e4f7.0ad3.bc8c.00ab.d890.
              849d.8da7.5746.155e.dab9.edfd.5048.706c.
              92b6.655d.cc5c.a4d4.1698.6886.64f6.f872.
              25d1.8b6d.49a2.5b76.b224.d928.66a1.2e08.
              4ec3.fa42.0b95.4cee.3d23.c2a6.3294.7b54.
              cbe9.dec4.4443.8e34.87ff.2f9b.8239.e37c.
              fbd7.f381.9ea3.40bf.38a5.3630.d56a.0952
          --
        ::                                              ::  ++mcol:ahem:aes:
        ++  mcol                                        ::
          |=  {a/(list @) b/{p/@ q/@ r/@ s/@}}
          ^-  (list @)
          =+  c=[p=*@ q=*@ r=*@ s=*@]
          |-  ^-  (list @)
          ?~  a  ~
          =>  .(p.c (cut 3 [0 1] i.a))
          =>  .(q.c (cut 3 [1 1] i.a))
          =>  .(r.c (cut 3 [2 1] i.a))
          =>  .(s.c (cut 3 [3 1] i.a))
          :_  $(a t.a)
          %+  rep  3
          %+  turn
            %-  limo
            :~  [[p.c p.b] [q.c q.b] [r.c r.b] [s.c s.b]]
                [[p.c s.b] [q.c p.b] [r.c q.b] [s.c r.b]]
                [[p.c r.b] [q.c s.b] [r.c p.b] [s.c q.b]]
                [[p.c q.b] [q.c r.b] [r.c s.b] [s.c p.b]]
            ==
          |=  {a/{@ @} b/{@ @} c/{@ @} d/{@ @}}
          :(dif (pro a) (pro b) (pro c) (pro d))
        ::                                              ::  ++pode:ahem:aes:
        ++  pode                                        ::  explode to block
          |=  {a/bloq b/@ c/@}  ^-  (list @)
          =+  d=(rip a c)
          =+  m=(met a c)
          |-
          ?:  =(m b)
            d
          $(m +(m), d (weld d (limo [0 ~])))
        ::                                              ::  ++sube:ahem:aes:
        ++  sube                                        ::  s-box word
          |=  {a/@ b/@}  ^-  @
          (rep 3 (turn (pode 3 4 a) |=(c/@ (cut 3 [c 1] b))))
        --  ::
      |%
      ::                                                ::  ++be:ahem:aes:crypto
      ++  be                                            ::  block cipher
        |=  {a/? b/@ c/@H}  ^-  @uxH
        ~|  %be-aesc
        =>  %=    .
                +
              =>  +
              |%
              ::                                        ::  ++ankh:be:ahem:aes:
              ++  ankh                                  ::
                |=  {a/cipa b/@ c/@}
                (pode 5 nnb (cut 5 [(mul (ix.a b) nnb) nnb] c))
              ::                                        ::  ++sark:be:ahem:aes:
              ++  sark                                  ::
                |=  {c/(list @) d/(list @)}
                ^-  (list @)
                ?~  c  ~
                ?~  d  !!
                [(mix i.c i.d) $(c t.c, d t.d)]
              ::                                        ::  ++srow:be:ahem:aes:
              ++  srow                                  ::
                |=  {a/cipa b/(list @)}  ^-  (list @)
                =+  [c=0 d=~ e=ro.a]
                |-
                ?:  =(c nnb)
                  d
                :_  $(c +(c))
                %+  rep  3
                %+  turn
                  (limo [0 p.e] [1 q.e] [2 r.e] [3 s.e] ~)
                |=  {f/@ g/@}
                (cut 3 [f 1] (snag (mod (add g c) nnb) b))
              ::                                        ::  ++subs:be:ahem:aes:
              ++  subs                                  ::
                |=  {a/cipa b/(list @)}  ^-  (list @)
                ?~  b  ~
                [(sube i.b su.a) $(b t.b)]
              --
            ==
        =+  [d=?:(a pen pin) e=(pode 5 nnb c) f=1]
        =>  .(e (sark e (ankh d 0 b)))
        |-
        ?.  =(nnr f)
          =>  .(e (subs d e))
          =>  .(e (srow d e))
          =>  .(e (mcol e co.d))
          =>  .(e (sark e (ankh d f b)))
          $(f +(f))
        =>  .(e (subs d e))
        =>  .(e (srow d e))
        =>  .(e (sark e (ankh d nnr b)))
        (rep 5 e)
      ::                                                ::  ++ex:ahem:aes:crypto
      ++  ex                                            ::  key expand
        |=  a/@I  ^-  @
        =+  [b=a c=0 d=su:pen i=nnk]
        |-
        ?:  =(i (mul nnb +(nnr)))
          b
        =>  .(c (cut 5 [(dec i) 1] b))
        =>  ?:  =(0 (mod i nnk))
              =>  .(c (ror 3 1 c))
              =>  .(c (sube c d))
              .(c (mix c (pow (dec (div i nnk)) 2)))
            ?:  &((gth nnk 6) =(4 (mod i nnk)))
              .(c (sube c d))
            .
        =>  .(c (mix c (cut 5 [(sub i nnk) 1] b)))
        =>  .(b (can 5 [i b] [1 c] ~))
        $(i +(i))
      ::                                                ::  ++ix:ahem:aes:crypto
      ++  ix                                            ::  key expand, inv
        |=  a/@  ^-  @
        =+  [i=1 j=*@ b=*@ c=co:pin]
        |-
        ?:  =(nnr i)
          a
        =>  .(b (cut 7 [i 1] a))
        =>  .(b (rep 5 (mcol (pode 5 4 b) c)))
        =>  .(j (sub nnr i))
        %=    $
            i  +(i)
            a
          %+  can  7
          :~  [i (cut 7 [0 i] a)]
              [1 b]
              [j (cut 7 [+(i) j] a)]
          ==
        ==
      --
    ::                                                  ::  ++ecba:aes:crypto
    ++  ecba                                            ::  AES-128 ECB
      ~%  %ecba  +>  ~
      |_  key/@H
      ::                                                ::  ++en:ecba:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecba:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 4 4 10)
        =:
          key  (~(net fe 7) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecba
    ::                                                  ::  ++ecbb:aes:crypto
    ++  ecbb                                            ::  AES-192 ECB
      ~%  %ecbb  +>  ~
      |_  key/@I
      ::                                                ::  ++en:ecbb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 1 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 6 4 12)
        =:
          key  (rsh 6 1 (~(net fe 8) key))
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecbb
    ::                                                  ::  ++ecbc:aes:crypto
    ++  ecbc                                            ::  AES-256 ECB
      ~%  %ecbc  +>  ~
      |_  key/@I
      ::                                                ::  ++en:ecbc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  blk/@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be & (ex key) blk)
      ::                                                ::  ++de:ecbc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  blk/@H  ^-  @uxH
        =+  (ahem 8 4 14)
        =:
          key  (~(net fe 8) key)
          blk  (~(net fe 7) blk)
        ==
        %-  ~(net fe 7)
        (be | (ix (ex key)) blk)
      --  ::ecbc
    ::                                                  ::  ++cbca:aes:crypto
    ++  cbca                                            ::  AES-128 CBC
      ~%  %cbca  +>  ~
      |_  {key/@H prv/@H}
      ::                                                ::  ++en:cbca:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecba key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbca:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecba key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbca
    ::                                                  ::  ++cbcb:aes:crypto
    ++  cbcb                                            ::  AES-192 CBC
      ~%  %cbcb  +>  ~
      |_  {key/@I prv/@H}
      ::                                                ::  ++en:cbcb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbb key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbb key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbcb
    ::                                                  ::  ++cbcc:aes:crypto
    ++  cbcc                                            ::  AES-256 CBC
      ~%  %cbcc  +>  ~
      |_  {key/@I prv/@H}
      ::                                                ::  ++en:cbcc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@  ^-  @ux
        =+  pts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  cts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  pts
          cts
        =+  cph=(~(en ecbc key) (mix prv i.pts))
        %=  $
          cts  [cph cts]
          pts  t.pts
          prv  cph
        ==
      ::                                                ::  ++de:cbcc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  txt/@  ^-  @ux
        =+  cts=?:(=(txt 0) `(list @)`~[0] (flop (rip 7 txt)))
        =|  pts/(list @)
        %+  rep  7
        ::  logically, flop twice here
        |-  ^-  (list @)
        ?~  cts
          pts
        =+  pln=(mix prv (~(de ecbc key) i.cts))
        %=  $
          pts  [pln pts]
          cts  t.cts
          prv  i.cts
        ==
      --  ::cbcc
    ::                                                  ::  ++inc:aes:crypto
    ++  inc                                             ::  inc. low bloq
      |=  {mod/bloq ctr/@H}
      ^-  @uxH
      =+  bqs=(rip mod ctr)
      ?~  bqs  0x1
      %+  rep  mod
      [(~(sum fe mod) i.bqs 1) t.bqs]
    ::                                                  ::  ++ctra:aes:crypto
    ++  ctra                                            ::  AES-128 CTR
      ~%  %ctra  +>  ~
      |_  {key/@H mod/bloq len/@ ctr/@H}
      ::                                                ::  ++en:ctra:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecba key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctra:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctra
    ::                                                  ::  ++ctrb:aes:crypto
    ++  ctrb                                            ::  AES-192 CTR
      ~%  %ctrb  +>  ~
      |_  {key/@I mod/bloq len/@ ctr/@H}
      ::                                                ::  ++en:ctrb:aes:crypto
      ++  en
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecbb key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctrb:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctrb
    ::                                                  ::  ++ctrc:aes:crypto
    ++  ctrc                                            ::  AES-256 CTR
      ~%  %ctrc  +>  ~
      |_  {key/@I mod/bloq len/@ ctr/@H}
      ::                                                ::  ++en:ctrc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  @ux
        =/  encrypt  ~(en ecbc key)
        =/  blocks  (add (div len 16) ?:(=((^mod len 16) 0) 0 1))
        ?>  (gte len (met 3 txt))
        %+  mix  txt
        %^  rsh  3  (sub (mul 16 blocks) len)
        %+  rep  7
        =|  seed=(list @ux)
        |-  ^+  seed
        ?:  =(blocks 0)  seed
        %=  $
          seed    [(encrypt ctr) seed]
          ctr     (inc mod ctr)
          blocks  (dec blocks)
        ==
      ::                                                ::  ++de:ctrc:aes:crypto
      ++  de                                            ::  decrypt
        en
      --  ::ctrc
    ::                                                  ::  ++doub:aes:crypto
    ++  doub                                            ::  double 128-bit
      |=  ::  string mod finite
          ::
          str/@H
      ::
      ::  field (see spec)
      ::
      ^-  @uxH
      %-  ~(sit fe 7)
      ?.  =((xeb str) 128)
        (lsh 0 1 str)
      (mix 0x87 (lsh 0 1 str))
    ::                                                  ::  ++mpad:aes:crypto
    ++  mpad                                            ::
      |=  {oct/@ txt/@}
      ::
      ::  pad message to multiple of 128 bits
      ::  by appending 1, then 0s
      ::  the spec is unclear, but it must be octet based
      ::  to match the test vectors
      ::
      ^-  @ux
      =+  pad=(mod oct 16)
      ?:  =(pad 0)  0x8000.0000.0000.0000.0000.0000.0000.0000
      (lsh 3 (sub 15 pad) (mix 0x80 (lsh 3 1 txt)))
    ::                                                  ::  ++suba:aes:crypto
    ++  suba                                            ::  AES-128 subkeys
      |=  key/@H
      =+  l=(~(en ecba key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++subb:aes:crypto
    ++  subb                                            ::  AES-192 subkeys
      |=  key/@I
      =+  l=(~(en ecbb key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++subc:aes:crypto
    ++  subc                                            ::  AES-256 subkeys
      |=  key/@I
      =+  l=(~(en ecbc key) 0)
      =+  k1=(doub l)
      =+  k2=(doub k1)
      ^-  {@ux @ux}
      [k1 k2]
    ::                                                  ::  ++maca:aes:crypto
    ++  maca                                            ::  AES-128 CMAC
      ~/  %maca
      |=  {key/@H oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(suba key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbca key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macb:aes:crypto
    ++  macb                                            ::  AES-192 CMAC
      ~/  %macb
      |=  {key/@I oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(subb key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcb key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++macc:aes:crypto
    ++  macc                                            :: AES-256 CMAC
      ~/  %macc
      |=  {key/@I oct/(unit @) txt/@}
      ^-  @ux
      =+  [sub=(subc key) len=?~(oct (met 3 txt) u.oct)]
      =+  ^=  pdt
        ?:  &(=((mod len 16) 0) !=(len 0))
          [& txt]
        [| (mpad len txt)]
      =+  ^=  mac
        %-  ~(en cbcc key 0)
        %+  mix  +.pdt
        ?-  -.pdt
          %&  -.sub
          %|  +.sub
        ==
      ::  spec says MSBs, LSBs match test vectors
      ::
      (~(sit fe 7) mac)
    ::                                                  ::  ++s2va:aes:crypto
    ++  s2va                                            ::  AES-128 S2V
      ~/  %s2va
      |=  {key/@H ads/(list @)}
      =+  res=(maca key `16 0x0)
      %^  maca  key  ~
      |-  ^-  @uxH
      ?~  ads  (maca key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (maca key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++s2vb:aes:crypto
    ++  s2vb                                            ::  AES-192 S2V
      ~/  %s2vb
      |=  {key/@I ads/(list @)}
      =+  res=(macb key `16 0x0)
      %^  macb  key  ~
      |-  ^-  @uxH
      ?~  ads  (macb key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (macb key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++s2vc:aes:crypto
    ++  s2vc                                            ::  AES-256 S2V
      ~/  %s2vc
      |=  {key/@I ads/(list @)}
      =+  res=(macc key `16 0x0)
      %^  macc  key  ~
      |-  ^-  @uxH
      ?~  ads  (macc key `16 0x1)
      ?~  t.ads
        ?:  (gte (xeb i.ads) 128)
          (mix i.ads res)
        %+  mix
          (doub res)
          (mpad (met 3 i.ads) i.ads)
      %=  $
        res  %+  mix
               (doub res)
               (macc key ~ i.ads)
        ads  t.ads
      ==
    ::                                                  ::  ++siva:aes:crypto
    ++  siva                                            ::  AES-128 SIV
      ~%  %siva  +>  ~
      |_  {key/@I vec/(list @)}
      ::                                                ::  ++en:siva:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
        =+  iv=(s2va k1 (weld vec (limo ~[txt])))
        =+  len=(met 3 txt)
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        :+
          iv
          len
        (~(en ctra k2 7 len hib) txt)
      ::                                                ::  ++de:siva:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 7 1 key) k2=(end 7 1 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctra k2 7 len hib) txt)
        ?.  =((s2va k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::siva
    ::                                                  ::  ++sivb:aes:crypto
    ++  sivb                                            ::  AES-192 SIV
      ~%  %sivb  +>  ~
      |_  {key/@J vec/(list @)}
      ::                                                ::  ++en:sivb:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 6 3 key) k2=(end 6 3 key)]
        =+  iv=(s2vb k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+  iv
          len
        (~(en ctrb k2 7 len hib) txt)
      ::                                                ::  ++de:sivb:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 6 3 key) k2=(end 6 3 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrb k2 7 len hib) txt)
        ?.  =((s2vb k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::sivb
    ::                                                  ::  ++sivc:aes:crypto
    ++  sivc                                            ::  AES-256 SIV
      ~%  %sivc  +>  ~
      |_  {key/@J vec/(list @)}
      ::                                                ::  ++en:sivc:aes:crypto
      ++  en                                            ::  encrypt
        ~/  %en
        |=  txt/@
        ^-  (trel @uxH @ud @ux)
        =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
        =+  iv=(s2vc k1 (weld vec (limo ~[txt])))
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  len=(met 3 txt)
        :+
          iv
          len
        (~(en ctrc k2 7 len hib) txt)
      ::                                                ::  ++de:sivc:aes:crypto
      ++  de                                            ::  decrypt
        ~/  %de
        |=  {iv/@H len/@ txt/@}
        ^-  (unit @ux)
        =+  [k1=(rsh 8 1 key) k2=(end 8 1 key)]
        =*  hib  (dis iv 0xffff.ffff.ffff.ffff.7fff.ffff.7fff.ffff)
        =+  ^=  pln
          (~(de ctrc k2 7 len hib) txt)
        ?.  =((s2vc k1 (weld vec (limo ~[pln]))) iv)
          ~
        `pln
      --  ::sivc
    --
  ::                                                    ::
  ::::                    ++ed:crypto                   ::  ed25519
    ::                                                  ::::
  ++  ed
    =>
      =+  =+  [b=256 q=(sub (bex 255) 19)]
          =+  fq=~(. fo q)
          =+  ^=  l
               %+  add
                 (bex 252)
               27.742.317.777.372.353.535.851.937.790.883.648.493
          =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
          =+  ii=(exp.fq (div (dec q) 4) 2)
          [b=b q=q fq=fq l=l d=d ii=ii]
      ~%  %coed  ..is  ~
      |%
      ::                                                ::  ++norm:ed:crypto
      ++  norm                                          ::
        |=(x/@ ?:(=(0 (mod x 2)) x (sub q x)))
      ::                                                ::  ++xrec:ed:crypto
      ++  xrec                                          ::  recover x-coord
        |=  y/@  ^-  @
        =+  ^=  xx
            %+  mul  (dif.fq (mul y y) 1)
                     (inv.fq +(:(mul d y y)))
        =+  x=(exp.fq (div (add 3 q) 8) xx)
        ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
          (norm (pro.fq x ii))
        (norm x)
      ::                                                ::  ++ward:ed:crypto
      ++  ward                                          ::  edwards multiply
        |=  {pp/{@ @} qq/{@ @}}  ^-  {@ @}
        =+  dp=:(pro.fq d -.pp -.qq +.pp +.qq)
        =+  ^=  xt
            %+  pro.fq
              %+  sum.fq
                (pro.fq -.pp +.qq)
              (pro.fq -.qq +.pp)
            (inv.fq (sum.fq 1 dp))
        =+  ^=  yt
            %+  pro.fq
              %+  sum.fq
                (pro.fq +.pp +.qq)
              (pro.fq -.pp -.qq)
            (inv.fq (dif.fq 1 dp))
        [xt yt]
      ::                                                ::  ++scam:ed:crypto
      ++  scam                                          ::  scalar multiply
        |=  {pp/{@ @} e/@}  ^-  {@ @}
        ?:  =(0 e)
          [0 1]
        =+  qq=$(e (div e 2))
        =>  .(qq (ward qq qq))
        ?:  =(1 (dis 1 e))
          (ward qq pp)
        qq
      ::                                                ::  ++etch:ed:crypto
      ++  etch                                          ::  encode point
        |=  pp/{@ @}  ^-  @
        (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
      ::                                                ::  ++curv:ed:crypto
      ++  curv                                          ::  point on curve?
        |=  {x/@ y/@}  ^-  ?
        .=  0
            %+  dif.fq
              %+  sum.fq
                (pro.fq (sub q (sit.fq x)) x)
              (pro.fq y y)
            (sum.fq 1 :(pro.fq d x x y y))
      ::                                                ::  ++deco:ed:crypto
      ++  deco                                          ::  decode point
        |=  s/@  ^-  (unit {@ @})
        =+  y=(cut 0 [0 (dec b)] s)
        =+  si=(cut 0 [(dec b) 1] s)
        =+  x=(xrec y)
        =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
        =+  pp=[x y]
        ?.  (curv pp)
          ~
        [~ pp]
      ::                                                ::  ++bb:ed:crypto
      ++  bb                                            ::
        =+  bby=(pro.fq 4 (inv.fq 5))
        [(xrec bby) bby]
      --  ::
    ~%  %ed  +  ~
    |%
    ::
    ++  point-add
      ~/  %point-add
      |=  [a-point=@udpoint b-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      =/  b-point-decoded=[@ @]  (need (deco b-point))
      ::
      %-  etch
      (ward a-point-decoded b-point-decoded)
    ::
    ++  scalarmult
      ~/  %scalarmult
      |=  [a=@udscalar a-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      ::
      %-  etch
      (scam a-point-decoded a)
    ::
    ++  scalarmult-base
      ~/  %scalarmult-base
      |=  scalar=@udscalar
      ^-  @udpoint
      %-  etch
      (scam bb scalar)
    ::
    ++  add-scalarmult-scalarmult-base
      ~/  %add-scalarmult-scalarmult-base
      |=  [a=@udscalar a-point=@udpoint b=@udscalar]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      ::
      %-  etch
      %+  ward
        (scam bb b)
      (scam a-point-decoded a)
    ::
    ++  add-double-scalarmult
      ~/  %add-double-scalarmult
      |=  [a=@udscalar a-point=@udpoint b=@udscalar b-point=@udpoint]
      ^-  @udpoint
      ::
      =/  a-point-decoded=[@ @]  (need (deco a-point))
      =/  b-point-decoded=[@ @]  (need (deco b-point))
      ::
      %-  etch
      %+  ward
        (scam a-point-decoded a)
      (scam b-point-decoded b)
    ::                                                  ::  ++puck:ed:crypto
    ++  puck                                            ::  public key
      ~/  %puck
      |=  sk/@I  ^-  @
      ?:  (gth (met 3 sk) 32)  !!
      =+  h=(shal (rsh 0 3 b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh 0 3 (cut 0 [3 (sub b 5)] h))
      =+  aa=(scam bb a)
      (etch aa)
    ::                                                  ::  ++suck:ed:crypto
    ++  suck                                            ::  keypair from seed
      |=  se/@I  ^-  @uJ
      =+  pu=(puck se)
      (can 0 ~[[b se] [b pu]])
    ::                                                  ::  ++shar:ed:crypto
    ++  shar                                            ::  curve25519 secret
      ~/  %shar
      |=  {pub/@ sek/@}
      ^-  @ux
      =+  exp=(shal (rsh 0 3 b) (suck sek))
      =.  exp  (dis exp (can 0 ~[[3 0] [251 (fil 0 251 1)]]))
      =.  exp  (con exp (lsh 3 31 0b100.0000))
      =+  prv=(end 8 1 exp)
      =+  crv=(fra.fq (sum.fq 1 pub) (dif.fq 1 pub))
      (curt prv crv)
    ::                                                  ::  ++sign:ed:crypto
    ++  sign                                            ::  certify
      ~/  %sign
      |=  {m/@ se/@}  ^-  @
      =+  sk=(suck se)
      =+  pk=(cut 0 [b b] sk)
      =+  h=(shal (rsh 0 3 b) sk)
      =+  ^=  a
          %+  add
            (bex (sub b 2))
          (lsh 0 3 (cut 0 [3 (sub b 5)] h))
      =+  ^=  r
          =+  hm=(cut 0 [b b] h)
          =+  ^=  i
              %+  can  0
              :~  [b hm]
                  [(met 0 m) m]
              ==
          (shaz i)
      =+  rr=(scam bb r)
      =+  ^=  ss
          =+  er=(etch rr)
          =+  ^=  ha
              %+  can  0
              :~  [b er]
                  [b pk]
                  [(met 0 m) m]
              ==
          (~(sit fo l) (add r (mul (shaz ha) a)))
      (can 0 ~[[b (etch rr)] [b ss]])
    ::                                                  ::  ++veri:ed:crypto
    ++  veri                                            ::  validate
      ~/  %veri
      |=  {s/@ m/@ pk/@}  ^-  ?
      ?:  (gth (div b 4) (met 3 s))  |
      ?:  (gth (div b 8) (met 3 pk))  |
      =+  cb=(rsh 0 3 b)
      =+  rr=(deco (cut 0 [0 b] s))
      ?~  rr  |
      =+  aa=(deco pk)
      ?~  aa  |
      =+  ss=(cut 0 [b b] s)
      =+  ha=(can 3 ~[[cb (etch u.rr)] [cb pk] [(met 3 m) m]])
      =+  h=(shaz ha)
      =((scam bb ss) (ward u.rr (scam u.aa h)))
    --  ::ed
  ::                                                    ::
  ::::                    ++scr:crypto                  ::  (2b3) scrypt
    ::                                                  ::::
  ++  scr
    ~%  %scr  ..is  ~
    |%
    ::                                                  ::  ++sal:scr:crypto
    ++  sal                                             ::  salsa20 hash
      |=  {x/@ r/@}                                     ::  with r rounds
      ?>  =((mod r 2) 0)                                ::
      =+  few==>(fe .(a 5))
      =+  ^=  rot
        |=  {a/@ b/@}
        (mix (end 5 1 (lsh 0 a b)) (rsh 0 (sub 32 a) b))
      =+  ^=  lea
        |=  {a/@ b/@}
        (net:few (sum:few (net:few a) (net:few b)))
      =>  |%
          ::                                            ::  ++qr:sal:scr:crypto
          ++  qr                                        ::  quarterround
            |=  y/{@ @ @ @ ~}
            =+  zb=(mix &2.y (rot 7 (sum:few &1.y &4.y)))
            =+  zc=(mix &3.y (rot 9 (sum:few zb &1.y)))
            =+  zd=(mix &4.y (rot 13 (sum:few zc zb)))
            =+  za=(mix &1.y (rot 18 (sum:few zd zc)))
            ~[za zb zc zd]
          ::                                            ::  ++rr:sal:scr:crypto
          ++  rr                                        ::  rowround
            |=  {y/(list @)}
            =+  za=(qr ~[&1.y &2.y &3.y &4.y])
            =+  zb=(qr ~[&6.y &7.y &8.y &5.y])
            =+  zc=(qr ~[&11.y &12.y &9.y &10.y])
            =+  zd=(qr ~[&16.y &13.y &14.y &15.y])
            ^-  (list @)  :~
              &1.za  &2.za  &3.za  &4.za
              &4.zb  &1.zb  &2.zb  &3.zb
              &3.zc  &4.zc  &1.zc  &2.zc
              &2.zd  &3.zd  &4.zd  &1.zd  ==
          ::                                            ::  ++cr:sal:scr:crypto
          ++  cr                                        ::  columnround
            |=  {x/(list @)}
            =+  ya=(qr ~[&1.x &5.x &9.x &13.x])
            =+  yb=(qr ~[&6.x &10.x &14.x &2.x])
            =+  yc=(qr ~[&11.x &15.x &3.x &7.x])
            =+  yd=(qr ~[&16.x &4.x &8.x &12.x])
            ^-  (list @)  :~
              &1.ya  &4.yb  &3.yc  &2.yd
              &2.ya  &1.yb  &4.yc  &3.yd
              &3.ya  &2.yb  &1.yc  &4.yd
              &4.ya  &3.yb  &2.yc  &1.yd  ==
          ::                                            ::  ++dr:sal:scr:crypto
          ++  dr                                        ::  doubleround
            |=  {x/(list @)}
            (rr (cr x))
          ::                                            ::  ++al:sal:scr:crypto
          ++  al                                        ::  add two lists
            |=  {a/(list @) b/(list @)}
            |-  ^-  (list @)
            ?~  a  ~  ?~  b  ~
            [i=(sum:few -.a -.b) t=$(a +.a, b +.b)]
          --  ::
      =+  xw=(rpp 5 16 x)
      =+  ^=  ow  |-  ^-  (list @)
                  ?~  r  xw
                  $(xw (dr xw), r (sub r 2))
      (rep 5 (al xw ow))
    ::                                                  ::  ++rpp:scr:crypto
    ++  rpp                                             ::  rip+filler blocks
      |=  {a/bloq b/@ c/@}
      =+  q=(rip a c)
      =+  w=(lent q)
      ?.  =(w b)
        ?.  (lth w b)  (slag (sub w b) q)
        ^+  q  (weld q (reap (sub b (lent q)) 0))
      q
    ::                                                  ::  ++bls:scr:crypto
    ++  bls                                             ::  split to sublists
      |=  {a/@ b/(list @)}
      ?>  =((mod (lent b) a) 0)
      |-  ^-  (list (list @))
      ?~  b  ~
      [i=(scag a `(list @)`b) t=$(b (slag a `(list @)`b))]
    ::                                                  ::  ++slb:scr:crypto
    ++  slb                                             ::
      |=  {a/(list (list @))}
      |-  ^-  (list @)
      ?~  a  ~
      (weld `(list @)`-.a $(a +.a))
    ::                                                  ::  ++sbm:scr:crypto
    ++  sbm                                             ::  scryptBlockMix
      |=  {r/@ b/(list @)}
      ?>  =((lent b) (mul 2 r))
      =+  [x=(snag (dec (mul 2 r)) b) c=0]
      =|  {ya/(list @) yb/(list @)}
      |-  ^-  (list @)
      ?~  b  (flop (weld yb ya))
      =.  x  (sal (mix x -.b) 8)
      ?~  (mod c 2)
        $(c +(c), b +.b, ya [i=x t=ya])
      $(c +(c), b +.b, yb [i=x t=yb])
    ::                                                  ::  ++srm:scr:crypto
    ++  srm                                             ::  scryptROMix
      |=  {r/@ b/(list @) n/@}
      ?>  ?&  =((lent b) (mul 2 r))
              =(n (bex (dec (xeb n))))
              (lth n (bex (mul r 16)))
          ==
      =+  [v=*(list (list @)) c=0]
      =.  v
        |-  ^-  (list (list @))
        =+  w=(sbm r b)
        ?:  =(c n)  (flop v)
        $(c +(c), v [i=[b] t=v], b w)
      =+  x=(sbm r (snag (dec n) v))
      |-  ^-  (list @)
      ?:  =(c n)  x
      =+  q=(snag (dec (mul r 2)) x)
      =+  z=`(list @)`(snag (mod q n) v)
      =+  ^=  w  |-  ^-  (list @)
                 ?~  x  ~  ?~  z  ~
                 [i=(mix -.x -.z) t=$(x +.x, z +.z)]
      $(x (sbm r w), c +(c))
    ::                                                  ::  ++hmc:scr:crypto
    ++  hmc                                             ::  HMAC-SHA-256
      |=  {k/@ t/@}
      (hml k (met 3 k) t (met 3 t))
    ::                                                  ::  ++hml:scr:crypto
    ++  hml                                             ::  w+length
      |=  {k/@ kl/@ t/@ tl/@}
      =>  .(k (end 3 kl k), t (end 3 tl t))
      =+  b=64
      =?  k  (gth kl b)  (shay kl k)
      =+  ^=  q  %+  shay  (add b tl)
       (add (lsh 3 b t) (mix k (fil 3 b 0x36)))
      %+  shay  (add b 32)
      (add (lsh 3 b q) (mix k (fil 3 b 0x5c)))
    ::                                                  ::  ++pbk:scr:crypto
    ++  pbk                                             :: PBKDF2-HMAC-SHA256
      ~/  %pbk
      |=  {p/@ s/@ c/@ d/@}
      (pbl p (met 3 p) s (met 3 s) c d)
    ::                                                  ::  ++pbl:scr:crypto
    ++  pbl                                             ::  w+length
      ~/  %pbl
      |=  {p/@ pl/@ s/@ sl/@ c/@ d/@}
      =>  .(p (end 3 pl p), s (end 3 sl s))
      =+  h=32
      ::
      ::  max key length 1GB
      ::  max iterations 2^28
      ::
      ?>  ?&  (lte d (bex 30))
              (lte c (bex 28))
              !=(c 0)
          ==
      =+  ^=  l  ?~  (mod d h)
          (div d h)
        +((div d h))
      =+  r=(sub d (mul h (dec l)))
      =+  [t=0 j=1 k=1]
      =.  t  |-  ^-  @
        ?:  (gth j l)  t
        =+  u=(add s (lsh 3 sl (rep 3 (flop (rpp 3 4 j)))))
        =+  f=0  =.  f  |-  ^-  @
          ?:  (gth k c)  f
          =+  q=(hml p pl u ?:(=(k 1) (add sl 4) h))
          $(u q, f (mix f q), k +(k))
        $(t (add t (lsh 3 (mul (dec j) h) f)), j +(j))
      (end 3 d t)
    ::                                                  ::  ++hsh:scr:crypto
    ++  hsh                                             ::  scrypt
      ~/  %hsh
      |=  {p/@ s/@ n/@ r/@ z/@ d/@}
      (hsl p (met 3 p) s (met 3 s) n r z d)
    ::                                                  ::  ++hsl:scr:crypto
    ++  hsl                                             ::  w+length
      ~/  %hsl
      |=  {p/@ pl/@ s/@ sl/@ n/@ r/@ z/@ d/@}
      =|  v/(list (list @))
      =>  .(p (end 3 pl p), s (end 3 sl s))
      =+  u=(mul (mul 128 r) z)
      ::
      ::  n is power of 2; max 1GB memory
      ::
      ?>  ?&  =(n (bex (dec (xeb n))))
              !=(r 0)  !=(z 0)
              %+  lte
                  (mul (mul 128 r) (dec (add n z)))
                (bex 30)
              (lth pl (bex 31))
              (lth sl (bex 31))
          ==
      =+  ^=  b  =+  (rpp 3 u (pbl p pl s sl 1 u))
        %+  turn  (bls (mul 128 r) -)
        |=(a/(list @) (rpp 9 (mul 2 r) (rep 3 a)))
      ?>  =((lent b) z)
      =+  ^=  q
        =+  |-  ?~  b  (flop v)
            $(b +.b, v [i=(srm r -.b n) t=v])
        %+  turn  `(list (list @))`-
        |=(a/(list @) (rpp 3 (mul 128 r) (rep 9 a)))
      (pbl p pl (rep 3 (slb q)) u 1 d)
    ::                                                  ::  ++ypt:scr:crypto
    ++  ypt                                             ::  256bit {salt pass}
      |=  {s/@ p/@}
      ^-  @
      (hsh p s 16.384 8 1 256)
    --  ::scr
  ::                                                    ::
  ::::                    ++crub:crypto                 ::  (2b4) suite B, Ed
    ::                                                  ::::
  ++  crub  !:
    ^-  acru
    =|  {pub/{cry/@ sgn/@} sek/(unit {cry/@ sgn/@})}
    |%
    ::                                                  ::  ++as:crub:crypto
    ++  as                                              ::
      |%
      ::                                                ::  ++sign:as:crub:
      ++  sign                                          ::
        |=  msg=@
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        (jam [(sign:ed msg sgn.u.sek) msg])
      ::                                                ::  ++sure:as:crub:
      ++  sure                                          ::
        |=  txt=@
        ^-  (unit @ux)
        =+  ;;({sig/@ msg/@} (cue txt))
        ?.  (veri:ed sig msg sgn.pub)  ~
        (some msg)
      ::                                                ::  ++seal:as:crub:
      ++  seal                                          ::
        |=  {bpk/pass msg/@}
        ^-  @ux
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 1 bpk))
        =+  pk=(rsh 8 1 (rsh 3 1 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  smsg=(sign msg)
        (jam (~(en siva:aes shar ~) smsg))
      ::                                                ::  ++tear:as:crub:
      ++  tear                                          ::
        |=  {bpk/pass txt/@}
        ^-  (unit @ux)
        ?~  sek  ~|  %pubkey-only  !!
        ?>  =('b' (end 3 1 bpk))
        =+  pk=(rsh 8 1 (rsh 3 1 bpk))
        =+  shar=(shax (shar:ed pk cry.u.sek))
        =+  ;;({iv/@ len/@ cph/@} (cue txt))
        =+  try=(~(de siva:aes shar ~) iv len cph)
        ?~  try  ~
        (sure:as:(com:nu:crub bpk) u.try)
      --  ::as
    ::                                                  ::  ++de:crub:crypto
    ++  de                                              ::  decrypt
      |=  {key/@J txt/@}
      ^-  (unit @ux)
      =+  ;;({iv/@ len/@ cph/@} (cue txt))
      %^    ~(de sivc:aes (shaz key) ~)
          iv
        len
      cph
    ::                                                  ::  ++dy:crub:crypto
    ++  dy                                              ::  need decrypt
      |=  {key/@J cph/@}
      (need (de key cph))
    ::                                                  ::  ++en:crub:crypto
    ++  en                                              ::  encrypt
      |=  {key/@J msg/@}
      ^-  @ux
      (jam (~(en sivc:aes (shaz key) ~) msg))
    ::                                                  ::  ++ex:crub:crypto
    ++  ex                                              ::  extract
      |%
      ::                                                ::  ++fig:ex:crub:crypto
      ++  fig                                           ::  fingerprint
        ^-  @uvH
        (shaf %bfig pub)
      ::                                                ::  ++pac:ex:crub:crypto
      ++  pac                                           ::  private fingerprint
        ^-  @uvG
        ?~  sek  ~|  %pubkey-only  !!
        (end 6 1 (shaf %bcod sec))
      ::                                                ::  ++pub:ex:crub:crypto
      ++  pub                                           ::  public key
        ^-  pass
        (cat 3 'b' (cat 8 sgn.^pub cry.^pub))
      ::                                                ::  ++sec:ex:crub:crypto
      ++  sec                                           ::  private key
        ^-  ring
        ?~  sek  ~|  %pubkey-only  !!
        (cat 3 'B' (cat 8 sgn.u.sek cry.u.sek))
      --  ::ex
    ::                                                  ::  ++nu:crub:crypto
    ++  nu                                              ::
      |%
      ::                                                ::  ++pit:nu:crub:crypto
      ++  pit                                           ::  create keypair
        |=  {w/@ seed/@}
        =+  wid=(add (div w 8) ?:(=((mod w 8) 0) 0 1))
        =+  bits=(shal wid seed)
        =+  [c=(rsh 8 1 bits) s=(end 8 1 bits)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ::                                                ::  ++nol:nu:crub:crypto
      ++  nol                                           ::  activate secret
        |=  a/ring
        =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
        ~|  %not-crub-seckey  ?>  =('B' mag)
        =+  [c=(rsh 8 1 bod) s=(end 8 1 bod)]
        ..nu(pub [cry=(puck:ed c) sgn=(puck:ed s)], sek `[cry=c sgn=s])
      ::                                                ::  ++com:nu:crub:crypto
      ++  com                                           ::  activate public
        |=  a/pass
        =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
        ~|  %not-crub-pubkey  ?>  =('b' mag)
        ..nu(pub [cry=(rsh 8 1 bod) sgn=(end 8 1 bod)], sek ~)
      --  ::nu
    --  ::crub
  ::                                                    ::
  ::::                    ++crua:crypto                 ::  (2b5) suite B, RSA
    ::                                                  ::::
  ++  crua  !!
  ::                                                    ::
  ::::                    ++test:crypto                 ::  (2b6) test crypto
    ::                                                  ::::
  ++  test  ^?
    |%
    ::                                                  ::  ++trub:test:crypto
    ++  trub                                            ::  test crub
      |=  msg/@t
      ::
      ::  make acru cores
      ::
      =/  ali      (pit:nu:crub 512 (shaz 'Alice'))
      =/  ali-pub  (com:nu:crub pub:ex.ali)
      =/  bob      (pit:nu:crub 512 (shaz 'Robert'))
      =/  bob-pub  (com:nu:crub pub:ex.bob)
      ::
      ::  alice signs and encrypts a symmetric key to bob
      ::
      =/  secret-key  %-  shaz
          'Let there be no duplicity when taking a stand against him.'
      =/  signed-key   (sign:as.ali secret-key)
      =/  crypted-key  (seal:as.ali pub:ex.bob-pub signed-key)
      ::  bob decrypts and verifies
      =/  decrypt-key-attempt  (tear:as.bob pub:ex.ali-pub crypted-key)
      =/  decrypted-key    ~|  %decrypt-fail  (need decrypt-key-attempt)
      =/  verify-key-attempt   (sure:as.ali-pub decrypted-key)
      =/  verified-key     ~|  %verify-fail  (need verify-key-attempt)
      ::  bob encrypts with symmetric key
      =/  crypted-msg  (en.bob verified-key msg)
      ::  alice decrypts with same key
      `@t`(dy.ali secret-key crypted-msg)
    --  ::test
  ::                                                    ::
  ::::                    ++keccak:crypto               ::  (2b7) keccak family
    ::                                                  ::::
  ++  keccak
    |%
    ::
    ::  keccak
    ::
    ++  keccak-224  |=(a=octs (keccak 1.152 448 224 a))
    ++  keccak-256  |=(a=octs (keccak 1.088 512 256 a))
    ++  keccak-384  |=(a=octs (keccak 832 768 384 a))
    ++  keccak-512  |=(a=octs (keccak 576 1.024 512 a))
    ::
    ++  keccak  (cury (cury hash keccak-f) padding-keccak)
    ::
    ++  padding-keccak  (multirate-padding 0x1)
    ::
    ::  sha3
    ::
    ++  sha3-224  |=(a=octs (sha3 1.152 448 224 a))
    ++  sha3-256  |=(a=octs (sha3 1.088 512 256 a))
    ++  sha3-384  |=(a=octs (sha3 832 768 384 a))
    ++  sha3-512  |=(a=octs (sha3 576 1.024 512 a))
    ::
    ++  sha3  (cury (cury hash keccak-f) padding-sha3)
    ::
    ++  padding-sha3  (multirate-padding 0x6)
    ::
    ::  shake
    ::
    ++  shake-128  |=([o=@ud i=octs] (shake 1.344 256 o i))
    ++  shake-256  |=([o=@ud i=octs] (shake 1.088 512 o i))
    ::
    ++  shake  (cury (cury hash keccak-f) padding-shake)
    ::
    ++  padding-shake  (multirate-padding 0x1f)
    ::
    ::  rawshake
    ::
    ++  rawshake-128  |=([o=@ud i=octs] (rawshake 1.344 256 o i))
    ++  rawshake-256  |=([o=@ud i=octs] (rawshake 1.088 512 o i))
    ::
    ++  rawshake  (cury (cury hash keccak-f) padding-rawshake)
    ::
    ++  padding-rawshake  (multirate-padding 0x7)
    ::
    ::  core
    ::
    ++  hash
      ::  per:  permutation function with configurable width.
      ::  pad:  padding function.
      ::  rat:  bitrate, size in bits of blocks to operate on.
      ::  cap:  capacity, bits of sponge padding.
      ::  out:  length of desired output, in bits.
      ::  inp:  input to hash.
      |=  $:  per=$-(@ud $-(@ @))
              pad=$-([octs @ud] octs)
              rat=@ud
              cap=@ud
              out=@ud
              inp=octs
          ==
      ^-  @
      ::  urbit's little-endian to keccak's big-endian.
      =.  q.inp  (rev 3 inp)
      %.  [inp out]
      (sponge per pad rat cap)
    ::
    ::NOTE  if ++keccak ever needs to be made to operate
    ::      on bits rather than bytes, all that needs to
    ::      be done is updating the way this padding
    ::      function works. (and also "octs" -> "bits")
    ++  multirate-padding
      ::  dsb:  domain separation byte, reverse bit order.
      |=  dsb=@ux
      ?>  (lte dsb 0xff)
      |=  [inp=octs mut=@ud]
      ^-  octs
      =.  mut  (div mut 8)
      =+  pal=(sub mut (mod p.inp mut))
      =?  pal  =(pal 0)  mut
      =.  pal  (dec pal)
      :-  (add p.inp +(pal))
      ::  padding is provided in lane bit ordering,
      ::  ie, LSB = left.
      (cat 3 (con (lsh 3 pal dsb) 0x80) q.inp)
    ::
    ++  sponge
      ::  sponge construction
      ::
      ::  preperm:  permutation function with configurable width.
      ::  padding:  padding function.
      ::  bitrate:  size of blocks to operate on.
      ::  capacity:  sponge padding.
      |=  $:  preperm=$-(@ud $-(@ @))
              padding=$-([octs @ud] octs)
              bitrate=@ud
              capacity=@ud
          ==
      ::
      ::  preparing
      =+  bitrate-bytes=(div bitrate 8)
      =+  blockwidth=(add bitrate capacity)
      =+  permute=(preperm blockwidth)
      ::
      |=  [input=octs output=@ud]
      |^  ^-  @
        ::
        ::  padding
        =.  input  (padding input bitrate)
        ::
        ::  absorbing
        =/  pieces=(list @)
          ::  amount of bitrate-sized blocks.
          ?>  =(0 (mod p.input bitrate-bytes))
          =+  i=(div p.input bitrate-bytes)
          |-
          ?:  =(i 0)  ~
          :_  $(i (dec i))
          ::  get the bitrate-sized block of bytes
          ::  that ends with the byte at -.
          =-  (cut 3 [- bitrate-bytes] q.input)
          (mul (dec i) bitrate-bytes)
        =/  state=@
          ::  for every piece,
          %+  roll  pieces
          |=  [p=@ s=@]
          ::  pad with capacity,
          =.  p  (lsh 0 capacity p)
          ::  xor it into the state and permute it.
          (permute (mix s (bytes-to-lanes p)))
        ::
        ::  squeezing
        =|  res=@
        =|  len=@ud
        |-
        ::  append a bitrate-sized head of state to the
        ::  result.
        =.  res
          %+  con  (lsh 0 bitrate res)
          (rsh 0 capacity (lanes-to-bytes state))
        =.  len  (add len bitrate)
        ?:  (gte len output)
          ::  produce the requested bits of output.
          (rsh 0 (sub len output) res)
        $(res res, state (permute state))
      ::
      ++  bytes-to-lanes
        ::  flip byte order in blocks of 8 bytes.
        |=  a=@
        %+  can  6
        %+  turn  (rip 6 a)
        |=  b=@
        :-  1
        (lsh 3 (sub 8 (met 3 b)) (swp 3 b))
      ::
      ++  lanes-to-bytes
        ::  unflip byte order in blocks of 8 bytes.
        |=  a=@
        %+  can  6
        %+  turn
          =+  (rip 6 a)
          (weld - (reap (sub 25 (lent -)) 0x0))
        |=  a=@
        :-  1
        %+  can  3
        =-  (turn - |=(a=@ [1 a]))
        =+  (flop (rip 3 a))
        (weld (reap (sub 8 (lent -)) 0x0) -)
      --
    ::
    ++  keccak-f
      ::  keccak permutation function
      |=  [width=@ud]
      ::  assert valid blockwidth.
      ?>  =-  (~(has in -) width)
          (sy 25 50 100 200 400 800 1.600 ~)
      ::  assumes 5x5 lanes state, as is the keccak
      ::  standard.
      =+  size=5
      =+  lanes=(mul size size)
      =+  lane-bloq=(dec (xeb (div width lanes)))
      =+  lane-size=(bex lane-bloq)
      =+  rounds=(add 12 (mul 2 lane-bloq))
      |=  [input=@]
      ^-  @
      =*  a  input
      =+  round=0
      |^
        ?:  =(round rounds)  a
        ::
        ::  theta
        =/  c=@
          %+  roll  (gulf 0 (dec size))
          |=  [x=@ud c=@]
          %+  con  (lsh lane-bloq 1 c)
          %+  roll  (gulf 0 (dec size))
          |=  [y=@ud c=@]
          (mix c (get-lane x y a))
        =/  d=@
          %+  roll  (gulf 0 (dec size))
          |=  [x=@ud d=@]
          %+  con  (lsh lane-bloq 1 d)
          %+  mix
            =-  (get-word - size c)
            ?:(=(x 0) (dec size) (dec x))
          %^  ~(rol fe lane-bloq)  0  1
          (get-word (mod +(x) size) size c)
        =.  a
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ud a=_a]
          %+  mix  a
          %^  lsh  lane-bloq
            (sub lanes +(i))
          (get-word i size d)
        ::
        ::  rho and pi
        =/  b=@
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ b=@]
          =+  x=(mod i 5)
          =+  y=(div i 5)
          %+  con  b
          %^  lsh  lane-bloq
            %+  sub  lanes
            %+  add  +(y)
            %+  mul  size
            (mod (add (mul 2 x) (mul 3 y)) size)
          %^  ~(rol fe lane-bloq)  0
            (rotation-offset i)
          (get-word i lanes a)
        ::
        ::  chi
        =.  a
          %+  roll  (gulf 0 (dec lanes))
          |=  [i=@ud a=@]
          %+  con  (lsh lane-bloq 1 a)
          =+  x=(mod i 5)
          =+  y=(div i 5)
          %+  mix  (get-lane x y b)
          %+  dis
            =-  (get-lane - y b)
            (mod (add x 2) size)
          %^  not  lane-bloq  1
          (get-lane (mod +(x) size) y b)
        ::
        ::  iota
        =.  a
          =+  (round-constant round)
          (mix a (lsh lane-bloq (dec lanes) -))
        ::
        ::  next round
        $(round +(round))
      ::
      ++  get-lane
        ::  get the lane with coordinates
        |=  [x=@ud y=@ud a=@]
        =+  i=(add x (mul size y))
        (get-word i lanes a)
      ::
      ++  get-word
        ::  get word {n} from atom {a} of {m} words.
        |=  [n=@ud m=@ud a=@]
        (cut lane-bloq [(sub m +((mod n m))) 1] a)
      ::
      ++  round-constant
        |=  c=@ud
        =-  (snag (mod c 24) -)
        ^-  (list @ux)
        :~  0x1
            0x8082
            0x8000.0000.0000.808a
            0x8000.0000.8000.8000
            0x808b
            0x8000.0001
            0x8000.0000.8000.8081
            0x8000.0000.0000.8009
            0x8a
            0x88
            0x8000.8009
            0x8000.000a
            0x8000.808b
            0x8000.0000.0000.008b
            0x8000.0000.0000.8089
            0x8000.0000.0000.8003
            0x8000.0000.0000.8002
            0x8000.0000.0000.0080
            0x800a
            0x8000.0000.8000.000a
            0x8000.0000.8000.8081
            0x8000.0000.0000.8080
            0x8000.0001
            0x8000.0000.8000.8008
        ==
      ::
      ++  rotation-offset
        |=  x=@ud
        =-  (snag x -)
        ^-  (list @ud)
        :~   0   1  62  28  27
            36  44   6  55  20
             3  10  43  25  39
            41  45  15  21   8
            18   2  61  56  14
        ==
      --
    --  ::keccak
  ::                                                    ::
  ::::                    ++hmac:crypto                 ::  (2b8) hmac family
    ::                                                  ::::
  ++  hmac
    ~%  %hmac  ..is  ~
    =,  sha
    =>  |%
        ++  meet  |=([k=@ m=@] [[(met 3 k) k] [(met 3 m) m]])
        ++  flip  |=([k=@ m=@] [(swp 3 k) (swp 3 m)])
        --
    |%
    ::
    ::  use with @
    ::
    ++  hmac-sha1     (cork meet hmac-sha1l)
    ++  hmac-sha256   (cork meet hmac-sha256l)
    ++  hmac-sha512   (cork meet hmac-sha512l)
    ::
    ::  use with @t
    ::
    ++  hmac-sha1t    (cork flip hmac-sha1)
    ++  hmac-sha256t  (cork flip hmac-sha256)
    ++  hmac-sha512t  (cork flip hmac-sha512)
    ::
    ::  use with byts
    ::
    ++  hmac-sha1l    (cury hmac sha-1l 64 20)
    ++  hmac-sha256l  (cury hmac sha-256l 64 32)
    ++  hmac-sha512l  (cury hmac sha-512l 128 64)
    ::
    ::  main logic
    ::
    ++  hmac
      ~/  %hmac
      ::  boq: block size in bytes used by haj
      ::  out: bytes output by haj
      |*  [[haj=$-([@u @] @) boq=@u out=@u] key=byts msg=byts]
      ::  ensure key and message fit signaled lengths
      =.  dat.key  (end 3 wid.key dat.key)
      =.  dat.msg  (end 3 wid.msg dat.msg)
      ::  keys longer than block size are shortened by hashing
      =?  dat.key  (gth wid.key boq)  (haj wid.key dat.key)
      =?  wid.key  (gth wid.key boq)  out
      ::  keys shorter than block size are right-padded
      =?  dat.key  (lth wid.key boq)  (lsh 3 (sub boq wid.key) dat.key)
      ::  pad key, inner and outer
      =+  kip=(mix dat.key (fil 3 boq 0x36))
      =+  kop=(mix dat.key (fil 3 boq 0x5c))
      ::  append inner padding to message, then hash
      =+  (haj (add wid.msg boq) (add (lsh 3 wid.msg kip) dat.msg))
      ::  prepend outer padding to result, hash again
      (haj (add out boq) (add (lsh 3 out kop) -))
    --  ::  hmac
  ::                                                    ::
  ::::                    ++secp:crypto                 ::  (2b9) secp family
    ::                                                  ::::
  ++  secp
    ~%  %secp  ..is  ~
    |%
    +=  jaco  [x=@ y=@ z=@]                             ::  jacobian point
    +=  pont  [x=@ y=@]                                 ::  curve point
    ::
    ++  secp256k1
      %+  secp  32
      :*  p=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.  ::  modulo
              ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f
          a=0                                           ::  y^2=x^3+ax+b
          b=7
          ^=  g                                         ::  "prime" point
          :*  x=0x79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
                  029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
              y=0x483a.da77.26a3.c465.5da4.fbfc.0e11.08a8.
                  fd17.b448.a685.5419.9c47.d08f.fb10.d4b8
          ==
          n=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.  ::  prime order of g
              baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
      ==
    ::
    ++  secp
      ~/  %secp
      |=  [w=@ p=@ a=@ b=@ g=pont n=@]  :: being passed in from above
      =/  p  ~(. fo p)
      =/  n  ~(. fo n)
      ~%  %helper  ..$  ~
      |%
      ++  compress-point
        ~/  %compress-point
        |=  pont
        ^-  @
        (can 3 ~[w^x 1^(add 0x2 (cut 0 [0 1] y))])
      ::
      ++  serialize-point
        ~/  %serialize-point
        |=  pont
        ^-  @
        (can 3 ~[w^y w^x 1^0x4])
      ::
      ++  decompress-point
        ~/  %decompress-point
        |=  dat=@
        ^-  pont
        =+  x=(end 3 w a)
        =+  y=:(add (pow x 3) (mul a x) b)
        =+  s=(rsh 3 32 dat)
        :-  x
        ?:  =(0x2 s)  y
        ?:  =(0x3 s)  y
        ~|  [`@ux`s `@ux`dat]
        !!
      ::
      ++  priv-to-pub                                   ::  get pub from priv
        ~/  %priv-to-pub
        |=  prv=@
        ^-  pont
        (jc-mul g prv)
      ::
      ++  make-k                                        ::  deterministic nonce
        ~/  %make-k
        =,  mimes:html
        |=  [has=@uvI prv=@]
        ^-  @
        =*  hmc  hmac-sha256l:hmac
        =/  v  (fil 3 w 1)
        =/  k  0
        =.  k  (hmc w^k (as-octs (can 3 [w has] [w prv] [1 0x0] [w v] ~)))
        =.  v  (hmc w^k w^v)
        =.  k  (hmc w^k (as-octs (can 3 [w has] [w prv] [1 0x1] [w v] ~)))
        =.  v  (hmc w^k w^v)
        (hmc w^k w^v)
      ::
      ++  ecdsa-raw-sign                                ::  generate signature
        ~/  %ecdsa-raw-sign
        |=  [has=@uvI prv=@]
        ^-  [v=@ r=@ s=@]
        =/  z  has
        =/  k  (make-k has prv)
        =+  [r y]=(jc-mul g k)
        =/  s  (pro.n `@`(inv.n k) `@`(sum.n z (mul r prv)))
        =/  big-s  (gte (mul 2 s) ^n)
        :*  v=(mix (end 0 1 y) ?:(big-s 1 0))
            r=r
            s=?.(big-s s (sub ^n s))
        ==
      ::
      ++  ecdsa-raw-recover                             ::  get pubkey from sig
        ~/  %ecdsa-raw-recover
        |=  [has=@uvI sig=[v=@ r=@ s=@]]
        ^-  pont
        ?>  (lte v.sig 7)
        =/  x  r.sig
        =/  ysq  (sum.p b (exp.p 3 x))               ::  omits A=0
        =/  bet  (exp.p (div +(^p) 4) ysq)
        =/  y  ?:(=(1 (end 0 1 (mix v.sig bet))) bet (dif.p 0 bet))
        ?>  =(0 (dif.p ysq (pro.p y y)))
        ?<  =(0 (sit.n r.sig))
        ?<  =(0 (sit.n s.sig))
        =/  gz  (mul:jc [x y 1]:g (dif.n 0 has))
        =/  xy  (mul:jc [x y 1] s.sig)
        =/  qr  (add:jc gz xy)
        (from:jc (mul:jc qr (inv.n r.sig)))
      ::
      ++  jc-mul                                        ::  point x scalar
        |=  [a=pont n=@]
        ^-  pont
        (from:jc (mul:jc (into:jc a) n))
      ::
      ++  jc-add                                        ::  add points
        |=  [a=pont b=pont]
        ^-  pont
        (from:jc (add:jc (into:jc a) (into:jc b)))
      ::
      ++  jc                                            ::  jacobian core
        |%
        ++  add                                         ::  addition
          |=  [a=jaco b=jaco]
          ^-  jaco
          ?:  =(0 y.a)  b
          ?:  =(0 y.b)  a
          =/  u1  :(pro.p x.a z.b z.b)
          =/  u2  :(pro.p x.b z.a z.a)
          =/  s1  :(pro.p y.a z.b z.b z.b)
          =/  s2  :(pro.p y.b z.a z.a z.a)
          ?:  =(u1 u2)
            ?.  =(s1 s2)
              [0 0 1]
            (dub a)
          =/  h  (dif.p u2 u1)
          =/  r  (dif.p s2 s1)
          =/  h2  (pro.p h h)
          =/  h3  (pro.p h2 h)
          =/  u1h2  (pro.p u1 h2)
          =/  nx  (dif.p (pro.p r r) :(sum.p h3 u1h2 u1h2))
          =/  ny  (dif.p (pro.p r (dif.p u1h2 nx)) (pro.p s1 h3))
          =/  nz  :(pro.p h z.a z.b)
          [nx ny nz]
        ::
        ++  dub                                         ::  double
          |=  a=jaco
          ^-  jaco
          ?:  =(0 y.a)
            [0 0 0]
          =/  ysq  (pro.p y.a y.a)
          =/  s  :(pro.p 4 x.a ysq)
          =/  m  :(pro.p 3 x.a x.a)                     ::  omits A=0
          =/  nx  (dif.p (pro.p m m) (sum.p s s))
          =/  ny  (dif.p (pro.p m (dif.p s nx)) :(pro.p 8 ysq ysq))
          =/  nz  :(pro.p 2 y.a z.a)
          [nx ny nz]
        ::
        ++  mul                                         :: jaco x scalar
          |=  [a=jaco n=@]
          ^-  jaco
          ?:  =(0 y.a)
            [0 0 1]
          ?:  =(0 n)
            [0 0 1]
          ?:  =(1 n)
            a
          ?:  (gte n ^^n)
            $(n (mod n ^^n))
          ?:  =(0 (mod n 2))
            (dub $(n (div n 2)))
          (add a (dub $(n (div n 2))))
        ::
        ++  from                                        :: jaco -> point
          |=  a=jaco
          ^-  pont
          =/  z  (inv.p z.a)
          [:(pro.p x.a z z) :(pro.p y.a z z z)]
        ::
        ++  into                                        :: point -> jaco
          |=  pont
          ^-  jaco
          [x y z=1]
        --
      --
    --
  ::
  ++  blake
    ~%  %blake  ..is  ~
    |%
    ::TODO  generalize for both blake2 variants
    ++  blake2b
      ~/  %blake2b
      |=  [msg=byts key=byts out=@ud]
      ^-  @
      ::  initialization vector
      =/  iv=@
        0x6a09.e667.f3bc.c908.
          bb67.ae85.84ca.a73b.
          3c6e.f372.fe94.f82b.
          a54f.f53a.5f1d.36f1.
          510e.527f.ade6.82d1.
          9b05.688c.2b3e.6c1f.
          1f83.d9ab.fb41.bd6b.
          5be0.cd19.137e.2179
      ::  per-round constants
      =/  sigma=(list (list @ud))
        :~
          :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
          :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
          :~  11   8  12   0   5   2  15  13  10  14   3   6   7   1   9   4  ==
          :~   7   9   3   1  13  12  11  14   2   6   5  10   4   0  15   8  ==
          :~   9   0   5   7   2   4  10  15  14   1  11  12   6   8   3  13  ==
          :~   2  12   6  10   0  11   8   3   4  13   7   5  15  14   1   9  ==
          :~  12   5   1  15  14  13   4  10   0   7   6   3   9   2   8  11  ==
          :~  13  11   7  14  12   1   3   9   5   0  15   4   8   6   2  10  ==
          :~   6  15  14   9  11   3   0   8  12   2  13   7   1   4  10   5  ==
          :~  10   2   8   4   7   6   1   5  15  11   9  14   3  12  13   0  ==
          :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
          :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
        ==
      =>  |%
          ++  get-word-list
            |=  [h=@ w=@ud]
            ^-  (list @)
            %-  flop
            =+  l=(rip 6 h)
            =-  (weld - l)
            (reap (sub w (lent l)) 0)
          ::
          ++  get-word
            |=  [h=@ i=@ud w=@ud]
            ^-  @
            %+  snag  i
            (get-word-list h w)
          ::
          ++  put-word
            |=  [h=@ i=@ud w=@ud d=@]
            ^-  @
            %+  rep  6
            =+  l=(get-word-list h w)
            %-  flop
            %+  weld  (scag i l)
            [d (slag +(i) l)]
          ::
          ++  mod-word
            |*  [h=@ i=@ud w=@ud g=$-(@ @)]
            (put-word h i w (g (get-word h i w)))
          ::
          ++  pad
            |=  [byts len=@ud]
            (lsh 3 (sub len wid) dat)
          ::
          ++  compress
            |=  [h=@ c=@ t=@ud l=?]
            ^-  @
            ::  set up local work vector
            =+  v=(add (lsh 6 8 h) iv)
            ::  xor the counter t into v
            =.  v
              %-  mod-word
              :^  v  12  16
              (cury mix (end 0 64 t))
            =.  v
              %-  mod-word
              :^  v  13  16
              (cury mix (rsh 0 64 t))
            ::  for the last block, invert v14
            =?  v  l
              %-  mod-word
              :^  v  14  16
              (cury mix 0xffff.ffff.ffff.ffff)
            ::  twelve rounds of message mixing
            =+  i=0
            =|  s=(list @)
            |^
              ?:  =(i 12)
                ::  xor upper and lower halves of v into state h
                =.  h  (mix h (rsh 6 8 v))
                (mix h (end 6 8 v))
              ::  select message mixing schedule and mix v
              =.  s  (snag (mod i 10) sigma)
              =.  v  (do-mix 0 4 8 12 0 1)
              =.  v  (do-mix 1 5 9 13 2 3)
              =.  v  (do-mix 2 6 10 14 4 5)
              =.  v  (do-mix 3 7 11 15 6 7)
              =.  v  (do-mix 0 5 10 15 8 9)
              =.  v  (do-mix 1 6 11 12 10 11)
              =.  v  (do-mix 2 7 8 13 12 13)
              =.  v  (do-mix 3 4 9 14 14 15)
              $(i +(i))
            ::
            ++  do-mix
              |=  [na=@ nb=@ nc=@ nd=@ nx=@ ny=@]
              ^-  @
              =-  =.  v  (put-word v na 16 a)
                  =.  v  (put-word v nb 16 b)
                  =.  v  (put-word v nc 16 c)
                         (put-word v nd 16 d)
              %-  b2mix
              :*  (get-word v na 16)
                  (get-word v nb 16)
                  (get-word v nc 16)
                  (get-word v nd 16)
                  (get-word c (snag nx s) 16)
                  (get-word c (snag ny s) 16)
              ==
            --
          ::
          ++  b2mix
            |=  [a=@ b=@ c=@ d=@ x=@ y=@]
            ^-  [a=@ b=@ c=@ d=@]
            =.  x  (rev 3 8 x)
            =.  y  (rev 3 8 y)
            =+  fed=~(. fe 6)
            =.  a  :(sum:fed a b x)
            =.  d  (ror:fed 0 32 (mix d a))
            =.  c  (sum:fed c d)
            =.  b  (ror:fed 0 24 (mix b c))
            =.  a  :(sum:fed a b y)
            =.  d  (ror:fed 0 16 (mix d a))
            =.  c  (sum:fed c d)
            =.  b  (ror:fed 0 63 (mix b c))
            [a b c d]
          --
      ::  ensure inputs adhere to contraints
      =.  out  (max 1 (min out 64))
      =.  wid.msg  (min wid.msg (bex 128))
      =.  wid.key  (min wid.key 64)
      =.  dat.msg  (end 3 wid.msg dat.msg)
      =.  dat.key  (end 3 wid.key dat.key)
      ::  initialize state vector
      =+  h=iv
      ::  mix key length and output length into h0
      =.  h
        %-  mod-word
        :^  h  0  8
        %+  cury  mix
        %+  add  0x101.0000
        (add (lsh 3 1 wid.key) out)
      ::  keep track of how much we've compressed
      =*  mes  dat.msg
      =+  com=0
      =+  rem=wid.msg
      ::  if we have a key, pad it and prepend to msg
      =?  mes  (gth wid.key 0)
        (can 3 ~[rem^mes 128^(pad key 128)])
      =?  rem  (gth wid.key 0)
        (add rem 128)
      |-
      ::  compress 128-byte chunks of the message
      ?:  (gth rem 128)
        =+  c=(cut 3 [(sub rem 128) 128] mes)
        =.  com   (add com 128)
        %_  $
          rem   (sub rem 128)
          h     (compress h c com |)
        ==
      ::  compress the final bytes of the msg
      =+  c=(cut 3 [0 rem] mes)
      =.  com  (add com rem)
      =.  c  (pad [rem c] 128)
      =.  h  (compress h c com &)
      ::  produce output of desired length
      %^  rsh  3  (sub 64 out)
      ::  do some word
      %+  rep  6
      %+  turn  (flop (gulf 0 7))
      |=  a=@
      (rev 3 8 (get-word h a 8))
    --  ::blake
  ::
  ++  argon2
    ~%  %argon  ..is  ~
    |%
    ::
    ::  structures
    ::
    +=  argon-type  ?(%d %i %id %u)
    ::
    ::  shorthands
    ::
    ++  argon2-urbit
      |=  out=@ud
      (argon2 out %u 0x13 4 512.000 1 *byts *byts)
    ::
    ::  argon2 proper
    ::
    ::  main argon2 operation
    ++  argon2
      ::  out:       desired output size in bytes
      ::  typ:       argon2 type
      ::  version:   argon2 version (0x10/v1.0 or 0x13/v1.3)
      ::  threads:   amount of threads/parallelism
      ::  mem-cost:  kb of memory to use
      ::  time-cost: iterations to run
      ::  key:       optional secret
      ::  extra:     optional arbitrary data
      |=  $:  out=@ud
              typ=argon-type
              version=@ux
            ::
              threads=@ud
              mem-cost=@ud
              time-cost=@ud
            ::
              key=byts
              extra=byts
          ==
      ^-  $-([msg=byts sat=byts] @)
      ::
      ::  check configuration sanity
      ::
      ?:  =(0 threads)
        ~|  %parallelism-must-be-above-zero
        !!
      ?:  =(0 time-cost)
        ~|  %time-cost-must-be-above-zero
        !!
      ?:  (lth mem-cost (mul 8 threads))
        ~|  :-  %memory-cost-must-be-at-least-threads
            [threads %times 8 (mul 8 threads)]
        !!
      ?.  |(=(0x10 version) =(0x13 version))
        ~|  [%unsupported-version version %want [0x10 0x13]]
        !!
      ::
      ::  calculate constants and initialize buffer
      ::
      ::  for each thread, there is a row in the buffer.
      ::  the amount of columns depends on the memory-cost.
      ::  columns are split into groups of four.
      ::  a single such quarter section of a row is a segment.
      ::
      ::  blocks:     (m_prime)
      ::  columns:    row length (q)
      ::  seg-length: segment length
      =/  blocks=@ud
        ::  round mem-cost down to the nearest multiple of 4*threads
        =+  (mul 4 threads)
        (mul (div mem-cost -) -)
      =+  columns=(div blocks threads)
      =+  seg-length=(div columns 4)
      ::
      =/  buffer=(list (list @))
        (reap threads (reap columns 0))
      ::
      ::  main function
      ::
      ::  msg: the main input
      ::  sat: optional salt
      ~%  %argon2  ..argon2  ~
      |=  [msg=byts sat=byts]
      ^-  @
      ?:  (lth wid.sat 8)
        ~|  [%min-salt-length-is-8 wid.sat]
        !!
      ::
      ::  h0: initial 64-byte block
      =/  h0=@
        =-  (blake2b:blake - 0^0 64)
        :-  :(add 40 wid.msg wid.sat wid.key wid.extra)
        %+  can  3
        =+  (cury (cury rev 3) 4)
        :~  (prep-wid extra)
            (prep-wid key)
            (prep-wid sat)
            (prep-wid msg)
            4^(- (type-to-num typ))
            4^(- version)
            4^(- time-cost)
            4^(- mem-cost)
            4^(- out)
            4^(- threads)
        ==
      ::
      ::  do time-cost passes over the buffer
      ::
      =+  t=0
      |-
      ?:  (lth t time-cost)
        ::
        ::  process all four segments in the columns...
        ::
        =+  s=0
        |-
        ?.  (lth s 4)  ^$(t +(t))
        ::
        ::  ...of every row/thread
        ::
        =+  r=0
        |-
        ?.  (lth r threads)  ^$(s +(s))
        =;  new=_buffer
          $(buffer new, r +(r))
        %-  fill-segment
        :*  buffer   h0
            t        s          r
            blocks   columns    seg-length
            threads  time-cost  typ         version
        ==
      ::
      ::  mix all rows together and hash the result
      ::
      =+  r=0
      =|  final=@
      |-
      ?:  =(r threads)
        (hash 1.024^final out)
      =-  $(final -, r +(r))
      %+  mix  final
      (snag (dec columns) (snag r buffer))
    ::
    ::  per-segment computation
    ++  fill-segment
      |=  $:  buffer=(list (list @))
              h0=@
            ::
              itn=@ud
              seg=@ud
              row=@ud
            ::
              blocks=@ud
              columns=@ud
              seg-length=@ud
            ::
              threads=@ud
              time-cost=@ud
              typ=argon-type
              version=@ux
          ==
      ::
      ::  fill-segment utilities
      ::
      =>  |%
          ++  put-word
            |=  [rob=(list @) i=@ud d=@]
            %+  weld  (scag i rob)
            [d (slag +(i) rob)]
          --
      ^+  buffer
      ::
      ::  rob:   row buffer to operate on
      ::  do-i:  whether to use prns from input rather than state
      ::  rands: prns generated from input, if we do-i
      =+  rob=(snag row buffer)
      =/  do-i=?
        ?|  ?=(%i typ)
            &(?=(%id typ) =(0 itn) (lte seg 1))
            &(?=(%u typ) =(0 itn) (lte seg 2))
        ==
      =/  rands=(list (pair @ @))
        ?.  do-i  ~
        ::
        ::  keep going until we have a list of :seg-length prn pairs
        ::
        =+  l=0
        =+  counter=1
        |-  ^-  (list (pair @ @))
        ?:  (gte l seg-length)  ~
        =-  (weld - $(counter +(counter), l (add l 128)))
        ::
        ::  generate pseudorandom block by compressing metadata
        ::
        =/  random-block=@
          %+  compress  0
          %+  compress  0
          %^  lsh  3  968
          %+  rep  6
          =+  (cury (cury rev 3) 8)
          :~  (- counter)
              (- (type-to-num typ))
              (- time-cost)
              (- blocks)
              (- seg)
              (- row)
              (- itn)
          ==
        ::
        ::  split the random-block into 64-bit sections,
        ::  then extract the first two 4-byte sections from each.
        ::
        %+  turn  (flop (rip 6 random-block))
        |=  a=@
        ^-  (pair @ @)
        :-  (rev 3 4 (rsh 5 1 a))
        (rev 3 4 (end 5 1 a))
      ::
      ::  iterate over the entire segment length
      ::
      =+  sin=0
      |-
      ::
      ::  when done, produce the updated buffer
      ::
      ?:  =(sin seg-length)
        %+  weld  (scag row buffer)
        [rob (slag +(row) buffer)]
      ::
      ::  col: current column to process
      =/  col=@ud
        (add (mul seg seg-length) sin)
      ::
      ::  first two columns are generated from h0
      ::
      ?:  &(=(0 itn) (lth col 2))
        =+  (app-num (app-num 64^h0 col) row)
        =+  (hash - 1.024)
        $(rob (put-word rob col -), sin +(sin))
      ::
      ::  c1, c2: prns for picking reference block
      =/  [c1=@ c2=@]
        ?:  do-i  (snag sin rands)
        =+  =-  (snag - rob)
            ?:  =(0 col)  (dec columns)
            (mod (dec col) columns)
        :-  (rev 3 4 (cut 3 [1.020 4] -))
        (rev 3 4 (cut 3 [1.016 4] -))
      ::
      ::  ref-row: reference block row
      =/  ref-row=@ud
        ?:  &(=(0 itn) =(0 seg))  row
        (mod c2 threads)
      ::
      ::  ref-col: reference block column
      =/  ref-col=@ud
        =-  (mod - columns)
        %+  add
          ::  starting index
          ?:  |(=(0 itn) =(3 seg))  0
          (mul +(seg) seg-length)
        ::  pseudorandom offset
        =-  %+  sub  (dec -)
            %^  rsh  0  32
            %+  mul  -
            (rsh 0 32 (mul c1 c1))
        ::  reference area size
        ?:  =(0 itn)
          ?:  |(=(0 seg) =(row ref-row))  (dec col)
          ?:  =(0 sin)  (dec (mul seg seg-length))
          (mul seg seg-length)
        =+  sul=(sub columns seg-length)
        ?:  =(ref-row row)   (dec (add sul sin))
        ?:  =(0 sin)  (dec sul)
        sul
      ::
      ::  compress the previous and reference block
      ::  to create the new block
      ::
      =/  new=@
        %+  compress
          =-  (snag - rob)
          ::  previous index, wrap-around
          ?:  =(0 col)  (dec columns)
          (mod (dec col) columns)
        ::  get reference block
        %+  snag  ref-col
        ?:  =(ref-row row)  rob
        (snag ref-row buffer)
      ::
      ::  starting from v1.3, we xor the new block in,
      ::  rather than directly overwriting the old block
      ::
      =?  new  &(!=(0 itn) =(0x13 version))
        (mix new (snag col rob))
      $(rob (put-word rob col new), sin +(sin))
    ::
    ::  compression function (g)
    ++  compress
      ::  x, y: assumed to be 1024 bytes
      |=  [x=@ y=@]
      ^-  @
      ::
      =+  r=(mix x y)
      =|  q=(list @)
      ::
      ::  iterate over rows of r to get q
      ::
      =+  i=0
      |-
      ?:  (lth i 8)
        =;  p=(list @)
          $(q (weld q p), i +(i))
        %-  permute
        =-  (weld (reap (sub 8 (lent -)) 0) -)
        %-  flop
        %+  rip  7
        (cut 10 [(sub 7 i) 1] r)
      ::
      ::  iterate over columns of q to get z
      ::
      =/  z=(list @)  (reap 64 0)
      =.  i  0
      |-
      ::
      ::  when done, assemble z and xor it with r
      ::
      ?.  (lth i 8)
        (mix (rep 7 (flop z)) r)
      ::
      ::  permute the column
      ::
      =/  out=(list @)
        %-  permute
        :~  (snag i q)
            (snag (add i 8) q)
            (snag (add i 16) q)
            (snag (add i 24) q)
            (snag (add i 32) q)
            (snag (add i 40) q)
            (snag (add i 48) q)
            (snag (add i 56) q)
        ==
      ::
      ::  put the result into z per column
      ::
      =+  j=0
      |-
      ?:  =(8 j)  ^$(i +(i))
      =-  $(z -, j +(j))
      =+  (add i (mul j 8))
      %+  weld  (scag - z)
      [(snag j out) (slag +(-) z)]
    ::
    ::  permutation function (p)
    ++  permute
      ::NOTE  this function really just takes and produces
      ::      8 values, but taking and producing them as
      ::      lists helps clean up the code significantly.
      |=  s=(list @)
      ?>  =(8 (lent s))
      ^-  (list @)
      ::
      ::  list inputs as 16 8-byte values
      ::
      =/  v=(list @)
        %-  zing
        ^-  (list (list @))
        %+  turn  s
        |=  a=@
        ::  rev for endianness
        =+  (rip 6 (rev 3 16 a))
        (weld - (reap (sub 2 (lent -)) 0))
      ::
      ::  do permutation rounds
      ::
      =.  v  (do-round v 0 4 8 12)
      =.  v  (do-round v 1 5 9 13)
      =.  v  (do-round v 2 6 10 14)
      =.  v  (do-round v 3 7 11 15)
      =.  v  (do-round v 0 5 10 15)
      =.  v  (do-round v 1 6 11 12)
      =.  v  (do-round v 2 7 8 13)
      =.  v  (do-round v 3 4 9 14)
      ::  rev for endianness
      =.  v  (turn v (cury (cury rev 3) 8))
      ::
      ::  cat v back together into 8 16-byte values
      ::
      %+  turn  (gulf 0 7)
      |=  i=@
      =+  (mul 2 i)
      (cat 6 (snag +(-) v) (snag - v))
    ::
    ::  perform a round and produce updated value list
    ++  do-round
      |=  [v=(list @) na=@ nb=@ nc=@ nd=@]
      ^+  v
      =>  |%
          ++  get-word
            |=  i=@ud
            (snag i v)
          ::
          ++  put-word
            |=  [i=@ud d=@]
            ^+  v
            %+  weld  (scag i v)
            [d (slag +(i) v)]
          --
      =-  =.  v  (put-word na a)
          =.  v  (put-word nb b)
          =.  v  (put-word nc c)
                 (put-word nd d)
      %-  round
      :*  (get-word na)
          (get-word nb)
          (get-word nc)
          (get-word nd)
      ==
    ::
    ::  perform a round (bg) and produce updated values
    ++  round
      |=  [a=@ b=@ c=@ d=@]
      ^-  [a=@ b=@ c=@ d=@]
      ::  operate on 64 bit words
      =+  fed=~(. fe 6)
      =*  sum  sum:fed
      =*  ror  ror:fed
      =+  end=(cury (cury end 5) 1)
      =.  a  :(sum a b :(mul 2 (end a) (end b)))
      =.  d  (ror 0 32 (mix d a))
      =.  c  :(sum c d :(mul 2 (end c) (end d)))
      =.  b  (ror 0 24 (mix b c))
      =.  a  :(sum a b :(mul 2 (end a) (end b)))
      =.  d  (ror 0 16 (mix d a))
      =.  c  :(sum c d :(mul 2 (end c) (end d)))
      =.  b  (ror 0 63 (mix b c))
      [a b c d]
    ::
    ::  argon2 wrapper around blake2b (h')
    ++  hash
      =,  blake
      |=  [byts out=@ud]
      ^-  @
      ::
      ::  msg: input with byte-length prepended
      =+  msg=(prep-num [wid dat] out)
      ::
      ::  if requested size is low enough, hash directly
      ::
      ?:  (lte out 64)
        (blake2b msg 0^0 out)
      ::
      ::  build up the result by hashing and re-hashing
      ::  the input message, adding the first 32 bytes
      ::  of the hash to the result, until we have the
      ::  desired output size.
      ::
      =+  tmp=(blake2b msg 0^0 64)
      =+  res=(rsh 3 32 tmp)
      =.  out  (sub out 32)
      |-
      ?:  (gth out 64)
        =.  tmp  (blake2b 64^tmp 0^0 64)
        =.  res  (add (lsh 3 32 res) (rsh 3 32 tmp))
        $(out (sub out 32))
      %+  add  (lsh 3 out res)
      (blake2b 64^tmp 0^0 out)
    ::
    ::  utilities
    ::
    ++  type-to-num
      |=  t=argon-type
      ?-  t
        %d    0
        %i    1
        %id   2
        %u   10
      ==
    ::
    ++  app-num
      |=  [byts num=@ud]
      ^-  byts
      :-  (add wid 4)
      %+  can  3
      ~[4^(rev 3 4 num) wid^dat]
    ::
    ++  prep-num
      |=  [byts num=@ud]
      ^-  byts
      :-  (add wid 4)
      %+  can  3
      ~[wid^dat 4^(rev 3 4 num)]
    ::
    ++  prep-wid
      |=  a=byts
      (prep-num a wid.a)
    --
  ::
  ++  ripemd
    ~%  %ripemd  ..is  ~
    |%
    ++  ripemd-160
      ~/  %ripemd160
      |=  byts
      ^-  @
      ::  we operate on bits rather than bytes
      =.  wid  (mul wid 8)
      ::  add padding
      =+  (md5-pad wid dat)
      ::  endianness
      =.  dat
        %+  rep  5
        %+  turn  (rip 5 dat)
        |=(a=@ (rev 3 4 a))
      =*  x  dat
      =+  blocks=(div wid 512)
      =+  fev=~(. fe 5)
      ::  initial register values
      =+  h0=0x6745.2301
      =+  h1=0xefcd.ab89
      =+  h2=0x98ba.dcfe
      =+  h3=0x1032.5476
      =+  h4=0xc3d2.e1f0
      ::  i: current block
      =+  [i=0 j=0]
      =+  *[a=@ b=@ c=@ d=@ e=@]       ::  a..e
      =+  *[aa=@ bb=@ cc=@ dd=@ ee=@]  ::  a'..e'
      |^
        ?:  =(i blocks)
          %+  rep  5
          %+  turn  `(list @)`~[h4 h3 h2 h1 h0]
          ::  endianness
          |=(h=@ (rev 3 4 h))
        =:  a  h0     aa  h0
            b  h1     bb  h1
            c  h2     cc  h2
            d  h3     dd  h3
            e  h4     ee  h4
        ==
        ::  j: current word
        =+  j=0
        |-
        ?:  =(j 80)
          %=  ^$
            i   +(i)
            h1  :(sum:fev h2 d ee)
            h2  :(sum:fev h3 e aa)
            h3  :(sum:fev h4 a bb)
            h4  :(sum:fev h0 b cc)
            h0  :(sum:fev h1 c dd)
          ==
        %=  $
          j  +(j)
        ::
          a   e
          b   (fn j a b c d e (get (r j)) (k j) (s j))
          c   b
          d   (rol 10 c)
          e   d
        ::
          aa  ee
          bb  (fn (sub 79 j) aa bb cc dd ee (get (rr j)) (kk j) (ss j))
          cc  bb
          dd  (rol 10 cc)
          ee  dd
        ==
      ::
      ++  get  ::  word from x in block i
        |=  j=@ud
        =+  (add (mul i 16) +(j))
        (cut 5 [(sub (mul blocks 16) -) 1] x)
      ::
      ++  fn
        |=  [j=@ud a=@ b=@ c=@ d=@ e=@ m=@ k=@ s=@]
        =-  (sum:fev (rol s :(sum:fev a m k -)) e)
        =.  j  (div j 16)
        ?:  =(0 j)  (mix (mix b c) d)
        ?:  =(1 j)  (con (dis b c) (dis (not 0 32 b) d))
        ?:  =(2 j)  (mix (con b (not 0 32 c)) d)
        ?:  =(3 j)  (con (dis b d) (dis c (not 0 32 d)))
        ?:  =(4 j)  (mix b (con c (not 0 32 d)))
        !!
      ::
      ++  rol  (cury rol:fev 0)
      ::
      ++  k
        |=  j=@ud
        =.  j  (div j 16)
        ?:  =(0 j)  0x0
        ?:  =(1 j)  0x5a82.7999
        ?:  =(2 j)  0x6ed9.eba1
        ?:  =(3 j)  0x8f1b.bcdc
        ?:  =(4 j)  0xa953.fd4e
        !!
      ::
      ++  kk  ::  k'
        |=  j=@ud
        =.  j  (div j 16)
        ?:  =(0 j)  0x50a2.8be6
        ?:  =(1 j)  0x5c4d.d124
        ?:  =(2 j)  0x6d70.3ef3
        ?:  =(3 j)  0x7a6d.76e9
        ?:  =(4 j)  0x0
        !!
      ::
      ++  r
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
            7  4  13  1  10  6  15  3  12  0  9  5  2  14  11  8
            3  10  14  4  9  15  8  1  2  7  0  6  13  11  5  12
            1  9  11  10  0  8  12  4  13  3  7  15  14  5  6  2
            4  0  5  9  7  12  2  10  14  1  3  8  11  6  15  13
        ==
      ::
      ++  rr  ::  r'
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  5  14  7  0  9  2  11  4  13  6  15  8  1  10  3  12
            6  11  3  7  0  13  5  10  14  15  8  12  4  9  1  2
            15  5  1  3  7  14  6  9  11  8  12  2  10  0  4  13
            8  6  4  1  3  11  15  0  5  12  2  13  9  7  10  14
            12  15  10  4  1  5  8  7  6  2  13  14  0  3  9  11
        ==
      ::
      ++  s
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  11  14  15  12  5  8  7  9  11  13  14  15  6  7  9  8
            7  6  8  13  11  9  7  15  7  12  15  9  11  7  13  12
            11  13  6  7  14  9  13  15  14  8  13  6  5  12  7  5
            11  12  14  15  14  15  9  8  9  14  5  6  8  6  5  12
            9  15  5  11  6  8  13  12  5  12  13  14  11  8  5  6
        ==
      ::
      ++  ss  ::  s'
        |=  j=@ud
        %+  snag  j
        ^-  (list @)
        :~  8  9  9  11  13  15  15  5  7  7  8  11  14  14  12  6
            9  13  15  7  12  8  9  11  7  7  12  7  6  15  13  11
            9  7  15  11  8  6  6  14  12  13  5  14  13  13  7  5
            15  5  8  11  14  14  6  14  6  9  12  9  12  5  15  8
            8  5  12  9  12  5  14  6  8  13  6  5  15  13  11  11
        ==
      --
    ::
    ++  md5-pad
      |=  byts
      ^-  byts
      =+  (sub 511 (mod (add wid 64) 512))
      :-  :(add 64 +(-) wid)
      %+  can  0
      ~[64^(rev 3 8 wid) +(-)^(lsh 0 - 1) wid^dat]
    --
  ::
  ++  pbkdf
    =>  |%
        ++  meet  |=([p=@ s=@ c=@ d=@] [[(met 3 p) p] [(met 3 s) s] c d])
        ++  flip  |=  [p=byts s=byts c=@ d=@]
                  [wid.p^(rev 3 p) wid.s^(rev 3 s) c d]
        --
    |%
    ::
    ::  use with @
    ::
    ++  hmac-sha1     (cork meet hmac-sha1l)
    ++  hmac-sha256   (cork meet hmac-sha256l)
    ++  hmac-sha512   (cork meet hmac-sha512l)
    ::
    ::  use with @t
    ::
    ++  hmac-sha1t    (cork meet hmac-sha1d)
    ++  hmac-sha256t  (cork meet hmac-sha256d)
    ++  hmac-sha512t  (cork meet hmac-sha512d)
    ::
    ::  use with byts
    ::
    ++  hmac-sha1l    (cork flip hmac-sha1d)
    ++  hmac-sha256l  (cork flip hmac-sha256d)
    ++  hmac-sha512l  (cork flip hmac-sha512d)
    ::
    ::  main logic
    ::
    ++  hmac-sha1d    (cury pbkdf hmac-sha1l:hmac 20)
    ++  hmac-sha256d  (cury pbkdf hmac-sha256l:hmac 32)
    ++  hmac-sha512d  (cury pbkdf hmac-sha512l:hmac 64)
    ::
    ++  pbkdf
      ::TODO  jet me! ++hmac:hmac is an example
      |*  [[prf=$-([byts byts] @) out=@u] p=byts s=byts c=@ d=@]
      =>  .(dat.p (end 3 p), dat.s (end 3 s))
      ::
      ::  max key length 1GB
      ::  max iterations 2^28
      ::
      ~|  [%invalid-pbkdf-params c d]
      ?>  ?&  (lte d (bex 30))
              (lte c (bex 28))
              !=(c 0)
          ==
      =/  l
        ?~  (mod d out)
          (div d out)
        +((div d out))
      =+  r=(sub d (mul out (dec l)))
      =+  [t=0 j=1 k=1]
      =.  t
        |-  ^-  @
        ?:  (gth j l)  t
        =/  u
          %+  add  dat.s
          %^  lsh  3  wid.s
          %+  rep  3
          (flop (rpp:scr 3 4 j))
        =+  f=0
        =.  f
          |-  ^-  @
          ?:  (gth k c)  f
          =/  q
            %^  rev  3  out
            =+  ?:(=(k 1) (add wid.s 4) out)
            (prf [wid.p (rev 3 p)] [- (rev 3 - u)])
          $(u q, f (mix f q), k +(k))
        $(t (add t (lsh 3 (mul (dec j) out) f)), j +(j))
      (rev 3 d (end 3 d t))
    --
  --  ::crypto
::                                                      ::::
::::                      ++unity                       ::  (2c) unit promotion
  ::                                                    ::::
++  unity  ^?
  |%
  ::                                                    ::  ++drop-list:unity
  ++  drop-list                                         ::  collapse unit list
    |*  lut/(list (unit))
    ?.  |-  ^-  ?
        ?~(lut & ?~(i.lut | $(lut t.lut)))
      ~
    %-  some
    |-
    ?~  lut  ~
    [i=u:+.i.lut t=$(lut t.lut)]
  ::                                                    ::  ++drop-map:unity
  ++  drop-map                                          ::  collapse unit map
    |*  lum/(map term (unit))
    ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=(~ a))))
      ~
    (some (~(run by lum) need))
  ::                                                    ::  ++drop-pole:unity
  ++  drop-pole                                         ::  unit tuple
    |*  but/(pole (unit))
    ?~  but  !!
    ?~  +.but
      u:->.but
    [u:->.but (drop-pole +.but)]
  --
::                                                      ::::
::::                      ++format                      ::  (2d) common formats
  ::                                                    ::::
++  format  ^?
  |%
  ::                                                    ::  ++to-wain:format
  ++  to-wain                                           ::  atom to line list
    ~%  %lore  ..is  ~
    |=  lub/@
    =|  tez/(list @t)
    |-  ^+  tez
    =+  ^=  wor
      =+  [meg=0 i=0]
      |-  ^-  {meg/@ i/@ end/@f}
      =+  gam=(cut 3 [i 1] lub)
      ?:  =(0 gam)
        [meg i %.y]
      ?:  =(10 gam)
        [meg i %.n]
      $(meg (cat 3 meg gam), i +(i))
    ?:  end.wor
      (flop ^+(tez [meg.wor tez]))
    ?:  =(0 lub)  (flop tez)
    $(lub (rsh 3 +(i.wor) lub), tez [meg.wor tez])
  ::                                                    ::  ++of-wain:format
  ++  of-wain                                           ::  line list to atom
    |=  tez/(list @t)
    (rap 3 (join '\0a' tez))
  ::                                                    ::  ++of-wall:format
  ++  of-wall                                           ::  line list to tape
    |=  a/wall  ^-  tape
    ?~(a ~ "{i.a}\0a{$(a t.a)}")
  ::                                                    ::  ++en-beam:format
  ++  en-beam                                           ::  beam to path
    |=  bem/beam
    ^-  path
    [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
  ::                                                    ::  ++de-beam:format
  ++  de-beam                                           ::  parse path to beam
    |=  pax/path
    ^-  (unit beam)
    ?.  ?=({* * * *} pax)  ~
    %+  biff  (slaw %p i.pax)
    |=  who/ship
    %+  biff  (slaw %tas i.t.pax)
    |=  dex/desk
    %+  biff  (slay i.t.t.pax)
    |=  cis/coin
    ?.  ?=({$$ case} cis)  ~
    `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
  ::
  ++  json-rn                                           ::  json to rn parser
    %+  knee  *rn  |.
    ;~  plug
      (easy %d)
      ;~(pose (cold | hep) (easy &))
      ;~  plug  dim:ag
        ;~  pose
          ;~  pfix  dot
            %+  sear
              |=  a=tape
              =/  b  (rust a dum:ag)
              ?~  b  ~
              (some [(lent a) u.b])
            (plus (shim '0' '9'))
          ==
          (easy [0 0])
        ==
        ;~  pose
          ;~  pfix
            (just 'e')
            ;~  plug
              ;~(pose (cold | hep) (easy &))
              ;~  pose
                ;~(pfix (plus (just '0')) dim:ag)
                dim:ag
              ==
            ==
          ==
          (easy [& 0])
        ==
      ==
    ==
  ::                                                    ::  ++enjs:format
  ++  enjs  ^?                                          ::  json encoders
    |%
    ::                                                  ::  ++frond:enjs:format
    ++  frond                                           ::  object from k-v pair
      |=  {p/@t q/json}
      ^-  json
      [%o [[p q] ~ ~]]
    ::                                                  ::  ++pairs:enjs:format
    ++  pairs                                           ::  object from k-v list
      |=  a/(list {p/@t q/json})
      ^-  json
      [%o (~(gas by *(map @t json)) a)]
    ::                                                  ::  ++tape:enjs:format
    ++  tape                                            ::  string from tape
      |=  a/^tape
      ^-  json
      [%s (crip a)]
    ::                                                  ::  ++wall:enjs:format
    ++  wall                                            ::  string from wall
      |=  a/^wall
      ^-  json
      (tape (of-wall a))
    ::                                                  ::  ++ship:enjs:format
    ++  ship                                            ::  string from ship
      |=  a/^ship
      ^-  json
      (tape (slag 1 (scow %p a)))
    ::                                                  ::  ++numb:enjs:format
    ++  numb                                            ::  number from unsigned
      |=  a/@u
      ^-  json
      :-  %n
      ?:  =(0 a)  '0'
      %-  crip
      %-  flop
      |-  ^-  ^tape
      ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
    ::                                                  ::  ++time:enjs:format
    ++  time                                            ::  ms timestamp
      |=  a/^time
      =-  (numb (div (mul - 1.000) ~s1))
      (add (div ~s1 2.000) (sub a ~1970.1.1))
    ::                                                  ::  ++path:enjs:format
    ++  path                                            ::  string from path
      |=  a=^path
      ^-  json
      [%s (spat a)]
    ::                                                  ::  ++tank:enjs:format
    ++  tank                                            ::  tank as string arr
      |=  a=^tank
      ^-  json
      [%a (turn (wash [0 80] a) tape)]
    --  ::enjs
  ::                                                    ::  ++dejs:format
  ++  dejs                                              ::  json reparser
    =>  |%  ++  grub  *                                 ::  result
            ++  fist  $-(json grub)                     ::  reparser instance
        --  ::
    |%
    ::                                                  ::  ++ar:dejs:format
    ++  ar                                              ::  array as list
      |*  wit/fist
      |=  jon/json  ^-  (list _(wit *json))
      ?>  ?=({$a *} jon)
      (turn p.jon wit)
    ::                                                  ::  ++as:dejs:format
    ++  as                                              ::  array as set
      |*  a=fist
      (cu ~(gas in *(set _$:a)) (ar a))
    ::                                                  ::  ++at:dejs:format
    ++  at                                              ::  array as tuple
      |*  wil/(pole fist)
      |=  jon/json
      ?>  ?=({$a *} jon)
      ((at-raw wil) p.jon)
    ::                                                  ::  ++at-raw:dejs:format
    ++  at-raw                                          ::  array as tuple
      |*  wil/(pole fist)
      |=  jol/(list json)
      ?~  jol  !!
      ?-    wil                                         :: mint-vain on empty
          :: {wit/* t/*}
          {* t/*}
        =>  .(wil [wit *]=wil)
        ?~  t.wil  ?^(t.jol !! (wit.wil i.jol))
        [(wit.wil i.jol) ((at-raw t.wil) t.jol)]
      ==
    ::                                                  ::  ++bo:dejs:format
    ++  bo                                              ::  boolean
      |=(jon/json ?>(?=({$b *} jon) p.jon))
    ::                                                  ::  ++bu:dejs:format
    ++  bu                                              ::  boolean not
      |=(jon/json ?>(?=({$b *} jon) !p.jon))
    ::                                                  ::  ++ci:dejs:format
    ++  ci                                              ::  maybe transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (need (poq (wit jon)))
    ::                                                  ::  ++cu:dejs:format
    ++  cu                                              ::  transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (poq (wit jon))
    ::                                                  ::  ++di:dejs:format
    ++  di                                              ::  millisecond date
      %+  cu
        |=  a/@u  ^-  @da
        (add ~1970.1.1 (div (mul ~s1 a) 1.000))
      ni
    ::                                                  ::  ++mu:dejs:format
    ++  mu                                              ::  true unit
      |*  wit/fist
      |=  jon/json
      ?~(jon ~ (some (wit jon)))
    ::                                                  ::  ++ne:dejs:format
    ++  ne                                              ::  number as real
      |=  jon/json
      ^-  @rd
      ?>  ?=([%n *] jon)
      (rash p.jon (cook ryld (cook royl-cell:^so json-rn)))
    ::                                                  ::  ++ni:dejs:format
    ++  ni                                              ::  number as integer
      |=  jon/json
      ?>  ?=({$n *} jon)
      (rash p.jon dem)
    ::                                                  ::  ++no:dejs:format
    ++  no                                              ::  number as cord
      |=(jon/json ?>(?=({$n *} jon) p.jon))
    ::                                                  ::  ++of:dejs:format
    ++  of                                              ::  object as frond
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?>  ?=({$o {@ *} $~ $~} jon)
      |-
      ?-    wer                                         :: mint-vain on empty
          :: {{key/@t wit/*} t/*}
          {{key/@t *} t/*}
        =>  .(wer [[* wit] *]=wer)
        ?:  =(key.wer p.n.p.jon)
          [key.wer ~|(key+key.wer (wit.wer q.n.p.jon))]
        ?~  t.wer  ~|(bad-key+p.n.p.jon !!)
        ((of t.wer) jon)
      ==
    ::                                                  ::  ++ot:dejs:format
    ++  ot                                              ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?>  ?=({$o *} jon)
      ((ot-raw wer) p.jon)
    ::                                                  ::  ++ot-raw:dejs:format
    ++  ot-raw                                          ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jom/(map @t json)
      ?-    wer                                         :: mint-vain on empty
          :: {{key/@t wit/*} t/*}
          {{key/@t *} t/*}
        =>  .(wer [[* wit] *]=wer)
        =/  ten  ~|(key+key.wer (wit.wer (~(got by jom) key.wer)))
        ?~(t.wer ten [ten ((ot-raw t.wer) jom)])
      ==
    ::
    ++  ou                                              ::  object of units
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?>  ?=({$o *} jon)
      ((ou-raw wer) p.jon)
    ::                                                  ::  ++ou-raw:dejs:format
    ++  ou-raw                                          ::  object of units
      |*  wer/(pole {cord fist})
      |=  jom/(map @t json)
      ?-    wer                                         :: mint-vain on empty
          :: {{key/@t wit/*} t/*}
          {{key/@t *} t/*}
        =>  .(wer [[* wit] *]=wer)
        =/  ten  ~|(key+key.wer (wit.wer (~(get by jom) key.wer)))
        ?~(t.wer ten [ten ((ou-raw t.wer) jom)])
      ==
    ::                                                  ::  ++om:dejs:format
    ++  om                                              ::  object as map
      |*  wit/fist
      |=  jon/json
      ?>  ?=({$o *} jon)
      (~(run by p.jon) wit)
    ::                                                  ::  ++op:dejs:format
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      |=  jon/json  ^-  (map _(wonk *fel) _*wit)
      =/  jom  ((om wit) jon)
      %-  malt
      %+  turn  ~(tap by jom)
      |*  {a/cord b/*}
      =>  .(+< [a b]=+<)
      [(rash a fel) b]
    ::                                                  ::  ++pa:dejs:format
    ++  pa                                              ::  string as path
      (su ;~(pfix net (more net urs:ab)))
    ::                                                  ::  ++pe:dejs:format
    ++  pe                                              ::  prefix
      |*  {pre/* wit/fist}
      (cu |*(* [pre +<]) wit)
    ::                                                  ::  ++sa:dejs:format
    ++  sa                                              ::  string as tape
      |=(jon/json ?>(?=({$s *} jon) (trip p.jon)))
    ::                                                  ::  ++se:dejs:format
    ++  se                                              ::  string as aura
      |=  aur=@tas
      |=  jon=json
      ?>(?=([%s *] jon) (slav aur p.jon))
    ::                                                  ::  ++so:dejs:format
    ++  so                                              ::  string as cord
      |=(jon/json ?>(?=({$s *} jon) p.jon))
    ::                                                  ::  ++su:dejs:format
    ++  su                                              ::  parse string
      |*  sab/rule
      |=  jon/json  ^+  (wonk *sab)
      ?>  ?=({$s *} jon)
      (rash p.jon sab)
    ::                                                  ::  ++uf:dejs:format
    ++  uf                                              ::  unit fall
      |*  [def/* wit/fist]
      |=  jon/(unit json)
      ?~(jon def (wit u.jon))
    ::                                                  ::  ++un:dejs:format
    ++  un                                              ::  unit need
      |*  wit/fist
      |=  jon/(unit json)
      (wit (need jon))
    ::                                                  ::  ++ul:dejs:format
    ++  ul                                              ::  null
      |=(jon/json ?~(jon ~ !!))
    ::
    ++  za                                              ::  full unit pole
      |*  pod/(pole (unit))
      ?~  pod  &
      ?~  -.pod  |
      (za +.pod)
    ::
    ++  zl                                              ::  collapse unit list
      |*  lut/(list (unit))
      ?.  |-  ^-  ?
          ?~(lut & ?~(i.lut | $(lut t.lut)))
        ~
      %-  some
      |-
      ?~  lut  ~
      [i=u:+.i.lut t=$(lut t.lut)]
    ::
    ++  zp                                              ::  unit tuple
      |*  but/(pole (unit))
      ?~  but  !!
      ?~  +.but
        u:->.but
      [u:->.but (zp +.but)]
    ::
    ++  zm                                              ::  collapse unit map
      |*  lum/(map term (unit))
      ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=(~ a))))
        ~
      (some (~(run by lum) need))
    --  ::dejs
  ::                                                    ::  ++dejs-soft:format
  ++  dejs-soft                                         ::  json reparse to unit
    =,  unity
    =>  |%  ++  grub  (unit *)                          ::  result
            ++  fist  $-(json grub)                     ::  reparser instance
        --  ::
    ::
    ::  XX: this is old code that replaced a rewritten dejs.
    ::      the rewritten dejs rest-looped with ++redo.  the old
    ::      code is still in revision control -- revise and replace.
    ::
    |%
    ++  ar                                              ::  array as list
      |*  wit/fist
      |=  jon/json
      ?.  ?=({$a *} jon)  ~
      %-  zl
      |-
      ?~  p.jon  ~
      [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
    ::
    ++  at                                              ::  array as tuple
      |*  wil/(pole fist)
      |=  jon/json
      ?.  ?=({$a *} jon)  ~
      =+  raw=((at-raw wil) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::
    ++  at-raw                                          ::  array as tuple
      |*  wil/(pole fist)
      |=  jol/(list json)
      ?~  wil  ~
      :-  ?~(jol ~ (-.wil i.jol))
      ((at-raw +.wil) ?~(jol ~ t.jol))
    ::
    ++  bo                                              ::  boolean
      |=(jon/json ?.(?=({$b *} jon) ~ [~ u=p.jon]))
    ::
    ++  bu                                              ::  boolean not
      |=(jon/json ?.(?=({$b *} jon) ~ [~ u=!p.jon]))
    ::
    ++  ci                                              ::  maybe transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (biff (wit jon) poq)
    ::
    ++  cu                                              ::  transform
      |*  {poq/gate wit/fist}
      |=  jon/json
      (bind (wit jon) poq)
    ::
    ++  da                                              ::  UTC date
      |=  jon/json
      ?.  ?=({$s *} jon)  ~
      (bind (stud:chrono:userlib p.jon) |=(a/date (year a)))
    ::
    ++  di                                              ::  millisecond date
      %+  cu
        |=  a/@u  ^-  @da
        (add ~1970.1.1 (div (mul ~s1 a) 1.000))
      ni
    ::
    ++  mu                                              ::  true unit
      |*  wit/fist
      |=  jon/json
      ?~(jon (some ~) (bind (wit jon) some))
    ::
    ++  ne                                              ::  number as real
      |=  jon/json
      ^-  (unit @rd)
      ?.  ?=([%n *] jon)  ~
      (rush p.jon (cook ryld (cook royl-cell:^so json-rn)))
    ::
    ++  ni                                              ::  number as integer
      |=  jon/json
      ?.  ?=({$n *} jon)  ~
      (rush p.jon dem)
    ::
    ++  no                                              ::  number as cord
      |=  jon/json
      ?.  ?=({$n *} jon)  ~
      (some p.jon)
    ::
    ++  of                                              ::  object as frond
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?.  ?=({$o {@ *} ~ ~} jon)  ~
      |-
      ?~  wer  ~
      ?:  =(-.-.wer p.n.p.jon)
        ((pe -.-.wer +.-.wer) q.n.p.jon)
      ((of +.wer) jon)
    ::
    ++  ot                                              ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jon/json
      ?.  ?=({$o *} jon)  ~
      =+  raw=((ot-raw wer) p.jon)
      ?.((za raw) ~ (some (zp raw)))
    ::
    ++  ot-raw                                          ::  object as tuple
      |*  wer/(pole {cord fist})
      |=  jom/(map @t json)
      ?~  wer  ~
      =+  ten=(~(get by jom) -.-.wer)
      [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
    ::
    ++  om                                              ::  object as map
      |*  wit/fist
      |=  jon/json
      ?.  ?=({$o *} jon)  ~
      (zm (~(run by p.jon) wit))
    ::
    ++  op                                              ::  parse keys of map
      |*  {fel/rule wit/fist}
      %+  cu
        |=  a/(list (pair _(wonk *fel) _(need *wit)))
        (my:nl a)
      %-  ci  :_  (om wit)
      |=  a/(map cord _(need *wit))
      ^-  (unit (list _[(wonk *fel) (need *wit)]))
      %-  zl
      %+  turn  ~(tap by a)
      |=  {a/cord b/_(need *wit)}
      =+  nit=(rush a fel)
      ?~  nit  ~
      (some [u.nit b])
    ::
    ++  pe                                              ::  prefix
      |*  {pre/* wit/fist}
      (cu |*(* [pre +<]) wit)
    ::
    ++  sa                                              ::  string as tape
      |=  jon/json
      ?.(?=({$s *} jon) ~ (some (trip p.jon)))
    ::
    ++  so                                              ::  string as cord
      |=  jon/json
      ?.(?=({$s *} jon) ~ (some p.jon))
    ::
    ++  su                                              ::  parse string
      |*  sab/rule
      |=  jon/json
      ?.  ?=({$s *} jon)  ~
      (rush p.jon sab)
    ::
    ++  ul  |=(jon/json ?~(jon (some ~) ~))             ::  null
    ++  za                                              ::  full unit pole
      |*  pod/(pole (unit))
      ?~  pod  &
      ?~  -.pod  |
      (za +.pod)
    ::
    ++  zl                                              ::  collapse unit list
      |*  lut/(list (unit))
      ?.  |-  ^-  ?
          ?~(lut & ?~(i.lut | $(lut t.lut)))
        ~
      %-  some
      |-
      ?~  lut  ~
      [i=u:+.i.lut t=$(lut t.lut)]
    ::
    ++  zp                                              ::  unit tuple
      |*  but/(pole (unit))
      ?~  but  !!
      ?~  +.but
        u:->.but
      [u:->.but (zp +.but)]
    ::
    ++  zm                                              ::  collapse unit map
      |*  lum/(map term (unit))
      ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=(~ a))))
        ~
      (some (~(run by lum) need))
    --  ::dejs-soft
  --
::                                                      ::
::::                      ++differ                      ::  (2d) hunt-mcilroy
  ::                                                    ::::
++  differ  ^?
  =,  clay
  =,  format
  |%
  ::                                                    ::  ++berk:differ
  ++  berk                                              ::  invert diff patch
    |*  bur/(urge)
    |-  ^+  bur
    ?~  bur  ~
    :_  $(bur t.bur)
    ?-  -.i.bur
      %&  i.bur
      %|  [%| q.i.bur p.i.bur]
    ==
  ::                                                    ::  ++loss:differ
  ++  loss                                              ::  longest subsequence
    ~%  %loss  ..is  ~
    |*  {hel/(list) hev/(list)}
    |-  ^+  hev
    =+  ^=  sev
        =+  [inx=0 sev=*(map _i.-.hev (list @ud))]
        |-  ^+  sev
        ?~  hev  sev
        =+  guy=(~(get by sev) i.hev)
        %=  $
          hev  t.hev
          inx  +(inx)
          sev  (~(put by sev) i.hev [inx ?~(guy ~ u.guy)])
        ==
    =|  gox/{p/@ud q/(map @ud {p/@ud q/_hev})}
    =<  abet
    =<  main
    |%
    ::                                                  ::  ++abet:loss:differ
    ++  abet                                            ::  subsequence
      ^+  hev
      ?:  =(0 p.gox)  ~
      (flop q:(need (~(get by q.gox) (dec p.gox))))
    ::                                                  ::  ++hink:loss:differ
    ++  hink                                            ::  extend fits top
      |=  {inx/@ud goy/@ud}  ^-  ?
      ?|  =(p.gox inx)
          (lth goy p:(need (~(get by q.gox) inx)))
      ==
    ::                                                  ::  ++lonk:loss:differ
    ++  lonk                                            ::  extend fits bottom
      |=  {inx/@ud goy/@ud}  ^-  ?
      ?|  =(0 inx)
          (gth goy p:(need (~(get by q.gox) (dec inx))))
      ==
    ::                                                  ::  ++luna:loss:differ
    ++  luna                                            ::  extend
      |=  {inx/@ud goy/@ud}
      ^+  +>
      %_    +>.$
          gox
        :-  ?:(=(inx p.gox) +(p.gox) p.gox)
        %+  ~(put by q.gox)  inx
        :+  goy
          (snag goy hev)
        ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))
      ==
    ::                                                  ::  ++merg:loss:differ
    ++  merg                                            ::  merge all matches
      |=  gay/(list @ud)
      ^+  +>
      =+  ^=  zes
          =+  [inx=0 zes=*(list {p/@ud q/@ud})]
          |-  ^+  zes
          ?:  |(?=(~ gay) (gth inx p.gox))  zes
          ?.  (lonk inx i.gay)  $(gay t.gay)
          ?.  (hink inx i.gay)  $(inx +(inx))
          $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
      |-  ^+  +>.^$
      ?~(zes +>.^$ $(zes t.zes, +>.^$ (luna i.zes)))
    ::                                                  ::  ++main:loss:differ
    ++  main                                            ::
      =+  hol=hel
      |-  ^+  +>
      ?~  hol  +>
      =+  guy=(~(get by sev) i.hol)
      $(hol t.hol, +> (merg (flop `(list @ud)`?~(guy ~ u.guy))))
    --  ::
  ::                                                    ::  ++lurk:differ
  ++  lurk                                              ::  apply list patch
    |*  {hel/(list) rug/(urge)}
    ^+  hel
    =+  war=`_hel`~
    |-  ^+  hel
    ?~  rug  (flop war)
    ?-    -.i.rug
        %&
      %=   $
        rug  t.rug
        hel  (slag p.i.rug hel)
        war  (weld (flop (scag p.i.rug hel)) war)
      ==
    ::
        %|
      %=  $
        rug  t.rug
        hel  =+  gur=(flop p.i.rug)
             |-  ^+  hel
             ?~  gur  hel
             ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
        war  (weld q.i.rug war)
      ==
    ==
  ::                                                    ::  ++lusk:differ
  ++  lusk                                              ::  lcs to list patch
    |*  {hel/(list) hev/(list) lcs/(list)}
    =+  ^=  rag
        ^-  {$%({%& p/@ud} {%| p/_lcs q/_lcs})}
        [%& 0]
    =>  .(rag [p=rag q=*(list _rag)])
    =<  abet  =<  main
    |%
    ::                                                  ::  ++abet:lusk:differ
    ++  abet                                            ::
      =?  q.rag  !=([& 0] p.rag)  [p.rag q.rag]
      (flop q.rag)
    ::                                                  ::  ++done:lusk:differ
    ++  done                                            ::
      |=  new/_p.rag
      ^+  rag
      ?-  -.p.rag
        %|   ?-  -.new
              %|  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
              %&  [new [p.rag q.rag]]
            ==
        %&   ?-  -.new
              %|  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
              %&  [[%& (add p.p.rag p.new)] q.rag]
            ==
      ==
    ::                                                  ::  ++main:lusk:differ
    ++  main                                            ::
      |-  ^+  +
      ?~  hel
        ?~  hev
          ?>(?=(~ lcs) +)
        $(hev t.hev, rag (done %| ~ [i.hev ~]))
      ?~  hev
        $(hel t.hel, rag (done %| [i.hel ~] ~))
      ?~  lcs
        +(rag (done %| (flop hel) (flop hev)))
      ?:  =(i.hel i.lcs)
        ?:  =(i.hev i.lcs)
          $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
        $(hev t.hev, rag (done %| ~ [i.hev ~]))
      ?:  =(i.hev i.lcs)
        $(hel t.hel, rag (done %| [i.hel ~] ~))
      $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
    --  ::
  --  ::differ
::                                                      ::
::::                      ++html                        ::  (2e) text encodings
  ::                                                    ::::
++  html  ^?  ::  XX rename to web-txt
  =,  eyre
  |%
  ::                                                    ::
  ::::                    ++mimes:html                  ::  (2e1) MIME
    ::                                                  ::::
  ++  mimes  ^?
    |%
    ::                                                  ::  ++as-octs:mimes:html
    ++  as-octs                                         ::  atom to octstream
      |=  tam/@  ^-  octs
      [(met 3 tam) tam]
    ::                                                  ::  ++as-octt:mimes:html
    ++  as-octt                                         ::  tape to octstream
      |=  tep/tape  ^-  octs
      (as-octs (rap 3 tep))
    ::                                                  ::  ++en-mite:mimes:html
    ++  en-mite                                         ::  mime type to text
      |=  myn/mite
      %-  crip
      |-  ^-  tape
      ?~  myn  ~
      ?:  =(~ t.myn)  (trip i.myn)
      (weld (trip i.myn) `tape`['/' $(myn t.myn)])
    ::                                                  ::  ++en-base64:mimes:
    ++  en-base64                                       ::  encode base64
      |=  tig/@
      ^-  tape
      =+  poc=(~(dif fo 3) 0 (met 3 tig))
      =+  pad=(lsh 3 poc (swp 3 tig))
      =+  ^=  cha
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
      =+  ^=  sif
          |-  ^-  tape
          ?~  pad
            ~
          =+  d=(end 0 6 pad)
          [(cut 3 [d 1] cha) $(pad (rsh 0 6 pad))]
      (weld (flop (slag poc sif)) (reap poc '='))
    ::                                                  ::  ++de-base64:mimes:
    ++  de-base64                                       ::  decode base64
      =-  |=(a/cord (rash a fel))
      =<  fel=(cook |~(a/@ `@t`(swp 3 a)) (bass 64 .))
      =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
      ^=  siw
      ;~  pose
         (cook |=(a/@ (sub a 'A')) (shim 'A' 'Z'))
         (cook |=(a/@ (sub a 'G')) (shim 'a' 'z'))
         (cook |=(a/@ (add a 4)) (shim '0' '9'))
         (cold 62 (just '+'))
         (cold 63 (just '/'))
       ==
    ::
    ++  en-base58
      |=  dat=@
      =/  cha
        '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
      %-  flop
      |-  ^-  tape
      ?:  =(0 dat)  ~
      :-  (cut 3 [(mod dat 58) 1] cha)
      $(dat (div dat 58))
    ::
    ++  de-base58
      |=  t=tape
      =-  (scan t (bass 58 (plus -)))
      ;~  pose
        (cook |=(a=@ (sub a 56)) (shim 'A' 'H'))
        (cook |=(a=@ (sub a 57)) (shim 'J' 'N'))
        (cook |=(a=@ (sub a 58)) (shim 'P' 'Z'))
        (cook |=(a=@ (sub a 64)) (shim 'a' 'k'))
        (cook |=(a=@ (sub a 65)) (shim 'm' 'z'))
        (cook |=(a=@ (sub a 49)) (shim '1' '9'))
      ==
    --  ::mimes
  ::                                                    ::  ++en-json:html
  ++  en-json                                           ::  print json
    |^  |=(val/json (apex val ""))
    ::                                                  ::  ++apex:en-json:html
    ++  apex
      |=  {val/json rez/tape}
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
        ?:  ?=({@ ~} hed)
          [i.hed $(viz t.viz)]
        (weld hed $(viz t.viz))
     ::
          $o
        :-  '{'
        =.  rez  ['}' rez]
        =+  viz=~(tap by p.val)
        ?~  viz  rez
        !.
        |-  ^+  rez
        ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
        =.  rez  [',' $(viz t.viz)]
        ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
      ==
    ::                                                  ::  ++jesc:en-json:html
    ++  jesc                                            ::  escaped
      =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)])
      |=  a/@  ^-  tape
      ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
        $10  "\\n"
        $34  "\\\""
        $92  "\\\\"
      ==
    --  ::en-json
  ::                                                    ::  ++de-json:html
  ++  de-json                                           ::  parse JSON
    =<  |=(a/cord `(unit json)`(rush a apex))
    |%
    ::                                                  ::  ++abox:de-json:html
    ++  abox                                            ::  array
      %+  stag  %a
      (ifix [lac (wish rac)] (more (wish com) apex))
    ::                                                  ::  ++apex:de-json:html
    ++  apex                                            ::  any value
      %+  knee  *json  |.  ~+
      %+  ifix  [spac spac]
      ;~  pose
        (cold ~ (jest 'null'))
        (stag %b bool)
        (stag %s stri)
        (cook |=(s/tape [%n p=(rap 3 s)]) numb)
        abox
        obox
      ==
    ::                                                  ::  ++bool:de-json:html
    ++  bool                                            ::  boolean
      ;~  pose
        (cold & (jest 'true'))
        (cold | (jest 'false'))
      ==
    ::                                                  ::  ++digs:de-json:html
    ++  digs                                            ::  digits
      (star (shim '0' '9'))
    ::                                                  ::  ++esca:de-json:html
    ++  esca                                            ::  escaped character
      ;~  pfix  bas
        =*  loo
          =*  lip
            ^-  (list (pair @t @))
            [b+8 t+9 n+10 f+12 r+13 ~]
          =*  wow  `(map @t @)`(malt lip)
          (sear ~(get by wow) low)
        =*  tuf  ;~(pfix (just 'u') (cook tuft qix:ab))
        ;~(pose yel net say bas loo tuf)
      ==
    ::                                                  ::  ++expo:de-json:html
    ++  expo                                            ::  exponent
      ;~  (comp twel)
        (piec (mask "eE"))
        (mayb (piec (mask "+-")))
        digs
      ==
    ::                                                  ::  ++frac:de-json:html
    ++  frac                                            ::  fraction
      ;~(plug dot digs)
    ::                                                  ::  ++jcha:de-json:html
    ++  jcha                                            ::  string character
      ;~(pose ;~(less yel bas prn) esca)
    ::                                                  ::  ++mayb:de-json:html
    ++  mayb                                            ::  optional
      |*(bus/rule ;~(pose bus (easy ~)))
    ::                                                  ::  ++numb:de-json:html
    ++  numb                                            ::  number
      ;~  (comp twel)
        (mayb (piec hep))
        ;~  pose
          (piec (just '0'))
          ;~(plug (shim '1' '9') digs)
        ==
        (mayb frac)
        (mayb expo)
      ==
    ::                                                  ::  ++obje:de-json:html
    ++  obje                                            ::  object list
      %+  ifix  [(wish leb) (wish reb)]
      (more (wish com) pear)
    ::                                                  ::  ++obox:de-json:html
    ++  obox                                            ::  object
      (stag %o (cook malt obje))
    ::                                                  ::  ++pear:de-json:html
    ++  pear                                            ::  key-value
      ;~(plug ;~(sfix (wish stri) (wish col)) apex)
    ::                                                  ::  ++piec:de-json:html
    ++  piec                                            ::  listify
      |*  bus/rule
      (cook |=(a/@ [a ~]) bus)
    ::                                                  ::  ++stri:de-json:html
    ++  stri                                            ::  string
      (cook crip (ifix [yel yel] (star jcha)))
    ::                                                  ::  ++tops:de-json:html
    ++  tops                                            ::  strict value
      ;~(pose abox obox)
    ::                                                  ::  ++spac:de-json:html
    ++  spac                                            ::  whitespace
      (star (mask [`@`9 `@`10 `@`13 ' ' ~]))
    ::                                                  ::  ++twel:de-json:html
    ++  twel                                            ::  tape weld
      |=({a/tape b/tape} (weld a b))
    ::                                                  ::  ++wish:de-json:html
    ++  wish                                            ::  with whitespace
      |*(sef/rule ;~(pfix spac sef))
    --  ::de-json
  ::                                                    ::  ++en-xml:html
  ++  en-xml                                            ::  xml printer
    =<  |=(a/manx `tape`(apex a ~))
    |_  _[unq=`?`| cot=`?`|]
    ::                                                  ::  ++apex:en-xml:html
    ++  apex                                            ::  top level
      |=  {mex/manx rez/tape}
      ^-  tape
      ?:  ?=({$$ {{$$ *} ~}} g.mex)
        (escp v.i.a.g.mex rez)
      =+  man=`mane`n.g.mex
      =.  unq  |(unq =(%script man) =(%style man))
      =+  tam=(name man)
      =+  att=`mart`a.g.mex
      :-  '<'
      %+  welp  tam
      =-  ?~(att rez [' ' (attr att rez)])
      ^-  rez/tape
      ?:  &(?=(~ c.mex) |(cot ?^(man | (clot man))))
        [' ' '/' '>' rez]
      :-  '>'
      (many c.mex :(weld "</" tam ">" rez))
    ::                                                  ::  ++attr:en-xml:html
    ++  attr                                            ::  attributes to tape
      |=  {tat/mart rez/tape}
      ^-  tape
      ?~  tat  rez
      =.  rez  $(tat t.tat)
      ;:  weld
        (name n.i.tat)
        "=\""
        (escp(unq |) v.i.tat '"' ?~(t.tat rez [' ' rez]))
      ==
    ::                                                  ::  ++escp:en-xml:html
    ++  escp                                            ::  escape for xml
      |=  {tex/tape rez/tape}
      ?:  unq
        (weld tex rez)
      =+  xet=`tape`(flop tex)
      !.
      |-  ^-  tape
      ?~  xet  rez
      %=    $
        xet  t.xet
        rez  ?-  i.xet
               $34  ['&' 'q' 'u' 'o' 't' ';' rez]
               $38  ['&' 'a' 'm' 'p' ';' rez]
               $39  ['&' '#' '3' '9' ';' rez]
               $60  ['&' 'l' 't' ';' rez]
               $62  ['&' 'g' 't' ';' rez]
               *    [i.xet rez]
             ==
      ==
    ::                                                  ::  ++many:en-xml:html
    ++  many                                            ::  nodelist to tape
      |=  {lix/(list manx) rez/tape}
      |-  ^-  tape
      ?~  lix  rez
      (apex i.lix $(lix t.lix))
    ::                                                  ::  ++name:en-xml:html
    ++  name                                            ::  name to tape
      |=  man/mane  ^-  tape
      ?@  man  (trip man)
      (weld (trip -.man) `tape`[':' (trip +.man)])
    ::                                                  ::  ++clot:en-xml:html
    ++  clot  ~+                                        ::  self-closing tags
      %~  has  in
      %-  silt  ^-  (list term)  :~
        %area  %base  %br  %col  %command  %embed  %hr  %img  %inputt
        %keygen  %link  %meta  %param     %source   %track  %wbr
      ==
    --  ::en-xml
  ::                                                    ::  ++de-xml:html
  ++  de-xml                                            ::  xml parser
    =<  |=(a/cord (rush a apex))
    |_  ent/_`(map term @t)`[[%apos '\''] ~ ~]
    ::                                                  ::  ++apex:de-xml:html
    ++  apex                                            ::  top level
      =+  spa=;~(pose comt whit)
      %+  knee  *manx  |.  ~+
      %+  ifix  [(star spa) (star spa)]
      ;~  pose
        %+  sear  |=({a/marx b/marl c/mane} ?.(=(c n.a) ~ (some [a b])))
          ;~(plug head many tail)
        empt
      ==
    ::                                                  ::  ++attr:de-xml:html
    ++  attr                                            ::  attributes
      %+  knee  *mart  |.  ~+
      %-  star
      ;~  plug
        ;~(pfix (plus whit) name)
        ;~  pose
          (ifix [;~(plug tis yel) yel] (star ;~(less yel escp)))
          (ifix [;~(plug tis say) say] (star ;~(less say escp)))
          (easy ~)
        ==
      ==
    ::                                                  ::  ++cdat:de-xml:html
    ++  cdat                                            ::  CDATA section
      %+  cook
        |=(a/tape ^-(mars ;/(a)))
      %+  ifix
        [(jest '<![CDATA[') (jest ']]>')]
      %-  star
      ;~(less (jest ']]>') next)
    ::                                                  ::  ++chrd:de-xml:html
    ++  chrd                                            ::  character data
      %+  cook  |=(a/tape ^-(mars ;/(a)))
      (plus ;~(less yel ;~(pose (just `@`10) escp)))
    ::                                                  ::  ++comt:de-xml:html
    ++  comt                                            ::  comments
      =-  (ifix [(jest '<!--') (jest '-->')] (star -))
      ;~  pose
        ;~(less hep prn)
        whit
        ;~(less (jest '-->') hep)
      ==
    ::                                                  ::  ++escp:de-xml:html
    ++  escp                                            ::
      ;~(pose ;~(less led ban pad prn) enty)
    ::                                                  ::  ++enty:de-xml:html
    ++  enty                                            ::  entity
      %+  ifix  pad^mic
      ;~  pose
        =+  def=^+(ent (my:nl [%gt '>'] [%lt '<'] [%amp '&'] [%quot '"'] ~))
        %+  sear  ~(get by (~(uni by def) ent))
        (cook crip ;~(plug alf (stun 1^31 aln)))
        %+  cook  |=(a/@c ?:((gth a 0x10.ffff) '�' (tuft a)))
        =<  ;~(pfix hax ;~(pose - +))
        :-  (bass 10 (stun 1^8 dit))
        (bass 16 ;~(pfix (mask "xX") (stun 1^8 hit)))
      ==
    ::                                                  ::  ++empt:de-xml:html
    ++  empt                                            ::  self-closing tag
      %+  ifix  [gal (jest '/>')]
      ;~(plug ;~(plug name attr) (cold ~ (star whit)))
    ::                                                  ::  ++head:de-xml:html
    ++  head                                            ::  opening tag
      (ifix [gal ban] ;~(plug name attr))
    ::                                                  ::  ++many:de-xml:html
    ++  many                                            ::  contents
      ;~(pfix (star comt) (star ;~(sfix ;~(pose apex chrd cdat) (star comt))))
    ::                                                  ::  ++name:de-xml:html
    ++  name                                            ::  tag name
      =+  ^=  chx
          %+  cook  crip
          ;~  plug
              ;~(pose cab alf)
              (star ;~(pose cab dot alp))
          ==
      ;~(pose ;~(plug ;~(sfix chx col) chx) chx)
    ::                                                  ::  ++tail:de-xml:html
    ++  tail                                            ::  closing tag
      (ifix [(jest '</') ban] name)
    ::                                                  ::  ++whit:de-xml:html
    ++  whit                                            ::  whitespace
      (mask ~[' ' `@`0x9 `@`0xa])
    --  ::de-xml
  ::                                                    ::  ++en-urlt:html
  ++  en-urlt                                           ::  url encode
    |=  tep/tape
    ^-  tape
    %-  zing
    %+  turn  tep
    |=  tap/char
    =+  xen=|=(tig/@ ?:((gte tig 10) (add tig 55) (add tig '0')))
    ?:  ?|  &((gte tap 'a') (lte tap 'z'))
            &((gte tap 'A') (lte tap 'Z'))
            &((gte tap '0') (lte tap '9'))
            =('.' tap)
            =('-' tap)
            =('~' tap)
            =('_' tap)
        ==
      [tap ~]
    ['%' (xen (rsh 0 4 tap)) (xen (end 0 4 tap)) ~]
  ::                                                    ::  ++de-urlt:html
  ++  de-urlt                                           ::  url decode
    |=  tep/tape
    ^-  (unit tape)
    ?~  tep  [~ ~]
    ?:  =('%' i.tep)
      ?.  ?=({@ @ *} t.tep)  ~
      =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
      =+  val=(rush nag hex:ag)
      ?~  val  ~
      =+  nex=$(tep t.t.t.tep)
      ?~(nex ~ [~ [`@`u.val u.nex]])
    =+  nex=$(tep t.tep)
    ?~(nex ~ [~ i.tep u.nex])
  ::                                                    ::  ++en-purl:html
  ++  en-purl                                           ::  print purl
    =<  |=(pul/purl `tape`(apex %& pul))
    |%
    ::                                                  ::  ++apex:en-purl:html
    ++  apex                                            ::
      |=  qur/quri  ^-  tape
      ?-  -.qur
        %&  (weld (head p.p.qur) `tape`$(qur [%| +.p.qur]))
        %|  ['/' (weld (body p.qur) (tail q.qur))]
      ==
    ::                                                  ::  ++apix:en-purl:html
    ++  apix                                            ::  purf to tape
      |=  purf
      (weld (apex %& p) ?~(q "" `tape`['#' (trip u.q)]))
    ::                                                  ::  ++body:en-purl:html
    ++  body                                            ::
      |=  pok/pork  ^-  tape
      ?~  q.pok  ~
      |-
      =+  seg=(en-urlt (trip i.q.pok))
      ?~  t.q.pok
        ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
      (welp seg '/' $(q.pok t.q.pok))
    ::                                                  ::  ++head:en-purl:html
    ++  head                                            ::
      |=  har/hart
      ^-  tape
      ;:  weld
        ?:(&(p.har !?=(hoke r.har)) "https://" "http://")
      ::
        ?-  -.r.har
          %|  (trip (rsh 3 1 (scot %if p.r.har)))
          %&  =+  rit=(flop p.r.har)
              |-  ^-  tape
              ?~  rit  ~
              (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)]))
        ==
      ::
        ?~(q.har ~ `tape`[':' ((d-co:co 1) u.q.har)])
      ==
    ::                                                  ::  ++tail:en-purl:html
    ++  tail                                            ::
      |=  kay/quay
      ^-  tape
      ?:  =(~ kay)  ~
      :-  '?'
      |-  ^-  tape
      ?~  kay  ~
      ;:  welp
        (en-urlt (trip p.i.kay))
        ?~(q.i.kay ~ ['=' (en-urlt (trip q.i.kay))])
        ?~(t.kay ~ `tape`['&' $(kay t.kay)])
      ==
    --  ::
  ::                                                    ::  ++de-purl:html
  ++  de-purl                                           ::  url+header parser
    =<  |=(a/cord `(unit purl)`(rush a auri))
    |%
    ::                                                  ::  ++deft:de-purl:html
    ++  deft                                            ::  parse url extension
      |=  rax/(list @t)
      |-  ^-  pork
      ?~  rax
        [~ ~]
      ?^  t.rax
        [p.pok [ire q.pok]]:[pok=$(rax t.rax) ire=i.rax]
      =/  raf/(like term)
          =>  |=(a/@ ((sand %tas) (crip (flop (trip a)))))
          (;~(sfix (sear . sym) dot) [1^1 (flop (trip i.rax))])
      ?~  q.raf
        [~ [i.rax ~]]
      =+  `{ext/term {@ @} fyl/tape}`u.q.raf
      :-  `ext
      ?:(=(~ fyl) ~ [(crip (flop fyl)) ~])
    ::                                                  ::  ++apat:de-purl:html
    ++  apat                                            ::  2396 abs_path
      %+  cook  deft
      ;~(pfix net (more net smeg))
    ::                                                  ::  ++aurf:de-purl:html
    ++  aurf                                            ::  2396 with fragment
      %+  cook  |~(a/purf a)
      ;~(plug auri (punt ;~(pfix hax (cook crip (star pque)))))
    ::                                                  ::  ++auri:de-purl:html
    ++  auri                                            ::  2396 URL
      ;~  plug
        ;~(plug htts thor)
        ;~(plug ;~(pose apat (easy *pork)) yque)
      ==
    ::                                                  ::  ++auru:de-purl:html
    ++  auru                                            ::  2396 with maybe user
      %+  cook
        |=  $:  a/{p/? q/(unit user) r/{(unit @ud) host}}
                b/{pork quay}
            ==
        ^-  (pair (unit user) purl)
        [q.a [[p.a r.a] b]]
      ::
      ;~  plug
        ;~(plug htts (punt ;~(sfix urt:ab vat)) thor)
        ;~(plug ;~(pose apat (easy *pork)) yque)
      ==
    ::                                                  ::  ++htts:de-purl:html
    ++  htts                                            ::  scheme
      %+  sear  ~(get by (malt `(list (pair term ?))`[http+| https+& ~]))
      ;~(sfix scem ;~(plug col net net))
    ::                                                  ::  ++cock:de-purl:html
    ++  cock                                            ::  cookie
      %+  most  ;~(plug mic ace)
      ;~(plug toke ;~(pfix tis tosk))
    ::                                                  ::  ++dlab:de-purl:html
    ++  dlab                                            ::  2396 domainlabel
      %+  sear
        |=  a/@ta
        ?.(=('-' (rsh 3 (dec (met 3 a)) a)) [~ u=a] ~)
      %+  cook  |=(a/tape (crip (cass a)))
      ;~(plug aln (star alp))
    ::                                                  ::  ++fque:de-purl:html
    ++  fque                                            ::  normal query field
      (cook crip (plus pquo))
    ::                                                  ::  ++fquu:de-purl:html
    ++  fquu                                            ::  optional query field
      (cook crip (star pquo))
    ::                                                  ::  ++pcar:de-purl:html
    ++  pcar                                            ::  2396 path char
      ;~(pose pure pesc psub col vat)
    ::                                                  ::  ++pcok:de-purl:html
    ++  pcok                                            ::  cookie char
      ;~(less bas mic com yel prn)
    ::                                                  ::  ++pesc:de-purl:html
    ++  pesc                                            ::  2396 escaped
      ;~(pfix cen mes)
    ::                                                  ::  ++pold:de-purl:html
    ++  pold                                            ::
      (cold ' ' (just '+'))
    ::                                                  ::  ++pque:de-purl:html
    ++  pque                                            ::  3986 query char
      ;~(pose pcar net wut)
    ::                                                  ::  ++pquo:de-purl:html
    ++  pquo                                            ::  normal query char
      ;~(pose pure pesc pold net wut col com)
    ::                                                  ::  ++pure:de-purl:html
    ++  pure                                            ::  2396 unreserved
      ;~(pose aln hep dot cab sig)
    ::                                                  ::  ++psub:de-purl:html
    ++  psub                                            ::  3986 sub-delims
      ;~  pose
        zap  bus  pad  say  lit  rit
        tar  lus  com  mic  tis
      ==
    ::                                                  ::  ++ptok:de-purl:html
    ++  ptok                                            ::  2616 token
      ;~  pose
        aln  zap  hax  bus  cen  pad  say  tar  lus
        hep  dot  ket  cab  tec  bar  sig
      ==
    ::                                                  ::  ++scem:de-purl:html
    ++  scem                                            ::  2396 scheme
      %+  cook  |=(a/tape (crip (cass a)))
      ;~(plug alf (star ;~(pose aln lus hep dot)))
    ::                                                  ::  ++smeg:de-purl:html
    ++  smeg                                            ::  2396 segment
      (cook crip (star pcar))
    ::                                                  ::  ++tock:de-purl:html
    ++  tock                                            ::  6265 raw value
      (cook crip (plus pcok))
    ::                                                  ::  ++tosk:de-purl:html
    ++  tosk                                            ::  6265 quoted value
      ;~(pose tock (ifix [yel yel] tock))
    ::                                                  ::  ++toke:de-purl:html
    ++  toke                                            ::  2616 token
      (cook crip (plus ptok))
    ::                                                  ::  ++thor:de-purl:html
    ++  thor                                            ::  2396 host+port
      %+  cook  |*({* *} [+<+ +<-])
      ;~  plug
        thos
        ;~((bend) (easy ~) ;~(pfix col dim:ag))
      ==
    ::                                                  ::  ++thos:de-purl:html
    ++  thos                                            ::  2396 host, no local
      ;~  plug
        ;~  pose
          %+  stag  %&
          %+  sear                                      ::  LL parser weak here
            |=  a/(list @t)
            =+  b=(flop a)
            ?>  ?=(^ b)
            =+  c=(end 3 1 i.b)
            ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
          (most dot dlab)
        ::
          %+  stag  %|
          =+  tod=(ape:ag ted:ab)
          %+  bass  256
          ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
        ==
      ==
    ::                                                  ::  ++yque:de-purl:html
    ++  yque                                            ::  query ending
      ;~  pose
        ;~(pfix wut yquy)
        (easy ~)
      ==
    ::                                                  ::  ++yquy:de-purl:html
    ++  yquy                                            ::  query
      ;~  pose
        ::  proper query
        ::
        %+  more
          ;~(pose pad mic)
        ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
        ::
        ::  funky query
        ::
        %+  cook
          |=(a/tape [[%$ (crip a)] ~])
        (star pque)
      ==
    ::                                                  ::  ++zest:de-purl:html
    ++  zest                                            ::  2616 request-uri
      ;~  pose
        (stag %& (cook |=(a/purl a) auri))
        (stag %| ;~(plug apat yque))
      ==
    --  ::de-purl
  ::  +en-turf: encode +turf as a TLD-last domain string
  ::
  ++  en-turf
    |=  =turf
    ^-  @t
    (rap 3 (flop (join '.' turf)))
  ::  +de-turf: parse a TLD-last domain string into a TLD first +turf
  ::
  ++  de-turf
    |=  host=@t
    ^-  (unit turf)
    %+  rush  host
    %+  sear
      |=  =host:eyre
      ?.(?=(%& -.host) ~ (some p.host))
    thos:de-purl:html
  ::
  ::  MOVEME
  ::                                                    ::  ++fuel:html
  ++  fuel                                              ::  parse urbit fcgi
      |=  {bem/beam ced/noun:cred quy/quer}
      ^-  epic
      =+  qix=|-(`quay`?~(quy quy [[p q]:quy $(quy t.quy)]))
      [(malt qix) ;;(cred ced) bem]
  ::
  ++  hiss-to-request
    |=  =hiss
    ^-  request:http
    ::
    :*  ?-  p.q.hiss
          %conn  %'CONNECT'
          %delt  %'DELETE'
          %get   %'GET'
          %head  %'HEAD'
          %opts  %'OPTIONS'
          %post  %'POST'
          %put   %'PUT'
          %trac  %'TRACE'
        ==
    ::
      (crip (en-purl:html p.hiss))
    ::
      ^-  header-list:http
      ~!  q.q.hiss
      %+  turn  ~(tap by q.q.hiss)
      |=  [a=@t b=(list @t)]
      ^-  [@t @t]
      ?>  ?=(^ b)
      [a i.b]
    ::
      r.q.hiss
    ==
  --  ::  html
::                                                      ::
::::                      ++wired                       ::  wire formatting
  ::                                                    ::::
++  wired  ^?
  |%
  ::
  ++  auld                                              ::  ++auld:wired
    |=  sky/roof                                        ::  old style namespace
    ^-  slyt
    |=  {ref/* raw/*}
    =+  pux=((soft path) raw)
    ?~  pux  ~
    ?.  ?=({@ @ @ @ *} u.pux)  ~
    =+  :*  hyr=(slay i.u.pux)
            fal=(slay i.t.u.pux)
            dyc=(slay i.t.t.u.pux)
            ved=(slay i.t.t.t.u.pux)
            tyl=t.t.t.t.u.pux
        ==
    ?.  ?=({~ $$ $tas @} hyr)  ~
    ?.  ?=({~ $$ $p @} fal)  ~
    ?.  ?=({~ $$ $tas @} dyc)  ~
    ?.  ?=(^ ved)  ~
    =+  ron=q.p.u.hyr
    =+  bed=[[q.p.u.fal q.p.u.dyc (case p.u.ved)] (flop tyl)]
    =+  bop=(sky ref ~ ron bed)
    ?~  bop  ~
    ?~  u.bop  [~ ~]
    [~ ~ +.q.u.u.bop]
  ::                                                    ::  ++dray:wired
  ++  dray                                              ::  load tuple in path
    ::
    ::  .=  ~[p=~.ack q=~.~sarnel r=~..y]
    ::  (dray ~[p=%tas q=%p r=%f] %ack ~sarnel &)
    ::
    =-  |*  {a/{@tas (pole @tas)} b/*}  ^-  (paf a)
        =>  .(b `,(tup -.a +.a)`b)
        ?~  +.a  [(scot -.a b) ~]
        [(scot -.a -.b) `,(paf +.a)`(..$ +.a +.b)]
    :-  paf=|*(a/(pole) ?~(a $~ {(odo:raid ,-.a(. %ta)) ,(..$ +.a)}))
    ^=  tup
    |*  {a/@tas b/(pole @tas)}
    =+  c=(odo:raid a)
    ?~(b c {c (..$ ,-.b ,+.b)})
  ::                                                    ::  ++raid:wired
  ++  raid                                              ::  demand path odors
    ::
    ::  .=  [p=%ack q=~sarnel r=&]
    ::  (raid /ack/~sarnel+.y p=%tas q=%p r=%f ~)
    ::
    =-  |*  {a/path b/{@tas (pole @tas)}}
        =*  fog  (odo -.b)
        ?~  +.b  `fog`(slav -.b -.a)
        [`fog`(slav -.b -.a) (..$ +.a +.b)]
    ^=  odo
    |*  a/@tas
    |=  b/*
    =-  a(, (- b))                  ::  preserve face
    ?+  a   @
      $c  @c  $da  @da  $dr  @dr  $f   @f   $if  @if  $is  @is  $p   @p
      $u  @u  $uc  @uc  $ub  @ub  $ui  @ui  $ux  @ux  $uv  @uv  $uw  @uw
      $s  @s  $t   @t   $ta  @ta  $tas  @tas
    ==
::  ::                                                    ::  ++read:wired
::  ++  read                                              ::  parse odored path
::    =<  |*({a/path b/{@tas (pole @tas)}} ((+> b) a))
::    |*  b/{@tas (pole @tas)}
::    |=  a/path
::    ?~  a  ~
::    =+  hed=(slaw -.b i.a)
::    =*  fog  (odo:raid -.b)
::    ?~  +.b
::      ^-  (unit fog)
::      ?^(+.a ~ hed)
::    ^-  (unit {fog _(need *(..^$ +.b))})
::    (both hed ((..^$ +.b) +.a))
  --  ::wired
::                                                      ::
::::                      ++title                       ::  (2j) namespace
  ::                                                    ::::
++  title
  =>  |%
      ::                                                ::  ++clan:title
      ++  clan                                          ::  ship to rank
        |=  who=ship
        ^-  rank
        =/  wid  (met 3 who)
        ?:  (lte wid 1)   %czar
        ?:  =(2 wid)      %king
        ?:  (lte wid 4)   %duke
        ?:  (lte wid 8)   %earl
        ?>  (lte wid 16)  %pawn
      ::                                                ::  ++rank:title
      +$  rank  ?(%czar %king %duke %earl %pawn)        ::  ship width class
      ::                                                ::  ++name:title
      ++  name                                          ::  identity
        |=  who=ship
        ^-  ship
        ?.  ?=(%earl (clan who))  who
        (sein who)
      ::                                                ::  ++saxo:title
      ++  saxo                                          ::  autocanon
        |=  who=ship
        ^-  (list ship)
        =/  dad  (sein who)
        [who ?:(=(who dad) ~ $(who dad))]
      ::                                                ::  ++sein:title
      ++  sein                                          ::  autoboss
        |=  who=ship
        ^-  ship
        =/  mir  (clan who)
        ?-  mir
          $czar  who
          $king  (end 3 1 who)
          $duke  (end 4 1 who)
          $earl  (end 5 1 who)
          $pawn  (end 4 1 who)
        ==
      --
  |%
  ::                                                    ::  ++cite:title
  ++  cite                                              ::  render ship
    |=  who/@p
    ^-  tape
    =+  kind=(clan who)
    =+  name=(scow %p who)
    ?:  =(%earl kind)
      :(weld "~" (swag [15 6] name) "^" (swag [22 6] name))
    ?:  =(%pawn kind)
      :(weld (swag [0 7] name) "_" (swag [51 6] name))
    name
  ::                                                    ::  ++saxo:title
  ++  saxo                                              ::  autocanon
    |=  [our=ship now=@da who=ship]
    .^  (list ship)
        %j
        /(scot %p our)/saxo/(scot %da now)/(scot %p who)
    ==
  ::                                                    ::  ++sein:title
  ++  sein                                              ::  autoboss
    |=  [our=ship now=@da who=ship]
    .^  ship
        %j
        /(scot %p our)/sein/(scot %da now)/(scot %p who)
    ==
  ::                                                    ::  ++team:title
  ++  team                                              ::  our / our moon
    |=  [our=ship who=ship]
    ^-  ?
    ?|  =(our who)
        &(?=($earl (clan who)) =(our (^sein who)))
    ==
  --  ::title
::                                                      ::
::::                      ++milly                       ::  (2k) milliseconds
  ::                                                    ::::
++  milly  ^|
  |_  now/@da
  ::                                                    ::  ++around:milly
  ++  around                                            ::  relative msec
    |=  wen/@da
    ^-  @tas
    ?:  =(wen now)  %now
    ?:  (gth wen now)
      (cat 3 (scot %ud (msec (sub wen now))) %ms)
    (cat 3 '-' $(now wen, wen now))
  ::
  ++  about                                             ::  ++about:milly
    |=  wun/(unit @da)                                  ::  unit relative msec
    ^-  @tas
    ?~(wun %no (around u.wun))
  ::                                                    ::  ++mill:milly
  ++  mill                                              ::  msec diff
    |=  one/@dr
    ^-  @tas
    ?:  =(`@`0 one)  '0ms'
    (cat 3 (scot %ud (msec one)) %ms)
  ::                                                    ::  ++msec:milly
  ++  msec                                              ::  @dr to @ud ms
    |=(a/@dr `@ud`(div a (div ~s1 1.000)))
  ::                                                    ::  ++mull:milly
  ++  mull                                              ::  unit msec diff
    |=  une/(unit @dr)
    ^-  @tas
    ?~(une %no (mill u.une))
  --
::
::::
  ::
++  contain  ^?
  |%
  ::  +by-clock: interface core for a cache using the clock replacement algorithm
  ::
  ::    Presents an interface for a mapping, but somewhat specialized, and with
  ::    stateful accessors. The clock's :depth parameter is used as the maximum
  ::    freshness that an entry can have. The standard clock algorithm has a depth
  ::    of 1, meaning that a single sweep of the arm will delete the entry. For
  ::    more scan resistance, :depth can be set to a higher number.
  ::
  ::    Internally, :clock maintains a :lookup of type
  ::    `(map key-type [val=val-type fresh=@ud])`, where :depth.clock is the
  ::    maximum value of :fresh. Looking up a key increments its freshness, and a
  ::    sweep of the clock arm decrements its freshness.
  ::
  ::    The clock arm is stored as :queue, which is a `(qeu key-type)`. The head
  ::    of the queue represents the position of the clock arm. New entries are
  ::    inserted at the tail of the queue. When the clock arm sweeps, it
  ::    pops the head off the queue. If the :fresh of the head's entry in :lookup
  ::    is 0, remove the entry from the mapping and replace it with the new entry.
  ::    Otherwise, decrement the entry's freshness, put it back at the tail of
  ::    the queue, and pop the next head off the queue and try again.
  ::
  ::    Cache entries must be immutable: a key cannot be overwritten with a new
  ::    value. This property is enforced for entries currently stored in the
  ::    cache, but it is not enforced for previously deleted entries, since we
  ::    no longer remember what that key's value was supposed to be.
  ::
  ++  by-clock
    |*  [key-type=mold val-type=mold]
    |_  clock=(clock key-type val-type)
    ::  +get: looks up a key, marking it as fresh
    ::
    ++  get
      |=  key=key-type
      ^-  [(unit val-type) _clock]
      ::
      =+  maybe-got=(~(get by lookup.clock) key)
      ?~  maybe-got
        [~ clock]
      ::
      =.  clock  (freshen key)
      ::
      [`val.u.maybe-got clock]
    ::  +put: add a new cache entry, possibly removing an old one
    ::
    ++  put
      |=  [key=key-type val=val-type]
      ^+  clock
      ::  do nothing if our size is 0 so we don't decrement-underflow
      ::
      ?:  =(0 max-size.clock)
        clock
      ::  no overwrite allowed, but allow duplicate puts
      ::
      ?^  existing=(~(get by lookup.clock) key)
        ::  val must not change
        ::
        ?>  =(val val.u.existing)
        ::
        (freshen key)
      ::
      =?  clock  =(max-size.clock size.clock)
        evict
      ::
      %_  clock
        size    +(size.clock)
        lookup  (~(put by lookup.clock) key [val 1])
        queue   (~(put to queue.clock) key)
      ==
    ::  +freshen: increment the protection level on an entry
    ::
    ++  freshen
      |=  key=key-type
      ^+  clock
      %_    clock
          lookup
        %+  ~(jab by lookup.clock)  key
        |=  entry=[val=val-type fresh=@ud]
        entry(fresh (min +(fresh.entry) depth.clock))
      ==
    ::  +resize: changes the maximum size, removing entries if needed
    ::
    ++  resize
      |=  new-max=@ud
      ^+  clock
      ::
      =.  max-size.clock  new-max
      ::
      ?:  (gte new-max size.clock)
        clock
      ::
      (trim (sub size.clock new-max))
    ::  +evict: remove an entry from the cache
    ::
    ++  evict
      ^+  clock
      ::
      =.  size.clock  (dec size.clock)
      ::
      |-
      ^+  clock
      ::
      =^  old-key  queue.clock  ~(get to queue.clock)
      =/  old-entry  (~(got by lookup.clock) old-key)
      ::
      ?:  =(0 fresh.old-entry)
        clock(lookup (~(del by lookup.clock) old-key))
      ::
      %_    $
          lookup.clock
        (~(put by lookup.clock) old-key old-entry(fresh (dec fresh.old-entry)))
      ::
          queue.clock
        (~(put to queue.clock) old-key)
      ==
    ::  +trim: remove :count entries from the cache
    ::
    ++  trim
      |=  count=@ud
      ^+  clock
      ?:  =(0 count)
        clock
      $(count (dec count), clock evict)
    ::  +purge: removes all cache entries
    ::
    ++  purge
      ^+  clock
      %_  clock
        lookup  ~
        queue   ~
        size    0
      ==
    --
  ::  +to-capped-queue: interface door for +capped-queue
  ::
  ::    Provides a queue of a limited size where pushing additional items will
  ::    force pop the items at the front of the queue.
  ::
  ++  to-capped-queue
    |*  item-type=mold
    |_  queue=(capped-queue item-type)
    ::  +put: enqueue :item, possibly popping and producing an old item
    ::
    ++  put
      |=  item=item-type
      ^-  [(unit item-type) _queue]
      ::   are we already at max capacity?
      ::
      ?.  =(size.queue max-size.queue)
        ::  we're below max capacity, so push and increment size
        ::
        =.  queue.queue  (~(put to queue.queue) item)
        =.  size.queue   +(size.queue)
        ::
        [~ queue]
      ::  max is zero, the oldest item to return is the one which just went in.
      ::
      ?:  =(~ queue.queue)
        [`item queue]
      ::  we're at max capacity, so pop before pushing; size is unchanged
      ::
      =^  oldest  queue.queue  ~(get to queue.queue)
      =.  queue.queue          (~(put to queue.queue) item)
      ::
      [`oldest queue]
    ::  +get: pop an item off the queue, adjusting size
    ::
    ++  get
      ^-  [item-type _queue]
      ::
      =.  size.queue           (dec size.queue)
      =^  oldest  queue.queue  ~(get to queue.queue)
      ::
      [oldest queue]
    ::  change the :max-size of the queue, popping items if necessary
    ::
    ++  resize
      =|  pops=(list item-type)
      |=  new-max=@ud
      ^+  [pops queue]
      ::  we're not overfull, so no need to pop off more items
      ::
      ?:  (gte new-max size.queue)
        [(flop pops) queue(max-size new-max)]
      ::  we're above capacity; pop an item off and recurse
      ::
      =^  oldest  queue  get
      ::
      $(pops [oldest pops])
    --
  --
::                                                      ::
::::                      ++userlib                     ::  (2u) non-vane utils
  ::                                                    ::::
++  userlib  ^?
  |%
  ::                                                    ::
  ::::                    ++chrono:userlib              ::  (2uB) time
    ::                                                  ::::
  ++  chrono  ^?
    |%
    ::  +from-unix: unix timestamp to @da
    ::
    ++  from-unix
      |=  timestamp=@ud
      ^-  @da
      %+  add  ~1970.1.1
      (mul timestamp ~s1)
    ::                                                  ::  ++dawn:chrono:
    ++  dawn                                            ::  Jan 1 weekday
      |=  yer/@ud
      =+  yet=(sub yer 1)
      %-  mod  :_  7
      ;:  add
        1
        (mul 5 (mod yet 4))
        (mul 4 (mod yet 100))
        (mul 6 (mod yet 400))
      ==
    ::                                                  ::  ++daws:chrono:
    ++  daws                                            ::  date weekday
      |=  yed/date
      %-  mod  :_  7
      %+  add
        (dawn y.yed)
      (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1))
    ::                                                  ::  ++deal:chrono:
    ++  deal                                            ::  to leap sec time
      |=  yer/@da
      =+  n=0
      =+  yud=(yore yer)
      |-  ^-  date
      ?:  (gte yer (add (snag n lef:yu) ~s1))
        (yore (year yud(s.t (add n s.t.yud))))
      ?:  &((gte yer (snag n lef:yu)) (lth yer (add (snag n lef:yu) ~s1)))
        yud(s.t (add +(n) s.t.yud))
      ?:  =(+(n) (lent lef:yu))
        (yore (year yud(s.t (add +(n) s.t.yud))))
      $(n +(n))
    ::                                                  ::  ++lead:chrono:
    ++  lead                                            ::  from leap sec time
      |=  ley/date
      =+  ler=(year ley)
      =+  n=0
      |-  ^-  @da
      =+  led=(sub ler (mul n ~s1))
      ?:  (gte ler (add (snag n les:yu) ~s1))
        led
      ?:  &((gte ler (snag n les:yu)) (lth ler (add (snag n les:yu) ~s1)))
        ?:  =(s.t.ley 60)
          (sub led ~s1)
        led
      ?:  =(+(n) (lent les:yu))
        (sub led ~s1)
      $(n +(n))
    ::                                                  ::  ++dust:chrono:
    ++  dust                                            ::  print UTC format
      |=  yed/date
      ^-  tape
      =+  wey=(daws yed)
      =/  num  (d-co:co 1)  :: print as decimal without dots
      =/  pik  |=({n/@u t/wall} `tape`(scag 3 (snag n t)))
      ::
      "{(pik wey wik:yu)}, ".
      "{(num d.t.yed)} {(pik (dec m.yed) mon:yu)} {(num y.yed)} ".
      "{(num h.t.yed)}:{(num m.t.yed)}:{(num s.t.yed)} +0000"
    ::                                                  ::  ++stud:chrono:
    ++  stud                                            ::  parse UTC format
      =<  |=  a/cord                                    ::  expose parsers
          %+  biff  (rush a (more sepa elem))
          |=  b/(list _(wonk *elem))  ^-  (unit date)
          =-  ?.((za:dejs:format -) ~ (some (zp:dejs:format -)))
          ^+  =+  [*date u=unit]
              *{(u _[a y]) (u _m) (u _d.t) (u _+.t) ~}
          :~
              |-(?~(b ~ ?.(?=($y -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($m -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($d -.i.b) $(b t.b) `+.i.b)))
              |-(?~(b ~ ?.(?=($t -.i.b) $(b t.b) `+.i.b)))
          ==
      |%
      ::                                                ::  ++snug:stud:chrono:
      ++  snug                                          ::  position in list
        |=  a/(list tape)
        |=  b/tape
        =+  [pos=1 len=(lent b)]
        |-  ^-  (unit @u)
        ?~  a  ~
        ?:  =(b (scag len i.a))
          `pos
        $(pos +(pos), a t.a)
      ::                                                ::  ++sepa:stud:chrono:
      ++  sepa                                          ::  separator
        ;~(pose ;~(plug com (star ace)) (plus ace))
      ::                                                ::  ++elem:stud:chrono:
      ++  elem                                          ::  date element
        ;~  pose
          (stag %t t)  (stag %y y)  (stag %m m)  (stag %d d)
          (stag %w w)  (stag %z z)
        ==
      ::                                                ::  ++y:stud:chrono:
      ++  y                                             ::  year
        (stag %& (bass 10 (stun 3^4 dit)))
      ::                                                ::  ++m:stud:chrono:
      ++  m                                             ::  month
        (sear (snug mon:yu) (plus alf))
      ::                                                ::  ++d:stud:chrono:
      ++  d                                             ::  day
        (bass 10 (stun 1^2 dit))
      ::                                                ::  ++t:stud:chrono:
      ++  t                                             ::  hours:minutes:secs
        %+  cook  |=({h/@u @ m/@u @ s/@u} ~[h m s])
        ;~(plug d col d col d)
      ::
      ::  XX day of week is currently unchecked, and
      ::  timezone outright ignored.
      ::                                                ::  ++w:stud:chrono:
      ++  w                                             ::  day of week
        (sear (snug wik:yu) (plus alf))
      ::                                                ::  ++z:stud:chrono:
      ++  z                                             ::  time zone
        ;~(plug (mask "-+") dd dd)
      ::                                                ::  ++dd:stud:chrono:
      ++  dd                                            ::  two digits
        (bass 10 (stun 2^2 dit))
      --  ::
    ::                                                  ::  ++unt:chrono:userlib
    ++  unt                                             ::  Urbit to Unix time
      |=  a/@
      (div (sub a ~1970.1.1) ~s1)
    ::                                                  ::  ++yu:chrono:userlib
    ++  yu                                              ::  UTC format constants
      |%
      ::                                                ::  ++mon:yu:chrono:
      ++  mon                                           ::  months
        ^-  (list tape)
        :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
            "August"  "September"  "October"  "November"  "December"
        ==
      ::                                                ::  ++wik:yu:chrono:
      ++  wik                                           ::  weeks
        ^-  (list tape)
        :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
            "Friday"  "Saturday"
        ==
      ::                                                ::  ++lef:yu:chrono:
      ++  lef                                           ::  leapsecond dates
        ^-  (list @da)
        :~  ~2016.12.31..23.59.59   ~2015.6.30..23.59.59
            ~2012.6.30..23.59.59    ~2008.12.31..23.59.58
            ~2005.12.31..23.59.57   ~1998.12.31..23.59.56
            ~1997.6.30..23.59.55    ~1995.12.31..23.59.54
            ~1994.6.30..23.59.53    ~1993.6.30..23.59.52
            ~1992.6.30..23.59.51    ~1990.12.31..23.59.50
            ~1989.12.31..23.59.49   ~1987.12.31..23.59.48
            ~1985.6.30..23.59.47    ~1983.6.30..23.59.46
            ~1982.6.30..23.59.45    ~1981.6.30..23.59.44
            ~1979.12.31..23.59.43   ~1978.12.31..23.59.42
            ~1977.12.31..23.59.41   ~1976.12.31..23.59.40
            ~1975.12.31..23.59.39   ~1974.12.31..23.59.38
            ~1973.12.31..23.59.37   ~1972.12.31..23.59.36
            ~1972.6.30..23.59.35
        ==
      ::
      ::  +les:yu:chrono: leapsecond days
      ::
      ::    https://www.ietf.org/timezones/data/leap-seconds.list
      ::
      ++  les
        ^-  (list @da)
        :~  ~2017.1.1  ~2015.7.1  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1
            ~1997.7.1  ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1
            ~1990.1.1  ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1
            ~1980.1.1  ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1
            ~1974.1.1  ~1973.1.1  ~1972.7.1
        ==
      --  ::yu
    --  ::chrono
  ::                                                    ::
  ::::                    ++space:userlib               ::  (2uC) file utils
    ::                                                  ::::
  ++  space  ^?
    =,  clay
    |%
    ::                                                  ::  ++feel:space:userlib
    ++  feel                                            ::  simple file write
      |=  {pax/path val/cage}
      ^-  miso
      =+  dir=.^(arch %cy pax)
      ?~  fil.dir  [%ins val]
      [%mut val]
    ::                                                  ::  ++file:space:userlib
    ++  file                                            ::  simple file load
      |=  pax/path
      ^-  (unit)
      =+  dir=.^(arch %cy pax)
      ?~(fil.dir ~ [~ .^(* %cx pax)])
    ::                                                  ::  ++foal:space:userlib
    ++  foal                                            ::  high-level write
      |=  {pax/path val/cage}
      ^-  toro
      ?>  ?=({* * * *} pax)
      [i.t.pax [%& [[[t.t.t.pax (feel pax val)] ~]]]]
    ::                                                  ::  ++fray:space:userlib
    ++  fray                                            ::  high-level delete
      |=  pax/path
      ^-  toro
      ?>  ?=({* * * *} pax)
      [i.t.pax [%& [[[t.t.t.pax [%del ~]] ~]]]]
    ::                                                  ::  ++furl:space:userlib
    ++  furl                                            ::  unify changes
      |=  {one/toro two/toro}
      ^-  toro
      ~|  %furl
      ?>  ?&  =(p.one p.two)                            ::  same path
              &(?=(%& -.q.one) ?=(%& -.q.two))          ::  both deltas
          ==
      [p.one [%& (weld p.q.one p.q.two)]]
    --  ::space
  ::                                                    ::
  ::::                  ++unix:userlib                  ::  (2uD) unix line-list
    ::                                                  ::::
  ++  unix  ^?
    |%
    ::                                                  ::  ++lune:unix:userlib
    ++  lune                                            ::  cord by unix line
      ~%  %lune  ..is  ~
      |=  txt/@t
      ?~  txt
        ^-  (list @t)  ~
      =+  [byt=(rip 3 txt) len=(met 3 txt)]
      =|  {lin/(list @t) off/@}
      ^-  (list @t)
      %-  flop
      |-  ^+  lin
      ?:  =(off len)
        ~|  %noeol  !!
      ?:  =((snag off byt) 10)
        ?:  =(+(off) len)
          [(rep 3 (scag off byt)) lin]
        %=  $
          lin  [(rep 3 (scag off byt)) lin]
          byt  (slag +(off) byt)
          len  (sub len +(off))
          off  0
        ==
      $(off +(off))
    ::                                                  ::  ++nule:unix:userlib
    ++  nule                                            ::  lines to unix cord
      ~%  %nule  ..is  ~
      |=  lin/(list @t)
      ^-  @t
      %+  can  3
      %+  turn  lin
      |=  t/@t
      [+((met 3 t)) (cat 3 t 10)]
    --
  ::                                                    ::
  ::::                    ++scanf:userlib               ::  (2uF) exterpolation
    ::                                                  ::::
  ++  scanf
    =<  |*  {tape (pole _;/(*{$^(rule tape)}))}         ::  formatted scan
        =>  .(+< [a b]=+<)
        (scan a (parsf b))
    |%
    ::                                                  ::  ++parsf:scanf:
    ++  parsf                                           ::  make parser from:
      |*  a/(pole _;/(*{$^(rule tape)}))                ::  ;"chars{rule}chars"
      =-  (cook - (boil (norm a)))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
    ::
    ::  .=  (boil ~[[& dim] [| ", "] [& dim]]:ag)
    ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
    ::
    ::                                                  ::  ++boil:scanf:userlib
    ++  boil                                            ::
      |*  (list (each rule tape))
      ?~  +<  (easy ~)
      ?:  ?=(%| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
      %+  cook  |*({* *} [i t]=+<)
      ;~(plug p.i $(+< t))
    ::
    ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
    ::
    ::                                                  ::  ++norm:scanf:userlib
    ++  norm                                            ::
      |*  (pole _;/(*{$^(rule tape)}))
      ?~  +<  ~
      =>  .(+< [i=+<- t=+<+])
      :_  t=$(+< t)
      =+  rul=->->.i
      ^=  i
      ?~  rul     [%| p=rul]
      ?~  +.rul   [%| p=rul]
      ?@  &2.rul  [%| p=;;(tape rul)]
      [%& p=rul]
    --  ::scanf
  --
::  +harden: coerce %soft $hobo or pass-through
::
++  harden
  |*  task=mold
  |=  wrapped=(hobo task)
  ^-  task
  ?.  ?=(%soft -.wrapped)
    wrapped
  ;;(task +.wrapped)
::
++  zuse  %309                                          ::  hoon+zuse kelvin
::                                                      ::
::::                      ++azimuth                     ::  (2az) azimuth
  ::                                                    ::::
++  azimuth
  !:
  =*  address  address:rpc:ethereum
  ::  types
  ::
  =>  =>  [azimuth-types ethereum-types .]
      |%
      ++  complete-ship
        $:  state=point
            history=(list diff-point)  ::TODO  maybe block/event nr?  ::  newest first
            keys=(map life pass)
        ==
      ::
      ++  fleet  (map @p complete-ship)
      ::
      ++  eth-type
        |%
        ++  point
          :~  [%bytes-n 32]   ::  encryptionKey
              [%bytes-n 32]   ::  authenticationKey
              %bool           ::  hasSponsor
              %bool           ::  active
              %bool           ::  escapeRequested
              %uint           ::  sponsor
              %uint           ::  escapeRequestedTo
              %uint           ::  cryptoSuiteVersion
              %uint           ::  keyRevisionNumber
              %uint           ::  continuityNumber
          ==
        ++  deed
          :~  %address        ::  owner
              %address        ::  managementProxy
              %address        ::  spawnProxy
              %address        ::  votingProxy
              %address        ::  transferProxy
          ==
        --
      ::
      ++  eth-noun
        |%
        ++  point
          $:  encryption-key=octs
              authentication-key=octs
              has-sponsor=?
              active=?
              escape-requested=?
              sponsor=@ud
              escape-to=@ud
              crypto-suite=@ud
              key-revision=@ud
              continuity-number=@ud
          ==
        ++  deed
          $:  owner=address
              management-proxy=address
              spawn-proxy=address
              voting-proxy=address
              transfer-proxy=address
          ==
        --
      ::
      ++  function
        |%
        ++  azimuth
          $%  [%points who=@p]
              [%rights who=@p]
              [%get-spawned who=@p]
              [%dns-domains ind=@ud]
          ==
        --
      ::
      ::  #  diffs
      ::
      ++  update
        $%  [%full ships=(map ship point) dns=dnses heard=events]
            [%difs dis=(list (pair event-id diff-azimuth))]
        ==
      ::
      ::  #  constants
      ::
      ::  contract addresses
      ++  contracts  mainnet-contracts
      ++  mainnet-contracts
        |%
        ::  azimuth: data contract
        ::
        ++  azimuth
          0x223c.067f.8cf2.8ae1.73ee.5caf.ea60.ca44.c335.fecb
        ::
        ++  ecliptic
          0x6ac0.7b7c.4601.b5ce.11de.8dfe.6335.b871.c7c4.dd4d
        ::
        ++  linear-star-release
          0x86cd.9cd0.992f.0423.1751.e376.1de4.5cec.ea5d.1801
        ::
        ++  conditional-star-release
          0x8c24.1098.c3d3.498f.e126.1421.633f.d579.86d7.4aea
        ::
        ++  delegated-sending
          0xf790.8ab1.f1e3.52f8.3c5e.bc75.051c.0565.aeae.a5fb
        ::
        ::  launch: block number of azimuth deploy
        ::
        ++  launch  6.784.800
        ::
        ::  public: block number of azimuth becoming independent
        ::
        ++  public  7.033.765
        --
      ::
      ::  Testnet contract addresses
      ::
      ++  ropsten-contracts
        |%
        ++  azimuth
          0x308a.b6a6.024c.f198.b57e.008d.0ac9.ad02.1988.6579
        ::
        ++  ecliptic
          0x8b9f.86a2.8921.d9c7.05b3.113a.755f.b979.e1bd.1bce
        ::
        ++  linear-star-release
          0x1f8e.dd03.1ee4.1474.0aed.b39b.84fb.8f2f.66ca.422f
        ::
        ++  conditional-star-release
          0x0
        ::
        ++  delegated-sending
          0x3e8c.a510.354b.c2fd.bbd6.1502.52d9.3105.c9c2.7bbe
        ::
        ++  launch  4.601.630
        ++  public  launch
        --
      ::
        ::  ++  azimuth  0x863d.9c2e.5c4c.1335.96cf.ac29.d552.55f0.d0f8.6381  ::  local bridge
      ::  hashes of ship event signatures
      ++  azimuth-events
        |%
        ::
        ::  OwnerChanged(uint32,address)
        ++  owner-changed
          0x16d0.f539.d49c.6cad.822b.767a.9445.bfb1.
            cf7e.a6f2.a6c2.b120.a7ea.4cc7.660d.8fda
        ::
        ::  Activated(uint32)
        ++  activated
          0xe74c.0380.9d07.69e1.b1f7.06cc.8414.258c.
            d1f3.b6fe.020c.d15d.0165.c210.ba50.3a0f
        ::
        ::  Spawned(uint32,uint32)
        ++  spawned
          0xb2d3.a6e7.a339.f5c8.ff96.265e.2f03.a010.
            a854.1070.f374.4a24.7090.9644.1508.1546
        ::
        ::  EscapeRequested(uint32,uint32)
        ++  escape-requested
          0xb4d4.850b.8f21.8218.141c.5665.cba3.79e5.
            3e9b.b015.b51e.8d93.4be7.0210.aead.874a
        ::
        ::  EscapeCanceled(uint32,uint32)
        ++  escape-canceled
          0xd653.bb0e.0bb7.ce83.93e6.24d9.8fbf.17cd.
            a590.2c83.28ed.0cd0.9988.f368.90d9.932a
        ::
        ::  EscapeAccepted(uint32,uint32)
        ++  escape-accepted
          0x7e44.7c9b.1bda.4b17.4b07.96e1.00bf.7f34.
            ebf3.6dbb.7fe6.6549.0b1b.fce6.246a.9da5
        ::
        ::  LostSponsor(uint32,uint32)
        ++  lost-sponsor
          0xd770.4f9a.2519.3dbd.0b0c.b4a8.09fe.ffff.
            a7f1.9d1a.ae88.17a7.1346.c194.4482.10d5
        ::
        ::  ChangedKeys(uint32,bytes32,bytes32,uint32,uint32)
        ++  changed-keys
          0xaa10.e7a0.117d.4323.f1d9.9d63.0ec1.69be.
            bb3a.988e.8957.70e3.5198.7e01.ff54.23d5
        ::
        ::  BrokeContinuity(uint32,uint32)
        ++  broke-continuity
          0x2929.4799.f1c2.1a37.ef83.8e15.f79d.d91b.
            cee2.df99.d63c.d1c1.8ac9.68b1.2951.4e6e
        ::
        ::  ChangedSpawnProxy(uint32,address)
        ++  changed-spawn-proxy
          0x9027.36af.7b3c.efe1.0d9e.840a.ed0d.687e.
            35c8.4095.122b.2505.1a20.ead8.866f.006d
        ::
        ::  ChangedTransferProxy(uint32,address)
        ++  changed-transfer-proxy
          0xcfe3.69b7.197e.7f0c.f067.93ae.2472.a9b1.
            3583.fecb.ed2f.78df.a14d.1f10.796b.847c
        ::
        ::  ChangedManagementProxy(uint32,address)
        ++  changed-management-proxy
          0xab9c.9327.cffd.2acc.168f.afed.be06.139f.
            5f55.cb84.c761.df05.e051.1c25.1e2e.e9bf
        ::
        ::  ChangedVotingProxy(uint32,address)
        ++  changed-voting-proxy
          0xcbd6.269e.c714.57f2.c7b1.a227.74f2.46f6.
            c5a2.eae3.795e.d730.0db5.1768.0c61.c805
        ::
        ::  ChangedDns(string,string,string)
        ++  changed-dns
          0xfafd.04ad.e1da.ae2e.1fdb.0fc1.cc6a.899f.
            d424.063e.d5c9.2120.e67e.0730.53b9.4898
        --
      --
  ::
  ::  logic
  ::
  |%
  ++  pass-from-eth
    |=  [enc=octs aut=octs sut=@ud]
    ^-  pass
    %^  cat  3  'b'
    ?.  &(=(1 sut) =(p.enc 32) =(p.aut 32))
      (cat 8 0 0)
    (cat 8 q.aut q.enc)
  ::
  ++  point-from-eth
    |=  [who=@p point:eth-noun deed:eth-noun]
    ^-  point
    ::
    ::  ownership
    ::
    :+  :*  owner
            management-proxy
            voting-proxy
            transfer-proxy
        ==
      ::
      ::  network state
      ::
      ?.  active  ~
      :-  ~
      :*  key-revision
        ::
          (pass-from-eth encryption-key authentication-key crypto-suite)
        ::
          continuity-number
        ::
          [has-sponsor `@p`sponsor]
        ::
          ?.  escape-requested  ~
          ``@p`escape-to
      ==
    ::
    ::  spawn state
    ::
    ?.  ?=(?(%czar %king) (clan:title who))  ~
    :-  ~
    :*  spawn-proxy
        ~  ::TODO  call getSpawned to fill this
    ==
  ::
  ++  event-log-to-point-diff
    =,  azimuth-events
    =,  abi:ethereum
    |=  log=event-log:rpc:ethereum
    ^-  (unit (pair ship diff-point))
    ~?  ?=(~ mined.log)  %processing-unmined-event
    ::
    ?:  =(i.topics.log owner-changed)
      =/  [who=@ wer=address]
          (decode-topics t.topics.log ~[%uint %address])
      `[who %owner wer]
    ::
    ?:  =(i.topics.log activated)
      =/  who=@
        (decode-topics t.topics.log ~[%uint])
      `[who %activated who]
    ::
    ?:  =(i.topics.log spawned)
      =/  [pre=@ who=@]
          (decode-topics t.topics.log ~[%uint %uint])
      `[pre %spawned who]
    ::
    ?:  =(i.topics.log escape-requested)
      =/  [who=@ wer=@]
          (decode-topics t.topics.log ~[%uint %uint])
      `[who %escape `wer]
    ::
    ?:  =(i.topics.log escape-canceled)
      =/  who=@  (decode-topics t.topics.log ~[%uint])
      `[who %escape ~]
    ::
    ?:  =(i.topics.log escape-accepted)
      =/  [who=@ wer=@]
          (decode-topics t.topics.log ~[%uint %uint])
      `[who %sponsor & wer]
    ::
    ?:  =(i.topics.log lost-sponsor)
      =/  [who=@ pos=@]
          (decode-topics t.topics.log ~[%uint %uint])
      `[who %sponsor | pos]
    ::
    ?:  =(i.topics.log changed-keys)
      =/  who=@  (decode-topics t.topics.log ~[%uint])
      =/  [enc=octs aut=octs sut=@ud rev=@ud]
          %+  decode-results  data.log
          ~[[%bytes-n 32] [%bytes-n 32] %uint %uint]
      `[who %keys rev (pass-from-eth enc aut sut)]
    ::
    ?:  =(i.topics.log broke-continuity)
      =/  who=@  (decode-topics t.topics.log ~[%uint])
      =/  num=@  (decode-results data.log ~[%uint])
      `[who %continuity num]
    ::
    ?:  =(i.topics.log changed-management-proxy)
      =/  [who=@ sox=address]
          (decode-topics t.topics.log ~[%uint %address])
      `[who %management-proxy sox]
    ::
    ?:  =(i.topics.log changed-voting-proxy)
      =/  [who=@ tox=address]
          (decode-topics t.topics.log ~[%uint %address])
      `[who %voting-proxy tox]
    ::
    ?:  =(i.topics.log changed-spawn-proxy)
      =/  [who=@ sox=address]
          (decode-topics t.topics.log ~[%uint %address])
      `[who %spawn-proxy sox]
    ::
    ?:  =(i.topics.log changed-transfer-proxy)
      =/  [who=@ tox=address]
          (decode-topics t.topics.log ~[%uint %address])
      `[who %transfer-proxy tox]
    ::
    ::  warn about unimplemented events, but ignore
    ::  the ones we know are harmless.
    ~?  ?!  .=  i.topics.log
            ::  OwnershipTransferred(address,address)
            0x8be0.079c.5316.5914.1344.cd1f.d0a4.f284.
              1949.7f97.22a3.daaf.e3b4.186f.6b64.57e0
      [%unimplemented-event i.topics.log]
    ~
  ::
  ++  apply-point-diff
    |=  [pot=point dif=diff-point]
    ^-  point
    ?-  -.dif
      %full             new.dif
    ::
        %activated
      %_  pot
        net  `[0 0 0 &^(^sein:title who.dif) ~]
        kid  ?.  ?=(?(%czar %king) (clan:title who.dif))  ~
             `[0x0 ~]
      ==
    ::
    ::  ownership
    ::
      %owner           pot(owner.own new.dif)
      %transfer-proxy  pot(transfer-proxy.own new.dif)
      %management-proxy  pot(management-proxy.own new.dif)
      %voting-proxy      pot(voting-proxy.own new.dif)
    ::
    ::  networking
    ::
        ?(%keys %continuity %sponsor %escape)
      ?>  ?=(^ net.pot)
      ?-  -.dif
          %keys
        pot(life.u.net life.dif, pass.u.net pass.dif)
      ::
          %sponsor
        %=  pot
          sponsor.u.net  new.dif
          escape.u.net   ?:(has.new.dif ~ escape.u.net.pot)
        ==
      ::
        %continuity  pot(continuity-number.u.net new.dif)
        %escape      pot(escape.u.net new.dif)
      ==
    ::
    ::  spawning
    ::
        ?(%spawned %spawn-proxy)
      ?>  ?=(^ kid.pot)
      ?-  -.dif
          %spawned
        =-  pot(spawned.u.kid -)
        (~(put in spawned.u.kid.pot) who.dif)
      ::
        %spawn-proxy  pot(spawn-proxy.u.kid new.dif)
      ==
    ==
  ::
  ++  parse-id
    |=  id=@t
    ^-  azimuth:function
    |^
      ~|  id
      %+  rash  id
      ;~  pose
        (function %points 'points' shipname)
        (function %get-spawned 'getSpawned' shipname)
        (function %dns-domains 'dnsDomains' dem:ag)
      ==
    ::
    ++  function
      |*  [tag=@tas fun=@t rul=rule]
      ;~(plug (cold tag (jest fun)) (ifix [lit rit] rul))
    ::
    ++  shipname
      ;~(pfix sig fed:ag)
    --
  ::
  ++  function-to-call
    |%
    ++  azimuth
      |=  cal=azimuth:function
      ^-  [id=@t dat=call-data:rpc:ethereum]
      ?-  -.cal
          %points
        :-  (crip "points({(scow %p who.cal)})")
        ['points(uint32)' ~[uint+`@`who.cal]]
      ::
          %rights
        :-  (crip "rights({(scow %p who.cal)})")
        ['rights(uint32)' ~[uint+`@`who.cal]]
      ::
          %get-spawned
        :-  (crip "getSpawned({(scow %p who.cal)})")
        ['getSpawned(uint32)' ~[uint+`@`who.cal]]
      ::
          %dns-domains
        :-  (crip "dnsDomains({(scow %ud ind.cal)})")
        ['dnsDomains(uint256)' ~[uint+ind.cal]]
      ==
    --
  --
::                                                      ::
::::                      ++ethereum                    ::  (2eth) ethereum
  ::                                                    ::::
++  ethereum
  !:
  =>  [ethereum-types .]
  |%
  ::  deriving and using ethereum keys
  ::
  ++  key
    |%
    ++  address-from-pub
      =,  keccak:crypto
      |=  pub=@
      %^  end  3  20
      %+  keccak-256  64
      (rev 3 64 pub)
    ::
    ++  address-from-prv
      (cork pub-from-prv address-from-pub)
    ::
    ++  pub-from-prv
      =,  secp256k1:secp:crypto
      |=  prv=@
      %-  serialize-point
      (priv-to-pub prv)
    ::
    ++  sign-transaction
      =,  crypto
      |=  [tx=transaction:rpc pk=@]
      ^-  @ux
      ::  hash the raw transaction data
      =/  hash=@
        =/  dat=@
          %-  encode-atoms:rlp
          ::  with v=chain-id, r=0, s=0
          tx(chain-id [chain-id.tx 0 0 ~])
        =+  wid=(met 3 dat)
        %-  keccak-256:keccak
        [wid (rev 3 wid dat)]
      ::  sign transaction hash with private key
      =+  (ecdsa-raw-sign:secp256k1:secp hash pk)
      ::  complete transaction is raw data, with r and s
      ::  taken from the signature, and v as per eip-155
      %-  encode-atoms:rlp
      tx(chain-id [:(add (mul chain-id.tx 2) 35 v) r s ~])
    --
  ::
  ::  rlp en/decoding
  ::NOTE  https://github.com/ethereum/wiki/wiki/RLP
  ::
  ++  rlp
    |%
    ::NOTE  rlp encoding doesn't really care about leading zeroes,
    ::      but because we need to disinguish between no-bytes zero
    ::      and one-byte zero (and also empty list) we end up with
    ::      this awful type...
    +$  item
      $%  [%l l=(list item)]
          [%b b=byts]
      ==
    ::  +encode-atoms: encode list of atoms as a %l of %b items
    ::
    ++  encode-atoms
      |=  l=(list @)
      ^-  @
      %+  encode  %l
      %+  turn  l
      |=(a=@ b+[(met 3 a) a])
    ::
    ++  encode
      |=  in=item
      |^  ^-  @
          ?-  -.in
              %b
            ?:  &(=(1 wid.b.in) (lte dat.b.in 0x7f))
              dat.b.in
            =-  (can 3 ~[b.in [(met 3 -) -]])
            (encode-length wid.b.in 0x80)
          ::
              %l
            =/  out=@
              %+  roll  l.in
              |=  [ni=item en=@]
              (cat 3 (encode ni) en)
            %^  cat  3  out
            (encode-length (met 3 out) 0xc0)
          ==
      ::
      ++  encode-length
        |=  [len=@ off=@]
        ?:  (lth len 56)  (add len off)
        =-  (cat 3 len -)
        :(add (met 3 len) off 55)
      --
    ::  +decode-atoms: decode expecting a %l of %b items, producing atoms within
    ::
    ++  decode-atoms
      |=  dat=@
      ^-  (list @)
      =/  i=item  (decode dat)
      ~|  [%unexpected-data i]
      ?>  ?=(%l -.i)
      %+  turn  l.i
      |=  i=item
      ~|  [%unexpected-list i]
      ?>  ?=(%b -.i)
      dat.b.i
    ::
    ++  decode
      |=  dat=@
      ^-  item
      =/  bytes=(list @)  (flop (rip 3 dat))
      =?  bytes  ?=(~ bytes)  ~[0]
      |^  item:decode-head
      ::
      ++  decode-head
        ^-  [done=@ud =item]
        ?~  bytes
          ~|  %rlp-unexpected-end
          !!
        =*  byt  i.bytes
        ::  byte in 0x00-0x79 range encodes itself
        ::
        ?:  (lte byt 0x79)
          :-  1
          [%b 1^byt]
        ::  byte in 0x80-0xb7 range encodes string length
        ::
        ?:  (lte byt 0xb7)
          =+  len=(sub byt 0x80)
          :-  +(len)
          :-  %b
          len^(get-value 1 len)
        ::  byte in 0xb8-0xbf range encodes string length length
        ::
        ?:  (lte byt 0xbf)
          =+  led=(sub byt 0xb7)
          =+  len=(get-value 1 led)
          :-  (add +(led) len)
          :-  %b
          len^(get-value +(led) len)
        ::  byte in 0xc0-f7 range encodes list length
        ::
        ?:  (lte byt 0xf7)
          =+  len=(sub byt 0xc0)
          :-  +(len)
          :-  %l
          %.  len
          decode-list(bytes (slag 1 `(list @)`bytes))
        ::  byte in 0xf8-ff range encodes list length length
        ::
        ?:  (lte byt 0xff)
          =+  led=(sub byt 0xf7)
          =+  len=(get-value 1 led)
          :-  (add +(led) len)
          :-  %l
          %.  len
          decode-list(bytes (slag +(led) `(list @)`bytes))
        ~|  [%rip-not-bloq-3 `@ux`byt]
        !!
      ::
      ++  decode-list
        |=  rem=@ud
        ^-  (list item)
        ?:  =(0 rem)  ~
        =+  ^-  [don=@ud =item]  ::TODO  =/
          decode-head
        :-  item
        %=  $
          rem    (sub rem don)
          bytes  (slag don bytes)
        ==
      ::
      ++  get-value
        |=  [at=@ud to=@ud]
        ^-  @
        (rep 3 (flop (swag [at to] bytes)))
      --
    --
  ::
  ::  abi en/decoding
  ::NOTE  https://solidity.readthedocs.io/en/develop/abi-spec.html
  ::
  ++  abi
    =>  |%
        ::  solidity types. integer bitsizes ignored
        ++  etyp
          $@  $?  ::  static
                  %address  %bool
                  %int      %uint
                  %real     %ureal
                  ::  dynamic
                  %bytes    %string
              ==
          $%  ::  static
              [%bytes-n n=@ud]
              ::  dynamic
              [%array-n t=etyp n=@ud]
              [%array t=etyp]
          ==
        ::
        ::  solidity-style typed data. integer bitsizes ignored
        ++  data
          $%  [%address p=address]
              [%string p=tape]
              [%bool p=?]
              [%int p=@sd]
              [%uint p=@ud]
              [%real p=@rs]
              [%ureal p=@urs]
              [%array-n p=(list data)]
              [%array p=(list data)]
              [%bytes-n p=octs]  ::TODO  just @, because context knows length?
              [%bytes p=octs]
          ==
        --
    =,  mimes:html
    |%
    ::  encoding
    ::
    ++  encode-args
      ::  encode list of arguments.
      ::
      |=  das=(list data)
      ^-  tape
      (encode-data [%array-n das])
    ::
    ++  encode-data
      ::  encode typed data into ABI bytestring.
      ::
      |=  dat=data
      ^-  tape
      ?+  -.dat
        ~|  [%unsupported-type -.dat]
        !!
      ::
          %array-n
        ::  enc(X) = head(X[0]) ... head(X[k-1]) tail(X[0]) ... tail(X[k-1])
        ::  where head and tail are defined for X[i] being of a static type as
        ::  head(X[i]) = enc(X[i]) and tail(X[i]) = "" (the empty string), or as
        ::  head(X[i]) = enc(len( head(X[0])..head(X[k-1])
        ::                        tail(X[0])..tail(X[i-1]) ))
        ::  and tail(X[i]) = enc(X[i]) otherwise.
        ::
        ::  so: if it's a static type, data goes in the head. if it's a dynamic
        ::  type, a reference goes into the head and data goes into the tail.
        ::
        ::  in the head, we first put a placeholder where references need to go.
        =+  hol=(reap 64 'x')
        =/  hes=(list tape)
          %+  turn  p.dat
          |=  d=data
          ?.  (is-dynamic-type d)  ^$(dat d)
          hol
        =/  tas=(list tape)
          %+  turn  p.dat
          |=  d=data
          ?.  (is-dynamic-type d)  ""
          ^$(dat d)
        ::  once we know the head and tail, we can fill in the references in head.
        =-  (weld nes `tape`(zing tas))
        ^-  [@ud nes=tape]
        =+  led=(lent (zing hes))
        %+  roll  hes
        |=  [t=tape i=@ud nes=tape]
        :-  +(i)
        ::  if no reference needed, just put the data.
        ?.  =(t hol)  (weld nes t)
        ::  calculate byte offset of data we need to reference.
        =/  ofs/@ud
          =-  (div - 2)       ::  two hex digits per byte.
          %+  add  led        ::  count head, and
          %-  lent  %-  zing  ::  count all tail data
          (scag i tas)        ::  preceding ours.
        =+  ref=^$(dat [%uint ofs])
        ::  shouldn't hit this unless we're sending over 2gb of data?
        ~|  [%weird-ref-lent (lent ref)]
        ?>  =((lent ref) (lent hol))
        (weld nes ref)
      ::
          %array  ::  where X has k elements (k is assumed to be of type uint256):
        ::  enc(X) = enc(k) enc([X[1], ..., X[k]])
        ::  i.e. it is encoded as if it were an array of static size k, prefixed
        ::  with the number of elements.
        %+  weld  $(dat [%uint (lent p.dat)])
        $(dat [%array-n p.dat])
      ::
          %bytes-n
        ::  enc(X) is the sequence of bytes in X padded with zero-bytes to a
        ::  length of 32.
        ::  Note that for any X, len(enc(X)) is a multiple of 32.
        ~|  [%bytes-n-too-long max=32 actual=p.p.dat]
        ?>  (lte p.p.dat 32)
        (pad-to-multiple (render-hex-bytes p.dat) 64 %right)
      ::
          %bytes  ::  of length k (which is assumed to be of type uint256)
        ::  enc(X) = enc(k) pad_right(X), i.e. the number of bytes is encoded as a
        ::  uint256 followed by the actual value of X as a byte sequence, followed
        ::  by the minimum number of zero-bytes such that len(enc(X)) is a
        ::  multiple of 32.
        %+  weld  $(dat [%uint p.p.dat])
        (pad-to-multiple (render-hex-bytes p.dat) 64 %right)
      ::
          %string
        ::  enc(X) = enc(enc_utf8(X)), i.e. X is utf-8 encoded and this value is
        ::  interpreted as of bytes type and encoded further. Note that the length
        ::  used in this subsequent encoding is the number of bytes of the utf-8
        ::  encoded string, not its number of characters.
        $(dat [%bytes (lent p.dat) (swp 3 (crip p.dat))])
      ::
          %uint
        ::  enc(X) is the big-endian encoding of X, padded on the higher-order
        ::  (left) side with zero-bytes such that the length is a multiple of 32
        ::  bytes.
        (pad-to-multiple (render-hex-bytes (as-octs p.dat)) 64 %left)
      ::
          %bool
        ::  as in the uint8 case, where 1 is used for true and 0 for false
        $(dat [%uint ?:(p.dat 1 0)])
      ::
          %address
        ::  as in the uint160 case
        $(dat [%uint `@ud`p.dat])
      ==
    ::
    ++  is-dynamic-type
      |=  a=data
      ?.  ?=(%array-n -.a)
        ?=(?(%string %bytes %array) -.a)
      &(!=((lent p.a) 0) (lien p.a is-dynamic-type))
    ::
    ::  decoding
    ::
    ++  decode-topics  decode-arguments
    ::
    ++  decode-results
      ::  rex:  string of hex bytes with leading 0x.
      |*  [rex=@t tys=(list etyp)]
      =-  (decode-arguments - tys)
      %+  turn  (rip 9 (rsh 3 2 rex))
      (curr rash hex)
    ::
    ++  decode-arguments
      |*  [wos=(list @) tys=(list etyp)]
      =/  wos=(list @)  wos  ::  get rid of tmi
      =|  win=@ud
      =<  (decode-from 0 tys)
      |%
      ++  decode-from
        |*  [win=@ud tys=(list etyp)]
        ?~  tys  !!
        =-  ?~  t.tys  dat
            [dat $(win nin, tys t.tys)]
        (decode-one win ~[i.tys])
      ::
      ++  decode-one
        ::NOTE  we take (list etyp) even though we only operate on
        ::      a single etyp as a workaround for urbit/arvo#673
        |*  [win=@ud tys=(list etyp)]
        =-  [nin dat]=-  ::NOTE  ^= regular form broken
        ?~  tys  !!
        =*  typ  i.tys
        =+  wor=(snag win wos)
        ?+  typ
          ~|  [%unsupported-type typ]
          !!
        ::
            ?(%address %bool %uint)  ::  %int %real %ureal
          :-  +(win)
          ?-  typ
            %address  `@ux`wor
            %uint     `@ud`wor
            %bool     =(1 wor)
          ==
        ::
            %string
          =+  $(tys ~[%bytes])
          [nin (trip (swp 3 q.dat))]
        ::
            %bytes
          :-  +(win)
          ::  find the word index of the actual data.
          =/  lic=@ud  (div wor 32)
          ::  learn the bytelength of the data.
          =/  len=@ud  (snag lic wos)
          (decode-bytes-n +(lic) len)
        ::
            [%bytes-n *]
          :-  (add win +((div (dec n.typ) 32)))
          (decode-bytes-n win n.typ)
        ::
            [%array *]
          :-  +(win)
          ::  find the word index of the actual data.
          =.  win  (div wor 32)
          ::  read the elements from their location.
          %-  tail
          %^  decode-array-n  ~[t.typ]  +(win)
          (snag win wos)
        ::
            [%array-n *]
          (decode-array-n ~[t.typ] win n.typ)
        ==
      ::
      ++  decode-bytes-n
        |=  [fro=@ud bys=@ud]
        ^-  octs
        ::  parse {bys} bytes from {fro}.
        :-  bys
        %^  rsh  3
          =+  (mod bys 32)
          ?:(=(0 -) - (sub 32 -))
        %+  rep  8
        %-  flop
        =-  (swag [fro -] wos)
        +((div (dec bys) 32))
      ::
      ++  decode-array-n
        ::NOTE  we take (list etyp) even though we only operate on
        ::      a single etyp as a workaround for urbit/arvo#673
        ::NOTE  careful! produces lists without type info
        =|  res=(list)
        |*  [tys=(list etyp) fro=@ud len=@ud]
        ^-  [@ud (list)]
        ?~  tys  !!
        ?:  =(len 0)  [fro (flop `(list)`res)]
        =+  (decode-one fro ~[i.tys])  ::  [nin=@ud dat=*]
        $(res ^+(res [dat res]), fro nin, len (dec len))
      --
    --
  ::
  ::  communicating with rpc nodes
  ::NOTE  https://github.com/ethereum/wiki/wiki/JSON-RPC
  ::
  ++  rpc
    ::  types
    ::
    =>  =,  abi
        =,  format
        |%
        ::  raw call data
        ++  call-data
          $:  function=@t
              arguments=(list data)
          ==
        ::
        ::  raw transaction data
        +$  transaction
          $:  nonce=@ud
              gas-price=@ud
              gas=@ud
              to=address
              value=@ud
              data=@ux
              chain-id=@ux
          ==
        ::
        ::  ethereum json rpc api
        ::
        ::  supported requests.
        ++  request
          $%  [%eth-block-number ~]
              [%eth-call cal=call deb=block]
              $:  %eth-new-filter
                  fro=(unit block)
                  tob=(unit block)
                  adr=(list address)
                  top=(list ?(@ux (list @ux)))
              ==
              [%eth-get-block-by-number bon=@ud txs=?]
              [%eth-get-filter-logs fid=@ud]
              $:  %eth-get-logs
                  fro=(unit block)
                  tob=(unit block)
                  adr=(list address)
                  top=(list ?(@ux (list @ux)))
              ==
              $:  %eth-get-logs-by-hash
                  has=@
                  adr=(list address)
                  top=(list ?(@ux (list @ux)))
              ==
              [%eth-get-filter-changes fid=@ud]
              [%eth-get-transaction-count adr=address =block]
              [%eth-get-transaction-receipt txh=@ux]
              [%eth-send-raw-transaction dat=@ux]
          ==
        ::
        ::TODO  clean up & actually use
        ++  response
          $%  ::TODO
              [%eth-new-filter fid=@ud]
              [%eth-get-filter-logs los=(list event-log)]
              [%eth-get-logs los=(list event-log)]
              [%eth-get-logs-by-hash los=(list event-log)]
              [%eth-got-filter-changes los=(list event-log)]
              [%eth-transaction-hash haz=@ux]
          ==
        ::
        ++  event-log
          $:  ::  null for pending logs
              $=  mined  %-  unit
              $:  log-index=@ud
                  transaction-index=@ud
                  transaction-hash=@ux
                  block-number=@ud
                  block-hash=@ux
                  removed=?
              ==
            ::
              address=@ux
              data=@t
              ::  event data
              ::
              ::    For standard events, the first topic is the event signature
              ::    hash. For anonymous events, the first topic is the first
              ::    indexed argument.
              ::    Note that this does not support the "anonymous event with
              ::    zero topics" case. This has dubious usability, and using
              ::    +lest instead of +list saves a lot of ?~ checks.
              ::
              topics=(lest @ux)
          ==
        ::
        ::  data for eth_call.
        ++  call
          $:  from=(unit address)
              to=address
              gas=(unit @ud)
              gas-price=(unit @ud)
              value=(unit @ud)
              data=tape
          ==
        ::
        ::  minimum data needed to construct a read call
        ++  proto-read-request
          $:  id=(unit @t)
              to=address
              call-data
          ==
        ::
        ::  block to operate on.
        ++  block
          $%  [%number n=@ud]
              [%label l=?(%earliest %latest %pending)]
          ==
        --
    ::
    ::  logic
    ::
    |%
    ++  encode-call
      |=  call-data
      ^-  tape
      ::TODO  should this check to see if the data matches the function signature?
      =-  :(weld "0x" - (encode-args arguments))
      %+  scag  8
      %+  render-hex-bytes  32
      %-  keccak-256:keccak:crypto
      (as-octs:mimes:html function)
    ::
    ::  building requests
    ::
    ++  json-request
      =,  eyre
      |=  [url=purl jon=json]
      ^-  hiss
      :^  url  %post
        %-  ~(gas in *math)
        ~['Content-Type'^['application/json']~]
      (some (as-octt (en-json:html jon)))
    ::  +light-json-request: like json-request, but for %l
    ::
    ::    TODO: Exorcising +purl from our system is a much longer term effort;
    ::    get the current output types for now.
    ::
    ++  light-json-request
      |=  [url=purl:eyre jon=json]
      ^-  request:http
      ::
      :*  %'POST'
          (crip (en-purl:html url))
          ~[['content-type' 'application/json']]
          (some (as-octt (en-json:html jon)))
      ==
    ::
    ++  batch-read-request
      |=  req=(list proto-read-request)
      ^-  json
      a+(turn req read-request)
    ::
    ++  read-request
      |=  proto-read-request
      ^-  json
      %+  request-to-json  id
      :+  %eth-call
        ^-  call
        [~ to ~ ~ ~ `tape`(encode-call function arguments)]
      [%label %latest]
    ::
    ++  request-to-json
      =,  enjs:format
      |=  [riq=(unit @t) req=request]
      ^-  json
      %-  pairs
      =;  r=[met=@t pas=(list json)]
        ::TODO  should use request-to-json:rpc:jstd,
        ::      and probably (fall riq -.req)
        :*  jsonrpc+s+'2.0'
            method+s+met.r
            params+a+pas.r
            ::TODO  would just jamming the req noun for id be a bad idea?
            ?~  riq  ~
            [id+s+u.riq]~
        ==
      ?-  -.req
          %eth-block-number
        ['eth_blockNumber' ~]
      ::
          %eth-call
        :-  'eth_call'
        :~  (eth-call-to-json cal.req)
            (block-to-json deb.req)
        ==
      ::
          %eth-new-filter
        :-  'eth_newFilter'
        :_  ~
        :-  %o  %-  ~(gas by *(map @t json))
        =-  (murn - same)
        ^-  (list (unit (pair @t json)))
        :~  ?~  fro.req  ~
            `['fromBlock' (block-to-json u.fro.req)]
          ::
            ?~  tob.req  ~
            `['toBlock' (block-to-json u.tob.req)]
          ::
            ::TODO  fucking tmi
            ?:  =(0 (lent adr.req))  ~
            :+  ~  'address'
            ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
            :-  %a
            (turn adr.req (cork address-to-hex tape))
          ::
            ?~  top.req  ~
            :+  ~  'topics'
            (topics-to-json top.req)
        ==
      ::
          %eth-get-block-by-number
        :-  'eth_getBlockByNumber'
        :~  (tape (num-to-hex bon.req))
            b+txs.req
        ==
      ::
          %eth-get-filter-logs
        ['eth_getFilterLogs' (tape (num-to-hex fid.req)) ~]
      ::
          %eth-get-logs
        :-  'eth_getLogs'
        :_  ~
        :-  %o  %-  ~(gas by *(map @t json))
        =-  (murn - same)
        ^-  (list (unit (pair @t json)))
        :~  ?~  fro.req  ~
            `['fromBlock' (block-to-json u.fro.req)]
          ::
            ?~  tob.req  ~
            `['toBlock' (block-to-json u.tob.req)]
          ::
            ?:  =(0 (lent adr.req))  ~
            :+  ~  'address'
            ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
            :-  %a
            (turn adr.req (cork address-to-hex tape))
          ::
            ?~  top.req  ~
            :+  ~  'topics'
            (topics-to-json top.req)
        ==
      ::
          %eth-get-logs-by-hash
        :-  'eth_getLogs'
        :_  ~  :-  %o
        %-  ~(gas by *(map @t json))
        =-  (murn - same)
        ^-  (list (unit (pair @t json)))
        :~  `['blockHash' (tape (transaction-to-hex has.req))]
          ::
            ?:  =(0 (lent adr.req))  ~
            :+  ~  'address'
            ?:  =(1 (lent adr.req))  (tape (address-to-hex (snag 0 adr.req)))
            :-  %a
            (turn adr.req (cork address-to-hex tape))
          ::
            ?~  top.req  ~
            :+  ~  'topics'
            (topics-to-json top.req)
        ==
      ::
          %eth-get-filter-changes
        ['eth_getFilterChanges' (tape (num-to-hex fid.req)) ~]
      ::
          %eth-get-transaction-count
        :-  'eth_getTransactionCount'
        :~  (tape (address-to-hex adr.req))
            (block-to-json block.req)
        ==
      ::
          %eth-get-transaction-receipt
        ['eth_getTransactionReceipt' (tape (transaction-to-hex txh.req)) ~]
      ::
          %eth-send-raw-transaction
        ['eth_sendRawTransaction' (tape (num-to-hex dat.req)) ~]
      ==
    ::
    ++  eth-call-to-json
      =,  enjs:format
      |=  cal=call
      ^-  json
      :-  %o  %-  ~(gas by *(map @t json))
      =-  (murn - same)
      ^-  (list (unit (pair @t json)))
      :~  ?~  from.cal  ~
          `['from' (tape (address-to-hex u.from.cal))]
        ::
          `['to' (tape (address-to-hex to.cal))]
        ::
          ?~  gas.cal  ~
          `['gas' (tape (num-to-hex u.gas.cal))]
        ::
          ?~  gas-price.cal  ~
          `['gasPrice' (tape (num-to-hex u.gas-price.cal))]
        ::
          ?~  value.cal  ~
          `['value' (tape (num-to-hex u.value.cal))]
        ::
          ?~  data.cal  ~
          `['data' (tape data.cal)]
      ==
    ::
    ++  block-to-json
      |=  dob=block
      ^-  json
      ?-  -.dob
        %number   s+(crip '0' 'x' ((x-co:co 1) n.dob))
        %label    s+l.dob
      ==
    ::
    ++  topics-to-json
      |=  tos=(list ?(@ux (list @ux)))
      ^-  json
      :-  %a
      =/  ttj
        ;:  cork
          (cury render-hex-bytes 32)
          prefix-hex
          tape:enjs:format
        ==
      %+  turn  tos
      |=  t=?(@ (list @))
      ?@  t
        ?:  =(0 t)  ~
        (ttj `@`t)
      a+(turn t ttj)
    ::
    ::  parsing responses
    ::
    ::TODO  ++  parse-response  |=  json  ^-  response
    ::
    ++  parse-hex-result
      |=  j=json
      ^-  @
      ?>  ?=(%s -.j)
      (hex-to-num p.j)
    ::
    ++  parse-eth-new-filter-res  parse-hex-result
    ::
    ++  parse-eth-block-number  parse-hex-result
    ::
    ++  parse-transaction-hash  parse-hex-result
    ::
    ++  parse-eth-get-transaction-count  parse-hex-result
    ::
    ++  parse-event-logs
      (ar:dejs:format parse-event-log)
    ::
    ++  parse-event-log
      =,  dejs:format
      |=  log=json
      ^-  event-log
      =-  ((ot -) log)
      :~  =-  ['logIndex'^(cu - (mu so))]
          |=  li=(unit @t)
          ?~  li  ~
          =-  `((ou -) log)  ::TODO  not sure if elegant or hacky.
          :~  'logIndex'^(un (cu hex-to-num so))
              'transactionIndex'^(un (cu hex-to-num so))
              'transactionHash'^(un (cu hex-to-num so))
              'blockNumber'^(un (cu hex-to-num so))
              'blockHash'^(un (cu hex-to-num so))
              'removed'^(uf | bo)
          ==
        ::
          address+(cu hex-to-num so)
          data+so
        ::
          =-  topics+(cu - (ar so))
          |=  r=(list @t)
          ^-  (lest @ux)
          ?>  ?=([@t *] r)
          :-  (hex-to-num i.r)
          (turn t.r hex-to-num)
      ==
    --
  ::
  ::  utilities
  ::TODO  give them better homes!
  ::
  ++  num-to-hex
    |=  n=@
    ^-  tape
    %-  prefix-hex
    ?:  =(0 n)
      "0"
    %-  render-hex-bytes
    (as-octs:mimes:html n)
  ::
  ++  address-to-hex
    |=  a=address
    ^-  tape
    %-  prefix-hex
    (render-hex-bytes 20 `@`a)
  ::
  ++  transaction-to-hex
    |=  h=@
    ^-  tape
    %-  prefix-hex
    (render-hex-bytes 32 h)
  ::
  ++  prefix-hex
    |=  a=tape
    ^-  tape
    ['0' 'x' a]
  ::
  ++  render-hex-bytes
    ::  atom to string of hex bytes without 0x prefix and dots.
    |=  a=octs
    ^-  tape
    ((x-co:co (mul 2 p.a)) q.a)
  ::
  ++  pad-to-multiple
    |=  [wat=tape mof=@ud wer=?(%left %right)]
    ^-  tape
    =+  len=(lent wat)
    ?:  =(0 len)  (reap mof '0')
    =+  mad=(mod len mof)
    ?:  =(0 mad)  wat
    =+  tad=(reap (sub mof mad) '0')
    %-  weld
    ?:(?=(%left wer) [tad wat] [wat tad])
  ::
  ++  hex-to-num
    |=  a=@t
    (rash (rsh 3 2 a) hex)
  --
::
::  |jstd: json standard library
::
++  jstd
  =,  ^jstd
  |%
  ++  rpc
    =,  ^rpc
    |%
    ++  request-to-hiss
      |=  [url=purl:eyre req=request]
      ^-  hiss:eyre
      :-  url
      :+  %post
        %-  ~(gas in *math:eyre)
        ~['Content-Type'^['application/json']~]
      %-  some
      %-  as-octt:mimes:html
      (en-json:html (request-to-json req))
    ::
    ++  request-to-json
      |=  request
      ^-  json
      %-  pairs:enjs:format
      :~  jsonrpc+s+'0.2'
          id+s+id
          method+s+method
        ::
          :-  %params
          ^-  json
          ?-  -.params
            %list     [%a +.params]
            %object   [%o (~(gas by *(map @t json)) +.params)]
          ==
      ==
    --
  --
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
    |-  ^-  seed:able:jael
    =/  cub=acru:ames  (pit:nu:crub:crypto 512 eny)
    =/  who=ship  `@`fig:ex:cub
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
        =/  who  `@p`(slav %ud (rsh 3 4 id))
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
        :-  (slav %ud (rsh 3 5 id))
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
    |=  [=seed:able:jael =point:azimuth =live]
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
--  ::
