::                                                      ::::
::::                    ++clay                            ::  (1c) versioning
  ::                                                    ::::
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
  ::  CHANGED
  ++  care  ?($d $p %s $t $u $v $w $x $y $z)               ::  clay submode
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
