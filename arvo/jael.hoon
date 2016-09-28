!:                                                      ::  /van/jael
::                                                      ::  %reference
!?  150
::                                                      ::::
::::                        #  0                        ::  public structures
  ::                                                    ::::
|=  pit/vase
::                                                      ::::
::::                        #  1                        ::  private structures
  ::                                                    ::::
=>  |%
++  jael-state                                          ::  all crypto state
  $:  ver/$0                                            ::  %jael version 
      yen/(set duct)                                    ::  raw observers
      urb/jael-objective                                ::  all objective state
      sub/jael-subjective                               ::  all subjective state
  ==                                                    ::
++  jael-subjective                                     ::  urbit metadata
  $:  cod/farm                                          ::  our + dependencies
      $=  car                                           ::  secure channels
        %+  map  ship                                   ::  partner
        $:  yen/(set duct)                              ::  trackers
            dut/(unit pipe)                             ::  channel state
        ==                                              ::
      $=  neb                                           ::  neighborhood
        $:  dad/ship                                    ::  parent 
            pyr/(set ship)                              ::  peers
            kyz/(set ship)                              ::  children
        ==                                              ::
      $=  bal                                           ::  balance sheet
        $:  yen/(set duct)                              ::  trackers
        ==                                              ::  
      $=  own                                           ::  vault
        $:  yen/(set duct)                              ::  trackers
            lyf/life                                    ::  version
            jaw/(map life ring)                         ::  private keys
        ==                                              ::
  ==                                                    ::  
++  jael-objective                                      ::  objective urbit
  $:  pug/farm                                          ::  keys
      pry/(map ship (map ship jael-purse))              ::  promises
  ==                                                    ::
++  jael-remote                                         ::  remote notification
  %+  each  jael-purse                                  ::  addition
  jael-purse                                            ::  replacement
::                                                      ::
++  jael-balance                                        ::  balance sheet
  %+  pair                                              ::  
    (map ship jael-purse)                               ::  liabilities
  (map ship jael-purse)                                 ::  assets
::                                                      ::
++  jael-tally                                          ::  balance update
  %+  each  jael-balance                                ::  complete
  jael-action                                           ::  change
::                                                      ::
++  jael-action                                         ::  balance change
  %+  pair  ship                                        ::  partner
  %+  each  jael-delta                                  ::  %&/liability change
  jael-delta                                            ::  %|/asset change
::                                                      ::
++  jael-gift                                           ::  report
  $?  {$veil p/(unit pipe)}                             ::  secure channel
      {$vest p/jael-tally}                              ::  balance update
      {$vein p/life q/(map life ring)}                  ::  private keys
      {$vine p/(list jael-edit)}                        ::  raw actions
  ==                                                    ::
++  jael-right                                          ::  urbit commitment
  $%  {$apple p/(map site @)}                           ::  web api key
      {$block $~}                                       ::  banned
      {$email p/(set @ta)}                              ::  email addresses
      {$final p/(map ship @uvG)}                        ::  ticketed ships
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$guest $~}                                       ::  refugee visa
      {$hotel p/pile}                                   ::  reserved block
      {$jewel p/(map life ring)}                        ::  private keyring
      {$pword p/(map site (map @ta @t))}                ::  web passwd by user
      {$token p/(map site (map @ta @))}                 ::  app tokens by user
      {$urban p/(map hand (pair @da code))}             ::  urbit symmetric keys
  ==                                                    ::
++  jael-purse                                          ::  rights set
  (tree jael-right)                                     ::
::                                                      ::
++  jael-delta                                          ::  rights change
  $:  mor/jael-purse                                    ::  grow rights
      les/jael-purse                                    ::  lose rights
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$burn p/ship q/jael-purse}                       ::  destroy rights
      {$hail p/ship q/jael-remote}                      ::  remote update
      {$init p/@e}                                      ::  initialize
      {$meet p/(unit (unit ship)) q/farm}               ::  integrate pki from
      {$mint p/ship q/jael-purse}                       ::  create rights
      {$move p/ship q/ship r/jael-purse}                ::  transfer from/to
      {$nuke $~}                                        ::  cancel tracker
      {$open p/ship}                                    ::  make secret channel
      {$step p/bull q/ring}                             ::  update private key
      {$veil p/ship}                                    ::  view secret channel
      {$vein $~}                                        ::  view signing keys
      {$vest $~}                                        ::  view public balance
      {$vine $~}                                        ::  view secret history
      {$west p/ship q/path r/*}                         ::  remote request
  ==                                                    ::
++  jael-report-them                                    ::  report on neighbor
  $:  gur/wyll                                          ::  certificate
      lab/jael-purse                                    ::  our promises to
      own/jael-purse                                    ::  our promises from
  ==                                                    ::
++  jael-report-self                                    ::  report on self
  $:  gur/wyll                                          ::  certificate
      war/(map life ring)                               ::  private keys
  ==                                                    ::
++  jael-report-cash                                    ::  neighbors/assets
  $:  has/(map ship jael-purse)                         ::
  ==                                                    ::
++  jael-report-paid                                    ::  asset diff
  $:  dif/(list (trel ship ? jael-purse))               ::  who, +/-, what
  ==                                                    ::
++  jael-note                                           ::  out request $->
  $%  {$x $mess p/ship q/path r/*}                      ::  send message
  ==                                                    ::
++  jael-message                                        ::  p2p message
  $%  {$hail p/jael-purse}                              ::  reset rights
      {$meet p/farm}                                    ::  propagate pki
  ==                                                    ::
++  jael-grow                                           ::  unit of learning
  $%  {$sign p/mind q/@}                                ::  add/update signature
      {$step p/cert}                                    ::  add whole deed
  ==                                                    ::
++  jael-edit                                           ::  urbit change
  $%  $:  $fact                                         ::  certificate change
          rex/ship                                      ::  owner
          vie/(unit (unit ship))                        ::  made/heard from
          lyf/life                                      ::  deed added/modified
          gan/jael-grow                                 ::  info gained
      ==                                                ::
      $:  $rite                                         ::  rights change
          rex/ship                                      ::  issuer
          pal/ship                                      ::  issued to
          del/jael-delta                                ::  change
      ==                                                ::
  ==                                                    ::
++  jael-card                                           ::  i/o action
  (wind jael-note jael-gift)                            ::
::                                                      ::
++  jael-move                                           ::  output
  {p/duct q/jael-card}
--
::                                                      ::::
::::                        #  2                        ::  static data
  ::                                                    ::::
=>  |%
::                                                      ::  zeno
++  zeno                                                ::  boot fingerprints
  |=  ::  who: galaxy (0-255)
      ::
      who/ship
  ^-  pass
  !!
--
::                                                      ::::
::::                        #  3                        ::  stateless functions
  ::                                                    ::::
=>  |%
::                                                      ::  ++up
++  up                                                  ::  rights algebra
  |_  pig/jael-purse
      ::
      ::    +up: rights algebra
      ::
      ::  we store the various kinds of ++jael-right in
      ::  a binary tree, sorted by ++gor on the tag,
      ::  balanced by ++vor on the tag.  this tree, a
      ::  ++jael-purse, is also a valid ++map.  but
      ::  unlike a ++map, it has heterogeneous type.
      ::
      ::  this design is pretty generalized and should
      ::  probably be promoted deeper in the stack.  its
      ::  goal is to make it extremely easy to add new
      ::  forms of ++jael-right, without invalidating
      ::  existing purse nouns.
      ::
      ::  rights operations always crash if impossible;
      ::  the algebra has no concept of negative rights.
      ::
  ::                                                    ::  ++differ:up
  ++  differ                                            ::  delta pig->gob
    |=  gob/jael-purse
    ^-  jael-delta
    !!
  ::                                                    ::  ++exists:up
  ++  exists                                            ::  test presence
    |=  tag/@tas
    !=(~ (expose tag))
  ::                                                    ::  ++expose:up
  ++  expose                                            ::  typed extract
    |=  tag/@tas
    ::
    ::  if hoon had an equality test that informed
    ::  inference, this could be a |*, and its
    ::  product would be properly inferred.
    ::
    ^-  (unit jael-right)
    ?~  pig  ~
    ?:  =(tag -.n.pig)
      [~ u=n.pig]
    ?:((gor tag -.n.pig) $(pig l.pig) $(pig r.pig))
  ::                                                    ::  ++insert:up
  ++  insert                                            ::  pig plus gob
    |=  gob/jael-purse
    ^-  jael-purse
    !!
  ::                                                    ::  ++redact:up
  ++  redact                                            ::  conceal secrets
    |-  ^-  jael-purse
    ?~  pig  ~
    :_  [$(pig l.pig) $(pig r.pig)]
    =*  rys  n.pig
    ^-  jael-right
    ?+    -.rys  rys
        $apple
      [%apple (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $final
      [%final (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $pword
      :-  %pword 
      %-  ~(run by p.rys) 
      |=  (map @ta @t) 
      (~(run by +<) |=(@t (crip (runt [(met 3 +<) '*'] ~))))
    ::
        $jewel
      [%jewel (~(run by p.rys) |=(@ (mug +<)))]
    ::
        $token
      :-  %token 
      %-  ~(run by p.rys) 
      |=((map @ta @) (~(run by +<) |=(@ (mug +<))))
    ::
        $urban
      [%urban (~(run by p.rys) |=({@da code} [+<- (mug +<+)]))]
    ==
  ::                                                    ::  ++remove:up
  ++  remove                                            ::  pig minus gob
    |=  gob/jael-purse
    ^-  jael-purse
    !!   
  ::                                                    ::  ++update:up
  ++  update                                            ::  arbitrary change
    |=  del/jael-delta
    ^-  jael-purse
    (remove(pig (insert mor.del)) les.del)
  --   
::                                                      ::  ++we
++  we                                                  ::  wyll tool
  |_  pub/wyll
  ::                                                    ::  ++collate:we 
  ++  collate                                           ::  sort by version
    |=  com/$-({{life cert} {life cert}} ?)
    ^-  (list (pair life cert))
    (sort (~(tap by pub)) com)
  ::                                                    ::  ++current:we
  ++  current                                           ::  current number
    ^-  (unit life)
    (bind instant |=((pair life cert) p))
  ::                                                    ::  ++forward:we
  ++  forward                                           ::  sort oldest first
    (collate |=({a/{life *} b/{life *}} (lth -.a -.b)))
  ::                                                    ::  ++instant:we
  ++  instant                                           ::  current cert
    ^-  (unit (pair life cert))
    =+  reverse
    ?~(- ~ `i)
  ::                                                    ::  ++reverse:we
  ++  reverse                                           ::  sort latest first
    (collate |=({a/{life *} b/{life *}} (gth -.a -.b)))
  --
--
::                                                      ::::
::::                        #  4                        ::  engines
  ::                                                    ::::
=>  |%
::                          ## 4.a                      ::  of
++  of                                                  ::  main engine
  =|  moz/(list jael-move)                              ::::
  =|  $:  ::  sys: system context
          ::
          $=  sys
          $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now/@da
              eny/@e
          ==
          ::  all vane state
          ::
          jael-state
      ==
  ::  lex: all durable state
  ::  moz: pending actions
  ::
  =*  lex  ->
  |%
  ::                                                    ::  abet:of
  ++  abet                                              ::  resolve
    [(flop moz) lex]
  ::                                                    ::  call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen/duct
            tac/jael-task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  destroy promises
    ::    {$burn p/ship q/jael-purse)}
    ::
        $burn
      (cure abet:(~(deal ur urb) our p.tac [~ q.tac]))
    ::
    ::  remote update
    ::    {$hail p/ship q/jael-remote}
    ::
        $hail
      !!
    ::
    ::  initialize vane
    ::    {$init p/@e}
    ::
        $init
      !!
    ::
    ::  create promises
    ::    {$mint p/ship q/jael-purse}
    ::
        $mint
      (cure abet:(~(deal ur urb) our p.tac [q.tac ~]))
    ::
    ::  move promises
    ::    {$move p/ship q/ship r/jael-purse}
    ::
        $move
      =.  +>  (cure abet:(~(deal ur urb) our p.tac [~ r.tac]))
      =.  +>  (cure abet:(~(deal ur urb) our q.tac [r.tac ~]))
      +>
    ::
    ::  public-key update
    ::    {$meet p/ship q/farm}
    ::
        $meet
      (cure abet:(~(meet ur urb) p.tac q.tac))
    ::
    ::  cancel all trackers from duct
    ::    {$nuke $~}
    ::
        $nuke
      !!
    ::
    ::  open secure channel
    ::    {$open p/ship}
    ::
        $open
      !!
    ::
    ::  extend our certificate with a new private key
    ::    {$step p/bull}
    ::
        $step 
      !!
    ::
    ::  open secure channel
    ::    {$veil p/ship}
    ::
        $veil
      (curd abet:(~(veil ~(feed su urb sub) hen) p.tac eny.sys))
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      (curd abet:~(vein ~(feed su urb sub) hen))
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      (curd abet:~(vest ~(feed su urb sub) hen))
    ::
    ::  monitor all 
    ::    {$vine $~}
    ::
        $vine
      +>(yen (~(put in yen) hen))
    ::
    ::  execute remote request
    ::    {$west p/ship q/path r/*}
    ::
        $west
      ?>  =(~ q.tac)
      =+  mes=((hard jael-message) r.tac)
      ?-    -.mes
      ::
      ::  reset remote rights
      ::    {$hail p/jael-purse}
      ::
          $hail
        !!
      ::
      ::  share certificates
      ::    {$meet p/farm}
      ::
          $meet
        !!
      ==
    ==
  ::                                                    ::  curd:of
  ++  curd                                              ::  subjective moves
    |=  {moz/(list jael-move) sub/jael-subjective}
    +>(sub sub, moz (weld (flop moz) ^moz))
  ::                                                    ::  cure:of
  ++  cure                                              ::  objective edits
    |=  {hab/(list jael-edit) urb/jael-objective}
    ^+  +>
    (curd(urb urb) abet:(~(apex su urb sub) hab))
  --
::                          ## 4.b                      ::  su
++  su                                                  ::  subjective engine
  =|  moz/(list jael-move)                              ::::
  =|  $:  jael-objective
          jael-subjective
      ==
  ::  moz: moves in reverse order
  ::  urb: objective urbit state
  ::  sub: subjective urbit state
  ::
  =*  urb  -<
  =*  sub  ->
  |%
  ::                                                    ::  abet:su
  ++  abet                                              ::  resolve
    [(flop moz) sub]
  ::                                                    ::  apex:su
  ++  apex                                              ::  apply changes
    |=  hab/(list jael-edit)
    ^+  +>
    ?~  hab  +>
    %=    $
        hab  t.hab
        +>
      ?-  -.i.hab
        $rite  (paid +.i.hab)
        $fact  (said +.i.hab)
      ==
    ==
  ::                                                    ::  ++exec:su
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/jael-card}
    =/  noy  (~(tap in yen))
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moz [[i.noy cad] moz])
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::                                                  ::  ++veil:feed:su
    ++  veil                                            ::  secure channel
      |=  {who/ship eny/@e}
      ^+  ..feed
      =/  ruc  (~(get by car) who)
      =/  rec  ?~  ruc 
                 :-  `yen/(set duct)`[hen ~ ~] 
                 `dut/(unit pipe)`~
               u.ruc(yen (~(put in yen.u.ruc) hen))
      %_  ..feed
        moz  [[hen %give %veil dut.rec] moz]
        car  (~(put by car) who rec)
      ==
    ::                                                  ::  ++vein:feed:su
    ++  vein                                            ::  private keys
      %_  ..feed
        moz      [[hen %give %vein [lyf jaw]:own] moz]
        yen.own  (~(put in yen.own) hen)
      ==
    ::                                                  ::  ++vest:feed:su
    ++  vest                                            ::  balance
      %_  ..feed
        moz      [[hen %give %vest %& vest:form] moz]
        yen.bal  (~(put in yen.bal) hen)
      ==
    --
  ::                                                    ::  ++feel:su
  ++  feel                                              ::  update tracker
    |% 
    ::                                                  ::  ++veal:feel:su
    ++  veal                                            ::  kick subfarm
      ^+  ..feel
      =/  cod  veal:form
      ?:(=(^cod cod) ..feel ..feel(cod cod))
    ::                                                  ::  ++veil:feel:su
    ++  veil                                            ::  kick secure channel
      |=  who/ship
      ^+  ..feel
      =/  ruc  (~(get by car) who)
      ?~  ruc  ..feel
      =/  det  (veil:form who)
      ?:  =(`det dut.u.ruc)  ..feel 
      =.  car  (~(put by car) who [yen.u.ruc `det])
      (exec yen.u.ruc [%give %veil `det])
    ::                                                  ::  ++vein:feel:su
    ++  vein                                            ::  kick private keys
      ^+  ..feel
      =/  yam  vein:form
      ?:  =(yam +.own)  ..feel
      (exec(+.own yam) yen.own [%give %vein +.own])
    ::                                                  ::  ++vest:feel:su
    ++  vest                                            ::  kick balance
      |=  hug/jael-action
      ^+  ..feel
      ?:  =([~ ~] +.q.hug)  ..feel
      ::
      ::  notify all local listeners
      ::
      =.  ..feel  (exec yen.bal [%give %vest %| p.hug q.hug])
      ::
      ::  pig: purse report for partner
      ::
      ?.  ?=($| -.q.hug)  ..feel
      =*  pig  (~(lawn ur urb) our p.hug)
      %_    ..feel
          moz  :_  moz
        [*duct %pass /vest/(scot %p p.hug) %x %mess p.hug /j %hail pig]
      ==
    --
  ::                                                    ::  ++fire:su
  ++  fire                                              ::  propagate keys
    |_  hec/farm
    ++  home                                            ::  ++home:su
      |=  who/ship                                      ::  to ship
      %_    ..fire
          moz
        :_  moz
        [*duct %pass /meet/(scot %p who) %x %mess who /j [%meet hec]]
      == 
    ::                                                  ::  ++flow:su
    ++  flow                                            ::  to set of ships
      |=  tar/(set ship)
      =+  rot=(~(tap in (~(del in tar) our)))
      |-  ^+  ..fire
      ?~  rot  ..fire
      $(rot t.rot, ..fire (home i.rot))
    ::                                                  ::  ++spam:su
    ++  spam                                            ::  to list of sets
      |=  {via/(unit ship) jax/(list (set ship))}
      ^+  ..fire
      =-  (flow ?~(via - (~(del in -) u.via)))
      |-  ^-  (set ship)      
      ?~(jax ~ (~(uni in i.jax) $(jax t.jax)))
    --
  ::                                                    ::  ++form:su
  ++  form                                              ::  generate reports
    |%
    ::                                                  ::  ++veal:form:su
    ++  veal                                            ::  public dependencies
      =|  mor/(set ship)
      ^-  farm
      !!
    ::                                                  ::  ++veil:form:su
    ++  veil                                            ::  channel report
      |=  who/ship
      ^-  pipe
      ::
      ::  pub: will of who
      ::  exp: promises from our to who
      ::  imp: promises from who to our
      ::  out: symmetric key from our to who
      ::  inn: symmetric keys from who to our
      ::
      =/  pub  
        ^-  wyll
        =-  ?~(- ~ u.-)
        (~(get by pug.urb) who)
      ::
      =/  exp  
        ^-  jael-purse
        =-  ?~(- ~ u.-)
        (~(get by (~(got by pry.urb) our)) who)
      ::
      =/  imp  
        ^-  jael-purse
        =-  ?~(- ~ u.-)
        %.  our
        ~(get by (fall (~(get by pry.urb) who) *(map ship jael-purse)))
      ::
      =*  out
        ^-  (unit (pair hand bill))
        =+  (~(expose up exp) %urban)
        ?~  -  ~ 
        ?>  ?=($urban -.u.-)
        =*  pam  p.u.-
        ?~  pam  ~  
        ::  arbitrarily select root node of the map
        ::
        `n.pam
      ::
      =*  inn
          =+  (~(expose up imp) %urban)
          ^-  (map hand bill)
          ?~  -  ~
          ?>  ?=($urban -.u.-)
          p.u.-
      ::
      ^-  pipe
      [out inn ~(current we pub) (~(dads ur urb) who) pub]
    ::                                                  ::  ++vein:form:su
    ++  vein                                            ::  private key report
      ^-  (pair life (map life ring))
      (~(lean ur urb) our)
    ::                                                  ::  ++vest:form:su
    ++  vest                                            ::  balance report
      ^-  jael-balance
      :-  ::
          ::  raw: all our liabilities by ship
          ::  dud: delete liabilities to self
          ::  cul: mask secrets
          ::
          =*  raw  =-(?~(- ~ u.-) (~(get by pry.urb) our))
          =*  dud  (~(del by raw) our)
          =*  cul  (~(run by dud) |=(jael-purse ~(redact up +<)))
          cul
      ::
      ::  fub: all assets by ship
      ::  veg: all nontrivial assets, secrets masked
      ::
      =/  fub  
        ^-  (list (pair ship (unit jael-purse)))
        %+  turn
          (~(tap by pry.urb))
        |=  (pair ship (map ship jael-purse))
        [p (~(get by q) our)]
      =*  veg
        |-  ^-  (list (pair ship jael-purse))
        ?~  fub  ~
        =+  $(fub t.fub)
        ?~(q.i.fub - [[p.i.fub ~(redact up u.q.i.fub)] -])
      ::
      (~(gas by *(map ship jael-purse)) veg)
    --
  ::
  ++  open
    |=  $:  eny/@e
            hen/duct
            who/ship
        ==
    ^+  +>
    !!
      ::
      ::  if first meeting, add to child/peer sets.  children
      ::  have us as a parent; peers have the same rank as us
      ::
::
::      ?.  =(1 lyf.led)  +>.$
::      ?:  =(our dad)
::        +>.$(kyz.neb (~(put in kyz.neb) rex.led))
::      ?.  =((clan rex.led) (clan our))
::        +>.$
::      +>.$(pyr.neb (~(put in pyr.neb) rex.led))
::
::      =.  moz
::        ?.  &(=(1 lyf.led) (~(has in kyz.neb) rex.led))
::          moz
::        :_  moz
::        =*  rug  ^-  farm
::            %-  ~(gas by *farm)
::            %+  skim  (~(tap by pug.urb))
::            |=({who/ship *} (lth who 65.536))
::        [%pass %x ~ %mess rex.led /x %meet rug]
  ++  paid                                              ::  track asset change
    |=  $:  ::  rex: promise from
            ::  pal: promise to
            ::  del: change to existing
            ::  bur: changes to symmetric keys
            ::
            rex/ship
            pal/ship
            del/jael-delta
        ==
    ^+  +>
    =*  bur  ?|  (~(exists up mor.del) %urban) 
                 (~(exists up les.del) %urban)
             ==
    ::  ignore empty delta; keep secrets out of metadata
    ::
    ?:  =([~ ~] del)  +>
    =.  del  [~(redact up mor.del) ~(redact up les.del)]
    ?.  =(our pal)
      ::
      ::  track promises we made to others
      ::
      ?.  =(our rex)  +>
      ::
      ::  track liabilities
      ::
      =.  +>  (vest:feel pal %& del) 
      ::
      ::  track secure channels
      ::
      ?.  bur  +>
      (veil:feel pal)
    ::
    ::  track private keys
    ::
    =.  +>  ?.  (~(exists up mor.del) %jewel)  
        +>
      vein:feel
    ::
    ::  track changes in secure channels
    ::
    ?.  bur  +>
    (veil:feel rex)
  ::                                                    ::  ++said:su
  ++  said                                              ::  track certificates
    |=  $:  ::  rex: ship whose will has changed
            ::  vie: change authorized by
            ::  lyf: modified/created version 
            ::  gan: modification
            ::
            rex/ship
            vie/(unit (unit ship))
            lyf/life
            gan/jael-grow
        ==
    ::  lip: this change as its own farm
    ::
    =/  lip  ^-  farm 
      =-  [[rex -] ~ ~]
      ^-  wyll
      =-  [[lyf -] ~ ~]
      ^-  cert
      ?-    -.gan
      ::
      ::  add a new certificate to this will
      ::    {$step p/cert}
      ::
          $step  p.gan
      ::
      ::  add a new signature to this certificate
      ::    {$sign p/mind q/@}
      ::
          $sign
        :-  dat:(~(got by (~(got by pug.urb) rex)) lyf)
        =-  [- ~ ~]
        [who.p.gan lyf.p.gan q.gan]
      ==
    ::
    ::  if our subfarm may have changed, reset it.
    ::
    =.  +>.$  ?.  |(=(our rex) (~(has by cod) rex))  +>.$
        veal:feel
    ::
    ::  kick secure channels
    ::
    =.  +>.$  (veil:feel rex)
    ::
    ::  if we signed a will for someone else, send it home
    ::
    ?:  &(=([~ ~] vie) !=(our rex))
      (~(home fire lip) rex)
    ::
    ::  propagate new data as appropriate
    ::
    %+  ~(spam fire lip)  
      ?~(vie ~ ?~(u.vie ~ `u.u.vie))
    ^-  (list (set ship))
    ::
    ::  if our will has changed, send to parents and kids;
    ::  if a new deed has been added, also to pals
    ::
    ?:  =(our rex)
      :*  [dad.neb ~ ~]
          kyz.neb
          ?.(=(%step -.gan) ~ [pyr.neb ~])
      ==
    :: 
    ::  forward star and galaxy updates to parents and kids
    ::
    ?.  (lth rex 65.536)
      ~
    :*  [dad.neb ~ ~]
        kyz.neb
        ~
    ==
  --
::                          ## 4.c                      ::  ++ur
++  ur                                                  ::  urbit engine
  =|  hab/(list jael-edit)                              ::::
  =|  jael-objective 
  ::
  ::  hab: side effects, reversed
  ::  urb: all urbit state
  ::
  =*  urb  -
  |%
  ::                                                    ::  ++abet:ur
  ++  abet                                              ::  resolve
    [(flop hab) `jael-objective`urb]
  ::
  ++  dads                                              ::  ++dads:ur
    |=  who/ship                                        ::  lineage
    ^-  (list ship)
    =/  ryg  (~(get by pug) who)
    ?~  ryg  (saxo who)
    =/  dad  dad.doc.dat.q:(need ~(instant we u.ryg))
    [who ?:(=(who dad) ~ $(who dad))]
  ::                                                    ::  ++deal:ur
  ++  deal                                              ::  change rights
    |=  {rex/ship pal/ship del/jael-delta}
    ^+  +>
    =.  hab  [[%rite rex pal del] hab]
    abet:(deal:~(able ex rex) pal del)
  ::
  ++  lawn                                              ::  ++lawn:ur
    |=  {rex/ship pal/ship}                             ::  debts, rex to pal
    ^-  jael-purse
    (lawn:~(able ex rex) pal)
  ::                                                    ::  ++leak:ur
  ++  leak                                              ::  private key
    |=  rex/ship
    ^-  (pair life ring)
    =/  lyn  lean:~(able ex rex)
    [p.lyn (~(got by q.lyn) p.lyn)]
  ::                                                    ::  ++lean:ur
  ++  lean                                              ::  private keys
    |=  rex/ship
    ^-  (pair life (map life ring))
    lean:~(able ex rex)
  ::                                                    ::  ++meet:ur
  ++  meet                                              ::  calculate merge
    |=  $:  ::  vie: authenticated source
            ::  cod: transmitted certificates
            ::
            vie/(unit (unit ship))
            cod/farm
        ==
    ^+  +>
    =+  lec=(~(tap by cod))
    |-  ^+  ..meet
    ?~  lec  ..meet
    %=  $
      lec     t.lec
      ..meet  abet:(grow:~(able ex p.i.lec) vie cod q.i.lec)
    ==
  ::                                                    ::  ++ex:ur
  ++  ex                                                ::  server engine
    ::  shy: private state
    ::  rug: domestic will
    ::
    =|  $:  shy/(map ship jael-purse)
            rug/wyll
        ==
    =|  ::  rex: server ship
        ::
        rex/ship
    |%
    ::                                                  ::  ++abet:ex:ur
    ++  abet                                            ::  resolve
      %_  ..ex
        pry  (~(put by pry) rex shy)
        pug  (~(put by pug) rex rug)
      ==
    ::                                                  ::  ++able:ex:ur
    ++  able                                            ::  initialize
      %_  .
        shy  (fall (~(get by pry) rex) *(map ship jael-purse))
        rug  (fall (~(get by pug) rex) *wyll)
      ==
    ::                                                  ::  ++deal:ex:ur
    ++  deal                                            ::  alter rights
      |=  {pal/ship del/jael-delta}
      ^+  +>
      =/  gob  (fall (~(get by shy) pal) *jael-purse)
      =*  hep  (~(update up gob) del)
      +>.$(shy (~(put by shy) pal hep))
    ::                                                  ::  ++lean:ex:ur
    ++  lean                                            ::  private keys
      ^-  (pair life (map life ring))
      ::
      ::  lyf: latest life of 
      ::  lab: promises by rex
      ::  par: promises by rex, to rex
      ::  jel: %jewel rights
      ::
      =/  lyf  `life`(need ~(current we (~(got by pug) rex)))
      =*  lab  (~(got by pry) rex)
      =*  par  (~(got by lab) rex)
      =/  jel  `jael-right`(need (~(expose up par) %jewel))
      ?>  ?=($jewel -.jel)
      [lyf p.jel]
    ::                                                  ::  ++lawn:ex:ur
    ++  lawn                                            ::  liabilities to pal
      |=  pal/ship
      ^-  jael-purse
      =-(?~(- ~ u.-) (~(get by shy) pal))
    ::                                                  ::  grow:ex:ur
    ++  grow                                            ::  merge wills
      |=  $:  ::  via: data source
              ::  cod: merge context
              ::  gur: input will
              ::  
              vie/(unit (unit ship))
              cod/farm
              gur/wyll
          ==
      ?:  |(=(~ gur) =(gur rug))  ..grow
      |^  ^+  ..grow
          ::
          ::  wap: ascending list of new certs
          ::  nem: previous life
          ::  pre: previous deed
          ::
          =/  wap
            ^-  (list (pair life cert))
            %+  sort  (~(tap by gur))
            |=  {a/{life *} b/{life *}}
            (lth -.a -.b)
          =/  nem  
            ^-  (unit life)  
            ?~(wap ~ ?:(=(1 p.i.wap) ~ `(dec p.i.wap)))
          =/  pre
            ^-  (unit deyd)
            (bind nem |=(life dat:(~(got by rug) +<))) 
          ::
          ::  merge each life
          ::
          |-  ^+  ..grow
          ?~  wap  ..grow
          ?>  |(?=($~ nem) =(p.i.wap +(u.nem)))
          ::
          ::  lub: merged deed and changes
          ::
          =+  lub=(grow-mate p.i.wap q.i.wap pre)
          %=  $
            wap  t.wap
            nem  `p.i.wap
            pre  `dat.q.lub
            rug  (~(put by rug) p.i.wap q.lub)
            hab  (weld (flop p.lub) hab)
          ==
      ::                                                ::  grow-lick/ex:ur
      ++  grow-lick                                     ::  check signature
        |=  {pub/pass ash/@ val/@}
        ^-  ?
        =+  ver=(sure:as:(com:nu:crub pub) *code val)
        ?~  ver  |
        =(ash u.ver)
      ::                                                ::  grow-like/ex:ur
      ++  grow-like                                     ::  verify signature
        |=  {myn/mind ash/@ val/@}
        ^-  ?
        =:  ..able  able(rex who.myn)
            gur     (fall (~(get by cod) who.myn) *wyll)
          ==
        (grow-lick (grow-look lyf.myn) ash val)
      ::                                                ::  grow-look/ex:ur
      ++  grow-look                                     ::  load public key
        |=  lyf/life 
        ^-  @
        ::
        ::  cascade search over old and new, new first
        ::
        |^  %-  (bond |.((need grow-look-find))) 
            grow-look-find(rug gur)
        ::                                              ::  grow-look-find:ex:ur
        ++  grow-look-find                              ::  
          ^-  (unit @)
          ::
          ::  crash if this life is revoked
          ::
          ?<  (~(has by rug) +(lyf))
          %+  biff  (~(get by rug) lyf)
          |=(cert `pub.dat)
        --
      ::                                                ::  grow-mate/ex:ur
      ++  grow-mate                                     ::  merge lives
        |=  $:  ::  num: life we're merging
                ::  new: new deed
                ::  pre: previous deed 
                ::  eld: old deed
                ::
                num/@ud
                new/cert
                pre/(unit deyd)
            ==
        =+  :*  eld=`(unit cert)`(~(get by rug) num) 
            ==
        ^-  (pair (list jael-edit) cert)
        ::
        ::  enforce artificial scarcity in lives
        ::
        ?>  (lte num 9)
        ::
        ::  if no new information, do nothing
        ::
        ?:  |(=(eld `new))
          ?>  ?=(^ eld) 
          [~ u.eld]
        ::
        ::  ash: hash of deed content
        ::  def: our default parent
        ::  dad: our declared parent
        ::  mir: our rank
        ::
        =/  ash  (sham %urbit rex num dat.new)
        =/  def  (sein rex)
        =*  dad  dad.doc.dat.new
        =/  mir  (clan rex)
        ?>  ?:  |(=(num 1) =(%earl mir) =(%pawn mir))
              ::
              ::  first parent must be default;
              ::  comets and moons may not migrate
              ::
              =(def dad)
            ::
            ::  all others may migrate to parent of same rank
            ::
            =((clan def) (clan dad))
        ::
        ::  if we have an old deed at this life, merge new signatures
        ::
        ?:  ?=(^ eld)
          ::
          ::  deed data must be identical
          ::
          ?>  =(dat.new dat.u.eld)
          ::
          ::  sow: all new signatures
          ::
          =+  sow=`(list (trel ship life @))`(~(tap by syg.new))
          |-  ^-  (pair (list jael-edit) cert)
          ?~  sow  [~ u.eld]
          ::
          ::  mor: all further edits
          ::  och: old signature for this signer
          ::
          =+  mor=$(sow t.sow)
          =+  och=(~(get by syg.q.mor) p.i.sow)
          ::
          ::  ignore obsolete/equivalent signature
          ::
          ?.  |(?=($~ och) (gth q.i.sow p.u.och))
            mor
          ::
          ::  verify and merge added signature
          ::
          ?>  (grow-like [p q]:i.sow ash r.i.sow)
          :_  q.mor(syg (~(put by syg.q.mor) p.i.sow [q r]:i.sow))
          :_  p.mor
          `jael-edit`[%fact rex vie num `jael-grow`[%sign [[p q] r]:i.sow]]
        ::
        ::  non-initial deeds must be signed by previous
        ::
        ?>  ?|  ?=($~ pre)
                =+  laz=(~(got by syg.new) rex)
                ?>  =(p.laz (dec num))
                (grow-lick pub.u.pre ash q.laz)
            ==
        ::
        ::  check the parent has signed, if necessary
        ::
        ?>  ?|  ::
                ::  no parent signature for existing, non-moon urbits
                ::
                ?&  ?=(^ pre)
                    =(dad.doc.u.pre dad)
                    !=(%earl mir)
                ==
                ::
                ::  initial fingerprint for galaxy is hardcoded
                ::
                ?&  =(%czar mir)
                    ?=($~ pre)
                    =((shaf %zeno pub.dat.new) (zeno rex))
                ==
                ::
                ::  the deed is homemade or sent by owner
                ::
                &(?=(^ vie) |(?=($~ u.vie) =(u.u.vie rex)))
                ::
                ::  check valid parent signature
                ::
                =+  par=(~(got by syg.new) dad)
                (grow-like [dad p.par] ash q.par)
            ==
        =-  [[%fact rex p.- num %step q.-]~ q.-]
        ^-  (pair (unit (unit ship)) cert)
        ::
        ::  the new deed is complete; report it
        ::
        ?:  (~(has by syg.new) dad)
          [vie new]
        ::
        ::  the new deed needs a parent signature; try to add it
        ::
        :-  [~ ~]
        ::
        ::  pev: life and ring of parent
        ::  val: new signature
        ::
        =/  pev  (leak dad)
        =*  val  (sign:as:(nol:nu:crub q.pev) *@ ash)
        new(syg (~(put by syg.new) dad [p.pev val]))
  --  --
--  --  
::                                                      ::::
::::                                                    ::  preamble
  ::                                                    ::::
::
::  lex: all durable %jael state
::
=|  lex/jael-state
|=  $:  :: 
        ::  now: current time
        ::  eny: unique entropy
        ::  ski: namespace resolver
        ::
        now/@da
        eny/@e                                        
        ski/sley
    ==
::                                                      ::::
::::                                                    ::  interface
  ::                                                    ::::
|%
::                                                      ::  ++call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen/duct
          hic/(hypo (hobo jael-task))
      ==
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard jael-task) p.q.hic)))
  ^-  {p/(list jael-move) q/_..^$}
  =^  did  lex  abet:~(call of [now eny] lex)
  [did ..^$]
::                                                      ::  ++doze
++  doze                                                ::  await
  |=  $:  ::  now: current time
          ::  hen: cause (XX why we need this?)
          ::
          now/@da 
          hen/duct
      ==
  ^-  (unit @da)
  ~
::                                                      ::  ++load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/jael-state
      ==
  ^+  ..^$
  ..^$(lex old)
::                                                      ::  ++scry
++  scry                                                ::  inspect
  |=  $:  ::  fur: event security
          ::  ren: access mode
          ::  why: owner
          ::  syd: desk (branch)
          ::  lot: case (version)
          ::  tyl: rest of path
          ::
          fur/(unit (set monk))
          ren/@tas 
          why/shop 
          syd/desk 
          lot/coin 
          tyl/spur
      ==
  ^-  (unit (unit cage))
  !!
::                                                      ::  ++stay
++  stay                                                ::  preserve
  lex
::                                                      ::  ++take
++  take                                                ::  accept
  |=  $:  ::  tea: order
          ::  hen: cause
          ::  hin: result
          ::
          tea/wire 
          hen/duct 
          hin/(hypo sign-arvo)
      ==
  ^-  {p/(list jael-move) q/_..^$}
  [~ ..^$]
--
