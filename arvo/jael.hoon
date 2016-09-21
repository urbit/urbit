!:                                                      ::  /van/jael
::                                                      ::  %orthodox
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
      nav/jael-objective                                ::  all objective state
      nix/jael-subjective                               ::  all subjective state
  ==                                                    ::
++  jael-subjective                                     ::  indexes / observers
  $:  sub/jael-subjective-urbit                         ::  urbit subjective
      sew/jael-subjective-web                           ::  web subjective
  ==                                                    ::
++  jael-subjective-urbit                               ::  urbit metadata
  $:  $=  dev                                           ::  indexes
        $:  dad/ship                                    ::  our parent
            pyr/(set ship)                              ::  peers
            kid/(set ship)                              ::  children
            ast/(map ship jael-purse)                   ::  assets from
        ==                                              ::
      $=  vew                                           ::  all trackers
        $:  lat/(set duct)                              ::  rights
            hin/(set duct)                              ::  channel, all
            yon/(map ship (set duct))                   ::  channel by ship
  ==    ==                                              ::
++  jael-subjective-web                                 ::  web metadata
  $~                                                    ::
++  jael-objective                                      ::  all universal state
  $:  urb/jael-urbit                                    ::  all urbit state
      web/(map site jael-web-domain)                    ::  all web state
  ==                                                    ::
++  jael-web-domain                                     ::  per foreign app
  $:  sec/(map @t jael-web-app)                         ::  client per api key
      usr/(map @ta (unit @t))                           ::  user/password
  ==                                                    ::
++  jael-web-app                                        ::  local app
  $:  key/(unit (pair @da @))                           ::  API key
      tok/(map @t (pair @da @))                         ::  token by username
  ==                                                    ::
++  jael-web-user                                       ::  per-user secrets
  $:  pas/(unit @t)                                     ::  password
      dey/(unit @t)                                     ::  display name
  ==                                                    ::
++  jael-ship                                           ::  per relationship
  $:  lab/(map ship jael-purse)                         ::  promises to
  ==                                                    ::
++  jael-urbit                                          ::  objective urbit
  $:  pug/gree                                          ::  all public state
      pry/(map ship jael-ship)                          ::  all specific state
  ==                                                    ::
++  jael-gift                                           ::  output
  $?  {$cash jael-report-cash}                          ::  account report
      {$paid jael-report-paid}                          ::  asset update
      {$self jael-report-self}                          ::  self dump
  ==                                                    ::
++  jael-right                                          ::  urbit commitment
  $%  {$email p/(set @ta)}                              ::  email addresses
      {$final p/(map ship @uvG)}                        ::  ticketed ships
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$guest $~}                                       ::  refugee visa
      {$hotel p/pile}                                   ::  reserved block
      {$jewel p/(map life ring)}                        ::  private keyring
      {$lived p/life}                                   ::  PKI knowledge
      {$token p/(map hand (pair @da code))}             ::  symmetric keys
  ==                                                    ::
++  jael-purse                                          ::  rights set
  (tree jael-right)                                     ::
::                                                      ::
++  jael-delta                                          ::  rights change
  $:  mor/jael-purse                                    ::  grow rights
      les/jael-purse                                    ::  lose rights
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$give p/ship q/jael-purse}                       ::  add rights
      {$line p/ship q/@da r/code}                       ::  outbound symkey
      {$link p/ship q/@da r/code}                       ::  inbound symkey
      {$meet p/ship q/gree}                             ::  integrate pki from
      {$nuke $~}                                        ::  cancel as tracker
      {$over p/ship q/jael-task}                        ::  mirror operation
      {$pall p/ship q/life}                             ::  our life acked
      {$ping p/ship}                                    ::  empty contact
      {$step p/lamp q/ring}                             ::  update private key
      {$take p/ship q/jael-purse}                       ::  subtract rights
      {$vein $~}                                        ::  watch private keys
      {$vest $~}                                        ::  watch balances
      {$view p/ship}                                    ::  watch secure channel
      {$vile p/site}                                    ::  watch website
      {$vine $~}                                        ::  watch balance log
      {$west p/ship q/path r/*}                         ::  remote request
      {$wink p/site q/@t r/(unit bill)}                 ::  set web API key
      {$wish p/site q/@t r/(unit @t)}                   ::  set web login
      {$wonk p/site q/@t r/@t s/(unit bill)}            ::  set web API token
  ==                                                    ::
++  jael-report-them                                    ::  report on neighbor
  $:  gur/grue                                          ::  certificate
      lab/jael-purse                                    ::  our promises to
      own/jael-purse                                    ::  our promises from
  ==                                                    ::
++  jael-report-self                                    ::  report on self
  $:  gur/grue                                          ::  certificate
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
      {$ping p/gree}                                    ::  propagate
  ==                                                    ::
++  jael-grow                                           ::  unit of learning
  $%  {$sign p/ship q/life r/@}                         ::  add/update signature
      {$step p/lace}                                    ::  add whole deed
  ==                                                    ::
++  jael-edit                                           ::  urbit change
  $%  $:  $fact                                         ::  certificate change
          rex/ship                                      ::  owner
          via/(unit ship)                               ::  made/heard from
          lyf/life                                      ::  deed added/modified
          gan/jael-grow                                 ::  info gained
      ==                                                ::
      $:  $rite                                         ::  rights change
          rex/ship                                      ::  issuer
          pal/ship                                      ::  issued to
          del/jael-delta                                ::  change
      ==                                                ::
  ==                                                    ::
++  jael-move                                           ::  output
  {p/duct q/(wind jael-note jael-gift)}
--
::                                                      ::::
::::                        #  2                        ::  static data
  ::                                                    ::::
=>  |%
::                                                      ::  zeno
++  zeno                                                ::  boot signatures
  |=  ::  who: galaxy (0-255)
      ::
      who/ship
  ^-  pass
  !!
--
::                                                      ::::
::::                        #  3                        ::  stateless functions
  ::                                                    ::::
      ::
      ::    (++pu: rights tool)
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
      ::    (imagined improvements)
      ::  
      ::  more rights: reputation stuff, foreign accounts...
      ::
      ::  blocked on hoon: if equality to constant informed
      ::  typed inference, ++expose could specialize.
      ::
=>  |%
::                                                      ::  up
++  up                                                  ::  rights engine
  |_  pig/jael-purse
  ::                                                    ::  differ:up
  ++  differ                                            ::  delta pig->gob
    |=  gob/jael-purse
    ^-  jael-delta
    !!
  ::                                                    ::  expose:up
  ++  expose                                            ::  typed extract
    |=  tag/@tas
    ^-  (unit jael-right)
    ?~  pig  ~
    ?:  =(tag -.n.pig)
      [~ u=n.pig]
    ?:((gor tag -.n.pig) $(pig l.pig) $(pig r.pig))
  ::                                                    ::  insert:up
  ++  insert                                            ::  pig plus gob
    |=  gob/jael-purse
    ^-  jael-purse
    !!
  ::                                                    ::  remove:up
  ++  remove                                            ::  pig minus gob
    |=  gob/jael-purse
    ^-  jael-purse
    !!
  ::                                                    ::  update:up
  ++  update                                            ::  arbitrary change
    |=  del/jael-delta
    ^-  jael-purse
    (remove(pig (insert mor.del)) les.del)
  --   
--
::                                                      ::::
::::                        #  4                        ::  reactors
  ::                                                    ::::
=>  |%
::                          ## 4.a                      ::  of
++  of                                                  ::  main reactor
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
    ::  add rights
    ::    {$give p/ship q/jael-purse)}
    ::
        $give  
      (cure abet:(~(give ur sys urb.nav) our [p q]:tac))
    ::
    ::  outbound symmetric key
    ::    {$link p/ship q/@da r/code}
    ::
        $link
      =*  ryt  `jael-right`[%token [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
      %-  cure
      abet:(~(give ur sys urb.nav) our p.tac [ryt ~ ~])
    ::
    ::  inbound symmetric key
    ::    {$line p/ship q/@da r/code}
    ::
        $line
      =*  ryt  `jael-right`[%token [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
      %-  cure
      abet:(~(give ur sys urb.nav) p.tac our [ryt ~ ~])
    ::
    ::  public-key update
    ::    {$meet p/ship q/gree}
    ::
        $meet
      (cure abet:(~(meet ur sys urb.nav) `p.tac q.tac))
    ::
    ::  cancel tracking from duct
    ::    {$nuke $~}
    ::
        $nuke
      !!
    ::
    ::  learn as other ship
    ::    {$over p/ship q/jael-task}
    ::
        $over
      $(our p.tac, tac q.tac)
    ::
    ::  remote version acknowledgment
    ::    {$pall p/ship q/life}
    ::
        $pall
      !!
      ::(cure abet:(~(pall ur sys urb.nav) our p.tac q.tac))
    ::
    ::  request incidental contact
    ::    {$ping p/ship}
    ::
        $ping
      !!
      :: (cure [%ping p.tac ~]~)
    ::
    ::  extend our certificate with a new private key
    ::    {$step p/lamp}
    ::
        $step 
      !!
      ::  (cure abet:(~(step ur sys urb.nav) our p.tac))
    ::
    ::  remove rights
    ::    {$take p/ship q/jael-purse}
    ::  
        $take
      !!
      ::  (cure abet:(~(give ur sys urb.nav) our [p q]:tac))
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      !!
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      !!
    ::
    ::  monitor secure channel
    ::    {$view $~}
    ::
        $view
      !!
    ::
    ::  monitor website
    ::    {$vile p/site}
    ::
        $vile 
      !!
    ::
    ::  monitor all 
    ::    {$vine $~}
    ::
        $vine
      !!
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
        ::  (cure (~(hail ur urb.nav) p.tac our p.mes))
        !!
      ::
      ::  share certificates
      ::    {$ping p/gree}
      ::
          $ping
        ::  (cure (~(meet ur urb.nav) p.tac p.mes))
        !!
      ==
    ::
    ::  set/clear web API key
    ::    {$wink p/site q/@t r/(unit bill)}
    ::
        $wink
      !!
    ::
    ::  set/clear web password
    ::    {$wish p/site q/@t r/(unit @t)}
    ::
        $wish
      !!
    ::
    ::  set/clear web API token
    ::    {$wonk p/site q/@t r/@t s/(unit bill)}
    ::
        $wonk
      !!
    ==
  ::                                                    ::  curd:of
  ++  curd                                              ::  subjective moves
    |=  {moz/(list jael-move) sub/jael-subjective-urbit}
    +>(sub.nix sub, moz (weld (flop moz) ^moz))
  ::                                                    ::  cure:of
  ++  cure                                              ::  objective edits
    |=  {hab/(list jael-edit) urb/jael-urbit}
    ^+  +>
    (curd(urb.nav urb) abet:(~(apex su urb sub.nix) hab))
  --
::                          ## 4.b                      ::  su
++  su                                                  ::  subjective reactor
  =|  moz/(list jael-move)                              ::::
  =|  $:  jael-urbit
          jael-subjective-urbit
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
    ?~  hab  +>
    $(hab t.hab, +> (pano:(repo:(echo i.hab) i.hab) i.hab))
  ::                                                    ::  echo:su
  ++  echo                                              ::  update indexes
    |=  led/jael-edit
    ^+  +>
    ?-    -.led
    ::
    ::  new certificate state
    ::    {$fact rex/ship via/(unit ship) lyf/life gan/jael-grow}
    ::
        $fact
      ::  new deed
      ::    {$step p/lace}
      ::
      ?.  ?=($step -.gan.led)  +>
      =+  dad=dad.doc.dat.p.gan.led
      ::
      ::  if self update, update cached parent state
      ::
      ?:  =(our rex.led)
        +>.$(dad.dev dad)
      ::
      ::  if first meeting, add to child/peer sets.  children
      ::  have us as a parent; peers have the same rank as us
      ::
      ?.  =(1 lyf.led)  +>.$
      ?:  =(our dad)
        +>.$(kid.dev (~(put in kid.dev) rex.led))
      ?.  =((clan rex.led) (clan our))
        +>.$
      +>.$(pyr.dev (~(put in pyr.dev) rex.led))
    ::
    ::  new rights
    ::    {$rite rex/ship pal/ship del/jael-delta}
    ::
        $rite
      ::
      ::  track all promises made to us
      ::
      ?.  =(our pal.led)  +>.$
      =*  haz  (fall (~(get by ast.dev) rex.led) *jael-purse)
      =/  nex  (~(update up haz) del.led)
      %=    +>.$
          ast.dev
        ?:  =(~ nex)
          (~(del by ast.dev) rex.led)
        (~(put by ast.dev) rex.led nex)
      == 
    ==
  ::                                                    ::  pano:su
  ++  pano                                              ::  update network
    |=  led/jael-edit
    ^+  +>
    ?.  ?=($fact -.led)  +>
    ?-    -.gan.led
    ::
    ::  new signature
    ::    {$sign p/ship q/life r/@}
    :: 
        $sign
      !!
    ::
    ::  new deed
    ::    {$deed p/lace}
    ::
        $step
      !!
    ==
::      ::
::      ::  if newborn child, send all star and galaxy wills
::      ::
::      =.  moz
::        ?.  &(=(1 lyf.led) (~(has in kid.dev) rex.led))
::          moz
::        :_  moz
::        =*  rug  ^-  gree
::            %-  ~(gas by *gree)
::            %+  skim  (~(tap by pug.urb))
::            |=({who/ship *} (lth who 65.536))
::        [%pass %x ~ %mess rex.led /x %ping rug]
::      
::    ==
::    ::
::    ::  propagate this specific update
::    ::
::    =-  %=    +>
::            moz
::          %+  weld
::            (turn lat |=(ship [%pass %x ~ %mess +< /x %ping gur.led]))
::          moz
::        ==
::    ::  
::    ::  lat: ships to target, not including self or source
::    ::  tag: set of ships to target
::    ::  jad: list of ship sets to target
::    ::
::    ^=  lat  ^-  (list ship)
::    =-  (~(tap by (~(del by (~(del by tag) via.led)) our)))
::    ^=  tag  ^-  (set ship)
::    =-  |-(?~(jad ~ (~(uni in i.jad) $(jad t.jad))))
::    ^=  jad  ^-  (list (set ship))
::    ?.  &(=(our via.led) !=(our rex.led))
::      [rex.led ~ ~]
::    ::
::    ::  if we signed a will for someone else, send it home
::    ::
::    ?:  &(=(our via.led) !=(our rex.led))
::      [rex.led ~ ~]
::    ::
::    ::  if our will has changed, send to parents and kids;
::    ::  if a new deed has been added, also to pals
::    ::
::    ?:  =(our rex.led)
::      :*  [dad.dev ~ ~]
::          kid.dev
::          ?.(=(%new way.led) ~ [pal.dev ~])
::      ==
::    :: 
::    ::  propagate star and galaxy updates to parents and kids
::    ::
::    ?.  (lth rex.led 65.536)
::      ~
::    :*  [dad.dev ~ ~]
::        kid.dev
::        ~
  ::                                                    ::  repo:su
  ++  repo                                              ::  update trackers
    |=  led/jael-edit
    ^+  +>
    ?-    -.led
    ::
    ::  new certificate state
    ::    {$fact rex/ship via/ship lyf/life way/?($new $old) gur/gree}
    ::
        $fact
      !!
    ::
    ::  new rights
    ::    {$paid p/ship q/jael-delta}
    ::
        $rite
      +>
    ==
  --
::                          ## 4.c                      ::  ur
++  ur                                                  ::  urbit reactor
  =|  hab/(list jael-edit)                              ::::
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
          jael-urbit
      ==
  ::  urb: all urbit state
  ::  hab: side effects, reversed
  ::
  =*  urb  ->
  |%
  ::                                                  ::  abet:ur
  ++  abet                                            ::  resolve
    [(flop hab) `jael-urbit`urb]
  ::                                                  ::  give:ur
  ++  give                                            ::  grant rights
    |=  {rex/ship pal/ship lab/jael-purse}
    ^+  +>
    !!
  ::                                                  ::  meet:ur
  ++  meet                                            ::  calculate merge
    |=  $:  ::  via: authenticated source
            ::  cod: transmitted certificates
            ::
            via/(unit ship)
            cod/gree
        ==
    ^+  +>
    =+  lec=(~(tap by cod))
    |-  ^+  ..meet
    ?~  lec  ..meet
    %=  $
      lec     t.lec
      ..meet  abet:(grow:~(able ex p.i.lec) via cod q.i.lec)
    ==
  ::                                                    ::  ex:ur
  ++  ex                                                ::  server reactor
    ::  shy: private state
    ::  rug: domestic will
    ::
    =|  $:  shy/jael-ship
            rug/grue
        ==
    =|  ::  rex: server ship
        ::
        rex/ship
    |%
    ::                                                  ::  abet:ex:ur
    ++  abet                                            ::  resolve
      %_  ..ex
        pry  (~(put by pry) rex shy)
        pug  (~(put by pug) rex rug)
      ==
    ::                                                  ::  able:ex:ur
    ++  able                                            ::  initialize
      %_  .
        shy  (fall (~(get by pry) rex) *jael-ship)
        rug  (fall (~(get by pug) rex) *grue)
      ==
    ::                                                  ::  grow:ex:ur
    ++  grow                                            ::  merge wills
      |=  $:  ::  via: data source
              ::  cod: merge context
              ::  gur: input will
              ::  
              via/(unit ship)
              cod/gree
              gur/grue
          ==
      ?:  |(=(~ q.gur) =(gur rug))  ..grow
      |^  ^+  ..grow
          ::
          ::  wap: ascending list of new certs
          ::  nem: previous life
          ::  pre: previous deed
          ::
          =/  wap
            ^-  (list (pair life lace))
            %+  sort  (~(tap by q.gur))
            |=  {a/{life *} b/{life *}}
            (lth -.a -.b)
          =/  nem  
            ^-  (unit life)  
            ?~(wap ~ ?:(=(1 p.i.wap) ~ `(dec p.i.wap)))
          =/  pre
            ^-  (unit lama)
            (bind nem |=(life dat:(~(got by q.rug) +<))) 
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
            wap    t.wap
            nem    `p.i.wap
            pre    `dat.q.lub
            q.rug  (~(put by q.rug) p.i.wap q.lub)
            hab    (weld (flop p.lub) hab)
          ==
      ::                                                ::  grow-leak/ex:ur
      ::                                                ::  get private key
      ++  grow-leak
        |=  {who/ship lyf/life}
        ^-  @
        ::  lab: promises by who
        ::  par: promises by who, to who
        ::  jel: private key by life
        ::
        =*  lab  lab:(~(got by pry) who)
        =*  par  (~(got by lab) who)
        =/  jel  `jael-right`(need (~(expose up par) %jewel))
        ?>  ?=($jewel -.jel)
        (~(got by p.jel) lyf)
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
            gur     (fall (~(get by cod) who.myn) *grue)
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
          ?>  =(p.rug lyf)
          %+  biff  (~(get by q.rug) lyf)
          |=(lace `pub.dat)
        --
      ::                                                ::  grow-mate/ex:ur
      ++  grow-mate                                     ::  merge lives
        |=  $:  ::  num: life we're merging
                ::  new: new deed
                ::  pre: previous deed 
                ::  eld: old deed
                ::
                num/@ud
                new/lace
                pre/(unit lama)
            ==
        =+  :*  eld=`(unit lace)`(~(get by q.rug) num) 
            ==
        ^-  (pair (list jael-edit) lace)
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
          |-  ^-  (pair (list jael-edit) lace)
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
          `jael-edit`[%fact rex via num `jael-grow`[%sign i.sow]]
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
                ::  the deed is made by us or sent by owner
                ::
                |(?=($~ via) =(u.via rex))
                ::
                ::  check valid parent signature
                ::
                =+  par=(~(got by syg.new) dad)
                (grow-like [dad p.par] ash q.par)
            ==
        =-  [[%fact rex p.- num %step q.-]~ q.-]
        ^-  (pair (unit ship) lace)
        ::
        ::  the new deed is complete; report it
        ::
        ?:  (~(has by syg.new) dad)
          [via new]
        ::
        ::  the new deed needs a signature; try to add it
        ::
        :-  ~
        ::
        ::  lyf: life of parent
        ::  rig: secret key of parent
        ::  val: new signature
        ::
        =*  lyf  p:(~(got by pug) dad)
        =*  rig  (grow-leak dad lyf)
        =*  val  (sign:as:(nol:nu:crub rig) *@ ash)
        new(syg (~(put by syg.new) dad [lyf val]))
    --
    ::                                                ::  unto:ex:ur:of
    ++  unto                                          ::  client reactor
      |=  ::  pal: client ship
          ::
          pal/ship
      ::
      ::  cly: client state
      ::
      ::  =+  (fall (~(get by rel) pal) *jael-friend) 
      =*  cly  -
      |%
      ::                                              ::  abet:unto:ex:ur
      ++  abet                                        ::  resolve
        ^+  ..unto
        !!
      ::                                              ::  give:unto:ex:ur
      ++  give                                        ::  credit
        |=  pig/jael-purse
        ^+  +>
        !!
      --
    --
  --  
--
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
::                                                      ::
::::                                                    ::  interface
  ::                                                    ::::
|%
::                                                      ::  call
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
::                                                      ::  doze
++  doze                                                ::  await
  |=  $:  ::  now: current time
          ::  hen: cause (XX why we need this?)
          ::
          now/@da 
          hen/duct
      ==
  ^-  (unit @da)
  ~
::                                                      ::  load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/jael-state
      ==
  ^+  ..^$
  ..^$(lex old)
::                                                      ::  scry
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
::                                                      ::  stay
++  stay                                                ::  preserve
  lex
::                                                      ::  take
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
