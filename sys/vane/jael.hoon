!:                                                      ::  /van/jael
::                                                      ::  %reference/0
!?  150
::
::
::  %jael: secrets and promises.
::
::  todo:
::
::    - communication with other vanes:
::      - actually use %behn for expiring secrets
::      - report %ames propagation errors to user
::
::    - nice features:
::      - scry namespace
::      - task for converting invites to tickets
::
|=  pit/vase
=,  pki:jael
=,  rights:jael
=,  able:jael
=,  title
=,  crypto
=,  jael
=,  ethe
=,  constitution:ethe
=,  ethereum
=,  constitution:ethereum
::                                                      ::::
::::                    # models                        ::  data structures
  ::                                                    ::::
::  the %jael state comes in two parts: absolute
::  and relative.
::
::  ++state-absolute is objective -- defined without
::  reference to our ship.  if you steal someone else's
::  private keys, we have a place to put them.  when
::  others make promises to us, we store them in the
::  same structures we use to make promises to others.
::
::  ++state-relative is subjective, denormalized and
::  derived.  it consists of all the state we need to
::  manage subscriptions efficiently.
::
=>  |%
++  state                                               ::  all vane state
  $:  ver/$0                                            ::  vane version
      yen/(set duct)                                    ::  raw observers
      urb/state-absolute                                ::  all absolute state
      sub/state-relative                                ::  all relative state
      etn=state-eth-node                                ::  eth connection state
  ==                                                    ::
++  state-relative                                      ::  urbit metadata
  $:  $=  bal                                           ::  balance sheet (vest)
        $:  yen/(set duct)                              ::  trackers
        ==                                              ::
      $=  own                                           ::  vault (vein)
        $:  yen/(set duct)                              ::  trackers
            lyf/life                                    ::  version
            jaw/(map life ring)                         ::  private keys
        ==                                              ::
      $=  puk                                           ::  public keys (pubs)
        $:  yen=(jug ship duct)                         ::  trackers
            kyz=(map ship public)                       ::  public key state
        ==                                              ::
      $=  eth                                           ::  ethereum (vent)
        ::TODO  the subscribers here never hear dns or hul...
        $:  yen=(set duct)                              ::  trackers
            dns=dnses                                   ::  on-chain dns state
            hul=(map ship hull)                         ::  on-chain ship state
            ::TODO  do we want (map ship diff-hull) too?
        ==                                              ::
  ==                                                    ::
++  state-absolute                                      ::  absolute urbit
  $:  pry/(map ship (map ship safe))                    ::  promises
      eve=logs                                          ::  on-chain events
  ==                                                    ::
++  state-eth-node                                      ::  node config + meta
  $:  source=(each ship node-src)                       ::  learning from
      heard=(set event-id)                              ::  processed events
      latest-block=@ud                                  ::  last heard block
  ==                                                    ::
++  node-src                                            ::  ethereum node comms
  $:  node=purl:eyre                                    ::  node url
      filter-id=@ud                                     ::  current filter
      poll-timer=@da                                    ::  next filter poll
  ==                                                    ::
::                                                      ::
++  message                                             ::  p2p message
  $%  [%hail p=remote]                                  ::  reset rights
      [%nuke ~]                                         ::  cancel trackers
      [%vent ~]                                         ::  view ethereum events
      [%vent-result p=chain]                            ::  tmp workaround
  ==                                                    ::
++  card                                                ::  i/o action
  (wind note:able gift)                                 ::
::                                                      ::
++  move                                                ::  output
  {p/duct q/card}                                       ::
--  ::
::                                                      ::::
::::                    # light                         ::  light cores
  ::                                                    ::::
=>  |%
::                                                      ::  ++py
::::                    ## sparse/light                 ::  sparse range
  ::                                                    ::::
++  py
  ::  because when you're a star with 2^16 unissued
  ::  planets, a (set) is kind of lame...
  ::
  |_  a/pile
  ::                                                    ::  ++dif:py
  ++  dif                                               ::  add/remove a->b
    |=  b/pile
    ^-  (pair pile pile)
    [(sub(a b) a) (sub b)]
  ::                                                    ::  ++div:py
  ++  div                                               ::  allocate
    |=  b/@ud
    ^-  (unit (pair pile pile))
    =<  ?-(- %& [~ p], %| ~)
    |-  ^-  (each (pair pile pile) @u)
    ?:  =(0 b)
      [%& ~ a]
    ?~  a  [%| 0]
    =/  al  $(a l.a)
    ?-    -.al
        %&  [%& p.p.al a(l q.p.al)]
        %|
      =.  b  (^sub b p.al)
      =/  top  +((^sub q.n.a p.n.a))
      ?:  =(b top)
        [%& a(r ~) r.a]
      ?:  (lth b top)
        :+  %&  a(r ~, q.n (add p.n.a (dec b)))
        =.  p.n.a  (add p.n.a b)
        (uni(a r.a) [n.a ~ ~])
      =/  ar  $(a r.a, b (^sub b top))
      ?-    -.ar
          %&  [%& a(r p.p.ar) q.p.ar]
          %|  [%| :(add top p.al p.ar)]
      ==
    ==
  ::
  ++  gas                                               ::  ++gas:py
    |=  b/(list @)  ^-  pile                            ::  insert list
    ?~  b  a
    $(b t.b, a (put i.b))
  ::                                                    ::  ++gud:py
  ++  gud                                               ::  validate
    =|  {bot/(unit @) top/(unit @)}
    |-  ^-  ?
    ?~  a  &
    ?&  (lte p.n.a q.n.a)
        ?~(top & (lth +(q.n.a) u.top))
        ?~(bot & (gth p.n.a +(u.bot)))
    ::
        ?~(l.a & (vor p.n.a p.n.l.a))
        $(a l.a, top `p.n.a)
    ::
        ?~(l.a & (vor p.n.a p.n.l.a))
        $(a r.a, bot `q.n.a)
    ==
  ::                                                    ::  ++int:py
  ++  int                                               ::  intersection
    |=  b/pile  ^-  pile
    ?~  a  ~
    ?~  b  ~
    ?.  (vor p.n.a p.n.b)  $(a b, b a)
    ?:  (gth p.n.a q.n.b)
      (uni(a $(b r.b)) $(a l.a, r.b ~))
    ?:  (lth q.n.a p.n.b)
      (uni(a $(b l.b)) $(a r.a, l.b ~))
    ?:  (gte p.n.a p.n.b)
      ?:  (lte q.n.a q.n.b)
        [n.a $(a l.a, r.b ~) $(a r.a, l.b ~)]
      [n.a(q q.n.b) $(a l.a, r.b ~) $(l.a ~, b r.b)]
    %-  uni(a $(r.a ~, b l.b))
    ?:  (lte q.n.a q.n.b)
      %-  uni(a $(l.b ~, a r.a))
      [n.b(q q.n.a) ~ ~]
    %-  uni(a $(l.a ~, b r.b))
    [n.b ~ ~]
  ::                                                    ::  ++put:py
  ++  put                                               ::  insert
    |=  b/@  ^-  pile
    (uni [b b] ~ ~)
  ::                                                    ::  ++sub:py
  ++  sub                                               ::  subtract
    |=  b/pile  ^-  pile
    ?~  b  a
    ?~  a  a
    ?:  (gth p.n.a q.n.b)
      $(b r.b, l.a $(a l.a, r.b ~))
    ?:  (lth q.n.a p.n.b)
      $(b l.b, r.a $(a r.a, l.b ~))
    %-  uni(a $(a l.a, r.b ~))
    %-  uni(a $(a r.a, l.b ~))
    ?:  (gte p.n.a p.n.b)
      ?:  (lte q.n.a q.n.b)
        ~
      $(b r.b, a [[+(q.n.b) q.n.a] ~ ~])
    ?:  (lte q.n.a q.n.b)
      $(b l.b, a [[n.a(q (min q.n.a (dec p.n.b)))] ~ ~])
    %-  uni(a $(b r.b, a [[+(q.n.b) q.n.a] ~ ~]))
    $(b l.b, a [[n.a(q (min q.n.a (dec p.n.b)))] ~ ~])
  ::                                                    ::  ++tap:py
  ++  tap                                               ::  into full list
    =|  out/(list @)
    |-  ^+  out
    ?~  a  out
    $(a l.a, out (welp (gulf n.a) $(a r.a)))
  ::                                                    ::  ++uni:py
  ++  uni                                               ::  merge two piles
    |=  b/pile
    ^-  pile
    ?~  b  a
    ?~  a  b
    ?.  (vor p.n.a p.n.b)  $(a b, b a)
    ?:  (lth +(q.n.b) p.n.a)
      $(b r.b, l.a $(a l.a, r.b ~))
    ?:  (lth +(q.n.a) p.n.b)
      $(b l.b, r.a $(a r.a, l.b ~))
    ?:  =(n.a n.b)  [n.a $(a l.a, b l.b) $(a r.a, b r.b)]
    ?:  (lth p.n.a p.n.b)
      ?:  (gth q.n.a q.n.b)
        $(b l.b, a $(b r.b))
      $(b l.b, a $(b r.b, a $(b r.a, r.a ~, q.n.a q.n.b)))
    ?:  (gth q.n.a q.n.b)
      $(a l.a, b $(a r.a, b $(a r.b, r.b ~, q.n.b q.n.a)))
    $(a l.a, b $(a r.a))
  --  ::py
::                                                      ::  ++ry
::::                    ## rights/light                 ::  rights algebra
  ::                                                    ::::
++  ry
  ::
  ::  we need to be able to combine rights, and
  ::  track changes by taking differences between them.
  ::
  ::  ++ry must always crash when you try to make it
  ::  do something that makes no sense.
  ::
  ::  language compromises: the type system can't enforce
  ::  that lef and ryt match, hence the asserts.
  ::
  =<  |_  $:  ::  lef: old right
              ::  ryt: new right
              ::
              lef/rite
              ryt/rite
          ==
      ::                                                ::  ++uni:ry
      ++  uni  ~(sum +> lef ryt)                        ::  add rights
      ::                                                ::  ++dif:ry
      ++  dif                                           ::  r->l: {add remove}
        ^-  (pair (unit rite) (unit rite))
        [~(dif +> ryt lef) ~(dif +> lef ryt)]
      ::                                                ::  ++sub:ry
      ++  sub                                           ::  l - r
        ^-  (unit rite)
        =/  vid  dif
        ~|  vid
        ?>(?=($~ q.vid) p.vid)
      --
  |_  $:  ::  lef: old right
          ::  ryt: new right
          ::
          lef/rite
          ryt/rite
      ==
  ::                                                    ::  ++sub-by:py
  ++  sub-by                                            ::  subtract elements
    |*  {new/(map) old/(map) sub/$-(^ *)}  ^+  new
    %-  ~(rep by new)
    |*  {{key/* val/*} acc/_^+(new ~)}
    =>  .(+<- [key val]=+<-)
    =/  var  (~(get by old) key)
    =.  val  ?~(var val (sub val u.var))
    ?~  val  acc
    (~(put by ,.acc) key val)
  ::                                                    ::  ++dif:ry
  ++  dif                                               ::  in r and not l
    ^-  (unit rite)
    |^  ?-  -.lef
          $apple  ?>(?=($apple -.ryt) (table %apple p.lef p.ryt))
          $block  ?>(?=($block -.ryt) ~)
          $email  ?>(?=($email -.ryt) (sable %email p.lef p.ryt))
          $final  ?>(?=($final -.ryt) (table %final p.lef p.ryt))
          $fungi  ?>(?=($fungi -.ryt) (noble %fungi p.lef p.ryt))
          $guest  ?>(?=($guest -.ryt) ~)
          $hotel  ?>(?=($hotel -.ryt) (bible %hotel p.lef p.ryt))
          $jewel  ?>(?=($jewel -.ryt) (table %jewel p.lef p.ryt))
          $login  ?>(?=($login -.ryt) (sable %login p.lef p.ryt))
          $pword  ?>(?=($pword -.ryt) (ruble %pword p.lef p.ryt))
          $token  ?>(?=($token -.ryt) (ruble %token p.lef p.ryt))
          $urban  ?>(?=($urban -.ryt) (table %urban p.lef p.ryt))
        ==
    ::                                                  ::  ++cable:dif:ry
    ++  cable                                           ::  diff atom
      |*  {nut/@tas new/@ old/@}
      ^-  (unit rite)
      ?:  =(new old)  ~
      `[nut new]
    ::                                                  ::  ++bible:dif:ry
    ++  bible                                           ::  diff pile
      |*  {nut/@tas old/(map dorm pile) new/(map dorm pile)}
      ^-  (unit rite)
      =;  mor/_new
        ?~(mor ~ `[nut mor])
      %^  sub-by  new  old
      |=({a/pile b/pile} (~(sub py a) b))
    ::                                                  ::  ++noble:dif:ry
    ++  noble                                           ::  diff map of @ud
      |*  {nut/@tas old/(map * @ud) new/(map * @ud)}
      ^-  (unit rite)
      =;  mor/_new
        ?~(mor ~ `[nut mor])
      %^  sub-by  new  old
      |=({a/@u b/@u} (sub a (min a b)))
    ::                                                  ::  ++ruble:dif:ry
    ++  ruble                                           ::  diff map of maps
      |*  {nut/@tas old/(map * (map)) new/(map * (map))}
      ^-  (unit rite)
      =;  mor/_new
        ?~(mor ~ `[nut mor])
      %^  sub-by  new  old
      =*  valu  (~(got by new))
      |=  {a/_^+(valu ~) b/_^+(valu ~)}  ^+  a
      (sub-by a b |*({a2/* b2/*} a2))
    ::                                                  ::  ++sable:dif:ry
    ++  sable                                           ::  diff set
      |*  {nut/@tas old/(set) new/(set)}
      ^-  (unit rite)
      =;  mor  ?~(mor ~ `[nut mor])
      (~(dif in new) old)
    ::                                                  ::  ++table:dif:ry
    ++  table                                           ::  diff map
      |*  {nut/@tas old/(map) new/(map)}
      ^-  (unit rite)
      =;  mor  ?~(mor ~ `[nut mor])
      (sub-by new old |*({a/* b/*} a))
    --  ::dif
  ::                                                    ::  ++sum:ry
  ++  sum                                               ::  lef new, ryt old
    ^-  rite
    |^  ?-  -.lef
          $apple  ?>(?=($apple -.ryt) [%apple (table p.lef p.ryt)])
          $block  ?>(?=($block -.ryt) [%block ~])
          $email  ?>(?=($email -.ryt) [%email (sable p.lef p.ryt)])
          $final  ?>(?=($final -.ryt) [%final (table p.lef p.ryt)])
          $fungi  ?>(?=($fungi -.ryt) [%fungi (noble p.lef p.ryt)])
          $guest  ?>(?=($guest -.ryt) [%guest ~])
          $hotel  ?>(?=($hotel -.ryt) [%hotel (bible p.lef p.ryt)])
          $jewel  ?>(?=($jewel -.ryt) [%jewel (table p.lef p.ryt)])
          $login  ?>(?=($login -.ryt) [%login (sable p.lef p.ryt)])
          $pword  ?>(?=($pword -.ryt) [%pword (ruble p.lef p.ryt)])
          $token  ?>(?=($token -.ryt) [%token (ruble p.lef p.ryt)])
          $urban  ?>(?=($urban -.ryt) [%urban (table p.lef p.ryt)])
        ==
    ::                                                  ::  ++bible:uni:ry
    ++  bible                                           ::  union pile
      |=  {new/(map dorm pile) old/(map dorm pile)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel dorm pile pile)
      (~(uni py q) r)
    ::                                                  ::  ++noble:uni:ry
    ++  noble                                           ::  union map of @ud
      |=  {new/(map term @ud) old/(map term @ud)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel term @ud @ud)
      (add q r)
    ::                                                  ::  ++ruble:uni:ry
    ++  ruble                                           ::  union map of maps
      |=  {new/(map site (map @t @t)) old/(map site (map @t @t))}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel site (map @t @t) (map @t @t))
      %-  (~(uno by q) r)
      |=  (trel @t @t @t)
      ?>(=(q r) r)
    ::                                                  ::  ++sable:uni:ry
    ++  sable                                           ::  union set
      |*  {new/(set) old/(set)}
      ^+  new
      (~(uni in old) new)
    ::                                                  ::  ++table:uni:ry
    ++  table                                           ::  union map
      |*  {new/(map) old/(map)}
      ^+  new
      %-  (~(uno by old) new)
      |=  (trel _p.-<.new _q.->.new _q.->.new)
      ?>(=(q r) r)
    --  ::uni
  --  ::ry
::                                                      ::  ++up
::::                    ## wallet^light                 ::  wallet algebra
  ::                                                    ::::
++  up
  ::  a set of rites is stored as a tree (++safe), sorted
  ::  by ++gor on the stem, balanced by ++vor on the stem.
  ::  (this is essentially a ++map with stem as key, but
  ::  ++map doesn't know how to link stem and bulb types.)
  ::  the goal of the design is to make it easy to add new
  ::  kinds of rite without a state adapter.
  ::
  ::  wallet operations always crash if impossible;
  ::  %jael has no concept of negative rights.
  ::
  ::  performance issues: ++differ and ++splice, naive.
  ::
  ::  external issues: much copy and paste from ++by.  it
  ::  would be nice to resolve this somehow, but not urgent.
  ::
  ::  language issues: if hoon had an equality test
  ::  that informed inference, ++expose could be
  ::  properly inferred, eliminating the ?>.
  ::
  |_  pig/safe
  ::                                                    ::  ++delete:up
  ++  delete                                            ::  delete right
    |=  ryt/rite
    ^-  safe
    ?~  pig
      ~
    ?.  =(-.ryt -.n.pig)
      ?:  (gor -.ryt -.n.pig)
        [n.pig $(pig l.pig) r.pig]
      [n.pig l.pig $(pig r.pig)]
    =/  dub  ~(sub ry n.pig ryt)
    ?^  dub  [u.dub l.pig r.pig]
    |-  ^-  safe
    ?~  l.pig  r.pig
    ?~  r.pig  l.pig
    ?:  (vor -.n.l.pig -.n.r.pig)
      [n.l.pig l.l.pig $(l.pig r.l.pig)]
    [n.r.pig $(r.pig l.r.pig) r.r.pig]
  ::                                                    ::  ++differ:up
  ++  differ                                            ::  delta pig->gob
    |=  gob/safe
    ^-  bump
    |^  [way way(pig gob, gob pig)]
    ++  way
      %-  intern(pig ~)
      %+  skip  linear(pig gob)
      |=(rite (~(has in pig) +<))
    --
  ::                                                    ::  ++exists:up
  ++  exists                                            ::  test presence
    |=  tag/@tas
    !=(~ (expose tag))
  ::                                                    ::  ++expose:up
  ++  expose                                            ::  typed extract
    |=  tag/@tas
    ^-  (unit rite)
    ?~  pig  ~
    ?:  =(tag -.n.pig)
      [~ u=n.pig]
    ?:((gor tag -.n.pig) $(pig l.pig) $(pig r.pig))
  ::                                                    ::  ++insert:up
  ++  insert                                            ::  insert item
    |=  ryt/rite
    ^-  safe
    ?~  pig
      [ryt ~ ~]
    ?:  =(-.ryt -.n.pig)
      [~(uni ry ryt n.pig) l.pig r.pig]
    ?:  (gor -.ryt -.n.pig)
      =.  l.pig  $(pig l.pig)
      ?>  ?=(^ l.pig)
      ?:  (vor -.n.pig -.n.l.pig)
        [n.pig l.pig r.pig]
      [n.l.pig l.l.pig [n.pig r.l.pig r.pig]]
    =.  r.pig  $(pig r.pig)
    ?>  ?=(^ r.pig)
    ?:  (vor -.n.pig -.n.r.pig)
      [n.pig l.pig r.pig]
    [n.r.pig [n.pig l.pig l.r.pig] r.r.pig]
  ::                                                    ::  ++intern:up
  ++  intern                                            ::  insert list
    |=  lin/(list rite)
    ^-  safe
    ?~  lin  pig
    =.  pig  $(lin t.lin)
    (insert i.lin)
  ::                                                    ::  ++linear:up
  ++  linear                                            ::  convert to list
    =|  lin/(list rite)
    |-  ^+  lin
    ?~  pig  ~
    $(pig r.pig, lin [n.pig $(pig l.pig)])
  ::                                                    ::  ++redact:up
  ++  redact                                            ::  conceal secrets
    |-  ^-  safe
    ?~  pig  ~
    :_  [$(pig l.pig) $(pig r.pig)]
    =*  rys  n.pig
    ^-  rite
    ?+    -.rys  rys
        $apple
      [%apple (~(run by p.rys) |=(@ (shax +<)))]
    ::
        $final
      [%final (~(run by p.rys) |=(@ (shax +<)))]
    ::
        $login
      [%login ~]
    ::
        $pword
      :-  %pword
      %-  ~(run by p.rys)
      |=  (map @ta @t)
      (~(run by +<) |=(@t (fil 3 (met 3 +<) '*')))
    ::
        $jewel
      [%jewel (~(run by p.rys) |=(@ (shax +<)))]
    ::
        $token
      :-  %token
      %-  ~(run by p.rys)
      |=((map @ta @) (~(run by +<) |=(@ (shax +<))))
    ::
        $urban
      [%urban (~(run by p.rys) |=({@da code:ames} [+<- (shax +<+)]))]
    ==
  ::                                                    ::  ++remove:up
  ++  remove                                            ::  pig minus gob
    |=  gob/safe
    ^-  safe
    =/  buv  ~(tap by gob)
    |-  ?~  buv  pig
        $(buv t.buv, pig (delete i.buv))
  ::                                                    ::  ++splice:up
  ++  splice                                            ::  pig plus gob
    |=  gob/safe
    ^-  safe
    =/  buv  ~(tap by gob)
    |-  ?~  buv  pig
        $(buv t.buv, pig (insert i.buv))
  ::                                                    ::  ++update:up
  ++  update                                            ::  arbitrary change
    |=  del/bump
    ^-  safe
    (splice(pig (remove les.del)) mor.del)
  --
::                                                      ::  ++ez
::::                    ## ethereum^light               ::  wallet algebra
  ::                                                    ::::
++  ez
  ::  simple ethereum-related utility arms.
  ::
  |%
  ::
  ::  +order-events: sort changes by block and log numbers
  ::
  ++  order-events
    |=  loz=(list (pair event-id diff-constitution))
    ^+  loz
    %+  sort  loz
    ::  sort by block number, then by event log number,
    ::TODO  then by diff priority.
    |=  [[[b1=@ud l1=@ud] *] [[b2=@ud l2=@ud] *]]
    ?.  =(b1 b2)  (lth b1 b2)
    ?.  =(l1 l2)  (lth l1 l2)
    &
  --
--
::                                                      ::::
::::                    #  heavy                        ::  heavy engines
  ::                                                    ::::
=>  |%
::                                                      ::  ++of
::::                    ## main^heavy                   ::  main engine
  ::                                                    ::::
++  of
  ::  this core handles all top-level %jael semantics,
  ::  changing state and recording moves.
  ::
  ::  logically we could nest the ++su and ++ur cores
  ::  within it, but we keep them separated for clarity.
  ::  the ++curd and ++cure arms complete relative and
  ::  absolute effects, respectively, at the top level.
  ::
  ::  a general pattern here is that we use the ++ur core
  ::  to generate absolute effects (++change), then invoke
  ::  ++su to calculate the derived effect of these changes.
  ::
  ::  for ethereum-related events, this is preceded by
  ::  invocation of ++et, which produces ethereum-level
  ::  changes (++chain). these get turned into absolute
  ::  effects by ++cute.
  ::
  ::  arvo issues: should be merged with the top-level
  ::  vane interface when that gets cleaned up a bit.
  ::
  =|  moz/(list move)
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
          state
      ==
  ::  lex: all durable state
  ::  moz: pending actions
  ::
  =*  lex  ->
  |%
  ::                                                    ::  ++abet:of
  ++  abet                                              ::  resolve
    [(flop moz) lex]
  ::                                                    ::  ++burb:of
  ++  burb                                              ::  per ship
    |=  who/ship
    ~(able ~(ex ur urb) who)
  ::                                                    ::  ++scry:of
  ++  scry                                              ::  read
    |=  {syd/@tas pax/path}  ^-  (unit gilt)
    =^  mar  pax  =/(a (flop pax) [-.a (flop t.+.a)])
    ?>  ?=(_-:*gilt mar)
    =-  (biff - (flit |=(a/gilt =(-.a mar))))
    ~  ::TODO
  ::                                                    ::  ++call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen/duct
            tac/task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  destroy promises
    ::    {$burn p/ship q/safe}
    ::
        $burn
      %+  cure  our.tac
      abet:abet:(deal:(burb our.tac) p.tac [~ q.tac])
    ::
    ::  remote update
    ::    {$hail p/ship q/remote}
    ::
        $hail
      %+  cure  our.tac
      abet:abet:(hail:(burb our.tac) p.tac q.tac)
    ::
    ::  initialize vane
    ::    [%init our=ship]
    ::
        $init
      %+  cute  our.tac  =<  abet
      (~(init et our.tac now.sys etn.lex) our.tac)
    ::
    ::  set ethereum source
    ::    [%look p=(each ship purl)]
    ::
        %look
      %+  cute  our.tac  =<  abet
      (~(look et our.tac now.sys etn.lex) src.tac)
    ::
    ::  create promises
    ::    {$mint p/ship q/safe}
    ::
        $mint
      %+  cure  our.tac
      abet:abet:(deal:(burb our.tac) p.tac [q.tac ~])
    ::
    ::
    ::  move promises
    ::    {$move p/ship q/ship r/safe}
    ::
        $move
      =.  +>
        %+  cure  our.tac
        abet:abet:(deal:(burb our.tac) p.tac [~ r.tac])
      =.  +>
        %+  cure  our.tac
        abet:abet:(deal:(burb our.tac) q.tac [r.tac ~])
      +>
    ::
    ::  cancel all trackers from duct
    ::    {$nuke $~}
    ::
        $nuke
      %_  +>
        yen          (~(del in yen) hen)
        yen.bal.sub  (~(del in yen.bal.sub) hen)
        yen.own.sub  (~(del in yen.own.sub) hen)
        yen.eth.sub  (~(del in yen.eth.sub) hen)
      ==
    ::
    ::  watch public keys
    ::    [%pubs our=ship who=ship]
    ::
        %pubs
      %-  curd  =<  abet
      (~(pubs ~(feed su our.tac urb sub etn) hen) who.tac)
    ::
    ::  seen after breach
    ::    [%meet our=ship who=ship]
    ::
        %meet
      %+  cure  our.tac
      [[%meet who.tac]~ urb]
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      (curd abet:~(vein ~(feed su our.tac urb sub etn) hen))
    ::
    ::  watch ethereum events
    ::    [%vent ~]
    ::
        %vent
      =.  moz  [[hen %give %mack ~] moz]
      (curd abet:~(vent ~(feed su our.tac urb sub etn) hen))
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      (curd abet:~(vest ~(feed su our.tac urb sub etn) hen))
    ::
    ::  monitor all
    ::    {$vine $~}
    ::
        $vine
      +>(yen (~(put in yen) hen))
    ::
    ::  authenticated remote request
    ::    {$west p/ship q/path r/*}
    ::
        $west
      =+  mes=((hard message) r.tac)
      =*  our  p.p.tac
      =*  dem  q.p.tac
      ?-    -.mes
      ::
      ::  reset remote rights
      ::    [%hail p=remote]
      ::
          %hail
        %+  cure  our
        abet:abet:(hail:(burb our) dem p.mes)
      ::
      ::  cancel trackers
      ::    [%nuke ~]
      ::
          %nuke
        $(tac mes)
      ::
      ::  view ethereum events
      ::    [%vent ~]
      ::
          %vent
        ~&  %west-vent
        $(tac [%vent our])
      ::
      ::
          %vent-result
        ::  ignore if not from currently configured source.
        ?.  &(-.source.etn =(dem p.source.etn))
          +>.$
        =.  moz  [[hen %give %mack ~] moz]
        %+  cute  our  =<  abet
        (~(hear-vent et our now.sys etn.lex) p.mes)
      ==
    ==
  ::
  ++  take
    |=  [tea=wire hen=duct hin=sign]
    ^+  +>
    ?>  ?=([@ *] tea)
    =+  our=(slav %p i.tea)
    =*  wir  t.tea
    ?-  hin
        [%a %woot *]
      ?~  q.hin  ~&(%coop-fine +>.$)
      ?~  u.q.hin  ~&(%ares-fine +>.$)
      ~&  [%woot-bad p.u.u.q.hin]
      ~_  q.u.u.q.hin
      ::TODO  fail:et
      +>.$
    ::
        [%e %sigh *]
      %+  cute  our  =<  abet
      (~(sigh et our now.sys etn.lex) wir p.hin)
    ::
        [%b %wake ~]
      %+  cute  our  =<  abet
      ~(wake et our now.sys etn.lex)
    ::
        [%j %vent *]
      %+  cute  our  =<  abet
      (~(hear-vent et our now.sys etn.lex) p.hin)
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  {moz/(list move) sub/state-relative}
    +>(sub sub, moz (weld (flop moz) ^moz))
  ::                                                    ::  ++cure:of
  ++  cure                                              ::  absolute edits
    |=  {our/ship hab/(list change) urb/state-absolute}
    ^+  +>
    (curd(urb urb) abet:(~(apex su our urb sub etn) hab))
  ::                                                    ::  ++cute:of
  ++  cute                                              ::  ethereum changes
    |=  $:  our=ship
            mos=(list move)
            ven=chain
            net=state-eth-node
        ==
    ^+  +>
    %-  cure(etn net, moz (weld (flop mos) moz))
    [our abet:(link:(burb our) ven)]
  --
::                                                      ::  ++su
::::                    ## relative^heavy               ::  subjective engine
  ::                                                    ::::
++  su
      ::  the ++su core handles all derived state,
      ::  subscriptions, and actions.
      ::
      ::  ++feed:su registers subscriptions.
      ::
      ::  ++feel:su checks if a ++change should notify
      ::  any subscribers.
      ::
      ::  ++fire:su generates outgoing network messages.
      ::
      ::  ++form:su generates the actual report data.
      ::
  =|  moz/(list move)
  =|  evs=logs
  =|  $:  our/ship
          state-absolute
          state-relative
          state-eth-node
      ==
  ::  moz: moves in reverse order
  ::  urb: absolute urbit state
  ::  sub: relative urbit state
  ::
  =*  urb  ->-
  =*  sub  ->+<
  =*  etn  ->+>
  |%
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
    ::TODO  we really want to just send the %give, but ames is being a pain.
    :: =>  (exec yen.eth [%give %vent |+evs])
    =>  ?~  evs  .
        (vent-pass yen.eth |+evs)
    [(flop moz) sub]
  ::                                                    ::  ++apex:su
  ++  apex                                              ::  apply changes
    |=  hab/(list change)
    ^+  +>
    ?~  hab  +>
    %=    $
        hab  t.hab
        +>
      ?-  -.i.hab
        %ethe  (file can.i.hab)
        %meet  (meet +.i.hab)
        %rite  (paid +.i.hab)
      ==
    ==
  ::                                                    ::  ++exec:su
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moz [[i.noy cad] moz])
  ::
  ++  vent-pass
    |=  [yen=(set duct) res=chain]
    =+  yez=~(tap in yen)
    |-  ^+  ..vent-pass
    ?~  yez  ..vent-pass
    =*  d  i.yez
    ?>  ?=([[%a @ @ *] *] d)
    =+  our=(slav %p i.t.i.d)
    =+  who=(slav %p i.t.t.i.d)
    %+  exec  [d ~ ~]
    :+  %pass
      /(scot %p our)/vent-result
    ^-  note:able
    [%a %want [our who] /j/(scot %p our)/vent-result %vent-result res]
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
    ::
    ++  pubs
      |=  who=ship
      %_  ..feed
        moz      =/  pub  (~(get by kyz.puk) who)
                 ?~  pub  moz
                 ?:  =(0 life.u.pub)  moz
                 [[hen %give %pubs u.pub] moz]
        yen.puk  (~(put ju yen.puk) who hen)
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
    ::
    ++  vent
      %.  [[hen ~ ~] &+eve]
      %_  vent-pass
      :: %_  ..feed  ::TODO  see ++abet
        :: moz      [[hen %give %vent &+eve] moz]
        yen.eth  (~(put in yen.eth) hen)
      ==
    --
  ::                                                    ::  ++feel:su
  ++  feel                                              ::  update tracker
    |%
    ::                                                  ::  ++pubs:feel:su
    ++  pubs                                            ::  kick public keys
      ::  puz: new public key states
      |=  puz=(map ship public)
      =/  pus  ~(tap by puz)
      ::
      ::  process change for each ship separately
      ::
      |-  ^+  ..feel
      ?~  pus  ..feel
      =;  fel  $(pus t.pus, ..feel fel)
      =*  who  p.i.pus
      =*  pub  q.i.pus
      ::
      ::  update public key store and notify subscribers
      ::  of the new state
      ::
      ~&  [%sending-pubs-about who]
      %+  exec(kyz.puk (~(put by kyz.puk) who pub))
        (~(get ju yen.puk) who)
      [%give %pubs pub]
    ::                                                  ::  ++vein:feel:su
    ++  vein                                            ::  kick private keys
      ^+  ..feel
      =/  yam  vein:form
      ?:  =(yam +.own)  ..feel
      (exec(+.own yam) yen.own [%give %vein +.own])
    ::                                                  ::  ++vest:feel:su
    ++  vest                                            ::  kick balance
      |=  hug/action
      ^+  ..feel
      ?:  =([~ ~] +.q.hug)  ..feel
      ::
      ::  notify all local listeners
      ::
      =.  ..feel  (exec yen.bal [%give %vest %| p.hug q.hug])
      ::
      ::  pig: purse report for partner
      ::
      ?.  ?=(%| -.q.hug)  ..feel
      =*  pig  (~(lawn ur urb) our p.hug)
      %_    ..feel
          moz
        :_  moz
        :^  *duct  %pass  /vest/(scot %p p.hug)
        :+  %a  %want
        :+  [our p.hug]  /j
        ^-  message
        [%hail |+pig]
      ==
    ::
    ++  vent
      |=  can=chain
      ^+  ..feel
      ::TODO  see ++abet
      :: (exec yen.eth [%give %vent can])
      (vent-pass yen.eth can)
    --
  ::                                                    ::  ++form:su
  ++  form                                              ::  generate reports
    |%
    ::                                                  ::  ++vein:form:su
    ++  vein                                            ::  private key report
      ^-  (pair life (map life ring))
      (~(lean ur urb) our)
    ::                                                  ::  ++vest:form:su
    ++  vest                                            ::  balance report
      ^-  balance
      :-  ::
          ::  raw: all our liabilities by ship
          ::  dud: delete liabilities to self
          ::  cul: mask secrets
          ::
          =*  raw  =-(?~(- ~ u.-) (~(get by pry.urb) our))
          =*  dud  (~(del by raw) our)
          =*  cul  (~(run by dud) |=(safe ~(redact up +<)))
          cul
      ::
      ::  fub: all assets by ship
      ::  veg: all nontrivial assets, secrets masked
      ::
      =/  fub
        ^-  (list (pair ship (unit safe)))
        %+  turn
          ~(tap by pry.urb)
        |=  (pair ship (map ship safe))
        [p (~(get by q) our)]
      =*  veg
        |-  ^-  (list (pair ship safe))
        ?~  fub  ~
        =+  $(fub t.fub)
        ?~(q.i.fub - [[p.i.fub ~(redact up u.q.i.fub)] -])
      ::
      (~(gas by *(map ship safe)) veg)
    --
  ::                                                    ::  ++paid:su
  ++  paid                                              ::  track asset change
    |=  $:  ::  rex: promise from
            ::  pal: promise to
            ::  del: change to existing
            ::
            rex/ship
            pal/ship
            del/bump
        ==
    ^+  +>
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
      (vest:feel pal %& del)
    ::
    ::  track private keys
    ::
    ?.  (~(exists up mor.del) %jewel)  +>
    vein:feel
  ::                                                    ::  ++meet:su
  ++  meet                                              ::  seen after breach
    |=  who=ship
    ^+  +>
    =+  ~|  [%met-unknown-ship who]
        (~(got by kyz.puk) who)
    (pubs:feel [[who -(live &)] ~ ~])
  ::                                                    ::  ++file:su
  ++  file                                              ::  process event logs
    ::TODO  whenever we add subscriptions for data,
    ::      outsource the updating of relevant state
    ::      to a ++feel arm.
    |=  [new=? evs=logs]
    ^+  +>
    =?  +>  new
      ::TODO  should we be mutating state here,
      ::      or better to move this into ++vent:feel?
      +>(dns.eth *dnses, hul.eth ~, kyz.puk ~)
    =?  +>  |(new !=(0 ~(wyt by evs)))
      %-  vent:feel
      ?:(new &+evs |+evs)
    ::
    =+  vez=(order-events:ez ~(tap by evs))
    =|  kyz=(map ship public)
    |^  ?~  vez  (pubs:feel kyz)
        =^  kyn  ..file  (file-event i.vez)
        $(vez t.vez, kyz kyn)
    ::
    ++  get-public
      |=  who=ship
      ^-  public
      %+  fall  (~(get by kyz) who)
      ::NOTE  we can only do this because ++pubs:feel
      ::      sends out entire new state, rather than
      ::      just the processed changes.
      %+  fall  (~(get by kyz.puk) who)
      %*(. *public live |)
    ::
    ++  file-keys
      |=  [who=ship =life =pass]
      ^+  kyz
      =/  pub  (get-public who)
      =/  puk  (~(get by pubs.pub) life)
      ?^  puk
        ::  key known, nothing changes
        ~|  [%key-mismatch who life]
        ?>(=(u.puk pass) kyz)
      %+  ~(put by kyz)  who
      :+  live.pub
        (max life life.pub)
      (~(put by pubs.pub) life pass)
    ::
    ++  file-discontinuity
      |=  who=ship
      =+  (get-public who)
      (~(put by kyz) who -(live |))
    ::
    ++  file-event
      |=  [wer=event-id dif=diff-constitution]
      ^+  [kyz ..file]
      ?:  (~(has in heard) wer)
        ~&  %ignoring-already-heard-event
        [kyz ..file]
      ::
      ::  sanity check, should never fail if we operate correctly
      ::
      ?>  (gte block.wer latest-block)
      =:  evs           (~(put by evs) wer dif)
          heard         (~(put in heard) wer)
          latest-block  (max latest-block block.wer)
      ==
      ?-  -.dif
        %hull   (file-hull +.dif)
        %dns    [kyz (file-dns +.dif)]
      ==
    ::
    ++  file-hull
      |=  [who=ship dif=diff-hull]
      ^+  [kyz ..file]
      =-  ::TODO  =; with just the type
        :-  ?:  ?=(%& -.new)
              (file-keys who p.new)
            ?:  p.new  kyz
            (file-discontinuity who)
        ..file(hul.eth (~(put by hul.eth) who hel))
      ::  hel: updated hull
      ::  new: new keypair or "kept continuity?" (yes is no-op)
      ^-  [hel=hull new=(each (pair life pass) ?)]
      =+  hul=(fall (~(get by hul.eth) who) *hull)
      ::
      ::  sanity checks, should never fail if we operate correctly
      ::
      ~|  [%diff-order-insanity -.dif]
      ?>  ?+  -.dif  &
            %spawned      ?>  ?=(^ kid.hul)
                          !(~(has in spawned.u.kid.hul) who.dif)
            %keys         ?>  ?=(^ net.hul)
                          =(life.dif +(life.u.net.hul))
            %continuity   ?>  ?=(^ net.hul)
                          =(new.dif +(continuity-number.u.net.hul))
          ==
      ::
      ::  apply hull changes, catch continuity and key changes
      ::
      :-  (apply-hull-diff hul dif)
      =*  nop  |+&  ::  no-op
      ?+  -.dif  nop
        %continuity   |+|
        %keys         &+[life pass]:dif
        %full         ?~  net.new.dif  nop
                      ::TODO  do we want/need to do a diff-check
                      &+[life pass]:u.net.new.dif
      ==
    ::
    ++  file-dns
      |=  dns=dnses
      ..file(dns.eth dns)
    --
  --
::                                                      ::  ++ur
::::                    ## absolute^heavy               ::  objective engine
  ::                                                    ::::
++  ur
      ::  the ++ur core handles primary, absolute state.
      ::  it is the best reference for the semantics of
      ::  the urbit pki.
      ::
      ::  it is absolutely verboten to use [our] in ++ur.
      ::
  =|  hab/(list change)
  =|  state-absolute
  ::
  ::  hab: side effects, reversed
  ::  urb: all urbit state
  ::
  =*  urb  -
  |%
  ::                                                    ::  ++abet:ur
  ++  abet                                              ::  resolve
    [(flop hab) `state-absolute`urb]
  ::
  ++  link
    |=  ven=chain
    %_  +>
      hab   [[%ethe ven] hab]
      eve   ?:  ?=(%& -.ven)  p.ven
            (~(uni by eve) p.ven)
    ==
  ::                                                    ::  ++lawn:ur
  ++  lawn                                              ::  debts, rex to pal
    |=  {rex/ship pal/ship}
    ^-  safe
    (lawn:~(able ex rex) pal)
  ::                                                    ::  ++lean:ur
  ++  lean                                              ::  private keys
    |=  rex/ship
    ^-  (pair life (map life ring))
    lean:~(able ex rex)
  ::                                                    ::  ++ex:ur
  ++  ex                                                ::  server engine
    ::  shy: private state
    ::  rug: domestic will
    ::
    =|  $:  shy/(map ship safe)
        ==
    =|  ::  rex: server ship
        ::
        rex/ship
    |%
    ::                                                  ::  ++abet:ex:ur
    ++  abet                                            ::  resolve
      %_  ..ex
        pry  (~(put by pry) rex shy)
      ==
    ::                                                  ::  ++able:ex:ur
    ++  able                                            ::  initialize
      %_  .
        shy  (fall (~(get by pry) rex) *(map ship safe))
      ==
    ::                                                  ::  ++deal:ex:ur
    ++  deal                                            ::  alter rights
      |=  {pal/ship del/bump}
      ^+  +>
      =/  gob  (fall (~(get by shy) pal) *safe)
      =*  hep  (~(update up gob) del)
      %_  +>.$
        shy  (~(put by shy) pal hep)
        hab  [[%rite rex pal del] hab]
      ==
    ::
    ++  hail                                            ::  ++hail:ex:ur
      |=  {pal/ship rem/remote}                         ::  report rights
      ^+  +>
      =/  gob  (fall (~(get by shy) pal) *safe)
      ::  yer: pair of change and updated safe.
      =/  yer  ^-  (pair bump safe)
        ?-  -.rem
          ::  change: add rem. result: old + rem.
          %&  [[p.rem ~] (~(splice up gob) p.rem)]
          ::  change: difference. result: rem.
          %|  [(~(differ up gob) p.rem) p.rem]
        ==
      %_  +>.$
        shy  (~(put by shy) pal q.yer)
        hab  [[%rite rex pal p.yer] hab]
      ==
    ::                                                  ::  ++lean:ex:ur
    ++  lean                                            ::  private keys
      ^-  (pair life (map life ring))
      ::
      ::  par: promises by rex, to rex
      ::  jel: %jewel rights
      ::  lyf: latest life of
      ::
      =*  par  (~(got by shy) rex)
      =/  jel=rite  (need (~(expose up par) %jewel))
      ?>  ?=($jewel -.jel)
      =;  lyf=life
        [lyf p.jel]
      %+  roll  ~(tap in ~(key by p.jel))
      |=  [liv=life max=life]
      ?:((gth liv max) liv max)
    ::                                                  ::  ++lawn:ex:ur
    ++  lawn                                            ::  liabilities to pal
      |=  pal/ship
      ^-  safe
      =-(?~(- ~ u.-) (~(get by shy) pal))
    --
  --
::                                                      ::  ++et
::::                    ## ethereum^heavy               ::  ethereum engine
  ::                                                    ::::
++  et
  ::
  ::  the ++et core handles all logic necessary to maintain the
  ::  absolute record of on-chain state changes, "events".
  ::
  ::  we either subscribe to a parent ship's existing record, or
  ::  communicate directly with an ethereum node.
  ::
  ::  moves: effects; either behn timers, subscriptions,
  ::         or ethereum node rpc requests.
  ::  reset: whether the found changes assume a fresh state.
  ::  changes: on-chain changes heard from our source.
  ::
  =|  moves=(list move)
  =+  reset=|
  =|  changes=logs
  |_  $:  our=ship
          now=@da
          state-eth-node
      ==
  +*  etn  +<+>
  ::
  ::  +|  outward
  ::
  ::  +abet: produce results
  ::
  ++  abet
    ^-  [(list move) chain state-eth-node]
    [(flop moves) ?:(reset &+changes |+changes) etn]
  ::
  ::  +put-move: store side-effect
  ::
  ++  put-move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ::  +put-request: store rpc request to ethereum node
  ::
  ++  put-request
    |=  [wir=wire id=(unit @t) req=request]
    (put-move (rpc-hiss wir (request-to-json id req)))
  ::
  ::  +put-change: store change made by event
  ::
  ++  put-change
    |=  [cause=event-id dif=diff-constitution]
    ?<  (~(has by changes) cause)  ::  one diff per event
    +>(changes (~(put by changes) cause dif))
  ::
  ::  +|  move-generation
  ::
  ::  +wrap-note: %pass a note using a made-up duct
  ::
  ++  wrap-note
    |=  [wir=wire not=note:able]
    ^-  move
    :-  [/jael/eth-logic ~ ~]
    [%pass (weld /(scot %p our) wir) not]
  ::
  ::  +rpc-hiss: make an http request to our ethereum rpc source
  ::
  ++  rpc-hiss
    |=  [wir=wire jon=json]
    ^-  move
    %+  wrap-note  wir
    :^  %e  %hiss  ~
    :+  %json-rpc-response  %hiss
    ?>  ?=(%| -.source)
    !>  (json-request node.p.source jon)
  ::
  ::  +|  source-operations
  ::
  ::  +listen-to-ship: depend on who for ethereum events
  ::
  ++  listen-to-ship
    |=  [our=ship who=ship]
    %-  put-move(source &+who)
    %+  wrap-note  /vent/(scot %p who)
    [%a %want [our who] /j/(scot %p our)/vent `*`[%vent ~]]
  ::
  ::  +unsubscribe-from-source: stop listening to current source ship
  ::
  ++  unsubscribe-from-source
    |=  our=ship
    %-  put-move
    ?>  ?=(%& -.source)
    %+  wrap-note  /vent/(scot %p p.source)
    ::TODO  should we maybe have a %nuke-vent,
    ::      or do we have a unique duct here?
    [%a %want [our p.source] /j/(scot %p our)/vent `*`[%nuke ~]]
  ::
  ++  listen-to-node
    |=  url=purl:eyre
    new-filter(source |+%*(. *node-src node url))
  ::
  ::  +|  filter-operations
  ::
  ::  +new-filter: request a new polling filter
  ::
  ::    Listens only to the Ships state contract, and only from
  ::    the last-heard block onward.
  ::
  ++  new-filter
    %-  put-request
    :+  /filter/new  `'new filter'
    :*  %eth-new-filter
        `[%number +(latest-block)]  ::TODO  or Ships origin block when 0
        ~  ::TODO  we should probably chunck these, maybe?
        ::  https://stackoverflow.com/q/49339489
        ~[ships:contracts]
        ~
    ==
  ::
  ::  +read-filter: get all events the filter captures
  ::
  ++  read-filter
    ?>  ?=(%| -.source)
    %-  put-request
    :+  /filter/logs  `'filter logs'
    [%eth-get-filter-logs filter-id.p.source]
  ::
  ::  +poll-filter: get all new events since the last poll (or filter creation)
  ::
  ++  poll-filter
    ?>  ?=(%| -.source)
    %-  put-request
    :+  /filter/changes  `'poll filter'
    [%eth-get-filter-changes filter-id.p.source]
  ::
  ::  +wait-poll: remind us to poll in four minutes
  ::
  ::    Four minutes because Ethereum RPC filters time out after five.
  ::    We don't check for an existing timer or clear an old one here,
  ::    sane flows shouldn't see this being called superfluously.
  ::
  ++  wait-poll
    ?>  ?=(%| -.source)
    =+  wen=(add now ~m4)
    %-  put-move(poll-timer.p.source wen)
    (wrap-note /poll %b %wait wen)
  ::
  ::  +cancel-wait-poll: remove poll reminder
  ::
  ++  cancel-wait-poll
    ?>  ?=(%| -.source)
    %-  put-move(poll-timer.p.source *@da)
    %+  wrap-note  /poll/cancel
    [%b %rest poll-timer.p.source]
  ::
  ::  +|  configuration
  ::
  ::  +init: initialize with default ethereum connection
  ::
  ::    for galaxies, we default to a localhost geth node.
  ::    for stars and under, we default to the parent ship.
  ::
  ++  init
    |=  our=ship
    ^+  +>
    ::TODO  ship or node as sample?
    =+  bos=(sein:title our)
    ?.  =(our bos)
      (listen-to-ship our bos)
    =+  (need (de-purl:html 'http://localhost:8545'))
    (listen-to-node -(p.p |))
  ::
  ::  +look: configure the source of ethereum events
  ::
  ++  look
    |=  src=(each ship purl:eyre)
    ^+  +>
    =.  +>
      ?:  ?=(%| -.source)
        cancel-wait-poll
      (unsubscribe-from-source our)
    ?:  ?=(%| -.src)
      (listen-to-node p.src)
    (listen-to-ship our p.src)
  ::
  ::  +|  subscription-results
  ::
  ::  +hear-vent: process incoming events
  ::
  ++  hear-vent
    |=  can=chain
    ^+  +>
    ?-  -.can
      %&   (assume p.can)
      ::
        %|
      =+  evs=~(tap by p.can)
      |-
      ?~  evs  +>.^$
      =.  +>.^$  (accept i.evs)
      $(evs t.evs)
    ==
  ::
  ::  +assume: clear state and process events
  ::
  ++  assume
    |=  evs=logs
    ^+  +>
    %.  |+evs
    %_  hear-vent
      heard         ~
      latest-block  0
      reset         &
    ==
  ::
  ::  +accept: process single event
  ::
  ++  accept
    |=  [cause=event-id dif=diff-constitution]
    ^+  +>
    ?:  (~(has in heard) cause)
      ~&  %accept-ignoring-duplicate-event
      +>.$
    (put-change cause dif)
  ::
  ::  +|  filter-results
  ::
  ::  +wake: kick polling, unless we changed source
  ::
  ++  wake
    ?.  ?=(%| -.source)  .
    poll-filter
  ::
  ::  +sigh: parse rpc response and process it
  ::
  ++  sigh
    |=  [cuz=wire mar=mark res=vase]
    ^+  +>
    ?:  ?=(%& -.source)  +>
    ?:  ?=(%tang mar)
      ::TODO  proper error handling
      ~&  %yikes
      ~_  q.res
      +>
    ?>  ?=(%json-rpc-response mar)
    ~|  res
    =+  rep=((hard response:rpc:jstd) q.res)
    ?+  cuz  ~|([%weird-sigh-wire cuz] !!)
        [%filter %new *]
      (take-new-filter rep)
    ::
        [%filter *]
      (take-filter-results rep)
    ==
  ::
  ::  +take-new-filter: store filter-id and read it
  ::
  ++  take-new-filter
    |=  rep=response:rpc:jstd
    ^+  +>
    ~|  rep
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%filter-error--retrying message.rep]
      new-filter
    ?>  ?=(%| -.source)
    =-  read-filter(filter-id.p.source -)
    (parse-eth-new-filter-res res.rep)
  ::
  ::  +take-filter-results: parse results into event-logs and process them
  ::
  ++  take-filter-results
    |=  rep=response:rpc:jstd
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ?.  =('filter not found' message.rep)
        ~&  [%unhandled-filter-error message.rep]
        +>
      ~&  %filter-timed-out--recreating
      new-filter
    ::  kick polling timer, only if it hasn't already been.
    =?  +>  ?&  ?=(%| -.source)
                (gth now poll-timer.p.source)
            ==
      wait-poll
    ?>  ?=(%a -.res.rep)
    =*  changes  p.res.rep
    ~&  [%filter-changes (lent changes)]
    |-  ^+  +>.^$
    ?~  changes  +>.^$
    =.  +>.^$
      (take-event-log (parse-event-log i.changes))
    $(changes t.changes)
  ::
  ::  +take-event-log: obtain changes from event-log
  ::
  ++  take-event-log
    |=  log=event-log
    ^+  +>
    ?~  mined.log
      ~&  %ignoring-unmined-event
      +>
    =*  place  u.mined.log
    ::
    ::TODO  if the block number is less than latest, that means we got
    ::      events out of order somehow and should probably reset.
    ?>  (gth block-number.place latest-block)
    ::
    ?:  (~(has in heard) block-number.place log-index.place)
      ~&  %ignoring-duplicate-event
      +>
    =+  cuz=[block-number.place log-index.place]
    ::
    ?:  =(event.log changed-dns:ships-events)
      =+  ^-  [pri=tape sec=tape ter=tape]
        %+  decode-results  data.log
        ~[%string %string %string]
      %+  put-change  cuz
      [%dns (crip pri) (crip sec) (crip ter)]
    ::
    =+  dif=(event-log-to-hull-diff log)
    ?~  dif  +>.$
    (put-change cuz %hull u.dif)
  ::
  --
--
::                                                      ::::
::::                    #  vane                         ::  interface
  ::                                                    ::::
::
::  lex: all durable %jael state
::
=|  lex/state
|=  $:  ::
        ::  now: current time
        ::  eny: unique entropy
        ::  ski: namespace resolver
        ::
        now/@da
        eny/@e
        ski/sley
    ==
|%
::                                                      ::  ++call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen/duct
          hic/(hypo (hobo task:able))
      ==
  ^-  {p/(list move) q/_..^$}
  =^  did  lex
    =-  abet:(~(call of [now eny] lex) hen -)
    ?.  ?=($soft -.q.hic)  q.hic
    ((hard task:able) p.q.hic)
  [did ..^$]
::                                                      ::  ++load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/state
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
  :: XX security
  ?.  =(lot [%$ %da now])  ~
  %-  some
  ?.  =(%$ ren)  ~
  %+  bind  (~(scry of [now eny] lex) syd tyl)
  =-  ~!  -  -
  |=(a/gilt [-.a (slot `@`3 !>(a))])
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
          hin/(hypo sign)
      ==
  ^-  {p/(list move) q/_..^$}
  =^  did  lex  abet:(~(take of [now eny] lex) tea hen q.hin)
  [did ..^$]
--
