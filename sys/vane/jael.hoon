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
      eth=state-ethereum                                ::  eth-specific state
  ==                                                    ::
++  state-relative                                      ::  urbit metadata
  $:  $=  bal                                           ::  balance sheet
        $:  yen/(set duct)                              ::  trackers
        ==                                              ::
      $=  own                                           ::  vault
        $:  yen/(set duct)                              ::  trackers
            lyf/life                                    ::  version
            jaw/(map life ring)                         ::  private keys
        ==                                              ::
  ==                                                    ::
++  state-absolute                                      ::  absolute urbit
  $:  pry/(map ship (map ship safe))                    ::  promises
  ==                                                    ::
++  state-ethereum                                      ::  known ethereum
  $:  ships=fleet                                       ::  ship states
      dns=dnses                                         ::  dns states
    ::                                                  ::  meta
      listeners=(set duct)                              ::  subscribers
      source=(each ship node-src)                       ::  learning from
      heard=events                                      ::  processed events
      latest-block=@ud                                  ::  last heard block
      checking=(map @p hull)                            ::  verifying
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
      [%vent ~]                                         ::  view ethereum state
      [%vent-result p=update:constitution:ethe]         ::  tmp workaround
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
      ~!  [old new]
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
      =^  mos  eth.lex
        abet:(~(init et /init now.sys eth.lex) our.tac)
      +>.$(moz (weld moz (flop mos)))
      :: %+  cure(moz (weld moz (flop mos)))  our.tac
      :: abet:(make:(burb our.tac) our.tac now.sys eny.sys p.tac q.tac)
    ::
    ::  set ethereum source
    ::    [%look p=(each ship purl)]
    ::
        %look
      =^  mos  eth.lex
        abet:(~(look et /init now.sys eth.lex) our.tac src.tac)
      +>.$(moz (weld moz (flop mos)))
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
        listeners.eth  (~(del in listeners.eth) hen)
      ==
    ::
    ::  watch private keys
    ::    {$vein $~}
    ::
        $vein
      (curd abet:~(vein ~(feed su our.tac urb sub) hen))
    ::
    ::  watch ethereum state
    ::    [%vent ~]
    ::
        %vent
      =^  mos  eth.lex
        abet:(~(vent-to et /vent now.sys eth.lex) hen)
      =.  mos  [[hen %give %mack ~] mos]
      +>.$(moz (weld moz (flop mos)))
    ::
        %vent-result
      =^  mos  eth.lex
        abet:(~(hear-vent et /vent-result now.sys eth.lex) p.tac)
      =.  mos  [[hen %give %mack ~] mos]
      +>.$(moz (weld moz (flop mos)))
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      (curd abet:~(vest ~(feed su our.tac urb sub) hen))
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
      ?-    -.mes
      ::
      ::  reset remote rights
      ::    [%hail p=remote]
      ::
          %hail
        %+  cure  p.p.tac
        abet:abet:(hail:(burb p.p.tac) q.p.tac p.mes)
      ::
      ::  cancel trackers
      ::    [%nuke ~]
      ::
          %nuke
        $(tac mes)
      ::
      ::  view ethereum state
      ::    [%vent ~]
      ::
          %vent
        $(tac mes)
      ::
      ::
          %vent-result
        ?.  &(-.source.eth =(q.p.tac p.source.eth))
          +>.$
        $(tac mes)
      ==
    ==
  ::                                                    ::  ++curd:of
  ++  curd                                              ::  relative moves
    |=  {moz/(list move) sub/state-relative}
    +>(sub sub, moz (weld (flop moz) ^moz))
  ::                                                    ::  ++cure:of
  ++  cure                                              ::  absolute edits
    |=  {our/ship hab/(list change) urb/state-absolute}
    ^+  +>
    (curd(urb urb) abet:(~(apex su our urb sub) hab))
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
  =|  $:  our/ship
          state-absolute
          state-relative
      ==
  ::  moz: moves in reverse order
  ::  urb: absolute urbit state
  ::  sub: relative urbit state
  ::
  =*  urb  ->-
  =*  sub  ->+
  |%
  ::                                                    ::  ++abet:su
  ++  abet                                              ::  resolve
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
        $rite  (paid +.i.hab)
      ==
    ==
  ::                                                    ::  ++exec:su
  ++  exec                                              ::  mass gift
    |=  {yen/(set duct) cad/card}
    =/  noy  ~(tap in yen)
    |-  ^+  ..exec
    ?~  noy  ..exec
    $(noy t.noy, moz [[i.noy cad] moz])
  ::                                                    ::  ++feed:su
  ++  feed                                              ::  subscribe to view
    |_  ::  hen: subscription source
        ::
        hen/duct
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
      =/  yer  ^-  (pair bump safe)
        ?-  -.rem
          %&  [[p.rem ~] (~(splice up gob) p.rem)]
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
      ::  lyf: latest life of
      ::  par: promises by rex, to rex
      ::  jel: %jewel rights
      ::
      :: =/  lyf  `life`(fall ~(current we rug) 1)
      :: =*  par  (~(got by shy) rex)
      :: =/  jel  `rite`(need (~(expose up par) %jewel))
      :: ?>  ?=($jewel -.jel)
      :: [lyf p.jel]
      [0 ~]  ::TODO
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
      ::  the ++et core handles ethereum-based state
      ::  and all network requests necessary to
      ::  maintain it.
      ::
      ::TODO  more words
  =|  moves=(list move)
  =|  changes=(jar [@ud @ud] diff-constitution)
  =|  requests=(jar wire (pair (unit @t) request))
  |_  $:  cuz=wire
          now=@da
          state-ethereum
      ==
  +*  eth  +<+>
  ::
  ++  abet
    ^-  [(list move) state-ethereum]
    =-  [(weld - (flop moves)) eth]
    %+  weld
      ^-  (list move)
      %+  turn  ~(tap by requests)
      |=  [w=wire r=(list (pair (unit @t) request))]
      %+  rpc-hiss  w
      a+(turn (flop r) request-to-json)
    ^-  (list move)
    ?:  =(0 ~(wyt by changes))  ~
    %+  update-to-all  %difs
    %+  sort  ~(tap by changes)
    ::  sort by block number, then by event log number,
    ::TODO  then by diff priority.
    |=  [[[b1=@ud l1=@ud] *] [[b2=@ud l2=@ud] *]]
    ?.  =(b1 b2)  (lth b1 b2)
    ?.  =(l1 l2)  (lth l1 l2)
    &
  ::
  ++  put-move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ++  put-moves
    |=  mos=(list move)
    %_(+> moves (weld (flop mos) moves))
  ::
  ++  make-change
    |=  [cause=[@ud @ud] dif=diff-constitution]
    (da(changes (~(add ja changes) cause dif)) cause [dif]~)
  ::
  ++  make-changes
    |=  [cause=[@ud @ud] dis=(list diff-constitution)]
    =-  (da(changes -) cause dis)
    %+  ~(put by changes)  cause
    (weld (flop dis) (~(get ja changes) cause))
  ::
  ++  put-request
    |=  [wir=wire id=(unit @t) req=request]
    %_(+> requests (~(add ja requests) wir id req))
  ::
  ++  put-individual-request
    |=  [wir=wire id=(unit @t) req=request]
    (put-move (rpc-hiss wir (request-to-json id req)))
  ::
  ::
  ++  wrap-note
    |=  [wir=wire not=note:able]
    ^-  move
    [[/jael/eth-logic ~ ~] %pass wir not]
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
  ++  update-to-all
    |=  upd=update
    ^-  (list move)
    %+  turn  ~(tap in listeners)
    |=  d=duct
    :: [d %give %vent upd]
    ::NOTE  we do a %pass instead of a %give because ames is bad
    ?>  ?=([[%a @ @ *] *] d)
    =+  our=(slav %p i.t.i.d)
    =+  who=(slav %p i.t.t.i.d)
    %+  wrap-note  /vent-result
    [%a %want [our who] /j/vent-result %vent-result upd]
  ::
  ::
  ++  listen-to-ship
    |=  [our=ship who=ship]
    %-  put-move(source &+who)
    %+  wrap-note  /vent/(scot %p who)
    [%a %want [our who] /j/vent `*`[%vent ~]]
  ::
  ++  unsubscribe-from-source
    |=  our=ship
    %-  put-move
    ?>  ?=(%& -.source)
    %+  wrap-note  /vent/(scot %p p.source)
    [%a %want [our p.source] /j/vent `*`[%nuke ~]]
  ::
  ::
  ++  read
    |=  [wir=wire cal=ships:function]
    =+  (ships:function-to-call cal)
    %^  put-request  wir  `id
    :+  %eth-call
      [~ ships:contracts ~ ~ ~ (encode-call dat)]
    ::NOTE  we can't make read calls to not the latest block. however,
    ::      you risk getting data that filter polling hasn't yet seen,
    ::      so probably poll the filter before doing any important reads.
    [%label %latest]
  ::
  ++  read-ships
    |=  [wir=wire who=(list @p)]
    ?~  who  +>
    =.  +>  (read wir %ships i.who)
    $(who t.who)
  ::
  ++  read-dns
    |=  wir=wire
    =+  inx=(gulf 0 2)
    |-
    ?~  inx  +>.^$
    =.  +>.^$  (read wir %dns-domains i.inx)
    $(inx t.inx)
  ::
  ::
  ++  listen-to-node
    |=  url=purl:eyre
    new-filter(source |+%*(. *node-src node url))
  ::
  ++  new-filter
    %-  put-individual-request
    :+  /filter/new  `'new filter'
    :*  %eth-new-filter
        `[%number +(latest-block)]  ::TODO  or Ships origin block when 0
        ~  ::TODO  we should probably chunck these, maybe?
        ::  https://stackoverflow.com/q/49339489
        ~[ships:contracts]
        ~
    ==
  ::
  ++  read-filter
    ?>  ?=(%| -.source)
    %-  put-individual-request
    :+  /filter/logs  `'filter logs'
    [%eth-get-filter-logs filter-id.p.source]
  ::
  ++  poll-filter
    ?>  ?=(%| -.source)
    %-  put-individual-request
    :+  /filter/changes  `'poll filter'
    [%eth-get-filter-changes filter-id.p.source]
  ::
  ++  wait-poll
    ?>  ?=(%| -.source)
    =+  wen=(add now ~m4)
    %-  put-move(poll-timer.p.source wen)
    (wrap-note /poll %b %wait wen)
  ::
  ++  cancel-wait-poll
    ?>  ?=(%| -.source)
    %-  put-move(poll-timer.p.source *@da)
    %+  wrap-note  /poll/cancel
    [%b %rest poll-timer.p.source]
  ::
  ::
  ++  init
    |=  our=ship
    ^+  +>
    ::TODO  ship or node as sample?
    ::TODO  set default polling time in config
    =+  bos=(sein:title our)
    ?.  =(our bos)
      (listen-to-ship our bos)
    =+  (need (de-purl:html 'http://localhost:8545'))
    (listen-to-node -(p.p |))
  ::
  ++  look
    |=  [our=ship src=(each ship purl:eyre)]
    ^+  +>
    =.  +>
      ?:  ?=(%| -.src)
        cancel-wait-poll
      (unsubscribe-from-source our)
    ?:  ?=(%| -.src)
      (listen-to-node p.src)
    (listen-to-ship our p.src)
  ::
  ++  run-check
    |=  save=?
    =+  wir=(weld /read ?:(save /reset /verify))
    =.  +>.$  (read-dns wir)
    (read-ships wir (gulf ~zod ~nec))  ::TODO  ~fes
  ::
  ::
  ++  vent-to
    |=  duc=duct
    ::TODO  check if we already know duc
    %-  put-move(listeners (~(put in listeners) duc))
    ~&  [%venting-to duc]
    :: [duc %give %vent %full ships dns heard]
    ::NOTE  we do a %pass instead of a %give because ames is bad
    ?>  ?=([[%a @ @ *] *] duc)
    =+  our=(slav %p i.t.i.duc)
    =+  who=(slav %p i.t.t.i.duc)
    %+  wrap-note  /vent-result
    [%a %want [our who] /j/vent-result %vent-result %full ships dns heard]
  ::
  ++  hear-vent
    |=  upd=update
    ^+  +>
    ?-  -.upd
      %full   (assume +.upd)
      ::
        %difs
      |-
      ?~  dis.upd  +>.^$
      =.  +>.^$  (accept i.dis.upd)
      $(dis.upd t.dis.upd)
    ==
  ::
  ++  assume
    |=  [s=fleet d=dnses h=events]
    ^+  +>
    ?:  &(=(s ships) =(d dns) =(h heard))  +>
    ~&  [%assume ~(wyt by s) ~(wyt in h)]
    %-  put-moves(ships s, dns d, heard h)
    (update-to-all %full s d h)
  ::
  ++  accept
    |=  [cause=[@ud @ud] dis=(list diff-constitution)]
    ^+  +>
    ?:  &(!=([0 0] cause) (~(has in heard) cause))
      ~&  %accept-ignoring-duplicate-event
      +>.$
    ~&  [%accept (lent dis)]
    (make-changes cause dis)
  ::
  ::
  ++  wake  poll-filter
  ::
  ++  sigh
    |=  [mar=mark res=vase]
    ^+  +>
    ?:  ?=(%& -.source)  +>
    ?>  ?=(%json-rpc-response mar)
    ~|  res
    =+  rep=((hard response:rpc:jstd) q.res)
    ~&  rpc+rep
    ?+  cuz  !!
        [%filter %new *]
      (take-new-filter rep)
    ::
        [%filter *]
      (take-filter-results rep)
    ::
        [%read @ta *]
      (take-read-results rep ?=(%reset i.t.cuz))
    ==
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
  ++  take-event-log
    |=  log=event-log
    ^+  +>
    ?~  mined.log
      ~&  %ignoring-unmined-event
      +>
    ::
    ::TODO  if the block number is less than latest, that means we got
    ::      events out of order somehow and should probably reset.
    ::
    =*  place  u.mined.log
    ?:  (~(has in heard) block-number.place log-index.place)
      ~&  %ignoring-duplicate-event
      +>
    =+  cuz=[block-number.place log-index.place]
    ::
    ?:  =(event.log changed-dns:ships-events)
      =+  ^-  [pri=tape sec=tape ter=tape]
        (decode-results data.log ~[%string %string %string])
      =?  +>.$  !=(pri.dns (crip pri))
        (make-change cuz %dns 0 (crip pri))
      =?  +>.$  !=(sec.dns (crip sec))
        (make-change cuz %dns 1 (crip sec))
      =?  +>.$  !=(ter.dns (crip ter))
        (make-change cuz %dns 2 (crip ter))
      +>.$
    ::
    =+  dis=(event-log-to-hull-diffs log)
    ?~  dis  +>.$
    (make-change cuz %hull i.dis)
  ::
  ::
  ++  take-read-results
    |=  [rep=response:rpc:jstd save=?]
    ^+  +>
    ?>  ?=(%batch -.rep)
    |-  ^+  +>.^$
    ?~  bas.rep  +>.^$
    =.  +>.^$
      (take-read-result i.bas.rep save)
    $(bas.rep t.bas.rep)
  ::
  ::NOTE  yes, this is awful
  ++  take-read-result
    |=  [rep=response:rpc:jstd save=?]
    ^+  +>
    ?<  ?=(%batch -.rep)
    ?:  ?=(%error -.rep)
      ~&  [%unhandled-read-error id.rep message.rep]
      +>
    =/  cal=ships:function  (parse-id id.rep)
    =+  wir=(weld /read ?:(save /reset /verify))
    ::TODO  think about a better way to structure the comparison code below
    ?-  -.cal  ::  ~&([%unhandled-read-result -.cal] +>.$)
        %ships
      ?>  ?=(%s -.res.rep)
      =/  hul=hull:eth-noun
        ~|  [id.rep p.res.rep]
        (decode-results p.res.rep hull:eth-type)
      ::  ignore inactive ships
      ?.  active.hul  +>.$
      ::  we store the read data for now, and only compare with state once we
      ::  have completed it by learning the spawned ships.
      =.  checking  (~(put by checking) who.cal (hull-from-eth hul))
      (read wir %get-spawned who.cal)
    ::
        %get-spawned
      ?>  ?=(%s -.res.rep)
      =+  hul=(~(got by checking) who.cal)
      =/  kis=(list @p)
        ::TODO  can we let this be if we're cool with just @ ?
        %-  (list @p)  ::NOTE  because arrays are still typeless
        (decode-results p.res.rep [%array %uint]~)
      =.  hul  hul(spawned (~(gas in *(set @p)) kis))
      ::
      =+  have=(~(get by ships) who.cal)
      =.  +>.$
        ?~  have
          ~&  [%completely-missing who.cal]
          ?.  save  +>.$
          ~&  [%storing-chain-version-of who.cal]
          (make-change [0 0] %hull who.cal %full hul)
        ::
        =*  huv  state.u.have
        ?:  =(huv hul)  +>.$
        ~&  [%differs-from-chain-version who.cal]
        ~&  [%what %have %chain]
        ::TODO  can we maybe re-use some ++redo code to simplify this?
        ~?  !=(owner.huv owner.hul)
          :-  %owner-differs
          [owner.huv owner.hul]
        ~?  !=(encryption-key.huv encryption-key.hul)
          :-  %encryption-key-differs
          [encryption-key.huv encryption-key.hul]
        ~?  !=(authentication-key.huv authentication-key.hul)
          :-  %authentication-key-differs
          [authentication-key.huv authentication-key.hul]
        ~?  !=(key-revision.huv key-revision.hul)
          :-  %key-revision-differs
          [key-revision.huv key-revision.hul]
        ~?  !=(continuity-number.huv continuity-number.hul)
          :-  %continuity-number-differs
          [continuity-number.huv continuity-number.hul]
        ~?  !=(spawn-count.huv spawn-count.hul)
          :-  %spawn-count-differs
          [spawn-count.huv spawn-count.hul]
        ~?  !=(spawned.huv spawned.hul)
          :-  %spawned-differs
          [spawned.huv spawned.hul]
        ~?  !=(sponsor.huv sponsor.hul)
          :-  %sponsor-differs
          [sponsor.huv sponsor.hul]
        ~?  !=(escape.huv escape.hul)
          :-  %escape-differs
          [escape.huv escape.hul]
        ~?  !=(spawn-proxy.huv spawn-proxy.hul)
          :-  %spawn-proxy-differs
          [spawn-proxy.huv spawn-proxy.hul]
        ~?  !=(transfer-proxy.huv transfer-proxy.hul)
          :-  %transfer-proxy-differs
          [transfer-proxy.huv transfer-proxy.hul]
        ::
        ~&  %$
        ?.  save  +>.$
        ~&  [%storing-chain-version-of who.cal]
        (make-change [0 0] %hull who.cal %full hul)
      ::
      =.  checking  (~(del by checking) who.cal)
      (read-ships wir kis)
    ::
        %dns-domains
      ?>  ?=(%s -.res.rep)
      =+  dom=(crip (decode-results p.res.rep ~[%string]))
      ?:  =(0 ind.cal)
        ?:  =(pri.dns dom)  +>.$
        ~&  [%primary-dns-differs pri.dns dom]
        ?.  save  +>.$
        (make-change [0 0] %dns 0 dom)
      ?:  =(1 ind.cal)
        ?:  =(sec.dns dom)  +>.$
        ~&  [%secondary-dns-differs sec.dns dom]
        ?.  save  +>.$
        (make-change [0 0] %dns 1 dom)
      ?:  =(2 ind.cal)
        ?:  =(ter.dns dom)  +>.$
        ~&  [%tertiary-dns-differs ter.dns dom]
        ?.  save  +>.$
        (make-change [0 0] %dns 2 dom)
      !!
    ==
  ::
  ::
  ++  da
    |=  [[block=@ud log=@ud] dis=(list diff-constitution)]
    ^+  +>
    =.  heard  (~(put in heard) block log)
    =.  latest-block  (max latest-block block)
    |^  ?~  dis  +>.^$
        =.  ..da
          =*  dif  i.dis
          ?-  -.dif
            %hull   (da-hull +.dif)
            %dns    (da-dns +.dif)
          ==
        $(dis t.dis)
    ::
    ++  da-hull
      |=  [who=@p dif=diff-hull]
      =-  ..da(ships -)
      ::  if new, first dif must be %full
      ?>  |((~(has by ships) who) ?=(%full -.dif))
      =+  old=(fall (~(get by ships) who) *complete-ship)
      ::  catch key changes, store them in the key map
      =?  keys.old  ?=(%keys -.dif)
        ~?  &((gth rev.dif 0) !(~(has by keys.old) (dec rev.dif)))
          [%missing-previous-key-rev who (dec rev.dif)]
        (~(put by keys.old) rev.dif enc.dif aut.dif)
      ::  for full, store the new keys in case we don't have them yet
      =?  keys.old  ?=(%full -.dif)
        =,  new.dif
        ~?  &((gth key-revision 0) !(~(has by keys.old) (dec key-revision)))
          [%missing-previous-key-rev who (dec key-revision)]
        %+  ~(put by keys.old)  key-revision
        [encryption-key authentication-key]
      =.  state.old     (apply-hull-diff state.old dif)
      =.  history.old   [dif history.old]
      ::  apply dif to ship state
      (~(put by ships) who old)
    ::
    ++  da-dns
      |=  [ind=@ud new=@t]
      ?:  =(0 ind)  ..da(pri.dns new)
      ?:  =(1 ind)  ..da(sec.dns new)
      ?:  =(2 ind)  ..da(ter.dns new)
      !!
    ::
    ++  da-heard
      |=  [block=@ud log=@ud]
      =-  ..da(heard har, latest-block las)
      ^-  [har=(set (pair @ud @ud)) las=@ud]
      :-  (~(put in heard) block log)
      (max latest-block block)
    --
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
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard task:able) p.q.hic)))
  ^-  {p/(list move) q/_..^$}
  =^  did  lex  abet:(~(call of [now eny] lex) hen q.hic)
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
  =*  req  q.hin
  ?-  req
      [%a %woot *]
    ?~  q.req  ~&(%coop-fine [~ ..^$])
    ?~  u.q.req  ~&(%ares-fine [~ ..^$])
    ~&  [%woot-bad p.u.u.q.req]
    ~_  q.u.u.q.req
    [~ ..^$]
  ::
      [%e %sigh *]
    ~&  [%got-sigh p.p.req]
    =^  moz  eth.lex  abet:(~(sigh et tea now eth.lex) p.req)
    [moz ..^$]
  ::
      [%b %wake ~]
    =^  moz  eth.lex  abet:~(wake et tea now eth.lex)
    [moz ..^$]
  ::
      [%j %vent *]
    ~&  %got-vent
    =^  moz  eth.lex  abet:(~(hear-vent et tea now eth.lex) p.req)
    [moz ..^$]
  ==
--
