/-  *sss
/+  *mip
::
|%
++  mk-subs                                  ::  Create sub-map.
  |*  [=(lake) paths=mold]
  -:+6:(da lake paths)
::
++  mk-pubs                                  ::  Create pub-map.
  |*  [=(lake) paths=mold]
  -:+6:(du lake paths)
::
++  mk-mar                                   ::  Create mar.
  |*  =(lake)
  |_  =(response:poke lake *)
  ++  grow
    |%
    ++  noun  response
    --
  ++  grab
    |%
    ++  noun  (response:poke lake *)
    --
  ++  grad  %noun
  --
++  fled                                     ::  Like +sped but head is a path.
  |=  vax=vase
  ^-  vase
  :_  q.vax
  %-  ~(play ut p.vax)
  =-  [%wtgr [%wtts - [%& 2]~] [%$ 1]]
  =/  pax  ~|  %path-none  ;;(path -.q.vax)
  |-  ^-  spec
  ?~  pax  [%base %null]
  [%bccl ~[[%leaf %ta -.pax] $(pax +.pax)]]
::
++  zoom  |=  =noun  ~|  %need-path  $/sss/;;(path noun)
::
++  da                                       ::  Manage subscriptions.
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  from    (on-rock:poke lake paths)
    +$  into    (response:poke lake paths)
    +$  result  (request:poke paths)
    +$  fail    [paths ship dude]
    +$  flow    [=aeon stale=_| fail=_| =rock:lake]
    +$  subs    [%0 (map [ship dude paths] (unit flow))]
    --
  |=  $:  sub=subs
          =bowl:gall
          result-type=type
          on-rock-type=type
          fail-type=type
      ==
  =>  .(sub +.sub)
  |%
  ++  surf                                   ::  Subscribe to [ship dude path].
    |=  which=[ship dude paths]
    ^-  (quip card:agent:gall subs)
    ?+  flow=(~(get by sub) which)  `0/sub
      ~                 [~[(pine which)] 0/(~(put by sub) which ~)]
      [~ ~]             [~[(pine which)] 0/sub]
      [~ ~ [* %& * *]]  [~[(scry `+(aeon.u.u.flow) which)] 0/sub]
    ==
  ++  quit  (corl (lead %0) ~(del by sub))   ::  Unsub from [ship dude path].
  ++  read                                   ::  See current subscribed states.
    ^-  (map [ship dude paths] [stale=? fail=? =rock:lake])
    %-  malt  %+  murn  ~(tap by sub)
    |=  [key=[ship dude paths] val=(unit flow)]
    ?~  val  ~
    `[key +.u.val]
  ::                                         ::  Check poke-ack for errors.
  ::                                         ::  If an %sss-on-rock poke nacks,
  ++  chit                                   ::  that state is flagged as failed.
    |=  [[aeon=term ship=term dude=term path=paths] =sign:agent:gall]
    ^-  subs
    :-  %0
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  sub
    %+  ~(jab by sub)  [(slav %p ship) dude path]
    |=  (unit flow)
    =/  =flow  (need +<)
    ?>  =(aeon.flow (slav %ud aeon))
    `flow(fail &)
  ::                                         ::  Check poke-ack for errors.
  ::                                         ::  If a scry request nacks,
  ++  tell                                   ::  that state is flagged as stale.
    |=  [[ship=term =dude aeon=term path=paths] =sign:agent:gall]
    ^-  (quip card:agent:gall subs)
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  `0/sub
    =/  current  [ship=(slav %p ship) dude=dude path=path]
    ?+    flow=(~(get by sub) current)  `0/sub
        [~ ~ *]
      =.  stale.u.u.flow  &
      :_  0/(~(put by sub) current u.flow)
      ~[(on-rock-poke current u.u.flow ~)]
    ::
        [~ ~]
      :_  0/(~(del by sub) current)  :_  ~
      :*  %pass   (zoom surf-fail/aeon/ship/dude/path)
          %agent  [our dap]:bowl
          %poke   %sss-surf-fail  fail-type  ^-  fail
          [path ship dude]:current
      ==
    ==
  ::                                         ::  Check if we're still interested
  ::                                         ::  in a wave. If no, no-op.
  ::                                         ::  If yes, scry.
  ++  behn                                   ::  (See https://gist.github.com/belisarius222/7f8452bfea9b199c0ed717ab1778f35b)
    |=  [ship=term =dude aeon=term path=paths]
    ^-  (list card:agent:gall)
    %-  fall  :_  ~  %-  mole  |.
    =/  ship  (slav %p ship)
    =/  aeon  (slav %ud aeon)
    ?:  (lte aeon aeon:(fall (~(got by sub) ship dude path) *flow))  ~
    ~[(scry `aeon ship dude path)]
  ::
  ++  apply                                  ::  Handle response from publisher.
    |=  res=(response:poke lake paths)
    ^-  (quip card:agent:gall subs)
    %-  fall  :_  `0/sub  %-  mole  |.
    =*  current  [src.bowl dude.res path.res]
    =/  old=flow  (fall (~(got by sub) current) *flow)
    ?-    type.res
        %tomb
      =/  =flow  old(stale &)
      :_  0/(~(put by sub) current `flow)  :_  ~
      (on-rock-poke current flow ~)
    ::
        %yore
      :_  0/sub  :_  ~
      (pine src.bowl dude.res path.res)
    ::
        %nigh
      :_  0/sub  :_  ~
      (behn-s25 [dude aeon path]:res)
    ::
        %scry
      =/  [wave=(unit wave:lake) =flow]
        ?-  what.res
          %rock  ?>  (gte aeon.res aeon.old)
                 [~ [aeon.res | | rock.res]]
          %wave  ?>  =(aeon.res +(aeon.old))
                 [`wave.res [aeon.res | | (wash:lake rock.old wave.res)]]
        ==
      :_  0/(~(put by sub) current `flow)
      :~  (on-rock-poke current flow wave)
          (scry `+(aeon.res) src.bowl dude.res path.res)
      ==
    ==
  ::
  ::  Non-public facing arms below
  ::
  ++  behn-s25
    |=  [=dude =aeon path=noun]
    ^-  card:agent:gall
    :*  %pass  (zoom behn/(scot %p src.bowl)^dude^(scot %ud aeon)^path)
        %arvo  %b  %wait  (add ~s25 now.bowl)
    ==
  ++  pine  |=  [ship dude paths]  (scry ~ +<)
  ++  scry
    |=  [when=(unit aeon) who=ship which=dude where=paths]
    ^-  card:agent:gall
    =/  when  ?~  when  ~  (scot %ud u.when)
    :*  %pass   (zoom scry-request/(scot %p who)^which^when^where)
        %agent  [who which]
        %poke   %sss-to-pub  :-  result-type  ^-  result
        [where dap.bowl ^when]
    ==
  ++  on-rock-poke
    |=  [[=ship =dude path=paths] flow wave=(unit wave:lake)]
    ^-  card:agent:gall
    :*  %pass   (zoom on-rock/(scot %ud aeon)^(scot %p ship)^dude^path)
        %agent  [our dap]:bowl
        %poke   %sss-on-rock  on-rock-type  ^-  from
        [path ship dude stale fail rock wave]
    ==
  --
++  du                                       ::  Manage publications.
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  into    (request:poke paths)
    +$  result  (response:poke lake paths)
    +$  rule    [rocks=_1 waves=_5]          ::  Retention policy
    +$  tide
      $:  rok=((mop aeon rock:lake) gte)
          wav=((mop aeon wave:lake) lte)
          rul=rule
          mem=(mip ship dude @da)
      ==
    +$  buoy
      $:  tid=$~(*tide $@(aeon tide))
          alo=(unit (set ship))
      ==
    +$  pubs  [%0 (map paths buoy)]
    --
  |=  [pub=pubs =bowl:gall result-type=type]
  =>  .(pub +.pub)
  =*  rok  ((on aeon rock:lake) gte)
  =*  wav  ((on aeon wave:lake) lte)
  |%
  ++  rule                                   ::  Set new retention policy.
    |=  [path=paths =^rule]
    ^-  pubs
    :-  %0
    %+  ~(jab by pub)  path
    |=  =buoy
    ?@  tid.buoy  buoy
    buoy(tid (form tid.buoy(rul rule)))
  ::
  ++  wipe                                   ::  Create new rock and wipe rest.
    |=  path=paths
    ^-  pubs
    :-  %0
    %+  ~(jab by pub)  path
    |=  =buoy
    ?@  tid.buoy  buoy
    %*  .  buoy(tid (form tid.buoy(rul [0 1])))
      rul.tid  rul.tid.buoy
      wav.tid  ~
    ==
  ++  give                                   ::  Give a wave on a path.
    |=  [path=paths =wave:lake]
    ^-  (quip card:agent:gall pubs)
    ?~  ((soft ^path) path)  ~|  %need-path  !!
    =/  buoy  (~(gut by pub) path *buoy)
    =?  buoy  ?=(@ tid.buoy)
      %*(. buoy(tid *tide) rok.tid (put:rok ~ +(tid.buoy) *rock:lake))
    ?>  ?=(^ tid.buoy)
    =*  tide  tid.buoy
    =/  next=aeon  +((latest tide))
    :-  %+  murn  ~(tap bi mem.tide)
        |=  [=ship =dude =@da]
        ?:  (lth da now.bowl)  ~
        `(send scry/wave/wave ship dude next path)
    :-  %0
    %+  ~(put by pub)  path
    =/  last=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =.  wav.tide  (put:wav wav.tide next wave)
    =.  mem.tide  ~
    ?.  =(next (add aeon.last waves.rul.tide))  buoy
    buoy(tid (form tide))
  ::
  ++  fork                                   ::  Fork a pub into an empty path.
    |=  [from=paths to=paths]
    ^-  pubs
    :-  %0
    ?<  (~(has by pub) to)
    (~(put by pub) to (~(got by pub) from))
  ::
  ++  copy                                   ::  Fork a sub into an empty path.
    |=  [sub=_(mk-subs lake *) from=[ship dude *] to=paths]
    ^-  pubs
    :-  %0
    ?<  (~(has by pub) to)
    %+  ~(put by pub)  to
    %*  .  *$<(aeon buoy)
      rok.tid  (put:rok ~ [aeon rock]:(need (~(got by +:sub) from)))
    ==
  ::
  ++  perm                                   ::  Change permissions with gate.
    |=  [where=(list paths) diff=$-((unit (set ship)) (unit (set ship)))]
    ^-  pubs
    %+  edit  where
    |=  =buoy
    =/  new=_alo.buoy  (diff alo.buoy)
    ?@  tid.buoy  buoy(alo new)
    %=  buoy
      alo      new
      mem.tid  ?~  new  mem.tid.buoy
               %.  mem.tid.buoy
               ~(int by (malt (turn ~(tap in u.new) (late *(map @ @)))))
    ==
  ++  public  (curr perm _~)                 ::  Make list of paths public.
  ++  secret  (curr perm _`~)                ::  Make list of paths secret.
  ::                                         ::  Block ships from paths.
  ++  block                                  ::  No-ops on public paths.
    |=  [who=(list ship) whence=(list paths)]
    ^-  pubs
    %+  perm  whence
    |=  old=(unit (set ship))
    ?~  old  ~  `(~(dif in u.old) (sy who))
  ::                                         ::  Allow ships to paths.
  ++  allow                                  ::  Any public paths will no-op.
    |=  [who=(list ship) where=(list paths)]
    ^-  pubs
    %+  perm  where
    |=  old=(unit (set ship))
    ?~  old  ~  `(~(gas in u.old) who)
  ::                                         ::  Kill a list of paths, i.e. tell
  ++  kill                                   ::  subs to not expect updates.
    (curr edit |=(=buoy buoy(tid (latest tid.buoy))))
  ::                                         ::  Reopen list of killed paths.
  ++  read                                   ::  See current published states.
    ^-  (map paths [allowed=(unit (set ship)) =rock:lake])
    %-  malt  %+  murn  ~(tap by pub)
    |=  [path=paths =buoy]
    ^-  (unit [paths (unit (set ship)) rock:lake])
    ?@  tide=tid.buoy  ~
    :^  ~  path  alo.buoy  =<  rock
    =/  snap=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    %+  roll  (tap:wav (lot:wav wav.tide `aeon.snap ~))
    |=  [[=aeon =wave:lake] =_snap]
    ?.  =(aeon +(aeon.snap))  snap
    [aeon (wash:lake rock.snap wave)]
  ::
  ++  apply                                  ::  Handle request from subscriber.
    |=  req=(request:poke paths)
    ^-  (quip card:agent:gall pubs)
    =/  =buoy  (~(gut by pub) path.req *buoy)
    ?<  &(?=(^ alo.buoy) !(~(has in u.alo.buoy) src.bowl))
    ?@  tid.buoy
      :_  0/pub  :_  ~
      (send tomb/~ src.bowl dude.req tid.buoy path.req)
    ?~  when.req
      =/  last  (fall (pry:rok rok.tid.buoy) *[=key =val]:rok)
      :_  0/pub  :_  ~
      (send scry/rock/val.last src.bowl dude.req key.last path.req)
    ?^  dat=(get:wav wav.tid.buoy u.when.req)
      :_  0/pub  :_  ~
      (send scry/wave/u.dat src.bowl [dude u.when path]:req)
    ?:  %+  lte  u.when.req
        key::(fall (ram:wav wav.tid.buoy) (pry:rok rok.tid.buoy) [=key val]:wav)
      :_  0/pub  :_  ~
      (send yore/~ src.bowl [dude u.when path]:req)
    ?>  =(u.when.req +((latest tid.buoy)))
    :-  ~[(send nigh/~ src.bowl [dude u.when path]:req)]
    :-  %0
    %+  ~(put by pub)  path.req
    %=  buoy
      mem.tid  (~(put bi mem.tid.buoy) src.bowl dude.req (add ~s25 now.bowl))
    ==
  ::
  ::  Non-public facing arms below
  ::
  ++  send
    |=  [payload=_|3:*(response:poke lake paths) =ship =dude =aeon path=paths]
    ^-  card:agent:gall
    =*  mark  (cat 3 %sss- name:lake)
    :*  %pass   (zoom scry-response/(scot %p ship)^dude^(scot %ud aeon)^path)
        %agent  [ship dude]
        %poke   mark  result-type  ^-  (response:poke lake paths)
        [path dap.bowl aeon payload]
    ==
  ++  latest
    |=  =$@(aeon tide)
    ^-  aeon
    ?@  tide  tide
    %+  max  (fall (bind (pry:rok rok.tide) head) 0)
    (fall (bind (ram:wav wav.tide) head) 0)
  ::
  ++  edit
    |=  [ps=(list paths) edit=$-(buoy buoy)]
    ^-  pubs
    :-  %0
    %-  ~(rep in (sy ps))
    |=  [path=paths =_pub]
    %-  fall  :_  pub  %-  mole  |.
    (~(jab by pub) path edit)
  ::
  ++  form
    |=  =tide
    ^+  tide
    =/  max-rock=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =/  max-wave  (fall (bind (ram:wav wav.tide) head) 0)
    =.  rok.tide
      %+  gas:rok  +<-:gas:rok
      %-  tab:rok  :_  [~ +(rocks.rul.tide)]
      ?:  ?|  =(waves.rul.tide 0)
              (lth max-wave (add aeon.max-rock waves.rul.tide))
          ==
        rok.tide
      %+  put:rok  rok.tide
      %+  roll  (tab:wav wav.tide `aeon.max-rock max-wave)
      |:  [*[now=aeon =wave:lake] `[prev=aeon =rock:lake]`max-rock]
      ~|  %aeon-awry
      ?>  =(now +(prev))
      [now (wash:lake rock wave)]
    ~|  %rock-zero
    tide(wav (lot:wav wav.tide (bind (ram:rok rok.tide) |=([r=@ *] (dec r))) ~))
  --
--
