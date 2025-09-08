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
      [~ ~ [* %& * *]]  [~[(pine which)] 0/sub]
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
      ~[(on-rock-poke fake=& current u.u.flow ~)]
    ::
        [~ ~]
      :_  0/(~(del by sub) current)  :_  ~
      :*  %pass   (zoom surf-fail/aeon/ship/dude/path)
          %agent  [our dap]:bowl
          %poke   %sss-surf-fail  fail-type  ^-  fail
          [path ship dude]:current
      ==
    ==
  ++  apply                                  ::  Handle response from publisher.
    |=  res=(response:poke lake paths)
    ^-  (quip card:agent:gall subs)
    =*  current  [src.bowl dude.res path.res]
    =/  old=flow  (fall (~(got by sub) current) *flow)
    ?:  ?=(%tomb what.res)
      =/  =flow  old(stale &)
      :_  0/(~(put by sub) current `flow)  :_  ~
      (on-rock-poke fake=& current flow ~)
    ::
    =/  [wave=(unit wave:lake) new=(unit flow)]
      ?-  what.res
        %rock  ?:  (lte aeon.res aeon.old)  [~ ~]
               [~ `[aeon.res | | rock.res]]
        %wave  ?:  (lte aeon.res aeon.old)  [~ ~]
               ?>  =(aeon.res +(aeon.old))
               [`wave.res `[aeon.res | | (wash:lake rock.old wave.res)]]
      ==
    ?~  new  `0/sub
    :_  0/(~(put by sub) current new)  :_  ~
    (on-rock-poke fake=& current u.new wave)
  ::
  ++  handle-fake-on-rock
    |=  =(on-rock:poke lake paths)
    ^-  (list card:agent:gall)
    ?~  flow=(~(get by sub) [src from path]:on-rock)  ~
    ?~  u.flow  ~
    ?.  =([stale fail rock]:u.u.flow [stale fail rock]:on-rock)  ~
    ~[(on-rock-poke fake=| [src from path]:on-rock u.u.flow wave.on-rock)]
  ::
  ::  Non-public facing arms below
  ::
  ++  pine
    |=  [who=ship which=dude where=paths]
    ^-  card:agent:gall
    :*  %pass   (zoom scry-request/(scot %p who)^which^where)
        %agent  [who which]
        %poke   sss-to-pub/[result-type `result`[where dap.bowl]]
    ==
  ++  on-rock-poke
    |=  [fake=? [=ship =dude path=paths] flow wave=(unit wave:lake)]
    ^-  card:agent:gall
    :*  %pass   %+  zoom  ?:(fake %fake %on-rock)
                (scot %ud aeon)^(scot %p ship)^dude^path
        %agent  [our dap]:bowl
        %poke   ?:(fake %sss-fake-on-rock %sss-on-rock)
        on-rock-type  `from`[path ship dude stale fail rock wave]
    ==
  --
++  du                                       ::  Manage publications.
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  into    (request:poke paths)
    +$  result  (response:poke lake paths)
    +$  rule    $~  [`5 5]
                [horizon=(unit @ud) frequency=@ud] ::  Retention policy
    +$  tide
      $:  rok=((mop aeon rock:lake) gte)
          wav=((mop aeon wave:lake) lte)
          rul=rule
          mem=(jug ship dude)
      ==
    +$  buoy
      $:  tid=$~(*tide $@(aeon tide))
          alo=(unit (set ship))
      ==
    ++  pubs
      =<  $>(%1 versioned)
      |%
      ++  update
        |=  =versioned
        ^-  pubs
        ?-    -.versioned
            %1  versioned
            %0
          :-  %1
          %-  ~(run by +.versioned)
          |=  =buoy-0:^versioned
          ^-  buoy
          %=    buoy-0
              tid
            ?@  tid.buoy-0  tid.buoy-0
            ^-  tide
            %=    tid.buoy-0
                rocks.rul
              ?:  =(waves.rul.tid.buoy-0 0)  ~
              `(mul [+(rocks) waves]:rul.tid.buoy-0)
            ::
                mem
              ^-  (jug ship dude)
              %-  ~(run by mem.tid.buoy-0)
              |=  =(map dude @da)
              ^-  (set dude)
              ~(key by map)
            ==
          ==
        ==
      ++  versioned
        =<  $%  [%0 (map paths buoy-0)]
                [%1 (map paths buoy)]
            ==
        |%
        +$  buoy-0
          $:  tid=$~(*tide-0 $@(aeon tide-0))
              alo=(unit (set ship))
          ==
        +$  tide-0
          $:  rok=((mop aeon rock:lake) gte)
              wav=((mop aeon wave:lake) lte)
              rul=[rocks=@ud waves=@ud]
              mem=(mip ship dude @da)
          ==
        --
      --
    --
  |=  [pub=versioned:pubs =bowl:gall result-type=type]
  =>  .(pub +:(update:pubs pub))
  =*  rok  ((on aeon rock:lake) gte)
  =*  wav  ((on aeon wave:lake) lte)
  |%
  ++  rule                                   ::  Set new retention policy.
    |=  [path=paths =^rule]
    ^-  pubs
    :-  %1
    %-  fall  :_  (~(put by pub) path %*(. *$<(aeon buoy) rul.tid rule))
    %-  mole  |.
    %+  ~(jab by pub)  path
    |=  =buoy
    ?@  tid.buoy  buoy
    buoy(tid (form tid.buoy(rul rule)))
  ::
  ++  wipe                                   ::  Create new rock and wipe rest.
    |=  path=paths
    ^-  pubs
    :-  %1
    %+  ~(jab by pub)  path
    |=  =buoy
    ?@  tid.buoy  buoy
    %*  .  buoy(tid (form tid.buoy(rul [`0 1])))
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
    :-  %-  zing
        %+  turn  ~(tap by mem.tide)
        |=  [=ship =(set dude)]
        %+  turn  ~(tap in set)
        |=  =dude
        (send wave/[next wave] ship dude path)
    :-  %1
    %+  ~(put by pub)  path
    =/  last=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =.  wav.tide  (put:wav wav.tide next wave)
    =.  mem.tide  ~
    ?.  =(next (add aeon.last frequency.rul.tide))  buoy
    buoy(tid (form tide))
  ::
  ++  fork                                   ::  Fork a pub into an empty path.
    |=  [from=paths to=paths]
    ^-  pubs
    :-  %1
    ?<  (~(has by pub) to)
    (~(put by pub) to (~(got by pub) from))
  ::
  ++  copy                                   ::  Fork a sub into an empty path.
    |=  [sub=_(mk-subs lake *) from=[ship dude *] to=paths]
    ^-  pubs
    :-  %1
    ?<  (~(has by pub) to)
    %+  ~(put by pub)  to
    %*  .  *$<(aeon buoy)
      rok.tid  (put:rok ~ [aeon rock]:(need (~(got by +:sub) from)))
    ==
  ::
  ++  perm                                   ::  Change permissions with gate.
    |=  [where=(list paths) diff=$-((unit (set ship)) (unit (set ship)))]
    ^-  (quip card:agent:gall pubs)
    %+  edit  where
    |=  =buoy
    =/  new=_alo.buoy  (diff alo.buoy)
    ?@  tid.buoy  buoy(alo new)
    %=  buoy
      alo      new
      mem.tid  ?~  new  mem.tid.buoy
               %.  mem.tid.buoy
               ~(int by (malt (turn ~(tap in u.new) (late *(set @)))))
    ==
  ++  public  (curr perm _~)                 ::  Make list of paths public.
  ++  secret  (curr perm _`~)                ::  Make list of paths secret.
  ::                                         ::  Block ships from paths.
  ++  block                                  ::  No-ops on public paths.
    |=  [who=(list ship) whence=(list paths)]
    ^-  (quip card:agent:gall pubs)
    %+  perm  whence
    |=  old=(unit (set ship))
    ?~  old  ~  `(~(dif in u.old) (sy who))
  ::                                         ::  Allow ships to paths.
  ++  allow                                  ::  Any public paths will no-op.
    |=  [who=(list ship) where=(list paths)]
    ^-  (quip card:agent:gall pubs)
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
      :_  1/pub  :_  ~
      (send tomb/~ src.bowl dude.req path.req)
    :_  1/pub  :_  ~
    =/  last  (fall (pry:rok rok.tid.buoy) *[=key =val]:rok)
    (send rock/last src.bowl dude.req path.req)
  ::
  ++  tell
    |=  [[ship=term =dude aeon=term path=paths] =sign:agent:gall]
    ^-  (quip card:agent:gall pubs)
    ?>  ?=(%poke-ack -.sign)
    ?^  p.sign  `1/pub
    =/  =buoy  (~(gut by pub) path *buoy)
    ?<  &(?=(^ alo.buoy) !(~(has in u.alo.buoy) src.bowl))
    ?@  tid.buoy
      :_  1/pub  :_  ~
      (send tomb/~ src.bowl dude path)
    ::
    =>  .(aeon +((slav %ud aeon)))
    ?^  dat=(get:wav wav.tid.buoy aeon)
      :_  1/pub  :_  ~
      (send wave/[aeon u.dat] src.bowl dude path)
    =/  last  (fall (pry:rok rok.tid.buoy) [=key =val]:rok)
    ?:  (lte aeon key.last)
      :_  1/pub  :_  ~
      (send rock/last src.bowl dude path)
    :-  ~
    :-  %1
    %+  ~(put by pub)  path
    %=  buoy
      mem.tid  (~(put ju mem.tid.buoy) src.bowl dude)
    ==
  ::
  ::  Non-public facing arms below
  ::
  ++  send
    |=  [payload=_|2:*(response:poke lake paths) =ship =dude path=paths]
    ^-  card:agent:gall
    =*  mark  (cat 3 %sss- name:lake)
    =/  callback=^path
      ?:  ?=(%tomb what.payload)  (zoom tomb-response/(scot %p ship)^dude^path)
      (zoom scry-response/(scot %p ship)^dude^(scot %ud aeon.payload)^path)
    :*  %pass   callback
        %agent  [ship dude]
        %poke   mark  result-type  ^-  (response:poke lake paths)
        [path dap.bowl payload]
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
    ^-  (quip card:agent:gall pubs)
    %-  ~(rep in (sy ps))
    |=  [path=paths caz=(list card:agent:gall) %1 =_pub]
    ?~  old=(~(get by pub) path)  [caz 1/pub]
    =/  new=buoy  (edit u.old)
    :_  1/(~(put by pub) path new)
    %-  weld  :_  caz
    ^-  (list card:agent:gall)
    ?@  tid.u.old  ~
    ?@  tid.new
      %-  zing
      %+  turn  ~(tap by mem.tid.u.old)
      |=  [=ship =(set dude)]
      (turn ~(tap in set) |=(=dude (send tomb/~ ship dude path)))
    ?~  alo.new  ~
    =/  new-alo=(jug ship dude)
      (malt (turn ~(tap in u.alo.new) (late *(set @))))
    %-  zing
    %+  turn  ~(tap by (~(dif by mem.tid.u.old) new-alo))
    |=  [=ship =(set dude)]
    (turn ~(tap in set) |=(=dude (send tomb/~ ship dude path)))
  ::
  ++  form
    |=  =tide
    ^+  tide
    =/  max-rock=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    =/  max-wave  (fall (bind (ram:wav wav.tide) head) 0)
    =?    rok.tide                        ::  Create new rock.
        ?&  !=(frequency.rul.tide 0)
            (gte max-wave (add aeon.max-rock frequency.rul.tide))
        ==
      %+  put:rok  rok.tide
      %+  roll  (tab:wav wav.tide `aeon.max-rock max-wave)
      |:  [*[now=aeon =wave:lake] `[prev=aeon =rock:lake]`max-rock]
      ~|  %aeon-awry
      ?>  =(now +(prev))
      [now (wash:lake rock wave)]
    =.  rok.tide
      ?~  horizon.rul.tide                ::  Only keep genesis and latest.
        (gas:rok ~ (murn ~[(ram:rok rok.tide) (pry:rok rok.tide)] same))
      %^  lot:rok  rok.tide               ::  Delete beyond horizon.
        ~
      (mole |.((sub max-wave (max [u.horizon frequency]:rul.tide))))
    ~|  %rock-zero
    tide(wav (lot:wav wav.tide (bind (ram:rok rok.tide) |=([r=@ *] (dec r))) ~))
  --
--
