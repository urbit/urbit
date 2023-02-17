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
    +$  flow  [=aeon fail=_| =rock:lake]
    --
  |_  [sub=(map [ship dude paths] flow) =bowl:gall result-type=type on-rock-type=type]
  ++  surf  pine                             ::  Subscribe to [ship dude path].
  ++  read                                   ::  See current subscribed states.
    ^-  (map [ship dude paths] [fail=? rock:lake])
    %-  ~(run by sub)
    |=  =flow
    [fail rock]:flow
  ::                                         ::  Check poke-acks for errors.
  ::                                         ::  If an %sss-on-rock poke nacks,
  ++  chit                                   ::  that state is flagged as failed.
    |=  [[aeon=term ship=term dude=term path=paths] =sign:agent:gall]
    ^+  sub
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  sub
    %+  ~(jab by sub)  [(slav %p ship) dude path]
    |=  =flow
    ?>  =(aeon.flow (slav %ud aeon))
    flow(fail &)
  ::                                         ::  Check if we're still interested
  ::                                         ::  in a wave. If no, no-op.
  ::                                         ::  If yes, scry.
  ++  behn                                   ::  (See https://gist.github.com/belisarius222/7f8452bfea9b199c0ed717ab1778f35b)
    |=  [ship=term =dude aeon=term path=paths]
    ^-  (list card:agent:gall)
    =/  ship  (slav %p ship)
    =/  aeon  (slav %ud aeon)
    ?:  (lte aeon aeon:(~(got by sub) ship dude path))  ~
    ~[(scry `aeon ship dude path)]
  ::
  ++  apply                                  ::  Handle response from publisher.
    |=  res=(response:poke lake paths)
    ^-  (quip card:agent:gall _sub)
    ?-    type.res
        %yore
      :_  sub  :_  ~
      (pine src.bowl dude.res path.res)
    ::
        %nigh
      :_  sub  :_  ~
      (behn-s25 [dude aeon path]:res)
    ::
        %scry
      =*  current  [src.bowl dude.res path.res]
      =/  [wave=(unit wave:lake) =flow]
        =/  old=flow  (~(gut by sub) current *flow)
        ?-  what.res
          %rock  ?>  (gte aeon.res aeon.old)
                 `[aeon.res | rock.res]
          %wave  ~|  [%weird-wave res=res old=old]
                 ?>  =(aeon.res +(aeon.old))
                 [`wave.res [aeon.res | (wash:lake rock.old wave.res)]]
        ==
      :_  (~(put by sub) current flow)
      %-  flop
      :~  (scry `+(aeon.res) src.bowl dude.res path.res)
          :*  %pass   (zoom on-rock/(scot %ud aeon.flow)^(scot %p src.bowl)^dude.res^path.res)
              %agent  [our dap]:bowl
              %poke   %sss-on-rock  on-rock-type  ^-  from
              [path.res src.bowl dude.res rock.flow wave]
      ==  ==
    ==
  ::
  ::  Non-public facing arms below
  ::
  +$  from    (on-rock:poke lake paths)
  +$  into    (response:poke lake paths)
  +$  result  (request:poke paths)
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
    =/  when  ?~  when  %~  (scot %ud u.when)
    :*  %pass   (zoom request/scry/(scot %p who)^which^when^where)
        %agent  [who which]
        %poke   %sss-to-pub  :-  result-type  ^-  result
        [where which ^when]
    ==
  --
++  du                                       ::  Manage publications.
  |*  [=(lake) paths=mold]
  =>
    |%
    +$  rule  [rocks=_1 waves=_5]            ::  Retention policy
    +$  tide
      $:  rok=((mop aeon rock:lake) gte)
          wav=((mop aeon wave:lake) lte)
          rul=rule
          mem=(mip aeon [ship dude] @da)
      ==
    --
  |_  [pub=(map paths tide) =bowl:gall result-type=type]
  +*  rok  ((on aeon rock:lake) gte)
      wav  ((on aeon wave:lake) lte)
  ::
  ++  rule                                   ::  Set new retention policy.
    |=  [path=paths =^rule]
    ^+  pub
    %+  ~(jab by pub)  path
    |=  =tide
    (form tide(rul rule))
  ::
  ++  wipe                                   ::  Create new rock and wipe rest.
    |=  path=paths
    ^+  pub
    %+  ~(jab by pub)  path
    |=  =tide
    %*  .  (form tide(rul [0 1]))
      rul  rul.tide
      wav  ~
    ==
  ++  give                                   ::  Give a wave on a path.
    |=  [path=paths =wave:lake]
    ^-  (quip card:agent:gall _pub)
    ?~  ;;((soft ^path) path)  ~|  %need-path  !!
    =/  =tide  (~(gut by pub) path *tide)
    =/  next=aeon
      .+  %+  max
        (fall (bind (pry:rok rok.tide) head) 0)
      (fall (bind (ram:wav wav.tide) head) 0)
    ::
    :_  %+  ~(put by pub)  path
        =/  last=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
        =.  wav.tide  (put:wav wav.tide next wave)
        =.  mem.tide  (~(del by mem.tide) next)
        ?.  =(next (add aeon.last waves.rul.tide))  tide
        (form tide)
    ::
    %+  murn  ~(tap by (~(gut by mem.tide) next ~))
    |=  [[=ship =dude] =@da]
    ?:  (lth da now.bowl)  ~
    `(send scry/wave/wave ship dude next path)
  ++  read                                   ::  See current published states.
    ^-  (map paths rock:lake)
    %-  ~(run by pub)
    |=  =tide
    =<  rock
    =/  snap=[=aeon =rock:lake]  (fall (pry:rok rok.tide) *[key val]:rok)
    %+  roll  (tap:wav (lot:wav wav.tide `aeon.snap ~))
    |=  [[=aeon =wave:lake] =_snap]
    ?.  =(aeon +(aeon.snap))  snap
    [aeon (wash:lake rock.snap wave)]
  ::
  ++  apply                                  ::  Handle request from subscriber.
    |=  req=(request:poke paths)
    ^-  (quip card:agent:gall _pub)
    =/  =tide  (~(gut by pub) path.req *tide)
    ?~  when.req
      =/  last  (fall (pry:rok rok.tide) *[=key =val]:rok)
      :_  pub  :_  ~
      (send scry/rock/val.last src.bowl dude.req key.last path.req)
    ?^  dat=(get:wav wav.tide u.when.req)
      :_  pub  :_  ~
      (send scry/wave/u.dat src.bowl [dude u.when path]:req)
    ?.  (gth u.when.req key::(fall (ram:wav wav.tide) [key=+(u.when.req) **]))
      :_  pub  :_  ~
      (send yore/~ src.bowl [dude u.when path]:req)
    :-  ~[(send nigh/~ src.bowl [dude u.when path]:req)]
    %+  ~(put by pub)  path.req
    %=  tide  mem
      %^  ~(put bi mem.tide)  u.when.req  [src.bowl dude.req]
      (add ~s25 now.bowl)
    ==
  ::
  ::  Non-public facing arms below
  ::
  +$  into    (request:poke paths)
  +$  result  (response:poke lake paths)
  ++  send
    |=  [payload=_|3:*(response:poke lake paths) =ship =dude =aeon path=paths]
    ^-  card:agent:gall
    =*  mark  (cat 3 %sss- name:lake)
    :*  %pass   (zoom response/scry/(scot %p ship)^dude^(scot %ud aeon)^path)
        %agent  [ship dude]
        %poke   mark  result-type  ^-  (response:poke lake paths)
        [path dap.bowl aeon payload]
    ==
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
