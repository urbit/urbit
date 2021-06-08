/+  version
=,  clay
=,  space:userlib
=,  format
|%
+$  state  state-2
+$  state-2  [%2 pith-2]
+$  state-1  [%1 pith-1]
+$  state-0  [%0 pith-0]
+$  any-state
  $%  state-2
      state-1
      state-0
  ==
+$  pith-2                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ota=(unit [=ship =desk =aeon])                    ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
      ::  map desk to the currently ongoing fuse request
      ::  and the latest version numbers for beaks to
      fus=(map desk per-fuse)
      ::  used for fuses - every time we get a fuse we
      ::  bump this. used when calculating hashes to
      ::  ensure they're unique even when the same
      ::  request is made multiple times.
      hxs=(map desk @ud)
  ==                                                    ::
+$  pith-1                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ota=(unit [=ship =desk =aeon])                    ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
  ==                                                    ::
+$  pith-0                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      autoload-on=?                                     ::
      cur-hoon=@uvI                                     ::
      cur-arvo=@uvI                                     ::
      cur-zuse=@uvI                                     ::
      cur-vanes=(map @tas @uvI)                         ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
  ==
+$  per-desk                                            ::  per-desk state
  $:  auto=?                                            ::  escalate on failure
      gem=?(%this %that germ)                           ::  strategy
      her=@p                                            ::  from ship
      sud=@tas                                          ::  from desk
      cas=case                                          ::  at case
  ==
+$  per-fuse                                            ::  per fuse state
      ::  map [ship desk] to latest version number we
      ::  have for them. used for things we're %trak-ing
      ::  our invariant here is to store the latest version
      ::  number we've heard of.
  $:  mox=(map [ship desk] let=@ud)
      ::  relevant parts of originating request
      kf=kiln-fuse-data
  ==
+$  kiln-commit  term                                   ::
+$  kiln-mount                                          ::
  $:  pax=path                                          ::
      pot=term                                          ::
  ==
+$  kiln-unmount  $@(term [knot path])                  ::
+$  kiln-sync                                           ::
  $:  syd=desk                                          ::
      her=ship                                          ::
      sud=desk                                          ::
  ==
+$  kiln-unsync                                         ::
  $:  syd=desk                                          ::
      her=ship                                          ::
      sud=desk                                          ::
  ==
+$  kiln-merge                                          ::
  $@  ~
  $:  syd=desk                                          ::
      ali=ship                                          ::
      sud=desk                                          ::
      cas=case                                          ::
      gim=?(%auto germ)                                 ::
  ==
+$  fuse-source  [who=ship des=desk ver=$@(%trak case)]
::  actual poke
+$  kiln-fuse
  $@  ~
  $:  syd=desk
      $@  ~  :: signifies clearing the fuse
      $:  overwrite=flag  :: force overwrite previous fuse
          bas=fuse-source
          con=(list [fuse-source germ])
      ==
  ==
::  state tracked by kiln
+$  kiln-fuse-data
  $:  syd=desk
      bas=fuse-source
      con=(list [fuse-source germ])
  ==
::  Request to list current fuses. ~ means "list all"
::
+$  kiln-fuse-list  (unit desk)
--
|=  [bowl:gall state]
?>  =(src our)
|_  moz=(list card:agent:gall)
+$  state      ^state      ::  proxy
+$  any-state  ^any-state  ::  proxy
++  abet                                                ::  resolve
  [(flop moz) `state`+<+.$]
::
++  emit
  |=  card:agent:gall
  %_(+> moz [+< moz])
::
++  emil                                              ::  return cards
  |=  (list card:agent:gall)
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  render
  |=  [mez=tape sud=desk who=ship syd=desk]
  :^  %palm  [" " ~ ~ ~]  leaf+(weld "kiln: " mez)
  ~[leaf+"from {<sud>}" leaf+"on {<who>}" leaf+"to {<syd>}"]
::
++  on-load
  =>
  |%
  ++  state-1-to-2
    |=  s=state-1
    ^-  state-2
    =/  p=pith-1  +.s
    :-  %2
    [rem.p syn.p ota.p commit-timer.p *(map desk per-fuse) *(map desk @ud)]
  --
  |=  [hood-version=@ud old=any-state]
  =<  abet
  =?  .  ?=(%0 -.old)
    =/  recognized-ota=(unit [syd=desk her=ship sud=desk])
      =/  syncs=(list [[syd=desk her=ship sud=desk] =aeon])
        ~(tap by syn.old)
      |-  ^-  (unit [syd=desk her=ship sud=desk])
      ?~  syncs
        ~
      ?:  &(=(%base syd.i.syncs) !=(our her.i.syncs) =(%kids sud.i.syncs))
        `[syd her sud]:i.syncs
      $(syncs t.syncs)
    =.  +<+.$.abet
      %-  state-1-to-2
      =-  old(- %1, |3 [ota=~ commit-timer.old], syn -)
      ?~  recognized-ota
        syn
      (~(del by syn) [syd her sud]:u.recognized-ota)
    ::
    =?  ..abet  ?=(^ recognized-ota)
      (poke-internal:update `[her sud]:u.recognized-ota)
    +(old +<+.$.abet)
  ::
  =?  old  ?=(%1 -.old)  (state-1-to-2 old)
  ?>  ?=(%2 -.old)
  =.  +<+.$.abet  old
  ..abet
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  [~ ~]
      [%x %kiln %ota ~]        ``noun+!>(ota)
      [%x %kiln %our ~]        ``noun+!>(our)
      [%x %kiln %base-hash ~]
    =/  ver  (base-hash:version our now)
    ``noun+!>(?~(ver 0v0 i.ver))
  ==
::
++  poke-commit
  |=  [mon=kiln-commit auto=?]
  =<  abet
  =.  +>.$  (emit %pass /commit %arvo %c [%dirk mon])
  ?.  auto
    +>.$
  =/  recur  ~s1
  =.  commit-timer
    [/kiln/autocommit (add now recur) recur mon]
    (emit %pass way.commit-timer %arvo %b [%wait nex.commit-timer])
::
++  poke-autocommit
  |=  [mon=kiln-commit auto=?]
  =<  abet
  =.  +>.$  (emit %pass /commit %arvo %c [%dirk mon])
  ?.  auto
    +>.$
  =/  recur  ~s1
  =.  commit-timer
    [/kiln/autocommit (add now recur) recur mon]
  (emit %pass way.commit-timer %arvo %b [%wait nex.commit-timer])
::
++  poke-cancel-autocommit
  |=  ~
  abet:(emit %pass way.commit-timer %arvo %b [%rest nex.commit-timer])
::
++  poke-mount
  |=  kiln-mount
  =+  bem=(de-beam pax)
  ?~  bem
    =+  "can't mount bad path: {<pax>}"
    abet:(spam leaf+- ~)
  abet:(emit %pass /mount %arvo %c [%mont pot u.bem])
::
++  poke-unmount
  |=  mon=kiln-unmount
  ?^  mon
    =+  bem=(de-beam mon)
    ?~  bem
      =+  "can't unmount bad path: {<mon>}"
      abet:(spam leaf+- ~)
    abet:(emit %pass /unmount-beam %arvo %c [%ogre [[p q r] s]:u.bem])
  abet:(emit %pass /unmount-point %arvo %c [%ogre mon])
::
++  poke-track                                        ::
  |=  hos=kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already tracking" [sud her syd]:hos) ~)
  abet:abet:start-track:(auto hos)
::
++  update
  |%
  ++  make-wire
    |=  =path
    ?>  ?=(^ ota)
    %-  welp
    :_  path
    /kiln/ota/(scot %p ship.u.ota)/[desk.u.ota]/(scot %ud aeon.u.ota)
  ::
  ++  check-ota
    |=  =wire
    ?~  ota
      |
    ?&  ?=([@ @ @ *] wire)
        =(i.wire (scot %p ship.u.ota))
        =(i.t.wire desk.u.ota)
        =(i.t.t.wire (scot %ud aeon.u.ota))
    ==
  ::
  ++  render
    |=  [mez=tape error=(unit (pair term tang))]
    %+  spam
      ?~  ota
        leaf+mez
      :^  %palm  [" " ~ ~ ~]  leaf+(weld "kiln: " mez)
      ~[leaf+"from {<desk.u.ota>}" leaf+"on {<ship.u.ota>}"]
    ?~  error
      ~
    [>p.u.error< q.u.error]
  ::
  ++  render-ket
    |=  [mez=tape error=(unit (pair term tang))]
    ?>  ?=(^ ota)
    =<  ?>(?=(^ ota) .)
    %+  spam
      :^  %palm  [" " ~ ~ ~]  leaf+(weld "kiln: " mez)
      ~[leaf+"from {<desk.u.ota>}" leaf+"on {<ship.u.ota>}"]
    ?~  error
      ~
    [>p.u.error< q.u.error]
  ::
  ::  If destination desk doesn't exist, need a %init merge.  If this is
  ::  its first revision, it probably doesn't have a mergebase yet, so
  ::  use %take-that.
  ::
  ++  get-germ
    |=  =desk
    =+  .^(=cass:clay %cw /(scot %p our)/[desk]/(scot %da now))
    ?-  ud.cass
      %0  %init
      %1  %take-that
      *   %mate
    ==
  ::
  ++  poke
    |=  arg=(unit [=ship =desk])
    abet:(poke-internal arg)
  ::
  ++  poke-internal
    |=  arg=(unit [=ship =desk])
    ^+  ..abet
    =?  ..abet  =(arg (bind ota |=([=ship =desk =aeon] [ship desk])))
      (render "restarting OTA sync" ~)
    =?  ..abet  ?=(^ ota)
      =.  ..abet  (render-ket "cancelling OTA sync" ~)
      ..abet(ota ~)
    ?~  arg
      ..abet
    =.  ota  `[ship.u.arg desk.u.arg *aeon]
    =.  ..abet  (render "starting OTA sync" ~)
    %:  emit
      %pass  (make-wire /find)  %arvo  %c
      %warp  ship.u.arg  desk.u.arg  `[%sing %y ud+1 /]
    ==
  ::
  ++  take
    |=  [=wire =sign-arvo]
    ^+  ..abet
    ?>  ?=(^ ota)
    ?.  (check-ota wire)
      ..abet
    ?.  ?=([@ @ @ @ *] wire)
      ..abet
    ?+  i.t.t.t.wire  ~&([%strange-ota-take t.t.t.wire] ..abet)
      %find        (take-find sign-arvo)
      %sync        (take-sync sign-arvo)
      %download    (take-download sign-arvo)
      %merge-home  (take-merge-home sign-arvo)
      %merge-kids  (take-merge-kids sign-arvo)
    ==
  ::
  ++  take-find
    |=  =sign-arvo
    ?>  ?=(%writ +<.sign-arvo)
    ?>  ?=(^ ota)
    =.  ..abet  (render-ket "activated OTA" ~)
    %:  emit
      %pass  (make-wire /sync)  %arvo  %c
      %warp  ship.u.ota  desk.u.ota  `[%sing %w da+now /]
    ==
  ::
  ++  take-sync
    |=  =sign-arvo
    ?>  ?=(%writ +<.sign-arvo)
    ?>  ?=(^ ota)
    ?~  p.sign-arvo
      =.  ..abet  (render-ket "OTA cancelled (1), retrying" ~)
      (poke-internal `[ship desk]:u.ota)
    =.  ..abet  (render-ket "downloading OTA update" ~)
    =?  aeon.u.ota  ?=(%w p.p.u.p.sign-arvo)
      ud:;;(cass:clay q.q.r.u.p.sign-arvo)
    %:  emit
      %pass  (make-wire /download)  %arvo  %c
      %warp  ship.u.ota  desk.u.ota  `[%sing %v ud+aeon.u.ota /]
    ==
  ::
  ++  take-download
    |=  =sign-arvo
    ^+  ..abet
    ?>  ?=(%writ +<.sign-arvo)
    ?>  ?=(^ ota)
    ?~  p.sign-arvo
      =.  ..abet  (render-ket "OTA cancelled (2), retrying" ~)
      (poke-internal `[ship desk]:u.ota)
    =.  ..abet  (render-ket "finished downloading OTA" ~)
    =.  aeon.u.ota  +(aeon.u.ota)
    =/  =germ  (get-germ %home)
    =.  ..abet  (render-ket "applying OTA to %home" ~)
    %-  emil
    :~  :*  %pass  (make-wire /merge-home)  %arvo  %c
            %merg  %home  ship.u.ota  desk.u.ota  ud+(dec aeon.u.ota)  germ
        ==
        :*  %pass  (make-wire /sync)  %arvo  %c
            %warp  ship.u.ota  desk.u.ota  `[%sing %z ud+aeon.u.ota /]
        ==
    ==
  ::
  ++  take-merge-home
    |=  =sign-arvo
    ?>  ?=(%mere +<.sign-arvo)
    ?>  ?=(^ ota)
    ?:  ?=([%| %ali-unavailable *] p.sign-arvo)
      =.  ..abet
        =/  =tape  "OTA to %home failed, maybe because sunk; restarting"
        (render-ket tape `p.p.sign-arvo)
      (poke-internal `[ship desk]:u.ota)
    ::
    ?:  ?=(%| -.p.sign-arvo)
      =/  =tape  "OTA to %home failed, waiting for next revision"
      (render-ket tape `p.p.sign-arvo)
    =.  ..abet  (render-ket "OTA to %home succeeded" ~)
    =.  ..abet  (render-ket "applying OTA to %kids" ~)
    =/  =germ  (get-germ %kids)
    %:  emit
      %pass  (make-wire /merge-kids)  %arvo  %c
      %merg  %kids  ship.u.ota  desk.u.ota  ud+(dec aeon.u.ota)  germ
    ==
  ::
  ++  take-merge-kids
    |=  =sign-arvo
    ?>  ?=(%mere +<.sign-arvo)
    ?>  ?=(^ ota)
    ?:  ?=([%| %ali-unavailable *] p.sign-arvo)
      =.  ..abet
        =/  =tape  "OTA to %kids failed, maybe because sunk; restarting"
        (render-ket tape `p.p.sign-arvo)
      (poke-internal `[ship desk]:u.ota)
    ::
    ?-  -.p.sign-arvo
      %&  (render-ket "OTA to %kids succeeded" ~)
      %|  (render-ket "OTA to %kids failed" `p.p.sign-arvo)
    ==
  --
::
++  poke-sync                                         ::
  |=  hos=kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already syncing" [sud her syd]:hos) ~)
  abet:abet:start-sync:(auto hos)
::
++  ota-info
  ?~  ota
    "OTAs disabled"
  "OTAs enabled from {<desk.u.ota>} on {<ship.u.ota>}"
::
++  poke-ota-info
  |=  *
  =<  abet  %-  spam
  :~  [%leaf ota-info]
      [%leaf "use |ota %disable or |ota ~sponsor %kids to reset it"]
  ==
::
++  poke-syncs                                        ::  print sync config
  |=  ~
  =<  abet  %-  spam
  :-  [%leaf ota-info]
  ?:  =(0 ~(wyt by syn))
    [%leaf "no other syncs configured"]~
  %+  turn  ~(tap in ~(key by syn))
  |=(a=kiln-sync (render "sync configured" [sud her syd]:a))
::
++  poke-unsync                                         ::
  |=  hus=kiln-unsync
  ?.  (~(has by syn) hus)
    abet:(spam (render "not syncing" [sud her syd]:hus) ~)
  %*  .  abet:abet:stop:(auto hus)
    syn  (~(del by syn) hus)
  ==
::
++  poke-merge                                        ::
  |=  kiln-merge
  ?~  +<  abet
  abet:abet:(merge:(work syd) ali sud cas gim)
::
++  poke-fuse-list
  =>
  |%
  ++  format-fuse
    |=  [into=desk pf=per-fuse]
    ^-  tank
    =/  sources=tape
        %+  reel
          con.kf.pf
        |=  [[fs=fuse-source g=germ] acc=tape]
        ^-  tape
        ;:  weld
          " ["
          (format-fuse-source fs)
          " "
          <g>
          "]"
          acc
        ==
    :-  %leaf
    ;:  weld
      "|fuse {<into>} "
      (format-fuse-source bas.kf.pf)
      sources
    ==
  ::  +format-fuse-source: fuse source -> beak -> path
  ::
  ++  format-fuse-source
    |=  fs=fuse-source
    ^-  tape
    =/  bec=beak  [who.fs des.fs ?:(?=([%trak] ver.fs) [%tas %track] ver.fs)]
    <(en-beam [bec /])>
  --
  |=  k=kiln-fuse-list
  ^+  abet
  %.  abet
  ?~  k
    ?~  fus
      (slog [leaf+"no ongoing fuses" ~])
    %-  slog
    %+  roll
      ~(tap by `(map desk per-fuse)`fus)
    |=  [[syd=desk pf=per-fuse] acc=tang]
    ^-  tang
    [(format-fuse syd pf) acc]
  =/  pfu=(unit per-fuse)  (~(get by fus) u.k)
  ?~  pfu
    (slog [leaf+"no ongoing fuse for {<u.k>}" ~])
  (slog [(format-fuse u.k u.pfu) ~])
::
++  poke-fuse
  |=  k=kiln-fuse
  ?~  k  abet
  =/  payload  +.k
  ?~  payload
    ::  cancelling an ongoing fuse
    %-  (slog [leaf+"cancelling fuse into {<syd.k>}" ~])
    =/  f  (fuzz syd.k now)
    ?~  f
      abet
    abet:abet:delete:u.f
  ?:  &(!overwrite.payload (~(has by fus) syd.k))
    ((slog [leaf+"existing fuse into {<syd.k>} - need =overwrite &" ~]) abet)
  =.  fus  (~(put by fus) syd.k [~ [syd.k bas.payload con.payload]])
  =/  old-cnt=@ud  (~(gut by hxs) syd.k 0)
  =.  hxs  (~(put by hxs) syd.k +(old-cnt))
  =/  f  (fuzz syd.k now)
  ?~  f
    abet
  abet:abet:fuse:u.f
::
++  poke-cancel
  |=  a=@tas
  abet:(emit %pass /cancel %arvo %c [%drop a])
::
++  poke-info
  |=  [mez=tape tor=(unit toro)]
  ?~  tor
    abet:(spam leaf+mez ~)
  abet:(emit:(spam leaf+mez ~) %pass /kiln %arvo %c [%info u.tor])
::
++  poke-rm
  |=  a=path
  =+  b=.^(arch %cy a)
  ?~  fil.b
    =+  ~[leaf+"No such file:" leaf+"{<a>}"]
    abet:(spam -)
  (poke-info "removed" `(fray a))
::
++  poke-label
  |=  [syd=desk lab=@tas]
  =+  pax=/(scot %p our)/[syd]/[lab]
  (poke-info "labeled {(spud pax)}" `[syd %| lab])
::
++  poke-schedule
  |=  [where=path tym=@da eve=@t]
  =.  where  (welp where /sched)
  %+  poke-info  "scheduled"
  =+  old=;;((map @da cord) (fall (file where) ~))
  `(foal where %sched !>((~(put by old) tym eve)))
::
++  poke-permission
  |=  [syd=desk pax=path pub=?]
  =<  abet
  %-  emit
  =/  =rite  [%r ~ ?:(pub %black %white) ~]
  [%pass /kiln/permission %arvo %c [%perm syd pax rite]]
::
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-kiln-bad-mark mark] !!)
    %kiln-autocommit         =;(f (f !<(_+<.f vase)) poke-autocommit)
    %kiln-cancel             =;(f (f !<(_+<.f vase)) poke-cancel)
    %kiln-cancel-autocommit  =;(f (f !<(_+<.f vase)) poke-cancel-autocommit)
    %kiln-commit             =;(f (f !<(_+<.f vase)) poke-commit)
    %kiln-gall-sear          =;(f (f !<(_+<.f vase)) poke-gall-sear)
    %kiln-goad-gall          =;(f (f !<(_+<.f vase)) poke-goad-gall)
    %kiln-info               =;(f (f !<(_+<.f vase)) poke-info)
    %kiln-label              =;(f (f !<(_+<.f vase)) poke-label)
    %kiln-merge              =;(f (f !<(_+<.f vase)) poke-merge)
    %kiln-fuse               =;(f (f !<(_+<.f vase)) poke-fuse)
    %kiln-fuse-list          =;(f (f !<(_+<.f vase)) poke-fuse-list)
    %kiln-mount              =;(f (f !<(_+<.f vase)) poke-mount)
    %kiln-ota                =;(f (f !<(_+<.f vase)) poke:update)
    %kiln-ota-info           =;(f (f !<(_+<.f vase)) poke-ota-info)
    %kiln-permission         =;(f (f !<(_+<.f vase)) poke-permission)
    %kiln-rm                 =;(f (f !<(_+<.f vase)) poke-rm)
    %kiln-schedule           =;(f (f !<(_+<.f vase)) poke-schedule)
    %kiln-sync               =;(f (f !<(_+<.f vase)) poke-sync)
    %kiln-syncs              =;(f (f !<(_+<.f vase)) poke-syncs)
    %kiln-track              =;(f (f !<(_+<.f vase)) poke-track)
    %kiln-unmount            =;(f (f !<(_+<.f vase)) poke-unmount)
    %kiln-unsync             =;(f (f !<(_+<.f vase)) poke-unsync)
  ==
::
++  poke-goad-gall
  |=  [force=? agent=(unit dude:gall)]
  abet:(emit %pass /kiln %arvo %g %goad force agent)
::
++  poke-gall-sear
  |=  =ship
  abet:(emit %pass /kiln %arvo %g %sear ship)
::
++  done
  |=  [way=wire saw=(unit error:ames)]
  ~?  ?=(^ saw)  [%kiln-nack u.saw]
  abet
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+  wire  ~|([%kiln-bad-take-agent wire -.sign] !!)
    [%kiln %fancy *]   ?>  ?=(%poke-ack -.sign)
                       (take-coup-fancy t.t.wire p.sign)
    [%kiln %spam *]    ?>  ?=(%poke-ack -.sign)
                       (take-coup-spam t.t.wire p.sign)
  ==
::
++  take-arvo
  |=  [=wire =sign-arvo]
  ?-  wire
      [%sync %merg *]   %+  take-mere-sync  t.t.wire
                        ?>(?=(%mere +<.sign-arvo) +>.sign-arvo)
      [%find-ship *]    %+  take-writ-find-ship  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%sync *]         %+  take-writ-sync  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%autocommit *]   %+  take-wake-autocommit  t.wire
                        ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
      [%ota *]          abet:(take:update t.wire sign-arvo)
      [%fuse-request @tas *]
                      =/  f  (fuzz i.t.wire now)
                      ?~  f
                        abet
                      abet:abet:(take:u.f t.t.wire sign-arvo)
      [%fuse @tas *]  ?>  ?=(%mere +<.sign-arvo)
                      =/  syd=desk  i.t.wire
                      ?.  ?=([%| *] +>.sign-arvo)
                        ?~  p.p.sign-arvo
                          abet
                        =/  msg=tape  "fuse merge conflict for {<syd>}"
                        %-  (slog [leaf+msg >p.p.sign-arvo< ~])
                        abet
                      %-  (slog leaf+"failed fuse for {<syd>}" p.p.sign-arvo)
                      abet
      *
    ?+    +<.sign-arvo
        ((slog leaf+"kiln: strange card {<+<.sign-arvo wire>}" ~) abet)
      %done  %+  done  wire
             ?>(?=(%done +<.sign-arvo) +>.sign-arvo)
      %mere  %+  take-mere  wire
             ?>(?=(%mere +<.sign-arvo) +>.sign-arvo)
    ==
  ==
++  take  |=(way=wire ?>(?=([@ ~] way) (work i.way))) ::  general handler
++  take-mere                                         ::
  |=  [way=wire are=(each (set path) (pair term tang))]
  ?.  ?=([@ ~] way)
    abet
  abet:abet:(mere:(take way) are)
::
++  take-coup-fancy                                   ::
  |=  [way=wire saw=(unit tang)]
  abet:abet:(coup-fancy:(take way) saw)
::
++  take-coup-spam                                    ::
  |=  [way=wire saw=(unit tang)]
  ~?  ?=(^ saw)  [%kiln-spam-lame u.saw]
  abet
::
++  take-mere-sync                                    ::
  |=  [way=wire mes=(each (set path) (pair term tang))]
  ?>  ?=([@ @ @ *] way)
  =/  hos=kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  ?.  (~(has by syn) hos)
    abet
  abet:abet:(mere:(auto hos) mes)
::
++  take-writ-find-ship                               ::
  |=  [way=wire rot=riot]
  ?>  ?=([@ @ @ *] way)
  =/  hos=kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  ?.  (~(has by syn) hos)
    abet
  abet:abet:(take-find-ship:(auto hos) rot)
::
++  take-writ-sync                                    ::
  |=  [way=wire rot=riot]
  ?>  ?=([@ @ @ *] way)
  =/  hos=kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  ?.  (~(has by syn) hos)
    abet
  abet:abet:(writ:(auto hos) rot)
::
++  take-wake-autocommit
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog u.error)
    ~&  %kiln-wake-autocommit-fail
    abet
  =.  nex.commit-timer  (add now tim.commit-timer)
  =<  abet
  %-  emil
  :~  [%pass /commit %arvo %c [%dirk mon.commit-timer]]
      [%pass way.commit-timer %arvo %b [%wait nex.commit-timer]]
  ==
::
::
++  spam
  |=  mes=(list tank)
  ((slog mes) ..spam)
::  state machine for fuses
::
++  fuzz
  |=  [syd=desk now=@da]
  =/  pfu=(unit per-fuse)  (~(get by fus) syd)
  ?~  pfu
    ~
  =*  kf  kf.u.pfu
  =*  mox  mox.u.pfu
  =/  should-delete=flag  |
  %-  some
  |%
  ::  finalize
  ::
  ++  abet
   ?:  should-delete
      ..fuzz(fus (~(del by fus) syd))
    ..fuzz(fus (~(put by fus) syd [mox kf]))
  ::
  ++  delete
    ^+  ..delete
    =.  should-delete  &
    ..delete
  ::  queue moves
  ::
  ++  blab
    |=  new=(list card:agent:gall)
    ^+  +>
    +>.$(moz (welp new moz))
  ::  +make-requests: send requests for each %trak source.
  ::
  ++  make-requests
    ^+  ..abet
    =/  movs=(list card:agent:gall)
      %+  murn
        [[bas.kf *germ] con.kf]
      |=  [fs=fuse-source germ]
      ^-  (unit card:agent:gall)
      ?^  ver.fs
        ::  static source, don't need to track
        ~
      =/  bec=beak  (realize-fuse-source fs)
      ?>  =(who.fs p.bec)
      ?>  =(des.fs q.bec)
      =/  hax=@ud  (mug [kf (~(got by hxs) syd)])
      =/  wir=wire
          /kiln/fuse-request/[syd]/(scot %p p.bec)/[q.bec]/(scot %ud hax)
      =/  rav=rave  [%next %w r.bec /]
      =/  rif=riff  [q.bec `rav]
      `[%pass wir %arvo %c [%warp who.fs rif]]
    ::  No need to keep state if all the sources are static
    ?~  movs
      delete
    (blab movs)
  ::
  ++  send-fuse
    ^+  ..abet
    =/  bas=beak  (realize-fuse-source bas.kf)
    =/  con=(list [beak germ])
      %+  turn
        con.kf
      |=  [fs=fuse-source g=germ]
      [(realize-fuse-source fs) g]
    %-  blab
    [%pass /kiln/fuse/[syd] %arvo %c [%fuse syd bas con]]~
  ::
  ++  fuse
    ^+  ..abet
    send-fuse:make-requests
  ::
  ++  take
    |=  [wir=wire =sign-arvo]
    ^+  ..fuse
    ?>  =((lent wir) 3)
    =/  who=ship  (slav %p (snag 0 wir))
    =/  src=desk  (snag 1 wir)
    =/  hax=@ud  (slav %ud (snag 2 wir))
    ?.  =(hax (mug [kf (~(got by hxs) syd)]))
      ::  If the hash in the wire doesn't match the current request
      ::  this is a response for a previous fuse that we can ignore.
      ..take
    ?>  ?=([%clay *] sign-arvo)
    =/  gif=gift:clay  +.sign-arvo
    ?>  ?=([%writ *] gif)
    ?~  p.gif
      %-  (slog leaf+"|fuse request failed for {<src>} on <who> - cancelling")
      delete
    =/  cas=cass:clay  !<(cass:clay +.r.u.p.gif)
    =.  mox  (~(put by mox) [who src] ud.cas)
    fuse
  ::
  ::  utility functions below
  ::
  ::  +realize-fuse-source: convert a fuse-source to a
  ::  fully realized beak.
  ::
  ++  realize-fuse-source
    |=  fs=fuse-source
    ^-  beak
    :+  who.fs
      des.fs
    ?@  ver.fs
      (realize-case [who.fs des.fs])
    `case`ver.fs
  ::
  ++  realize-case
    |=  [who=ship des=desk]
    ^-  case
    =/  let=(unit @ud)  (~(get by mox) [who des])
    ^-  case
    ?~  let
      da+now
    ud+u.let
  --
::
++  auto
  |=  kiln-sync
  =+  (~(gut by syn) [syd her sud] let=*@ud)
  |%
  ++  abet
    ..auto(syn (~(put by syn) [syd her sud] let))
  ::
  ++  blab
    |=  new=(list card:agent:gall)
    ^+  +>
    +>.$(moz (welp new moz))
  ::
  ++  warp
    |=  [=wire =ship =riff]
    (blab [%pass wire %arvo %c [%warp ship riff]] ~)
  ::
  ++  spam  |*(* %_(+> ..auto (^spam +<)))
  ++  stop
    =>  (spam (render "ended autosync" sud her syd) ~)
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (warp wire her sud ~)
  ::  XX duplicate of start-sync? see |track
  ::
  ++  start-track
    =>  (spam (render "activated track" sud her syd) ~)
    =.  let  1
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (warp wire her sud `[%sing %y ud+let /])
  ::
  ++  start-sync
    =>  (spam (render "finding ship and desk" sud her syd) ~)
    =/  =wire  /kiln/find-ship/[syd]/(scot %p her)/[sud]
    (warp wire her sud `[%sing %y ud+1 /])
  ::
  ++  take-find-ship
    |=  rot=riot
    =>  (spam (render "activated sync" sud her syd) ~)
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (warp wire her sud `[%sing %w [%da now] /])
  ::
  ++  writ
    |=  rot=riot
    ?~  rot
      =.  +>.$
        %^    spam
            leaf+"sync cancelled, retrying"
          (render "on sync" sud her syd)
        ~
      start-sync
    =.  let  ?.  ?=(%w p.p.u.rot)  let  ud:;;(cass:clay q.q.r.u.rot)
    =/  =wire  /kiln/sync/merg/[syd]/(scot %p her)/[sud]
    ::  germ: merge mode for sync merges
    ::
    ::    Initial merges from any source must use the %init germ.
    ::    Subsequent merges may use any germ, but if the source is
    ::    a remote ship with which we have not yet merged, we won't
    ::    share a merge-base commit and all germs but %only-that will
    ::    fail.
    ::
    ::    We want to always use %only-that for the first remote merge.
    ::    But we also want local syncs (%base to %home or %kids) to
    ::    succeed after that first remote sync. To accomplish both we
    ::    simply use %only-that for the first three sync merges.  (The
    ::    first two are from the pill.)
    ::
    =/  =germ
      =/  =cass
        .^(cass:clay %cw /(scot %p our)/[syd]/(scot %da now))
      ?:  =(0 ud.cass)
        %init
      ?:((gth 2 ud.cass) %only-that %mate)
    =<  %-  spam
        ?:  =(our her)  ~
        [(render "beginning sync" sud her syd) ~]
    (blab [%pass wire %arvo %c [%merg syd her sud ud+let germ]] ~)
  ::
  ++  mere
    |=  mes=(each (set path) (pair term tang))
    ?:  ?=([%| %ali-unavailable *] mes)
      =.  +>.$
        %^    spam
            leaf+"merge cancelled, maybe because sunk; restarting"
          (render "on sync" sud her syd)
        ~
      start-sync:stop
    =.  let  +(let)
    =.  +>.$
      %-  spam
      ?:  ?=(%& -.mes)
        [(render "sync succeeded" sud her syd) ~]
      ?+  p.p.mes
        :*  (render "sync failed" sud her syd)
            leaf+"please manually merge the desks with"
            leaf+"|merge %{(trip syd)} {(scow %p her)} %{(trip sud)}"
            leaf+""
            leaf+"error code: {<p.p.mes>}"
            q.p.mes
        ==
      ::
          %no-ali-disc
        :~  (render "sync activated" sud her syd)
            leaf+"note: blank desk {<sud>} on {<her>}"
        ==
      ==
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (warp wire her sud `[%sing %y ud+let /])
  --
::
++  work                                              ::  state machine
  |=  syd=desk
  =/  ,per-desk
      %+  ~(gut by rem)  syd
      =+  *per-desk
      %_(- cas [%da now])
  |%
  ++  abet                                            ::  resolve
    ..work(rem (~(put by rem) syd auto gem her sud cas))
  ::
  ++  blab
    |=  new=(list card:agent:gall)
    ^+  +>
    +>.$(moz (welp new moz))
  ::
  ++  win   .                                         ::  successful poke
  ++  lose
    ^+  .
    ~|  %kiln-work-fail
    .
  ::
  ++  perform                                         ::
    ^+  .
    ?<  ?=(%this gem)
    ?<  ?=(%that gem)
    (blab [%pass /kiln/[syd] %arvo %c [%merg syd her sud cas gem]] ~)
  ::
  ++  fancy-merge                                     ::  send to self
    |=  [syd=desk her=@p sud=desk gem=?(%auto germ)]
    ^+  +>
    =/  =cage  [%kiln-merge !>([syd her sud cas gem])]
    %-  blab  :_  ~
    [%pass /kiln/fancy/[^syd] %agent [our %hood] %poke cage]
  ::
  ++  spam  ::|=(tang ((slog +<) ..spam))
            |*(* +>(..work (^spam +<)))
  ++  merge
    |=  [her=@p sud=@tas cas=case gim=?(%auto germ)]
    ^+  +>
    ?.  ?=(%auto gim)
      perform(auto |, gem gim, her her, cas cas, sud sud)
    ?:  =(0 ud:.^(cass:clay %cw /(scot %p our)/[syd]/(scot %da now)))
      =>  $(gim %init)
      .(auto &)
    =>  $(gim %fine)
    .(auto &)
  ::
  ++  coup-fancy
    |=  saw=(unit tang)
    ?~  saw
      +>
    =+  :-  "failed to set up conflict resolution scratch space"
        "I'm out of ideas"
    lose:(spam leaf+-< leaf+-> u.saw)
  ::
  ++  mere
    |=  are=(each (set path) (pair term tang))
    ^+  +>
    ?:  =(%meld gem)
      ?:  ?=(%& -.are)
        ?.  auto
          =+  "merged with strategy {<gem>}"
          win:(spam leaf+- ?~(p.are ~ [>`(set path)`p.are< ~]))
        :: ~?  >  =(~ p.are)  [%mere-no-conflict syd]
        =>  .(+>.$ (spam leaf+"mashing conflicts" ~))
        =+  tic=(cat 3 syd '-scratch')
        =/  notations=(list [path (unit [mark vase])])
          %+  turn  ~(tap in p.are)
          |=  =path
          =/  =mark    -:(flop path)
          =/  =dais    .^(dais %cb /(scot %p our)/[syd]/(scot cas)/[mark])
          =/  base     .^(vase %cr (weld /(scot %p our)/[tic]/(scot cas) path))
          =/  ali      .^(vase %cr (weld /(scot %p her)/[sud]/(scot cas) path))
          =/  bob      .^(vase %cr (weld /(scot %p our)/[syd]/(scot cas) path))
          =/  ali-dif  (~(diff dais base) ali)
          =/  bob-dif  (~(diff dais base) bob)
          =/  mash     (~(mash dais base) [her sud ali-dif] [our syd bob-dif])
          :-  path
          ?~  mash
            ~
          `[mark (~(pact dais base) u.mash)]
        =/  [annotated=(list [path *]) unnotated=(list [path *])]
          (skid notations |=([* v=*] ?=(^ v)))
        =/  tic=desk  (cat 3 syd '-scratch')
        =/  tan=(list tank)
          %-  zing
          ^-  (list (list tank))
          :~  %-  tape-to-tanks
              """
              done setting up scratch space in {<[tic]>}
              please resolve the following conflicts and run
              |merge {<syd>} our {<[tic]>}
              """
              %^  tanks-if-any
                "annotated conflicts in:"  (turn annotated head)
              ""
              %^  tanks-if-any
                "unannotated conflicts in:"  (turn unnotated head)
              """
              some conflicts could not be annotated.
              for these, the scratch space contains
              the most recent common ancestor of the
              conflicting content.
              """
          ==
        =<  win
        %-  blab:(spam tan)
        :_  ~
        :*  %pass  /kiln/[syd]  %arvo  %c
            %info
            tic  %&
            %+  murn  notations
            |=  [=path dif=(unit [=mark =vase])]
            ^-  (unit [^path miso])
            ?~  dif
              ~
            `[path %mut mark.u.dif vase.u.dif]
        ==
      =+  "failed to merge with strategy meld"
      lose:(spam leaf+- >p.p.are< q.p.are)
    ?:  ?=(%& -.are)
      =+  "merged with strategy {<gem>}"
      win:(spam leaf+- ?~(p.are ~ [>`(set path)`p.are< ~]))
    ?.  auto
      =+  "failed to merge with strategy {<gem>}"
      lose:(spam leaf+- >p.p.are< q.p.are)
    ?+    gem
      (spam leaf+"strange auto" >gem< ~)
    ::
        %init
      =+  :-  "auto merge failed on strategy %init"
          "I'm out of ideas"
      lose:(spam leaf+-< leaf+-> [>p.p.are< q.p.are])
    ::
        %fine
      ?.  ?=(%bad-fine-merge p.p.are)
        =+  "auto merge failed on strategy %fine"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  (spam leaf+"%fine merge failed, trying %meet" ~)
      perform(gem %meet)
    ::
        %meet
      ?.  ?=(%meet-conflict p.p.are)
        =+  "auto merge failed on strategy %meet"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  (spam leaf+"%meet merge failed, trying %mate" ~)
      perform(gem %mate)
    ::
        %mate
      ?.  ?=(%mate-conflict p.p.are)
        =+  "auto merge failed on strategy %mate"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  .(gem %meld)
      =+  tic=(cat 3 syd '-scratch')
      =>  =+  :-  "%mate merge failed with conflicts,"
              "setting up scratch space at %{(trip tic)}"
          [tic=tic (spam leaf+-< leaf+-> q.p.are)]
      =.  ..mere  (fancy-merge tic our syd %init)
      =>  (spam leaf+"%melding %{(trip sud)} into scratch space" ~)
      %-  blab  :_  ~
      ?<  ?=(%this gem)
      ?<  ?=(%that gem)
      =/  note  [%merg (cat 3 syd '-scratch') her sud cas gem]
      [%pass /kiln/[syd] %arvo %c note]
    ==
  ::
  ++  tape-to-tanks
    |=  a=tape  ^-  (list tank)
    (scan a (more (just '\0a') (cook |=(a=tape leaf+a) (star prn))))
  ::
  ++  tanks-if-any
    |=  [a=tape b=(list path) c=tape]  ^-  (list tank)
    ?:  =(~ b)  ~
    (welp (tape-to-tanks "\0a{c}{a}") >b< ~)
  --
--
