/+  version
=,  clay
=,  space:userlib
=,  format
|%
+$  state  [%2 pith-2]
+$  state-2  state
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
      ark=(map desk arak)                               ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
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
::  $arak: foreign vat tracker
::
+$  arak
  $:  =ship
      =desk
      =aeon
      next=(list [=aeon kelvin=[@tas @ud]])
  ==
+$  per-desk                                            ::  per-desk state
  $:  auto=?                                            ::  escalate on failure
      gem=?(%this %that germ)                           ::  strategy
      her=@p                                            ::  from ship
      sud=@tas                                          ::  from desk
      cas=case                                          ::  at case
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
+$  kiln-fuse
  $@  ~
  $:  syd=desk
      bas=beak
      con=(list [beak germ])
  ==
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
    ::
    =/  sen=(map kiln-sync let=@ud)
      ?~  recognized-ota
        syn.old
      (~(del by syn.old) [syd her sud]:u.recognized-ota)
    ::  note that the new state has not yet been initialized
    ::
    =?  ..abet  ?=(^ recognized-ota)
      (poke:update `[her sud]:u.recognized-ota)
    ::
    +>(old [%1 rem.old syn=sen ota=~ commit-timer.old])
  ::
  =?  old  ?=(%1 -.old)
    :*  %2
        rem.old
        syn.old
        ota.old
        ark=~
        commit-timer.old
    ==
  ::
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
++  vats
  |_  [loc=desk rak=arak]
  ::
  ++  abet  ..vats(ark (~(put by ark) loc rak))
  ++  abed
    |=  lac=desk
    ~_  leaf/"kiln: {<lac>} not installed"
    ..abet(loc lac, rak (~(got by ark) lac))
  ::
  ++  emit  |=(card:agent:gall ..abet(..vats (^emit +<)))
  ++  emil  |=((list card:agent:gall) ..abet(..vats (^emil +<)))
  ++  here  "{<loc>} from {<[ship desk]:rak>}"
  ++  make-wire  |=(step=@tas /kiln/vats/[loc]/[step])
  ++  from-wire
    |=  =wire
    ?>  ?=([%kiln %vats @ @ ~] wire)
    (abed i.t.t.wire)
  ::
  ++  uninstall 
    |=  lac=desk
    ^+  ..vats
    =.  ..abet  (abed lac)
    ~>  %slog.0^leaf/"kiln: uninstalling {here}"
    =.  ..abet
      %-  emil
      %+  turn  (get-apps lac)
      |=  =dude:gall
      [%pass /kiln/vats/uninstall %arvo %g %fade dude %idle]
    ::
    ..vats(ark (~(del by ark) lac))
  ::
  ++  install
    |=  [lac=desk her=ship rem=desk]
    ^+  ..abet
    =/  got  (~(get by ark) lac)
    ?:  =(`[her rem] got)
      ~>  %slog.0^leaf/"kiln: already tracking {here:(abed lac)}, ignoring"
      ..abet
    =?  ..vats  ?=(^ got)  (uninstall lac)
    =:  loc  lac
        rak  [her rem *aeon next=~]
      ==
    ~>  %slog.0^leaf/"kiln: beginning install into {here}"
    %:  emit
      %pass  (make-wire %find)  %arvo  %c
      %warp  ship.rak  desk.rak  `[%sing %y ud+1 /]
    ==
  ::
  ++  take
    |=  [=wire syn=sign-arvo]
    ^+  ..abet
    =.  ..abet  (from-wire wire)
    ?>  ?=([%kiln %vats @ @ ~] wire)
    ?+    i.t.t.t.wire
        ~>  %slog.0^leaf/"kiln: vats-bad-take {<t.t.t.wire>}"
        ..abet
      %find      (take-find syn)
      %sync      (take-sync syn)
      %download  (take-download syn)
      %merge     (take-merge syn)
    ==
  ::
  ++  take-find
    |=  syn=sign-arvo
    ?>  ?=(%writ +<.syn)
    ~>  %slog.0^leaf/"kiln: activated install into {here}"
    %:  emit
      %pass  (make-wire %sync)  %arvo  %c
      %warp  ship.rak  desk.rak  `[%sing %w da+now /]
    ==
  ::
  ++  take-sync
    |=  syn=sign-arvo
    ?>  ?=(%writ +<.syn)
    ?~  p.syn
      ~>  %slog.0^leaf/"kiln: cancelled (1) install into {here}, retrying"
      (install loc [ship desk]:rak)  ::  TODO reset aeon?
    ~>  %slog.0^leaf/"kiln: downloading update for {here}"
    =?  aeon.rak  ?=(%w p.p.u.p.syn)  ud:;;(cass:clay q.q.r.u.p.syn)
    %:  emit
      %pass  (make-wire %download)  %arvo  %c
      %warp  ship.rak  desk.rak  `[%sing %v ud+aeon.rak /]
    ==
  ::
  ++  take-download
    |=  syn=sign-arvo
    ?>  ?=(%writ +<.syn)
    ?~  p.syn
      ~>  %slog.0^leaf/"kiln: cancelled (2) install into {here}, retrying"
      (install loc [ship desk]:rak)  ::  TODO reset aeon?
    ~>  %slog.0^leaf/"kiln: finished downloading update for {here}"
    ::  TODO: check kelvin here
    =.  aeon.rak  +(aeon.rak)
    =/  =germ  (get-germ loc)
    ~>  %slog.0^leaf/"kiln: merging into {here}"
    %-  emil
    :~  :*  %pass  (make-wire %merge)  %arvo  %c
            %merg  loc  ship.rak  desk.rak  ud+(dec aeon.rak)  germ
        ==
        :*  %pass  (make-wire %sync)  %arvo  %c
            %warp  ship.rak  desk.rak  `[%sing %z ud+aeon.rak /]
    ==  ==
  ::
  ++  take-merge
    |=  syn=sign-arvo
    ?>  ?=(%mere +<.syn)
    ?:  ?=([%| %ali-unavailable *] p.syn)
      %-  %+  slog
            :-  %leaf
            "kiln: merge into {here} failed, maybe because sunk; restarting"
          p.p.syn
      (install loc [ship desk]:rak)  ::  TODO reset aeon?
    ?:  ?=(%| -.p.syn)
      %-  %+  slog
            :-  %leaf
            "kiln: merge into {here} failed, waiting for next revision"
          p.p.syn
      ..abet
    ::
    ~>  %slog.0^leaf/"merge into {here} succeeded"
    ..abet
  --
::  +get-apps: find which apps Gall is running on a desk
::
::    TODO: move to zuse?
::
++  get-apps
  |=  =desk
  ^-  (list dude:gall)
  %~  tap  in
  .^((set dude:gall) ge+/(scot %p our)/[desk]/(scot %da now))
::  +get-germ: select merge strategy into local desk
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
++  update
  |%
  ++  poke
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
  ++  make-wire
    |=  =path
    ^-  wire
    ?>  ?=(^ ota)
    %-  welp  :_  path
    /kiln/ota/(scot %p ship.u.ota)/[desk.u.ota]/(scot %ud aeon.u.ota)
  ::
  ++  check-ota
    |=  =wire
    ^-  ?
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
      (poke `[ship desk]:u.ota)
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
      (poke `[ship desk]:u.ota)
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
      (poke `[ship desk]:u.ota)
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
      (poke `[ship desk]:u.ota)
    ::
    ?-  -.p.sign-arvo
      %&  (render-ket "OTA to %kids succeeded" ~)
      %|  (render-ket "OTA to %kids failed" `p.p.sign-arvo)
    ==
  --
::
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-kiln-bad-mark mark] !!)
    %kiln-autocommit         =;(f (f !<(_+<.f vase)) poke-autocommit)
    %kiln-cancel             =;(f (f !<(_+<.f vase)) poke-cancel)
    %kiln-cancel-autocommit  =;(f (f !<(_+<.f vase)) poke-cancel-autocommit)
    %kiln-commit             =;(f (f !<(_+<.f vase)) poke-commit)
    %kiln-fuse               =;(f (f !<(_+<.f vase)) poke-fuse)
    %kiln-gall-sear          =;(f (f !<(_+<.f vase)) poke-gall-sear)
    %kiln-goad-gall          =;(f (f !<(_+<.f vase)) poke-goad-gall)
    %kiln-info               =;(f (f !<(_+<.f vase)) poke-info)
    %kiln-install            =;(f (f !<(_+<.f vase)) poke-install)
    %kiln-label              =;(f (f !<(_+<.f vase)) poke-label)
    %kiln-merge              =;(f (f !<(_+<.f vase)) poke-merge)
    %kiln-mount              =;(f (f !<(_+<.f vase)) poke-mount)
    %kiln-ota                =;(f (f !<(_+<.f vase)) poke-ota)
    %kiln-ota-info           =;(f (f !<(_+<.f vase)) poke-ota-info)
    %kiln-permission         =;(f (f !<(_+<.f vase)) poke-permission)
    %kiln-rm                 =;(f (f !<(_+<.f vase)) poke-rm)
    %kiln-schedule           =;(f (f !<(_+<.f vase)) poke-schedule)
    %kiln-sync               =;(f (f !<(_+<.f vase)) poke-sync)
    %kiln-syncs              =;(f (f !<(_+<.f vase)) poke-syncs)
    %kiln-track              =;(f (f !<(_+<.f vase)) poke-track)
    %kiln-uninstall          =;(f (f !<(_+<.f vase)) poke-uninstall)
    %kiln-unmount            =;(f (f !<(_+<.f vase)) poke-unmount)
    %kiln-unsync             =;(f (f !<(_+<.f vase)) poke-unsync)
  ==
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
++  poke-cancel
  |=  a=@tas
  abet:(emit %pass /cancel %arvo %c [%drop a])
::
++  poke-cancel-autocommit
  |=  ~
  abet:(emit %pass way.commit-timer %arvo %b [%rest nex.commit-timer])
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
++  poke-fuse
  |=  k=kiln-fuse
  ?~  k  abet
  abet:(emit [%pass /kiln/fuse/[syd.k] %arvo %c [%fuse syd.k bas.k con.k]])
::
++  poke-gall-sear
  |=  =ship
  abet:(emit %pass /kiln %arvo %g %sear ship)
::
++  poke-goad-gall
  |=  [force=? agent=(unit dude:gall)]
  abet:(emit %pass /kiln %arvo %g %goad force agent)
::
++  poke-info
  |=  [mez=tape tor=(unit toro)]
  ?~  tor
    abet:(spam leaf+mez ~)
  abet:(emit:(spam leaf+mez ~) %pass /kiln %arvo %c [%info u.tor])
::
++  poke-install
  |=  [loc=desk her=ship rem=desk]
  abet:abet:(install:vats +<)
::
++  poke-label
  |=  [syd=desk lab=@tas]
  =+  pax=/(scot %p our)/[syd]/[lab]
  (poke-info "labeled {(spud pax)}" `[syd %| lab])
::
++  poke-merge
  |=  kiln-merge
  ?~  +<  abet
  abet:abet:(merge:(work syd) ali sud cas gim)
::
++  poke-mount
  |=  kiln-mount
  =+  bem=(de-beam pax)
  ?~  bem
    =+  "can't mount bad path: {<pax>}"
    abet:(spam leaf+- ~)
  abet:(emit %pass /mount %arvo %c [%mont pot u.bem])
::
++  poke-ota
  |=  arg=(unit [=ship =desk])
  abet:(poke:update arg)
::
++  poke-ota-info
  |=  *
  =<  abet  %-  spam
  :~  [%leaf get-ota-info]
      [%leaf "use |ota %disable or |ota ~sponsor %kids to reset it"]
  ==
::
++  poke-permission
  |=  [syd=desk pax=path pub=?]
  =<  abet
  %-  emit
  =/  =rite  [%r ~ ?:(pub %black %white) ~]
  [%pass /kiln/permission %arvo %c [%perm syd pax rite]]
::
++  poke-rm
  |=  a=path
  =+  b=.^(arch %cy a)
  ?~  fil.b
    =+  ~[leaf+"No such file:" leaf+"{<a>}"]
    abet:(spam -)
  (poke-info "removed" `(fray a))
::
++  poke-schedule
  |=  [where=path tym=@da eve=@t]
  =.  where  (welp where /sched)
  %+  poke-info  "scheduled"
  =+  old=;;((map @da cord) (fall (file where) ~))
  `(foal where %sched !>((~(put by old) tym eve)))
::
++  poke-sync
  |=  hos=kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already syncing" [sud her syd]:hos) ~)
  abet:abet:start-sync:(auto hos)
::
++  poke-syncs                                        ::  print sync config
  |=  ~
  =<  abet  %-  spam
  :-  [%leaf get-ota-info]
  ?:  =(0 ~(wyt by syn))
    [%leaf "no other syncs configured"]~
  %+  turn  ~(tap in ~(key by syn))
  |=(a=kiln-sync (render "sync configured" [sud her syd]:a))
::
++  poke-track
  |=  hos=kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already tracking" [sud her syd]:hos) ~)
  abet:abet:start-track:(auto hos)
::
++  poke-uninstall
  |=  loc=desk
  abet:(uninstall:vats +<)
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
++  poke-unsync
  |=  hus=kiln-unsync
  ?.  (~(has by syn) hus)
    abet:(spam (render "not syncing" [sud her syd]:hus) ~)
  %*  .  abet:abet:stop:(auto hus)
    syn  (~(del by syn) hus)
  ==
::
++  get-ota-info
  ?~  ota
    "OTAs disabled"
  "OTAs enabled from {<desk.u.ota>} on {<ship.u.ota>}"
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
++  done
  |=  [way=wire saw=(unit error:ames)]
  ~?  ?=(^ saw)  [%kiln-nack u.saw]
  abet
::
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
