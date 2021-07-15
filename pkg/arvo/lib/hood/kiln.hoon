jolt  *bill
/+  version
=,  clay
=,  space:userlib
=,  format
|%
+$  state    state-2
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
::    .next is a list of pending commits with future kelvins
::
+$  arak
  $:  =ship
      =desk
      =aeon
      next=(list [=aeon =weft])
      =rein
  ==
::  $rein: diff from desk manifest
::
::    .add: agents not in manifest that should be running
::    .sub: agents in manifest that should not be running
::
+$  rein
  $:  add=(set dude:gall)
      sub=(set dude:gall)
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
=|  moz=(list card:agent:gall)
|%
++  kiln  .
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
::  TODO reinstate with +vats
::    =?  ..abet  ?=(^ recognized-ota)
::      (poke:update `[her sud]:u.recognized-ota)
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
      [%x %kiln %ark ~]        ``noun+!>(ark)
      [%x %kiln %our ~]        ``noun+!>(our)
      [%x %kiln %base-hash ~]
    =/  ver  (base-hash:version our now)
    ``noun+!>(?~(ver 0v0 i.ver))
  ==
::
++  vats
  |_  [loc=desk rak=arak]
  ++  vats  .
  ++  abet  kiln(ark (~(put by ark) loc rak))
  ++  abed
    |=  lac=desk
    ~_  leaf/"kiln: {<lac>} not installed"
    vats(loc lac, rak (~(got by ark) lac))
  ::
  ++  here  "{<loc>} from {<[ship desk]:rak>}"
  ++  make-wire  |=(step=@tas /kiln/vats/[loc]/[step])
  ++  from-wire
    |=  =wire
    ?>  ?=([%kiln %vats @ @ ~] wire)
    (abed i.t.t.wire)
  ::
  ++  emit  |=(card:agent:gall vats(kiln (^emit +<)))
  ++  emil  |=((list card:agent:gall) vats(kiln (^emil +<)))
  ++  pass
    |%
    ++  find      (warp %find [%sing %y ud+1 /])
    ++  sync      (warp %sync [%sing %w da+now /])
    ++  download  (warp %download [%sing %v ud+aeon.rak /])
    ++  merge-main
      =/  germ  (get-germ loc)
      =/  =aeon  (dec aeon.rak)
      (clay-card %merge-main [%merg loc ship.rak desk.rak ud+aeon germ])
    ++  merge-kids
      =/  germ  (get-germ %kids)
      =/  =aeon  (dec aeon.rak)
      (clay-card %merge-kids [%merg %kids ship.rak desk.rak ud+aeon germ])
    ::
    ++  warp  |=([s=term r=rave] (clay-card s %warp ship.rak desk.rak `r))
    ++  clay-card
      |=  [step=@tas =task:clay]
      ^-  card:agent:gall
      [%pass (make-wire step) %arvo %c task]
    --
  ::  +uninstall: stop tracking apps on desk, and suspend apps
  ::
  ++  uninstall
    |=  lac=desk
    ^+  kiln
    =.  vats  (abed lac)
    ~>  %slog.0^leaf/"kiln: uninstalling {here}"
    =.  vats
      %-  emil
      %+  turn  (get-apps-have lac)
      |=  =dude:gall
      [%pass /kiln/vats/uninstall %arvo %g %idle dude]
    ::
    kiln(ark (~(del by ark) lac))
  ::  +install: set up desk sync to .lac to install all apps from [her rem]
  ::
  ++  install
    |=  [lac=desk her=ship rem=desk]
    ^+  vats
    =/  got  (~(get by ark) lac)
    ?:  =(`[her rem] got)
      ~>  %slog.0^leaf/"kiln: already tracking {here:(abed lac)}, ignoring"
      vats
    =?  kiln  ?=(^ got)  (uninstall lac)
    =:  loc  lac
        rak  [her rem *aeon next=~ *rein]
      ==
    ~>  %slog.0^leaf/"kiln: beginning install into {here}"
    (emit find:pass)
  ::  +reset: resync after failure
  ::
  ++  reset
    ^+  vats
    ~>  %slog.0^leaf/"kiln: resetting tracking for {here}"
    =.  ark  (~(del by ark) loc)
    (install loc [ship desk]:rak)
  ::  +bump: handle kernel kelvin upgrade
  ::
  ::    Apply merges to revive faded agents on all paused desks.
  ::
  ++  bump
    |=  except=(set desk)
    ^+  kiln
    =/  kel=weft  [%zuse zuse]
    =/  ded  (~(dif in (get-blockers kel)) except)
    ?^  ded
      ~>  %slog.0^leaf/"kiln: desks blocked upgrade {<ded>}"
      !!
    =/  liv  (skip ~(tap by ark) |=([d=desk *] (~(has in except) d)))
    ~>  %slog.0^leaf/"kiln: bump {<liv>}"
    =<  kiln
    |-  ^+  vats
    ?~  liv  vats
    $(liv t.liv, vats (emit merge-main:pass(loc p.i.liv, rak q.i.liv)))
  ::
  ++  take
    |=  [=wire syn=sign-arvo]
    ^+  vats
    =.  vats  (from-wire wire)
    ?>  ?=([%kiln %vats @ @ ~] wire)
    ?+    i.t.t.t.wire
        ~>  %slog.0^leaf/"kiln: vats-bad-take {<t.t.t.wire>}"
        vats
      %find        (take-find syn)
      %sync        (take-sync syn)
      %download    (take-download syn)
      %merge-main  (take-merge-main syn)
      %merge-kids  (take-merge-kids syn)
      %jolt        (take-onto syn)
    ==
  ::
  ++  take-find
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%writ +<.syn)
    ?~  p.syn
      ~>  %slog.0^leaf/"kiln: cancelled (1) install into {here}, aborting"
      vats(ark (~(del by ark) loc))
    ~>  %slog.0^leaf/"kiln: activated install into {here}"
    (emit sync:pass)
  ::
  ++  take-sync
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%writ +<.syn)
    ?~  p.syn
      ~>  %slog.0^leaf/"kiln: cancelled (1) install into {here}, retrying"
      reset
    ~>  %slog.0^leaf/"kiln: downloading update for {here}"
    =?  aeon.rak  ?=(%w p.p.u.p.syn)  ud:;;(cass:clay q.q.r.u.p.syn)
    (emit download:pass)
  ::
  ++  take-download
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%writ +<.syn)
    ?~  p.syn
      ~>  %slog.0^leaf/"kiln: cancelled (2) install into {here}, retrying"
      reset
    ~>  %slog.0^leaf/"kiln: finished downloading update for {here}"
    =/  old-weft  `weft`[%zuse zuse]
    =/  new-weft  (read-kelvin [ship desk aeon]:rak)
    =.  aeon.rak  +(aeon.rak)
    ::
    ?.  =(%base loc)
      ::  TODO: ?>  =(%zuse lal.new-weft) but more flexible for future renames
      ?:  (gth num.new-weft num.old-weft)
        ~>  %slog.0^leaf/"kiln: cannot install {here}, old kelvin {<new-weft>}"
        ~>  %slog.0^leaf/"kiln: will retry at foreign kelvin {<old-weft>}"
        (emit sync:pass)
      ?:  (lth num.new-weft num.old-weft)
        ~>  %slog.0^leaf/"kiln: future version {<new-weft>}, enqueueing"
        =.  next.rak  (snoc next.rak [(dec aeon.rak) new-weft])
        (emit sync:pass)
      ~>  %slog.0^leaf/"kiln: merging into {here}"
      (emil ~[merge-main sync]:pass)
    ::
    =/  blockers
      ?:  =(new-weft old-weft)
        ~
      (get-blockers new-weft)
    ::
    ?.  =(~ blockers)
      ~>  %slog.0^leaf/"kiln: OTA blocked on {<blockers>}"
      =-  (emil sync:pass - ~)
      :*  %give  %fact  [/vats]~  %kiln-vats-diff
          !>([%blocked arak=rak weft=new-weft blockers=blockers])
      ==
    ~>  %slog.0^leaf/"kiln: applying OTA to {here}, kelvin: {<new-weft>}"
    (emil ~[merge-main sync]:pass)
  ::
  ++  take-merge-main
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%mere +<.syn)
    ?:  ?=([%| %ali-unavailable *] p.syn)
      =+  "kiln: merge into {here} failed, maybe because sunk; restarting"
      %-  (slog leaf/- p.p.syn)
      reset
    ?:  ?=(%| -.p.syn)
      =+  "kiln: merge into {here} failed, waiting for next revision"
      %-  (slog leaf/- p.p.syn)
      vats
    =.  vats
      ~>  %slog.0^leaf/"merge into {here} succeeded; reviving agents"
      %-  emil
      %+  turn  (get-apps-want loc rein.rak)
      |=  =dude:gall
      [%pass /kiln/jolt/[dude] %arvo %g %jolt loc dude]
    ?.  =(%base loc)
      vats
    =.  kiln  (bump (sy %base %kids ~))
    (emit merge-kids:pass)
  ::
  ++  take-merge-kids
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%mere +<.syn)
    ?:  ?=([%| %ali-unavailable *] p.syn)
      ~>  %slog.0^leaf/"kiln: OTA to %kids failed, maybe peer sunk; restarting"
      =.  vats
        =/  fact  merge-kids-sunk/[rak p.p.syn]
        (emit %give %fact [/vats]~ %kiln-vats-diff !>(fact))
      reset
    =/  fact
      ?-  -.p.syn
        %&  ~>  %slog.0^leaf/"kiln: OTA to %kids succeeded"
            merge-kids/rak
        %|  ~>  %slog.0^leaf/"kiln: OTA to %kids failed {<p.p.syn>}"
            merge-kids-fail/[rak p.p.syn]
      ==
    (emit %give %fact [/vats]~ %kiln-vats-diff !>(fact))
  ::
  ++  take-onto
    |=  syn=sign-arvo
    ^+  vats
    =/  onto  ?>(?=(%onto -.syn) p.syn)
    ?-  -.onto
      %&  vats
      %|  (mean p.onto)  ::  TODO: kill arvo event on failure
    ==
  --
::  +get-ankh: extract $ankh from clay %v response $rant
::
++  get-ankh
  |=  =rant
  ^-  ankh
  ?>  ?=(%dome p.r.rant)
  !<(ankh q.r.rant)
::  +get-apps-have: find which apps Gall is running on a desk
::
++  get-apps-have
  |=  =desk
  ^-  (list dude:gall)
  %~  tap  in
  .^((set dude:gall) ge+/(scot %p our)/[desk]/(scot %da now))
::  +get-apps-want: find which apps should be running on a desk
::
::    TODO: preserve list order?
::
++  get-apps-want
  |=  [=desk =rein]
  ^-  (list dude:gall)
  =/  duz  (read-apes .^(=bill cx+(weld pax /desk/bill)))
  =/  daz  ~(tap in duz)
  =.  daz  (~(dif in daz) sub.rein)
  =.  daz  (~(uni in daz) add.rein)
  ~(tap in daz)
::  +get-blockers: find desks that would block a kernel update
::
++  get-blockers
  |=  kel=weft
  ^-  (set desk)
  %-  ~(gas in *(set desk))
  %+  murn  ~(tap by ark)
  |=  [=desk =arak]
  ?:  (lien next.arak |=([* k=weft] =(k kel)))
    ~
  `desk
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
::  +ankh-to-kelvin: read /sys.kelvin from an $ankh
::
++  ankh-to-kelvin
  |=  =ankh
  !<  weft
  q:(need (~(get an:cloy ankh) /sys/kelvin))
::
++  read-kelvin
  |=  [=ship =desk =aeon]
  ^-  weft
  =/  her  (scot %p ship)
  =/  syd  (scot %tas desk)
  =/  yon  (scot %ud aeon)
  ::
  =/  dom  .^(dome cs/~[her syd yon])
  =/  tak  (scot %uv (~(got by hit.dom) let.dom))
  =/  yak  .^(yaki cs/~[her syd yon %taki tak])
  =/  lob  (scot %uv (~(got by q.yak) /sys/kelvin))
  =/  bob  .^(blob cs/~[her syd yon %blob lob])
  ::
  ;;  weft
  ?-  -.bob
    %direct  q.q.bob
    %delta   q.r.bob
  ==
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
::  +peer: handle %watch
::
++  peer
  |=  =path
  ?>  (team:title our src)
  ?+    path  ~|(kiln-path/path !!)
      [%vats ~]
    =/  snap  [ota=ota ark=ark]
    abet(moz :_(moz [%give %fact ~ kiln-vats-snap/!>(snap)]))
  ==
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
      [%vats *]         abet:abet:(take:vats t.wire sign-arvo)
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
