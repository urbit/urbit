/-  *hood
/*  base-bill  %bill  /desk/bill
=,  clay
=,  space:userlib
=,  format
=*  dude  dude:gall
|%
+$  state    state-4
+$  state-4  [%4 pith-4]
+$  state-3  [%3 pith-3]
+$  state-2  [%2 pith-2]
+$  state-1  [%1 pith-1]
+$  state-0  [%0 pith-0]
+$  any-state
  $~  *state
  $%  state-4
      state-3
      state-2
      state-1
      state-0
  ==
+$  pith-4                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ark=(map desk arak)                               ::
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
+$  pith-3                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ark=(map desk arak-3)                             ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
      ::  map desk to the currently ongoing fuse request
      ::  and the latest version numbers for beaks to
      fus=(map desk per-fuse)
      ::  used for fuses - every time we get a fuse we
      ::  bump this. used when calculating hashes to
      ::  ensure they're unique even when the same
      ::  request is made multiple times.
      hxs=(map desk @ud)
  ==
+$  arak-3
  $:  =ship
      =desk
      =aeon
      next=(list [=aeon =weft])
      rein=rein-3
  ==
+$  rein-3
  $:  add=(set dude)
      sub=(set dude)
  ==
::
+$  pith-2                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ota=(unit [=ship =desk =aeon])                    ::
      commit-timer=[way=wire nex=@da tim=@dr mon=term]  ::
      fus=(map desk per-fuse)
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
  $:  syd=desk                                          ::  local desk
      her=ship                                          ::  foreign ship
      sud=desk                                          ::  foreign desk
  ==
+$  kiln-unsync                                         ::
  $:  syd=desk                                          ::  local desk
      her=ship                                          ::  foreign ship
      sud=desk                                          ::  foreign desk
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
++  on-init
  =<  abet
  ~>  %slog.0^leaf/"kiln: boot"
  =/  =rein  [liv=& add=(sy %hood %dojo ~) sub=~]  ::  TODO questionable
  =/  daz  (get-apps-want base-bill rein)
  %-  emil
  %-  zing  ^-  (list (list card:agent:gall))
  (turn daz start-dude:~(pass vats [%base *arak]))
::
++  on-load
  =>  |%
      +$  ota  [syd=desk her=ship sud=desk]
      --
  =|  old-ota=(unit ota)
  |=  [hood-version=@ud old=any-state]
  =?  old-ota  ?=(%0 -.old)
    =/  syncs=(list [ota =aeon])  ~(tap by syn.old)
    |-  ^-  (unit ota)
    ?~  syncs
      ~
    ?:  &(=([%base %kids] [syd sud]:i.syncs) !=(our her.i.syncs))
      `[syd her sud]:i.syncs
    $(syncs t.syncs)
  ::
  =?  old  ?=(%0 -.old)
    =?  syn.old  ?=(^ old-ota)  (~(del by syn.old) u.old-ota)
    [%3 [rem syn ark=~ commit-timer fus=~ hxs=~]:old]
  ::
  =?  old  ?=(%1 -.old)
    :*  %2
        rem.old
        syn.old
        ota.old
        commit-timer.old
        fus=~
        hxs=~
    ==
  ::
  =?  old-ota  ?=(%2 -.old)
    ?~  ota.old  ~
    `[%base ship desk]:u.ota.old
  ::
  =?  old  ?=(%2 -.old)
    :*  %3
        rem.old
        syn.old
        ark=~
        commit-timer.old
        fus.old
        hxs.old
    ==
  =?  old  ?=(%3 -.old)
    :*  %4
        rem.old
        syn.old
        ark=(~(run by ark.old) |=(a=arak-3 a(rein [liv=& rein.a])))
        commit-timer.old
        fus.old
        hxs.old
    ==
  ::
  ?>  ?=(%4 -.old)
  =.  +<+.$.abet  old
  =<  abet
  ?~  old-ota
    kiln
  abet:(install:vats %base [her sud]:u.old-ota)
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  [~ ~]
    [%x %kiln %lag ~]  ``loob+!>(.^(? //(scot %p our)//(scot %da now)/zen/lag))
  ::
      [%x %kiln %vat @ ~]
    =*  loc  i.t.t.t.path
    =/  ego  (scot %p our)
    =/  wen  (scot %da now)
    =/  rak=(unit arak)  (~(get by ark) loc)
    ?~  rak  [~ ~]
    =/  hog  .^(@uv cz+~[ego loc wen])
    =/  cas  .^(cass cw+~[ego loc wen])
    :^  ~  ~  %noun
    !>  ^-  vat
    [loc hog cas u.rak]
  ::
      [%x %kiln %vats ~]
    :^  ~  ~  %kiln-vats
    !>  ^-  (list vat)
    =/  ego  (scot %p our)
    =/  wen  (scot %da now)
    %+  turn  ~(tap by ark)
    |=  [loc=desk rak=arak]
    =/  hog  .^(@uv cz+~[ego loc wen])
    =/  cas  .^(cass cw+~[ego loc wen])
    [loc hog cas rak]
  ::
      [%x %kiln %ark ~]        ``noun+!>(ark)
      [%x %kiln %our ~]        ``noun+!>(our)
      [%x %kiln %base-hash ~]
    =/  ver  (mergebase-hashes our %base now (~(got by ark) %base))
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
    ~|  wire
    ?>  ?=([@ @ *] wire)
    (abed i.wire)
  ::
  ++  emit  |=(card:agent:gall vats(kiln (^emit +<)))
  ++  emil  |=((list card:agent:gall) vats(kiln (^emil +<)))
  ++  give
    |%
    ++  snap  [%give %fact ~[/kiln/vats] %kiln-vats-snap !>(ark)]
    ++  diff  |=(d=^diff [%give %fact ~[/kiln/vats] %kiln-vats-diff !>(d)])
    --
  ++  pass
    |%
    ++  pyre  |=(=tang [%pass /kiln/vats %arvo %d %pyre tang])
    ++  find      (warp %find [%sing %y ud+1 /])
    ++  sync-da   (warp %sync [%sing %w da+now /])
    ++  sync-ud   (warp %sync [%sing %w ud+aeon.rak /])
    ++  download  (warp %download [%sing %v ud+aeon.rak /])
    ++  warp  |=([s=term r=rave] (clay-card s %warp ship.rak desk.rak `r))
    ++  merge-main
      =/  germ  (get-germ loc)
      =/  =aeon  (dec aeon.rak)
      (clay-card %merge-main [%merg loc ship.rak desk.rak ud+aeon germ])
    ++  merge-kids
      =/  germ  (get-germ %kids)
      =/  =aeon  (dec aeon.rak)
      (clay-card %merge-kids [%merg %kids ship.rak desk.rak ud+aeon germ])
    ++  clay-card
      |=  [step=@tas =task:clay]
      ^-  card:agent:gall
      [%pass (make-wire step) %arvo %c task]
    ++  start-dude
      |=  =dude
      ^-  (list card:agent:gall)
      :-  [%pass /kiln/vats/[loc]/jolt/[dude] %arvo %g %jolt loc dude]
      ?.  (is-fish dude (read-bill our loc now))
        ~>  %slog.0^leaf/"kiln: jolt {<dude>}"
        ~
      ~>  %slog.0^leaf/"kiln: jolt {<dude>}, binding console"
      =/  =cage  [%drum-link !>([our dude])]
      [%pass /kiln/link/[dude] %agent [our %hood] %poke cage]~
    ++  stop-dude
      |=  =dude
      ^-  (list card:agent:gall)
      :-  [%pass /kiln/vats/[loc]/uninstall %arvo %g %idle dude]
      ?.  (is-fish dude (read-bill our loc now))
        ~>  %slog.0^leaf/"kiln: idle {<dude>}"
        ~
      ~>  %slog.0^leaf/"kiln: idle {<dude>}, unbinding console"
      =/  =cage  [%drum-unlink !>([our dude])]
      [%pass /kiln/link/[dude] %agent [our %hood] %poke cage]~
    --
  ::  +uninstall: stop tracking apps on desk, and suspend apps
  ::
  ++  uninstall
    |=  lac=desk
    ^+  kiln
    ?:  =(%base lac)
      =/  mes  "kiln: |uninstall: %base cannot be uninstalled"
      (^emit (pyre:pass leaf/mes ~))
    ?.  (~(has by ark) lac)
      ~>  %slog.0^leaf/"kiln: |uninstall: {<lac>} not installed, ignoring"
      kiln
    =.  vats  (abed lac)
    ~>  %slog.0^leaf/"kiln: uninstalling {here}"
    =.  vats  stop-agents
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
    =.  vats  (emit (diff:give %reset loc rak))
    =.  ark  (~(del by ark) loc)
    (install loc [ship desk]:rak)
  ::  +suspend: shut down all agents, keep syncing
  ::
  ++  suspend
    |=  lac=desk
    ^+  vats
    =/  got  (~(get by ark) lac)
    ?:  =(%base lac)
      =/  mes  "kiln: suspend: %base cannot be suspended"
      (emit (pyre:pass leaf/mes ~))
    ?.  (~(has by ark) lac)
      ~>  %slog.0^leaf/"kiln: suspend: {<lac>} not installed, ignoring"
      vats
    =.  vats  (abed lac)
    =.  liv.rein.rak  |
    =.  vats  stop-agents
    (emit (diff:give %suspend loc rak))
  ::  +revive: restart agents on a suspended desk
  ::
  ++  revive
    |=  lac=desk
    ^+  vats
    =.  vats  (abed lac)
    =.  liv.rein.rak  &
    =.  vats  (update-running-apps (get-apps-diff our loc now rein.rak))
    (emit (diff:give %revive loc rak))
  ::  +set-rein: adjust which agents are forced on or off
  ::
  ++  set-rein
    |=  [lac=desk new=rein]
    ^+  vats
    =.  vats  (abed lac)
    =^  old  rein.rak  [rein.rak new]
    ?+  [liv.old liv.new]  !!
      [%| %|]  vats
      [%| %&]  (revive lac)
      [%& %|]  (suspend lac)
      [%& %&]  (update-running-apps (get-apps-diff our loc now rein.rak))
    ==
  ::  +bump: handle kernel kelvin upgrade
  ::
  ::    Apply merges to revive faded agents on all paused desks.
  ::
  ++  bump
    |=  except=(set desk)
    ^+  kiln
    =/  kel=weft  [%zuse zuse]
    =/  ded  (~(dif in (get-blockers kel)) (~(put in except) %base))
    ?.  =(~ ded)
      =/  mes  "kiln: desks blocked upgrade to {<[- +]:kel>}: {<ded>}"
      (^emit (pyre:pass leaf/mes ~))
    =/  liv  (skip ~(tap in ~(key by ark)) ~(has in except))
    ~>  %slog.0^leaf/"kiln: bump {<liv>}"
    =<  kiln
    |-  ^+  vats
    ?~  liv  vats
    =.  vats  (abed i.liv)
    ::  skip to first commit at new kelvin
    ::
    =/  yon
      =*  nex  next.rak
      |-  ^-  (unit aeon)
      ?~  nex  ~
      ?:  =(kel weft.i.nex)
        `aeon.i.nex
      $(nex t.nex)
    ?~  yon
      =/  mes  "kiln: {here} killed upgrade to {<[- +]:kel>}"
      (emit (pyre:pass leaf/mes ~))
    =.  next.rak  (crank-next u.yon)
    =.  vats  (emit merge-main:pass)
    $(liv t.liv)
  ::  +stop-agents: internal helper to suspend agents on .loc
  ::
  ::    Will not shut down %hood or %dojo.
  ::
  ++  stop-agents
    ^+  vats
    =/  ded  (get-apps-live our loc now)
    =.  ded  (skip ded |=(d=dude ?=(?(%hood %dojo) d)))
    (stop-dudes ded)
  ::
  ++  take
    |=  [=wire syn=sign-arvo]
    ^+  kiln
    ?>  ?=([@ @ *] wire)
    ?:  ?=(%jolt i.t.wire)
      (take-onto wire syn)
    =<  abet
    =.  vats  (from-wire wire)
    ?+    i.t.wire
        ~>  %slog.0^leaf/"kiln: vats-bad-take {<wire>}"
        vats
      %find        (take-find syn)
      %sync        (take-sync syn)
      %download    (take-download syn)
      %merge-main  (take-merge-main syn)
      %merge-kids  (take-merge-kids syn)
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
    (emit sync-da:pass)
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
    =/  new-weft  (read-kelvin-foreign [ship desk aeon]:rak)
    =.  aeon.rak  +(aeon.rak)
    ::
    ?.  =(%base loc)
      ::  TODO: ?>  =(%zuse lal.new-weft) but more flexible for future renames
      ?:  (gth num.new-weft num.old-weft)
        ~>  %slog.0^leaf/"kiln: cannot install {here}, old kelvin {<new-weft>}"
        ~>  %slog.0^leaf/"kiln: will retry at foreign kelvin {<old-weft>}"
        =/  =diff  [%block loc rak new-weft blockers=(sy %base ~)]
        (emil sync-ud:pass (diff:give diff) ~)
      ?:  (lth num.new-weft num.old-weft)
        ~>  %slog.0^leaf/"kiln: future version {<new-weft>}, enqueueing"
        =.  next.rak  (snoc next.rak [(dec aeon.rak) new-weft])
        =/  =diff  [%block loc rak new-weft blockers=(sy %base ~)]
        (emil sync-ud:pass (diff:give diff) ~)
      ~>  %slog.0^leaf/"kiln: merging into {here}"
      =.  next.rak  (crank-next (dec aeon.rak))
      (emil ~[merge-main sync-ud]:pass)
    ::
    =/  blockers
      ?:  =(new-weft old-weft)
        ~
      (get-blockers new-weft)
    ::
    ?.  =(~ blockers)
      ~>  %slog.0^leaf/"kiln: OTA blocked on {<blockers>}"
      =.  next.rak  (snoc next.rak [(dec aeon.rak) new-weft])
      =/  =diff  [%block loc rak new-weft blockers]
      (emil sync-ud:pass (diff:give diff) ~)
    ~>  %slog.0^leaf/"kiln: applying OTA to {here}, kelvin: {<new-weft>}"
    =.  next.rak  (crank-next (dec aeon.rak))
    (emil ~[merge-main sync-ud]:pass)
  ::
  ++  take-merge-main
    |=  syn=sign-arvo
    ^+  vats
    ?>  ?=(%mere +<.syn)
    ?:  ?=([%| %ali-unavailable *] p.syn)
      =+  "kiln: merge into {here} failed, maybe because sunk; restarting"
      %-  (slog leaf/- p.p.syn)
      =.  vats  (emit (diff:give %merge-sunk loc rak p.p.syn))
      reset
    ?:  ?=(%| -.p.syn)
      =+  "kiln: merge into {here} failed, waiting for next revision"
      %-  (slog leaf/- p.p.syn)
      =.  vats  (emit (diff:give %merge-fail loc rak p.p.syn))
      vats
    ~>  %slog.0^leaf/"kiln: merge into {here} succeeded"
    =.  vats  (emit (diff:give %merge loc rak))
    =?  vats  liv.rein.rak
      (update-running-apps (get-apps-diff our loc now rein.rak))
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
      =.  vats  (emit (diff:give %merge-sunk %kids rak p.p.syn))
      reset
    ?-  -.p.syn
      %&  ~>  %slog.0^leaf/"kiln: OTA to %kids succeeded"
          (emit (diff:give %merge %kids rak))
      %|  ~>  %slog.0^leaf/"kiln: OTA to %kids failed {<p.p.syn>}"
          (emit (diff:give %merge-fail %kids rak p.p.syn))
    ==
  ::
  ++  take-onto
    |=  [=wire syn=sign-arvo]
    ^+  kiln
    =/  onto  ?>(?=([%gall %onto *] syn) p.syn)
    ?-  -.onto
      %&  kiln
      %|  (mean >p.onto< p.onto)
    ==
  ::
  ++  update-running-apps
    |=  [liv=(list dude) ded=(list dude)]
    =.  vats  (start-dudes liv)
    =.  vats  (stop-dudes ded)
    vats
  ::
  ++  start-dudes
    |=  daz=(list dude)
    ~>  %slog.0^leaf/"kiln: starting {<daz>}"
    (emil `(list card:agent:gall)`(zing (turn daz start-dude:pass)))
  ::
  ++  stop-dudes
    |=  daz=(list dude)
    ~>  %slog.0^leaf/"kiln: stopping {<daz>}"
    (emil `(list card:agent:gall)`(zing (turn daz stop-dude:pass)))
  ::  +crank-next: pop stale aeons from .next.rak
  ::
  ++  crank-next
    |=  new=aeon
    ^+  next.rak
    =/  rog  next.rak
    |-  ^+  next.rak
    ?~  rog  next.rak
    ?:  =(new aeon.i.rog)
      t.rog
    $(rog t.rog)
  --
::  +get-blockers: find desks that would block a kernel update
::
++  get-blockers
  |=  kel=weft
  ^-  (set desk)
  %-  ~(gas in *(set desk))
  %+  murn  ~(tap by ark)
  |=  [=desk =arak]
  ?:  =(%base desk)
    ~
  ?.  liv.rein.arak
    ~
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
    *  %take-that
    ::%1  %take-that
    ::*   %mate
  ==
::
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-kiln-bad-mark mark] !!)
    %kiln-autocommit         =;(f (f !<(_+<.f vase)) poke-autocommit)
    %kiln-bump               =;(f (f !<(_+<.f vase)) poke-bump)
    %kiln-cancel             =;(f (f !<(_+<.f vase)) poke-cancel)
    %kiln-cancel-autocommit  =;(f (f !<(_+<.f vase)) poke-cancel-autocommit)
    %kiln-commit             =;(f (f !<(_+<.f vase)) poke-commit)
    %kiln-fuse               =;(f (f !<(_+<.f vase)) poke-fuse)
    %kiln-fuse-list          =;(f (f !<(_+<.f vase)) poke-fuse-list)
    %kiln-gall-sear          =;(f (f !<(_+<.f vase)) poke-gall-sear)
    %kiln-info               =;(f (f !<(_+<.f vase)) poke-info)
    %kiln-install            =;(f (f !<(_+<.f vase)) poke-install)
    %kiln-label              =;(f (f !<(_+<.f vase)) poke-label)
    %kiln-merge              =;(f (f !<(_+<.f vase)) poke-merge)
    %kiln-mount              =;(f (f !<(_+<.f vase)) poke-mount)
    %kiln-nuke               =;(f (f !<(_+<.f vase)) poke-nuke)
    %kiln-suspend            =;(f (f !<(_+<.f vase)) poke-suspend)
    %kiln-permission         =;(f (f !<(_+<.f vase)) poke-permission)
    %kiln-revive             =;(f (f !<(_+<.f vase)) poke-revive)
    %kiln-rein               =;(f (f !<(_+<.f vase)) poke-rein)
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
++  poke-bump
  |=  except=(set desk)
  abet:(bump:vats +<)
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
        :(weld " [" (format-fuse-source fs) " " <g> "]" acc)
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
++  poke-gall-sear
  |=  =ship
  abet:(emit %pass /kiln %arvo %g %sear ship)
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
++  poke-nuke
  |=  [=term desk=?]
  =<  abet
  ?.  desk
    (emit %pass /nuke %arvo %g [%nuke term])
  %-  emil
  %+  turn  (get-apps-have our term now)
  |=([=dude ?] [%pass /nuke %arvo %g [%nuke dude]])
::
++  poke-permission
  |=  [syd=desk pax=path pub=?]
  =<  abet
  %-  emit
  =/  =rite  [%r ~ ?:(pub %black %white) ~]
  [%pass /kiln/permission %arvo %c [%perm syd pax rite]]
::
++  poke-rein
  |=  [=desk =rein]
  abet:abet:(set-rein:vats +<)
::
++  poke-revive
  |=  =desk
  abet:abet:(revive:vats desk)
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
++  poke-suspend
  |=  =desk
  abet:abet:(suspend:vats desk)
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
  ?:  =(0 ~(wyt by syn))
    [%leaf "no syncs configured"]~
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
::  +peer: handle %watch
::
++  peer
  |=  =path
  ?>  (team:title our src)
  ?+    path  ~|(kiln-path/path !!)
    [%vats ~]    abet
  ::
      [%ark ~]
    abet(moz :_(moz [%give %fact ~ %kiln-vats-snap !>(ark)]))
  ==
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+    wire  ~|([%kiln-bad-take-agent wire -.sign] !!)
      [%fancy *]
    ?>  ?=(%poke-ack -.sign)
    (take-coup-fancy t.wire p.sign)
  ::
      [%spam *]
    ?>  ?=(%poke-ack -.sign)
    (take-coup-spam t.wire p.sign)
  ::
      [%link @ ~]
    ?>  ?=(%poke-ack -.sign)
    ~>  %slog.0^leaf/"kiln: linked {<`@tas`i.t.wire>} to console"
    abet
  ==
::
++  take-arvo
  |=  [=wire =sign-arvo]
  ?-    wire
      [%sync %merg *]   %+  take-mere-sync  t.t.wire
                        ?>(?=(%mere +<.sign-arvo) +>.sign-arvo)
      [%find-ship *]    %+  take-writ-find-ship  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%sync *]         %+  take-writ-sync  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%autocommit *]   %+  take-wake-autocommit  t.wire
                        ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
      [%vats *]         abet:(take:vats t.wire sign-arvo)
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
      =/  bec=beak  (realize-fuse-source fs &)
      ?>  =(who.fs p.bec)
      ?>  =(des.fs q.bec)
      =/  hax=@ud  (mug [kf (~(got by hxs) syd)])
      =/  wir=wire
          /kiln/fuse-request/[syd]/(scot %p p.bec)/[q.bec]/(scot %ud hax)
      =/  rav=rave  [%sing %w r.bec /]
      =/  rif=riff  [q.bec `rav]
      `[%pass wir %arvo %c [%warp who.fs rif]]
    ::  No need to keep state if all the sources are static
    ?~  movs
      delete
    (blab movs)
  ::
  ++  send-fuse
    ^+  ..abet
    =/  bas=beak  (realize-fuse-source bas.kf |)
    =/  con=(list [beak germ])
      %+  turn
        con.kf
      |=  [fs=fuse-source g=germ]
      [(realize-fuse-source fs |) g]
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
    ?>  ?=([?(%clay %behn) %writ *] sign-arvo)
    =/  gif  +.sign-arvo
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
    |=  [fs=fuse-source incr=flag]
    ^-  beak
    :+  who.fs
      des.fs
    ?@  ver.fs
      (realize-case [who.fs des.fs incr])
    `case`ver.fs
  ::
  ++  realize-case
    |=  [who=ship des=desk incr=flag]
    ^-  case
    =/  let=(unit @ud)  (~(get by mox) [who des])
    ^-  case
    ?~  let
      da+now
    :-  %ud
    ?:  incr
      +(u.let)
    u.let
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
