/-  *hood
/+  strandio
=,  clay
=,  space:userlib
=,  format
=*  dude  dude:gall
|%
+$  state     state-10
+$  state-10  [%10 pith-10]
+$  state-9   [%9 pith-9]
+$  state-8   [%8 pith-9]
+$  state-7   [%7 pith-7]
+$  state-6   [%6 pith-6]
+$  state-5   [%5 pith-5]
+$  state-4   [%4 pith-4]
+$  state-3   [%3 pith-3]
+$  state-2   [%2 pith-2]
+$  state-1   [%1 pith-1]
+$  state-0   [%0 pith-0]
+$  any-state
  $~  *state
  $%  state-10
      state-9
      state-8
      state-7
      state-6
      state-5
      state-4
      state-3
      state-2
      state-1
      state-0
  ==
::
+$  pith-10
  $:  rem=(map desk per-desk)
      nyz=@ud
      zyn=(map kiln-sync sync-state)
      commit-timer=[way=wire nex=@da tim=@dr mon=term]
      ::  map desk to the currently ongoing fuse request
      ::  and the latest version numbers for beaks to
      fus=(map desk per-fuse)
      ::  used for fuses - every time we get a fuse we
      ::  bump this. used when calculating hashes to
      ::  ensure they're unique even when the same
      ::  request is made multiple times.
      hxs=(map desk @ud)
  ==
::
+$  pith-9
  $:  wef=(unit weft)
      rem=(map desk per-desk)
      syn=(map kiln-sync let=@ud)
      ark=(map desk arak-9)
      commit-timer=[way=wire nex=@da tim=@dr mon=term]
      ::  map desk to the currently ongoing fuse request
      ::  and the latest version numbers for beaks to
      fus=(map desk per-fuse)
      ::  used for fuses - every time we get a fuse we
      ::  bump this. used when calculating hashes to
      ::  ensure they're unique even when the same
      ::  request is made multiple times.
      hxs=(map desk @ud)
  ==
::
::  $rein-9: diff from desk manifest
::
::    .liv: suspended? if suspended, no agents should run
::    .add: agents not in manifest that should be running
::    .sub: agents in manifest that should not be running
::
+$  rein-9
  $:  liv=_&
      add=(set dude)
      sub=(set dude)
  ==
::
+$  pith-7
  $:  wef=(unit weft)
      rem=(map desk per-desk)
      syn=(map kiln-sync let=@ud)
      ark=(map desk arak-7)
      commit-timer=[way=wire nex=@da tim=@dr mon=term]
      ::  map desk to the currently ongoing fuse request
      ::  and the latest version numbers for beaks to
      fus=(map desk per-fuse)
      ::  used for fuses - every time we get a fuse we
      ::  bump this. used when calculating hashes to
      ::  ensure they're unique even when the same
      ::  request is made multiple times.
      hxs=(map desk @ud)
  ==
::
+$  arak-9
  $:  rail=(unit rail-9)
      rein=rein-9
  ==
::
++  rail-9
  $:  publisher=(unit ship)
      paused=?
      =ship
      =desk
      =aeon
      next=(list rung)
  ==
::
+$  arak-7
  $:  rail=(unit rail-7)
      rein=rein-9
  ==
::
+$  rail-7
  $:  paused=?
      =ship
      =desk
      =aeon
      next=(list rung)
  ==
::
+$  pith-6
  $:  wef=(unit weft)
      rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ark=(map desk arak-6)                             ::
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
::
+$  arak-6  [rail=rail-6 next=(list rung) rein=rein-9]
+$  rail-6  [paused=? =ship =desk =aeon]
::
+$  pith-5
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ark=(map desk arak-6)                             ::
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
::
+$  pith-4                                              ::
  $:  rem=(map desk per-desk)                           ::
      syn=(map kiln-sync let=@ud)                       ::
      ark=(map desk arak-4)                             ::
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
+$  arak-4
  $:  =ship
      =desk
      =aeon
      next=(list rung)
      rein=rein-9
  ==
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
      next=(list rung)
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
::  +fmt: format string for slogging
::
++  fmt
  |=  mes=tape
  [%0 %leaf (weld "kiln: " mes)]
::
++  render
  |=  [mez=tape sud=desk who=ship syd=desk kid=(unit desk)]
  :^  %palm  [" " ~ ~ ~]  leaf+(weld "kiln: " mez)
  :^  leaf+"from {<sud>}"  leaf+"on {<who>}"  leaf+"to {<syd>}"
  ?~  kid  ~
  [leaf+"then {<u.kid>}" ~]
::
++  sources
  =/  zyns=(list [[syd=desk her=ship sud=desk] *])  ~(tap by zyn)
  =|  sources=(map desk [ship desk])
  |-  ^+  sources
  ?~  zyns
    sources
  =.  sources  (~(put by sources) -.i.zyns)
  $(zyns t.zyns)
::
++  on-init
  =<  abet
  ~>  %slog.(fmt "boot")
  ::
  =+  .^(desks=(set desk) %cd /(scot %p our)//(scot %da now))
  =.  desks  (~(del in desks) %base)
  =.  desks  (~(del in desks) %kids)
  ::
  =/  sop=ship  (sein:title our now our)
  ::  set up base desk
  ::
  =?  ..on-init  ?=(?(%earl %duke %king) (clan:title our))
    abet:init:(apex:(sync %base sop %kids) `%kids)
  ::  install other desks and make them public
  ::
  =/  dez=(list desk)  ~(tap in desks)
  |-  ^+  ..on-init
  ?~  dez  ..on-init
  =.  ..on-init
    (emit %pass /kiln/init-zest %arvo %c %zest i.dez %live)
  =.  ..on-init
    %-  emit
    :^  %pass  /kiln/permission  %arvo
    [%c %perm i.dez / %r `[%black ~]]
  =/  src  (get-publisher our i.dez now)
  =?  ..on-init  &(?=(^ src) !=(our u.src))
    abet:init:(sync i.dez u.src i.dez)
  $(dez t.dez)
::
++  on-load
  =>  |%
      +$  ota  [syd=desk her=ship sud=desk]
      --
  =|  old-ota=(unit ota)
  |=  [hood-version=@ud old=any-state]
  =/  old-version  -.old
  =*  state        +<+.$.abet
  ::
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
  ::
  =?  old  ?=(%3 -.old)
    :-  %4
    +.old(ark (~(run by ark.old) |=(a=arak-3 a(rein [liv=& rein.a]))))
  ::
  =?  old  ?=(%4 -.old)
    :-  %5
    =-  +.old(ark -)
    %-   ~(run by ark.old)
    |=  a=arak-4
    ^-  arak-6
    [[paused=| ship desk aeon] next rein]:a
  ::
  =?  old  ?=(%5 -.old)
    [%6 ~ +.old]
  ::
  =?  old  ?=(%6 -.old)
    :-  %7
    =-  +.old(ark -)
    %-  ~(run by ark.old)
    |=  a=arak-6
    ^-  arak-7
    :_  rein.a
    ^-  (unit rail-7)
    `[paused.rail ship.rail desk.rail aeon.rail next]:a
  ::
  =?  old  ?=(%7 -.old)
    :-  %8
    =-  +.old(ark -)
    %-  ~(gas by *(map desk arak-9))
    %+  turn  ~(tap by ark.old)
    |=  [d=desk a=arak-7]
    ^-  [desk arak-9]
    :-  d
    :_  rein.a
    ?~  rail.a  ~
    `[(get-publisher our d now) u.rail.a]
  ::
  =?  old  ?=(%8 -.old)
    [%9 +.old]
  ::
  =^  cards-9=(list card:agent:gall)  old
    ?.  ?=(%9 -.old)
      `old
    =/  syn=(set kiln-sync)
      %-  ~(gas in ~(key by syn.old))
      %+  murn  ~(tap by ark.old)
      |=  [=desk =arak-9]
      ?~  rail.arak-9
        ~
      ?:  paused.u.rail.arak-9
        ~
      `u=[desk ship.u.rail.arak-9 desk.u.rail.arak-9]
    =/  zet=(list [desk zest])
      %+  murn  ~(tap by ark.old)
      |=  [=desk =arak-9]
      ^-  (unit [^desk zest])
      ?:  liv.rein.arak-9
        `[desk %held]
      ?~  rail.arak-9
        ~
      ?:  paused.u.rail.arak-9
        ~
      `[desk %held]
    ::
    :_  [%10 |1.+.old(syn 0, ark ~)]
    ;:  weld
      %+  turn  zet
      |=  [=desk =zest]
      [%pass /kiln/load-zest %arvo %c %zest desk zest]
    ::
      %+  turn  ~(tap in syn)
      |=  k=kiln-sync
      [%pass /kiln/load-sync %agent [our %hood] %poke %kiln-sync !>(k)]
    ::
      =/  ks  ~(tap in syn)
      |-  ^-  (list card:agent:gall)
      ?~  ks
        ~
      ?:  =(%base syd.i.ks)
        :_  ~
        :*  %pass  /kiln/load-kids  %agent  [our %hood]
            %poke  %kiln-kids  !>([i.ks `%kids])
        ==
      $(ks t.ks)
    ==
  ::
  ?>  ?=(%10 -.old)
  =.  state  old
  abet:(emil cards-9)
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  [~ ~]
      [%x %kiln %our ~]  ``noun+!>(our)
      [%x %kiln %lag ~]
    ``loob+!>(.^(? //(scot %p our)//(scot %da now)/zen/lag))
  ::
      [%x %kiln %base-hash ~]
    =/  ver  (mergebase-hashes our %base now (~(got by sources) %base))
    ``noun+!>(?~(ver 0v0 i.ver))
  ::
      [%x %kiln %syncs ~]  ``noun+!>(zyn)
      [%x %kiln %sources ~]  ``noun+!>(sources)
      [%x %kiln %pikes ~]
    =+  .^(=rock:tire %cx /(scot %p our)//(scot %da now)/tire)
    :^  ~  ~  %kiln-pikes
    !>  ^-  pikes
    %-  ~(rut by rock)
    |=  [=desk =zest wic=(set weft)]
    ^-  pike
    =+  .^(hash=@uv %cz /(scot %p our)/[desk]/(scot %da now))
    =/  sync  (~(get by sources) desk)
    [sync hash zest wic]
  ==
::
::  +get-germ: select merge strategy into local desk
::
::  If destination desk doesn't exist, need a %init merge.  Otherwise,
::  we just want what the remote has, so we use %only-that.
::
++  get-germ
  |=  =desk
  =+  .^(=cass:clay %cw /(scot %p our)/[desk]/(scot %da now))
  ?-  ud.cass
    %0  %init
    *   %only-that
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
    %kiln-kids               =;(f (f !<(_+<.f vase)) poke-kids)
    %kiln-label              =;(f (f !<(_+<.f vase)) poke-label)
    %kiln-merge              =;(f (f !<(_+<.f vase)) poke-merge)
    %kiln-mount              =;(f (f !<(_+<.f vase)) poke-mount)
    %kiln-nuke               =;(f (f !<(_+<.f vase)) poke-nuke)
    %kiln-pause              =;(f (f !<(_+<.f vase)) poke-pause)
    %kiln-permission         =;(f (f !<(_+<.f vase)) poke-permission)
    %kiln-revive             =;(f (f !<(_+<.f vase)) poke-revive)
    %kiln-rein               =;(f (f !<(_+<.f vase)) poke-rein)
    %kiln-rm                 =;(f (f !<(_+<.f vase)) poke-rm)
    %kiln-schedule           =;(f (f !<(_+<.f vase)) poke-schedule)
    %kiln-suspend            =;(f (f !<(_+<.f vase)) poke-suspend)
    %kiln-suspend-many       =;(f (f !<(_+<.f vase)) poke-suspend-many)
    %kiln-sync               =;(f (f !<(_+<.f vase)) poke-sync)
    %kiln-syncs              =;(f (f !<(_+<.f vase)) poke-syncs)
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
  |=  ~
  =<  abet
  =+  .^(=rock:tire %cx /(scot %p our)//(scot %da now)/tire)
  =/  wic
    %+  sort  ~(tap by wic:(~(got by rock) %base))
    |=  [[* a=@ud] [* b=@ud]]
    (gth a b)
  =.  wic  (skip wic |=([* a=@ud] (gte a zuse)))
  ?~  wic
    %-  (slog 'kiln: %base already up-to-date' ~)
    ..abet
  =/  kel  i.wic
  %-  emil
  =/  desks=(list [=desk =zest])
    %+  murn  ~(tap by rock)
    |=  [=desk =zest wic=(set weft)]
    ?:  |(=(%base desk) !?=(%live zest) (~(has in wic) kel))
      ~
    `u=[desk %held]
  [%pass /kiln/bump/zeal %arvo %c %zeal desks]~
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
  =+  .^(=rock:tire %cx /(scot %p our)//(scot %da now)/tire)
  =/  =zest
    ?~  got=(~(get by rock) loc)
      %dead
    zest.u.got
  =.  zyn
    ?~  got=(~(get by sources) loc)
      zyn
    (~(del by zyn) loc u.got)
  =?  ..abet  ?=(%dead zest)
    (emit %pass /kiln/install %arvo %c %zest loc ?:(=(our her) %live %held))
  ?:  (~(has by zyn) loc her rem)
    abet:(spam (render "already syncing" loc her rem ~) ~)
  ?:  =([our loc] [her rem])
    abet
  =/  sun  (sync loc her rem)
  ~>  %slog.(fmt "beginning install into {here:sun}")
  =<  abet:abet:init
  ?:  =(%base loc)
    (apex:sun `%kids)
  sun
::
++  poke-kids
  |=  [hos=kiln-sync nex=(unit desk)]
  abet:abet:(apex:(sync hos) nex)
::
++  poke-label
  |=  [syd=desk lab=@tas aey=(unit aeon)]
  =+  pax=/(scot %p our)/[syd]/[lab]
  (poke-info "labeled {(spud pax)}" `[syd %| lab aey])
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
++  poke-pause
  |=  =desk
  ?~  got=(~(get by sources) desk)
    abet:(spam leaf+"desk not installed: {<desk>}" ~)
  (poke-unsync desk u.got)
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
  abet:(emit %pass /kiln/rein %arvo %c %rein desk rein)
::
++  poke-revive
  |=  =desk
  abet:(emit %pass /kiln/revive %arvo %c %zest desk %live)
::
++  poke-rm
  |=  a=path
  =|  c=(list (unit toro))
  %+  poke-info  "removed: {<a>}"
  =-  %+  roll  -
      |=  [a=(unit toro) b=(unit toro)]
      (clap a b furl)
  |-  ^-  (list (unit toro))
  =+  b=.^(arch %cy a)
  ?:  ?=([^ ~] b)  (snoc c `(fray a))  
  =?  c  ?=(^ fil.b)  (snoc c `(fray a))
  %-  zing
  %+  turn  ~(tap by dir.b)
  |=  [kid=@ta ~]
  ^$(a (weld a /[kid]))
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
  (poke-suspend-many ~[desk])
::
++  poke-suspend-many
  |=  desks=(list desk)
  =<  abet
  %-  emil
  %+  turn
    %+  skim  desks
    |=  dek=desk
    ?:  (~(has in .^((set desk) %cd /(scot %p our)//(scot %da now))) dek)
      &  
    ~>  %slog.(fmt "desk does not yet exist: {<dek>}")  |   
  |=(=desk [%pass /kiln/suspend %arvo %c %zest desk %dead])
::
++  poke-sync
  |=  hos=kiln-sync
  ?:  (~(has by zyn) hos)
    abet:(spam (render "already syncing" [sud her syd ~]:hos) ~)
  ~>  %slog.(fmt "beginning sync into {<syd.hos>} from {<her.hos>}/{<sud.hos>}")
  abet:abet:init:(sync hos)
::
++  poke-syncs                                        ::  print sync config
  |=  ~
  =<  abet  %-  spam
  ?:  =(0 ~(wyt by zyn))
    [%leaf "no syncs configured"]~
  %+  turn  ~(tap by zyn)
  |=  [kiln-sync sync-state]
  (render "sync configured" sud her syd kid)
::
++  poke-uninstall
  |=  loc=desk
  =+  .^(=rock:tire %cx /(scot %p our)//(scot %da now)/tire)
  ?~  got=(~(get by rock) loc)
    abet:(spam leaf+"desk does not exist: {<loc>}" ~)
  ~>  %slog.(fmt "uninstalling {<loc>}")
  =?  ..on-init  !=(+<:got %dead)  
    (emit %pass /kiln/uninstall %arvo %c %zest loc %dead)
  ?~  sync=(~(get by sources) loc)
    abet
  (poke-unsync loc u.sync)
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
::  Don't need to cancel anything because new syncs will get a new nonce
::
++  poke-unsync
  |=  hus=kiln-unsync
  ?~  got=(~(get by zyn) hus)
    abet:(spam (render "not syncing" [sud her syd ~]:hus) ~)
  =.  zyn  (~(del by zyn) hus)
  abet:(spam (render "cancelling sync" sud.hus her.hus syd.hus kid.u.got) ~)
::  +peer: handle %watch
::
++  peer
  |=  =path
  ?>  (team:title our src)
  ?:  =(0 1)  abet  ::  avoid mint-vain
  ?+    path  ~|(kiln-path/path !!)
      [%vats ~]
    (mean leaf+"kiln: old subscription to /kiln/vats failed" ~)
  ==
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+      wire
        ?:  ?=(%poke-ack -.sign)
          ~?  ?=(^ p.sign)  [%kiln-poke-nack u.p.sign]
          abet
        ~|([%kiln-bad-take-agent wire -.sign] !!)
  ::
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
    ~>  %slog.(fmt "linked {<`@tas`i.t.wire>} to console")
    abet
  ==
::
++  take-arvo
  |=  [=wire =sign-arvo]
  ^+  abet
  ?-    wire
      [%sync %merg *]   abet
      [%find-ship *]    abet
      [%sync *]         abet
      [%zinc *]         (take-sync t.wire sign-arvo)
      [%autocommit *]   %+  take-wake-autocommit  t.wire
                        ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
      [%vats *]         abet
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
      %done  (done wire +>.sign-arvo)
      %mere  (take-mere wire +>.sign-arvo)
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
++  take-sync
  |=  [=wire =sign-arvo]
  ?>  ?=([@ @ @ *] wire)
  =*  syd  i.wire
  =/  her  (slav %p i.t.wire)
  =*  sud  i.t.t.wire
  ?.  (~(has by zyn) syd her sud)
    abet
  abet:abet:(take:(sync syd her sud) t.t.t.wire sign-arvo)
::
++  sync
  |=  kiln-sync
  =/  got  (~(get by zyn) syd her sud)
  =+  `sync-state`(fall got [(scot %uv nyz) ~ *@ud])
  =?  nyz  ?=(~ got)  +(nyz)
  |%
  ++  abet  ..sync(zyn (~(put by zyn) [syd her sud] nun kid let))
  ++  apex  |=(nex=(unit desk) ..abet(kid nex))
  ++  emit  |=(card:agent:gall ..abet(kiln (^emit +<)))
  ++  emil  |=((list card:agent:gall) ..abet(kiln (^emil +<)))
  ++  here  "{<syd>} from {<her>}/{<sud>}"
  ++  ware
    |=  =wire
    [%kiln %zinc syd (scot %p her) sud nun wire]
  ++  lard
    |=  [=wire =shed:khan]
    (emit %pass (ware wire) %arvo %k %lard %base shed)
  ++  merg
    |=  [=wire =desk]
    %:  emit
      %pass  (ware wire)  %arvo  %c
      %merg  desk  her  sud
      ud+(dec let)  (get-germ desk)
    ==
  ::
  ::  (re)Start a sync from scratch by finding what version the source
  ::  desk is at
  ::
  ++  init
    ^+  ..abet
    =.  let  0
    %+  lard  /init
    =/  m  (strand:rand ,vase)
    ;<  =riot:clay  bind:m  (warp:strandio her sud ~ %sing %y ud+1 /)
    ?>  ?=(^ riot)
    ~>  %slog.(fmt "activated install into {here}")
    ;<  now=@da     bind:m  get-time:strandio
    ;<  =riot:clay  bind:m  (warp:strandio her sud ~ %sing %w da+now /)
    ?>  ?=(^ riot)
    =+  !<(=cass:clay q.r.u.riot)
    (pure:m !>(ud.cass))
  ::
  ::  Listen for the next revision, and download it
  ::
  ++  next
    ^+  ..abet
    %+  lard  /next
    =/  m  (strand:rand ,vase)
    ;<  =riot:clay  bind:m  (warp:strandio her sud ~ %sing %w ud+let /)
    ?>  ?=(^ riot)
    ::  The syncs may have changed, so get the latest
    ::
    ;<  zyx=(map kiln-sync sync-state)  bind:m
      (scry:strandio (map kiln-sync sync-state) /gx/hood/kiln/syncs/noun)
    ?.  (~(has by zyx) syd her sud)
      (pure:m !>(%done))
    ~>  %slog.(fmt "downloading update for {here}")
    ;<  =riot:clay  bind:m  (warp:strandio her sud ~ %sing %v ud+let /)
    ?>  ?=(^ riot)
    (pure:m !>(%done))
  ::
  ::  Main control router
  ::
  ::  NB: %next, %main, and %kids are conceptually a single state with a
  ::  single error handling mechanism (move on to the next version).  We
  ::  cannot combine them into a single lard because when you update
  ::  main you may update spider, and in that case all active threads
  ::  are killed, which would stop us from continuing that thread.
  ::  Instead, we do the merges to syd and kid explicitly.
  ::
  ++  take
    |=  [=wire =sign-arvo]
    ^+  ..abet
    ?>  ?=([@ @ *] wire)
    ?.  =(nun i.wire)
      ..abet
    ?+      i.t.wire
          ~>  %slog.(fmt "sync-bad-take {<wire>}")
          ..abet
        %init
      ?.  =(0 let)
        ~>  %slog.(fmt "sync-bad-stage {<let>} {<wire>}")
        ..abet
      ?>  ?=(%arow +<.sign-arvo)
      ?:  ?=(%| -.p.sign-arvo)
        ~>  %slog.(fmt "activation failed into {here}; retrying sync")
        %-  (slog p.p.sign-arvo)
        init
      ::  Now that we know the revision, start main download loop
      ::
      =.  let  !<(@ud q.p.p.sign-arvo)
      next
    ::
        %next
      ?>  ?=(%arow +<.sign-arvo)
      ?:  ?=(%| -.p.sign-arvo)
        ::  ~>  %slog.(fmt "download failed into {here}; retrying sync")
        ::  %-  (slog p.p.sign-arvo)
        init
      ::
      ~>  %slog.(fmt "finished downloading update for {here}")
      =.  let  +(let)
      ::  If nothing changed, just ensure %kids is up-to-date and advance
      ::
      ?.  (get-remote-diff our syd now [her sud (dec let)])
        =<  next
        ?~  kid
          ~>  %slog.(fmt "remote is identical to {here}, skipping")
          ..abet
        ?.  (get-remote-diff our u.kid now [her sud (dec let)])
          ~>  %slog.(fmt "remote is identical to {here}, skipping")
          ..abet
        ~>  %slog.(fmt "remote is identical to {here}, merging into {<u.kid>}")
        (merg /kids u.kid)
      ::  Else start merging, but also immediately start listening to
      ::  the next revision.  Now, all errors should no-op -- we're
      ::  already waiting for the next revision.
      ::
      =.  ..abet  (merg /main syd)
      next
    ::
        %main
      ?>  ?=(%mere +<.sign-arvo)
      ?:  ?=(%| -.p.sign-arvo)
        =+  "kiln: merge into {here} failed, waiting for next revision"
        %-  (slog leaf/- p.p.sign-arvo)
        ..abet
      ~>  %slog.(fmt "merge into {<syd>} succeeded")
      ::  If we have a kids desk parameter, merge into that
      ::
      ?~  kid
        ..abet
      ~>  %slog.(fmt "kids merge into {<u.kid>}")
      (merg /kids u.kid)
    ::
        %kids
      ?>  ?=(%mere +<.sign-arvo)
      ?~  kid
        ..abet
      ::  Just notify; we've already started listening for the next
      ::  version
      ::
      ?-  -.p.sign-arvo
        %&  ~>  %slog.(fmt "kids merge to {<u.kid>} succeeded")
            ..abet
        %|  ~>  %slog.(fmt "kids merge to {<u.kid>} failed")
            %-  (slog p.p.sign-arvo)
            ..abet
      ==
    ==
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
