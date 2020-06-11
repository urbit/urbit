::                                                      ::  ::
::::  /hoon/kiln/hood/lib                               ::  ::
  ::                                                    ::  ::
/?  310                                                 ::  version
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
=,  clay
=,  space:userlib
=,  format
|%                                                      ::  ::
++  part  {$kiln $0 pith}                               ::  kiln state
++  pith                                                ::  ::
    $:  rem/(map desk per-desk)                         ::
        syn/(map kiln-sync let/@ud)                     ::
        autoload-on/?                                   ::
        cur-hoon/@uvI                                   ::
        cur-arvo/@uvI                                   ::
        cur-zuse/@uvI                                   ::
        cur-vanes/(map @tas @uvI)                       ::
        commit-timer/{way/wire nex/@da tim/@dr mon=term}
    ==                                                  ::
++  per-desk                                            ::  per-desk state
    $:  auto/?                                          ::  escalate on failure
        gem/germ                                        ::  strategy
        her/@p                                          ::  from ship
        sud/@tas                                        ::  from desk
        cas/case                                        ::  at case
    ==                                                  ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  kiln-commit  term                                   ::
++  kiln-mount                                          ::
    $:  pax/path                                        ::
        pot/term                                        ::
    ==                                                  ::
++  kiln-unmount  $@(term {knot path})                  ::
++  kiln-sync                                           ::
    $:  syd/desk                                        ::
        her/ship                                        ::
        sud/desk                                        ::
    ==                                                  ::
++  kiln-unsync                                         ::
    $:  syd/desk                                        ::
        her/ship                                        ::
        sud/desk                                        ::
    ==                                                  ::
++  kiln-merge                                          ::
    $:  syd/desk                                        ::
        ali/ship                                        ::
        sud/desk                                        ::
        cas/case                                        ::
        gim/?($auto germ)                               ::
    ==                                                  ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|=  {bowl:gall part}                                    ::  main kiln work
?>  =(src our)
|_  moz/(list card:agent:gall)
++  abet                                                ::  resolve
  [(flop moz) `part`+<+.$]
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
  |=  {mez/tape sud/desk who/ship syd/desk}
  :^  %palm  [" " ~ ~ ~]  leaf+mez
  ~[leaf+"from {<sud>}" leaf+"on {<who>}" leaf+"to {<syd>}"]
::
++  poke-commit
  |=  [mon/kiln-commit auto=?]
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
  |=  [mon/kiln-commit auto=?]
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
  |=  mon/kiln-unmount
  ?^  mon
    =+  bem=(de-beam mon)
    ?~  bem
      =+  "can't unmount bad path: {<mon>}"
      abet:(spam leaf+- ~)
    abet:(emit %pass /unmount-beam %arvo %c [%ogre [[p q r] s]:u.bem])
  abet:(emit %pass /unmount-point %arvo %c [%ogre mon])
::
++  poke-track                                        ::
  |=  hos/kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already tracking" [sud her syd]:hos) ~)
  abet:abet:start-track:(auto hos)
::
++  poke-sync                                         ::
  |=  hos/kiln-sync
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
  |=(a/kiln-sync (render "sync configured" [sud her syd]:a))
::
++  poke-unsync                                         ::
  |=  hus/kiln-unsync
  ?.  (~(has by syn) hus)
    abet:(spam (render "not syncing" [sud her syd]:hus) ~)
  %*  .  abet:abet:stop:(auto hus)
    syn  (~(del by syn) hus)
  ==
::
++  poke-merge                                        ::
  |=  kiln-merge
  abet:abet:(merge:(work syd) ali sud cas gim)
::
++  poke-cancel
  |=  a=@tas
  abet:(emit %pass /cancel %arvo %c [%drop a])
::
++  poke-info
  |=  {mez/tape tor/(unit toro)}
  ?~  tor
    abet:(spam leaf+mez ~)
  abet:(emit:(spam leaf+mez ~) %pass /kiln %arvo %c [%info u.tor])
::
++  poke-rm
  |=  a/path
  =+  b=.^(arch %cy a)
  ?~  fil.b
    =+  ~[leaf+"No such file:" leaf+"{<a>}"]
    abet:(spam -)
  (poke-info "removed" `(fray a))
::
++  poke-label
  |=  {syd/desk lab/@tas}
  =+  pax=/(scot %p our)/[syd]/[lab]
  (poke-info "labeled {(spud pax)}" `[syd %| lab])
::
++  poke-schedule
  |=  {where/path tym/@da eve/@t}
  =.  where  (welp where /sched)
  %+  poke-info  "scheduled"
  =+  old=;;((map @da cord) (fall (file where) ~))
  `(foal where %sched !>((~(put by old) tym eve)))
::
++  poke-permission
  |=  {syd/desk pax/path pub/?}
  =<  abet
  %-  emit
  =/  =rite  [%r ~ ?:(pub %black %white) ~]
  [%pass /kiln/permission %arvo %c [%perm syd pax rite]]
::
++  poke-autoload  |=(lod/(unit ?) abet:(poke:autoload lod))
++  poke-start-autoload  |=(~ abet:start:autoload)
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-kiln-bad-mark mark] !!)
    %kiln-commit             =;(f (f !<(_+<.f vase)) poke-commit)
    %kiln-autocommit         =;(f (f !<(_+<.f vase)) poke-autocommit)
    %kiln-info               =;(f (f !<(_+<.f vase)) poke-info)
    %kiln-label              =;(f (f !<(_+<.f vase)) poke-label)
    %kiln-cancel             =;(f (f !<(_+<.f vase)) poke-cancel)
    %kiln-mount              =;(f (f !<(_+<.f vase)) poke-mount)
    %kiln-rm                 =;(f (f !<(_+<.f vase)) poke-rm)
    %kiln-schedule           =;(f (f !<(_+<.f vase)) poke-schedule)
    %kiln-track              =;(f (f !<(_+<.f vase)) poke-track)
    %kiln-sync               =;(f (f !<(_+<.f vase)) poke-sync)
    %kiln-syncs              =;(f (f !<(_+<.f vase)) poke-syncs)
    %kiln-wipe-ford          =;(f (f !<(_+<.f vase)) poke-wipe-ford)
    %kiln-keep-ford          =;(f (f !<(_+<.f vase)) poke-keep-ford)
    %kiln-autoload           =;(f (f !<(_+<.f vase)) poke-autoload)
    %kiln-overload           =;(f (f !<(_+<.f vase)) poke-overload)
    %kiln-goad-gall          =;(f (f !<(_+<.f vase)) poke-goad-gall)
    %kiln-gall-sear          =;(f (f !<(_+<.f vase)) poke-gall-sear)
    %kiln-wash-gall          =;(f (f !<(_+<.f vase)) poke-wash-gall)
    %kiln-unmount            =;(f (f !<(_+<.f vase)) poke-unmount)
    %kiln-unsync             =;(f (f !<(_+<.f vase)) poke-unsync)
    %kiln-permission         =;(f (f !<(_+<.f vase)) poke-permission)
    %kiln-cancel-autocommit  =;(f (f !<(_+<.f vase)) poke-cancel-autocommit)
    %kiln-start-autoload     =;(f (f !<(_+<.f vase)) poke-start-autoload)
    %kiln-merge              =;(f (f !<(_+<.f vase)) poke-merge)
  ==
::
++  autoload
  |%
  ++  emit
    |=  a/card:agent:gall
    +>(..autoload (^emit a))
  ::
  ++  tracked-vanes
    ^-  (list @tas)
    ~[%ames %behn %clay %dill %eyre %ford %gall %iris %jael]
  ::
  ++  our-home  /(scot %p our)/home/(scot %da now)
  ++  sys-hash  |=(pax/path .^(@uvI %cz :(welp our-home /sys pax)))
  ++  hash-vane
    |=  syd/@tas  ^-  (pair term @uvI)
    [syd (sys-hash /vane/[syd]/hoon)]
  ::
  ++  rehash-vanes
    ^+  cur-vanes
    (malt (turn tracked-vanes hash-vane))
  ::
  ::
  ++  poke
    |=  lod/(unit ?)
    ?^  lod
      ..autoload(autoload-on u.lod)
    =.  autoload-on  !autoload-on
    (spam leaf+"turned autoload {?:(autoload-on "on" "off")}" ~)
  ::
  ++  start
    =.  cur-hoon  (sys-hash /hoon/hoon)
    =.  cur-arvo  (sys-hash /arvo/hoon)
    =.  cur-zuse  (sys-hash /zuse/hoon)
    =.  cur-vanes  rehash-vanes
    subscribe-next
  ::
  ++  subscribe-next
    %-  emit
    [%pass /kiln/autoload %arvo %c [%warp our %home `[%next %z da+now /sys]]]
  ::
  ++  writ  =>(check-new subscribe-next)
  ++  check-new
    ?.  autoload-on
      ..check-new
    =/  new-hoon  (sys-hash /hoon/hoon)
    =/  new-arvo  (sys-hash /arvo/hoon)
    ?:  |(!=(new-hoon cur-hoon) !=(new-arvo cur-arvo))
      =.  cur-hoon  new-hoon
      =.  cur-arvo  new-arvo
      =.  cur-vanes  rehash-vanes
      (emit %pass /kiln/reload/hoon %agent [our %hood] %poke %helm-reset !>(~))
      ::  XX  updates cur-vanes?
    =/  new-zuse  (sys-hash /zuse/hoon)
    ?:  !=(new-zuse cur-zuse)
      =.  cur-zuse  new-zuse
      =.  cur-vanes  rehash-vanes
      =/  =cage  [%helm-reload !>([%zuse tracked-vanes])]
      (emit [%pass /kiln/reload/zuse %agent [our %hood] %poke cage])
    (roll tracked-vanes load-vane)
  ::
  ++  load-vane
    =<  %_(. con ..load-vane)
    |:  $:{syd/@tas con/_.}
    =.  +>.$  con
    =/  new-vane  q:(hash-vane syd)
    ?:  =(`new-vane (~(get by cur-vanes) syd))
      +>.$
    =.  cur-vanes  (~(put by cur-vanes) syd new-vane)
    =/  =cage  [%helm-reload !>(~[syd])]
    (emit %pass /kiln/reload/[syd] %agent [our %hood] %poke cage)
  ::
  ++  coup-reload
    |=  {way/wire saw/(unit tang)}
    ~?  ?=(^ saw)  [%kiln-reload-lame u.saw]
    +>.$
  --
::
++  poke-overload
  ::  +poke-overload: wipes ford cache at {start}, and then every {recur}.
  |=  [recur=@dr start=@da]
  ?>  (gte start now)
  abet:(emit %pass /kiln/overload/(scot %dr recur) %arvo %b [%wait start])
::
++  poke-wipe-ford
  |=(percent=@ud abet:(emit %pass /kiln %arvo %f [%wipe percent]))
::
++  poke-keep-ford
  |=  [compiler-cache-size=@ud build-cache-size=@ud]
  =<  abet
  (emit %pass /kiln %arvo %f [%keep compiler-cache-size build-cache-size])
::
++  poke-goad-gall
  |=  [force=? agent=(unit dude:gall)]
  abet:(emit %pass /kiln %arvo %g %goad force agent)
::
++  poke-gall-sear
  |=  =ship
  abet:(emit %pass /kiln %arvo %g %sear ship)
::
++  poke-wash-gall  |=(* abet:(emit %pass /kiln %arvo %g [%wash ~]))
::
++  done
  |=  {way/wire saw/(unit error:ames)}
  ~?  ?=(^ saw)  [%kiln-nack u.saw]
  abet
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+  wire  ~|([%kiln-bad-take-agent wire -.sign] !!)
    [%kiln %fancy *]   ?>  ?=(%poke-ack -.sign)
                       (take-coup-fancy t.t.wire p.sign)
    [%kiln %reload *]  ?>  ?=(%poke-ack -.sign)
                       (take-coup-reload t.t.wire p.sign)
    [%kiln %spam *]    ?>  ?=(%poke-ack -.sign)
                       (take-coup-spam t.t.wire p.sign)
  ==
::
++  take-general
  |=  [=wire =sign-arvo]
  ?-  wire
      [%sync %merg *]   %+  take-mere-sync  t.t.wire
                        ?>(?=(%mere +<.sign-arvo) +>.sign-arvo)
      [%autoload *]     %+  take-writ-autoload  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%find-ship *]    %+  take-writ-find-ship  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%sync *]         %+  take-writ-sync  t.wire
                        ?>(?=(%writ +<.sign-arvo) +>.sign-arvo)
      [%overload *]     %+  take-wake-overload  t.wire
                        ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
      [%autocommit *]   %+  take-wake-autocommit  t.wire
                        ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
      *
    ?+  +<.sign-arvo  ~|([%kiln-bad-take-card +<.sign-arvo] !!)
      %done  %+  done  wire
             ?>(?=(%done +<.sign-arvo) +>.sign-arvo)
      %made  %+  take-made  wire
             ?>(?=(%made +<.sign-arvo) +>.sign-arvo)
      %mere  %+  take-mere  wire
             ?>(?=(%mere +<.sign-arvo) +>.sign-arvo)
    ==
  ==
++  take  |=(way/wire ?>(?=({@ ~} way) (work i.way))) ::  general handler
++  take-mere                                         ::
  |=  {way/wire are/(each (set path) (pair term tang))}
  abet:abet:(mere:(take way) are)
::
++  take-made
  |=  [way=wire date=@da result=made-result:ford]
  ::  hack for |overload
  ::
  ::    We might have gotten an ignorable response back for our cache priming
  ::    ford call. If it matches our magic wire, ignore it.
  ::
  ?:  =(/prime/cache way)
    ~&  %cache-primed
    abet
  abet:abet:(made:(take way) date result)
::
++  take-coup-fancy                                   ::
  |=  {way/wire saw/(unit tang)}
  abet:abet:(coup-fancy:(take way) saw)
::
++  take-coup-reload                                  ::
  |=  {way/wire saw/(unit tang)}
  abet:(coup-reload:autoload way saw)
::
++  take-coup-spam                                    ::
  |=  {way/wire saw/(unit tang)}
  ~?  ?=(^ saw)  [%kiln-spam-lame u.saw]
  abet
::
++  take-mere-sync                                    ::
  |=  {way/wire mes/(each (set path) (pair term tang))}
  ?>  ?=({@ @ @ *} way)
  =/  hos/kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  abet:abet:(mere:(auto hos) mes)
::
++  take-writ-find-ship                               ::
  |=  {way/wire rot/riot}
  ?>  ?=({@ @ @ *} way)
  =/  hos/kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  abet:abet:(take-find-ship:(auto hos) rot)
::
++  take-writ-sync                                    ::
  |=  {way/wire rot/riot}
  ?>  ?=({@ @ @ *} way)
  =/  hos/kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  abet:abet:(writ:(auto hos) rot)
::
++  take-writ-autoload
  |=  {way/wire rot/riot}
  ?>  ?=(~ way)
  ?>  ?=(^ rot)
  abet:writ:autoload
::
++  take-wake-overload
  |=  {way/wire error=(unit tang)}
  ?^  error
    %-  (slog u.error)
    ~&  %kiln-take-wake-overload-fail
    abet
  ?>  ?=({@ ~} way)
  =+  tym=(slav %dr i.way)
  ~&  %wake-overload-deprecated
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
  |=  mes/(list tank)
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
    |=  new/(list card:agent:gall)
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
    =.  let  ?.  ?=($w p.p.u.rot)  let  ud:;;(cass:clay q.q.r.u.rot)
    =/  =wire  /kiln/sync/merg/[syd]/(scot %p her)/[sud]
    ::  germ: merge mode for sync merges
    ::
    ::    Initial merges from any source must use the %init germ.
    ::    Subsequent merges may use any germ, but if the source is
    ::    a remote ship with which we have not yet merged, we won't
    ::    share a merge-base commit and all germs but %that will fail.
    ::
    ::    We want to always use %that for the first remote merge.
    ::    But we also want local syncs (%base to %home or %kids)
    ::    to succeed after that first remote sync. To accomplish both
    ::    we simply use %that for the first three sync merges.
    ::    (The first two are from the pill.)
    ::
    =/  =germ
      =/  =cass
        .^(cass:clay %cw /(scot %p our)/[syd]/(scot %da now))
      ?:  =(0 ud.cass)
        %init
      ?:((gth 3 ud.cass) %that %mate)
    =<  %-  spam
        ?:  =(our her)  ~
        [(render "beginning sync" sud her syd) ~]
    (blab [%pass wire %arvo %c [%merg syd her sud ud+let germ]] ~)
  ::
  ++  mere
    |=  mes=(each (set path) (pair term tang))
    ?:  ?=([%| %bad-fetch-ali *] mes)
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
          $no-ali-disc
        :~  (render "sync activated" sud her syd)
            leaf+"note: blank desk {<sud>} on {<her>}"
        ==
      ==
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (warp wire her sud `[%sing %y ud+let /])
  --
::
++  work                                              ::  state machine
  |=  syd/desk
  =/  ,per-desk
      %+  ~(gut by rem)  syd
      =+  *per-desk
      %_(- cas [%da now])
  |%
  ++  abet                                            ::  resolve
    ..work(rem (~(put by rem) syd auto gem her sud cas))
  ::
  ++  blab
    |=  new/(list card:agent:gall)
    ^+  +>
    +>.$(moz (welp new moz))
  ::
  ++  win   .                                         ::  successful poke
  ++  lose
    ^+  .
    ~|  %kiln-work-fail
    .
  ::
  ++  ford-fail
    |=(tan/tang ~|(%ford-fail (mean tan)))
  ::
  ++  unwrap-tang
    |*  res/(each * tang)
    ?:  ?=(%& -.res)
      p.res
    (ford-fail p.res)
  ::
  ++  perform                                         ::
    ^+  .
    (blab [%pass /kiln/[syd] %arvo %c [%merg syd her sud cas gem]] ~)
  ::
  ++  fancy-merge                                     ::  send to self
    |=  {syd/desk her/@p sud/desk gem/?($auto germ)}
    ^+  +>
    =/  =cage  [%kiln-merge !>([syd her sud cas gem])]
    %-  blab  :_  ~
    [%pass /kiln/fancy/[^syd] %agent [our %hood] %poke cage]
  ::
  ++  spam  ::|=(tang ((slog +<) ..spam))
            |*(* +>(..work (^spam +<)))
  ++  merge
    |=  {her/@p sud/@tas cas/case gim/?($auto germ)}
    ^+  +>
    ?.  ?=($auto gim)
      perform(auto |, gem gim, her her, cas cas, sud sud)
    ?:  =(0 ud:.^(cass:clay %cw /(scot %p our)/[syd]/(scot %da now)))
      =>  $(gim %init)
      .(auto &)
    =>  $(gim %fine)
    .(auto &)
  ::
  ++  coup-fancy
    |=  saw/(unit tang)
    ?~  saw
      =>  (spam leaf+"%melding %{(trip sud)} into scratch space" ~)
      %-  blab  :_  ~
      =/  note  [%merg (cat 3 syd '-scratch') her sud cas gem]
      [%pass /kiln/[syd] %arvo %c note]
    =+  :-  "failed to set up conflict resolution scratch space"
        "I'm out of ideas"
    lose:(spam leaf+-< leaf+-> u.saw)
  ::
  ++  mere
    |=  are/(each (set path) (pair term tang))
    ^+  +>
    ?:  =(%meld gem)
      ?:  ?=(%& -.are)
        ?.  auto
          =+  "merged with strategy {<gem>}"
          win:(spam leaf+- ?~(p.are ~ [>`(set path)`p.are< ~]))
        :: ~?  >  =(~ p.are)  [%mere-no-conflict syd]
        =+  "mashing conflicts"
        =>  .(+>.$ (spam leaf+- ~))
        =+  tic=(cat 3 syd '-scratch')
        %-  blab  :_  ~
        =,  ford
        :*  %pass  /kiln/[syd]  %arvo  %f
        :*  %build  live=%.n
            ^-  schematic
            :-  %list
            ^-  (list schematic)
            :: ~&  >  kiln-mashing+[p.are syd=syd +<.abet]
            %+  turn  ~(tap in p.are)
            |=  pax/path
            ^-  [schematic schematic]
            :-  [%$ %path -:!>(*path) pax]
            =/  base=schematic  [%scry %c %x `rail`[[our tic] (flop pax)]]
            ?>  ?=([%da @] cas)
            =/  alis=schematic
              [%pin p.cas `schematic`[%scry %c %x [[our syd] (flop pax)]]]
            =/  bobs=schematic
              [%scry %c %x [[our syd] (flop pax)]]
            =/  dali=schematic  [%diff [our syd] base alis]
            =/  dbob=schematic  [%diff [our syd] base bobs]
            =/  for=mark
                =+  (slag (dec (lent pax)) pax)
                ?~(- %$ i.-)
            ^-  schematic
            [%mash [our tic] for [[her sud] for dali] [[our syd] for dbob]]
        ==  ==
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
        $init
      =+  :-  "auto merge failed on strategy %init"
          "I'm out of ideas"
      lose:(spam leaf+-< leaf+-> [>p.p.are< q.p.are])
    ::
        $fine
      ?.  ?=($bad-fine-merge p.p.are)
        =+  "auto merge failed on strategy %fine"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  (spam leaf+"%fine merge failed, trying %meet" ~)
      perform(gem %meet)
    ::
        $meet
      ?.  ?=($meet-conflict p.p.are)
        =+  "auto merge failed on strategy %meet"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  (spam leaf+"%meet merge failed, trying %mate" ~)
      perform(gem %mate)
    ::
        $mate
      ?.  ?=($mate-conflict p.p.are)
        =+  "auto merge failed on strategy %mate"
        lose:(spam leaf+- >p.p.are< q.p.are)
      =>  .(gem %meld)
      =+  tic=(cat 3 syd '-scratch')
      =>  =+  :-  "%mate merge failed with conflicts,"
              "setting up scratch space at %{(trip tic)}"
          [tic=tic (spam leaf+-< leaf+-> q.p.are)]
      (fancy-merge tic our syd %init)
    ==
  ::
  ++  tape-to-tanks
    |=  a/tape  ^-  (list tank)
    (scan a (more (just '\0a') (cook |=(a/tape leaf+a) (star prn))))
  ::
  ++  tanks-if-any
    |=  {a/tape b/(list path) c/tape}  ^-  (list tank)
    ?:  =(~ b)  ~
    (welp (tape-to-tanks "\0a{c}{a}") >b< ~)
  ::
  ++  made
    |=  [date=@da result=made-result:ford]
    ::  |=  {dep/@uvH reg/gage:ford}
    ^+  +>
    ::
    ?:  ?=([%incomplete *] result)
      =+  "failed to mash"
      lose:(spam leaf+- tang.result)
    ?:  ?=([%complete %error *] result)
      =+  "failed to mash"
      lose:(spam leaf+- message.build-result.result)
    ?>  ?=([%complete %success %list *] result)
    =/  can=(list (pair path (unit miso)))
        %+  turn  results.build-result.result
        |=  res=build-result:ford
        ^-  (pair path (unit miso))
        ?>  ?=([%success ^ *] res)
        ~!  res
        =+  pax=(result-to-cage:ford head.res)
        =+  dif=(result-to-cage:ford tail.res)
        ::
        ?.  ?=($path p.pax)
          ~|  "strange path mark: {<p.pax>}"
          !!
        [;;(path q.q.pax) ?:(?=($null p.dif) ~ `[%dif dif])]
    :: ~&  >  kiln-made+[(turn can head) syd=syd +<.abet]
    =+  notated=(skid can |=({path a/(unit miso)} ?=(^ a)))
    =+  annotated=(turn `(list (pair path *))`-.notated head)
    =+  unnotated=(turn `(list (pair path *))`+.notated head)
    =+  `desk`(cat 3 syd '-scratch')
    =/  tan=(list tank)
        %-  zing
        ^-  (list (list tank))
        :~  %-  tape-to-tanks
            """
            done setting up scratch space in {<[-]>}
            please resolve the following conflicts and run
            |merge {<syd>} our {<[-]>}
            """
            %^  tanks-if-any
              "annotated conflicts in:"  annotated
            ""
            %^  tanks-if-any
              "unannotated conflicts in:"  unnotated
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
    :*  %info
        (cat 3 syd '-scratch')  %&
        %+  murn  can
        |=  {p/path q/(unit miso)}
        `(unit (pair path miso))`?~(q ~ `[p u.q])
    ==  ==
  --
--
