::                                                      ::  ::
::::  /hoon/kiln/hood/lib                               ::  ::
  ::                                                    ::  ::
/?  310                                                 ::  version
/-  hall
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
        syn/(map kiln-sync {let/@ud ust/bone})          ::
        autoload-on/?                                   ::
        cur-hoon/@uvI                                   ::
        cur-arvo/@uvI                                   ::
        cur-zuse/@uvI                                   ::
        cur-vanes/(map @tas @uvI)                       ::
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
=>  |%                                                  ::  arvo structures
    ++  card                                            ::
      $%  {$build wire ? schematic:ford}                ::
          {$drop wire @tas}                             ::
          {$info wire @tas nori}                        ::
          {$mont wire @tas beam}                        ::
          {$dirk wire @tas}                             ::
          {$ogre wire $@(@tas beam)}                    ::
          {$merg wire @tas @p @tas case germ}           ::
          {$perm wire desk path rite}                   ::
          {$poke wire dock pear}                        ::
          {$wipe wire @ud}                              ::
          [%keep wire compiler-cache-size=@ud build-cache-size=@ud]
          {$wait wire @da}                              ::
          {$warp wire ship riff}                        ::
      ==                                                ::
    ++  pear                                            ::  poke fruit
      $%  {$hall-command command:hall}                  ::
          {$kiln-merge kiln-merge}                      ::
          {$helm-reload (list term)}                    ::
          {$helm-reset ~}                              ::
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    --
|_  moz/(list move)
++  abet                                                ::  resolve
  [(flop moz) `part`+<+.$]
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))            ::  return card
++  emil                                                ::  return cards
  |=  (list card)
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  render
  |=  {mez/tape sud/desk who/ship syd/desk}
  :^  %palm  [" " ~ ~ ~]  leaf+mez
  ~[leaf+"from {<sud>}" leaf+"on {<who>}" leaf+"to {<syd>}"]
::
++  poke-commit
  |=  mon/kiln-commit
  abet:(emit %dirk /commit mon)
::
++  poke-mount
  |=  kiln-mount
  =+  bem=(de-beam pax)
  ?~  bem
    =+  "can't mount bad path: {<pax>}"
    abet:(spam leaf+- ~)
  abet:(emit %mont /mount pot u.bem)
::
++  poke-unmount
  |=  mon/kiln-unmount
  ?^  mon
    =+  bem=(de-beam mon)
    ?~  bem
      =+  "can't unmount bad path: {<mon>}"
      abet:(spam leaf+- ~)
    abet:(emit %ogre /unmount-beam [[p q r] s]:u.bem)
  abet:(emit %ogre /unmount-point mon)
::
++  poke-track                                        ::
  |=  hos/kiln-sync
  ?:  (~(has by syn) hos)
    abet:(spam (render "already syncing" [sud her syd]:hos) ~)
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
  |=  syd/desk
  abet:(emit %drop /cancel syd)
::
++  poke-info
  |=  {mez/tape tor/(unit toro)}
  ?~  tor
    abet:(spam leaf+mez ~)
  abet:(emit:(spam leaf+mez ~) %info /kiln u.tor)
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
  [%perm /kiln/permission syd pax %r ~ ?:(pub %black %white) ~]
::
++  poke-autoload  |=(lod/(unit ?) abet:(poke:autoload lod))
++  poke-start-autoload  |=(~ abet:start:autoload)
::
++  autoload
  |%
  ++  emit  |=(a/card +>(..autoload (^emit a)))
  ++  tracked-vanes
    ^-  (list @tas)
    ~[%ames %behn %clay %dill %eyre %ford %gall %jael %lient %rver]
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
    ^-  card
    [%warp /kiln/autoload our %home `[%next %z da+now /sys]]
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
      (emit %poke /kiln/reload/hoon [our %hood] %helm-reset ~)
      ::  XX  updates cur-vanes?
    =/  new-zuse  (sys-hash /zuse/hoon)
    ?:  !=(new-zuse cur-zuse)
      =.  cur-zuse  new-zuse
      =.  cur-vanes  rehash-vanes
      (emit %poke /kiln/reload/zuse [our %hood] %helm-reload [%zuse tracked-vanes])
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
    (emit [%poke /kiln/reload/[syd] [our %hood] %helm-reload ~[syd]])
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
  abet:(emit %wait /kiln/overload/(scot %dr recur) start)
::
++  poke-wipe-ford
  |=(percent=@ud abet:(emit %wipe /kiln percent))
::
++  poke-keep-ford
  |=  [compiler-cache-size=@ud build-cache-size=@ud]
  abet:(emit %keep /kiln compiler-cache-size build-cache-size)
::
++  mack
  |=  {way/wire saw/(unit tang)}
  ~?  ?=(^ saw)  [%kiln-nack u.saw]
  abet
::
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
  =+  ^-  hos/kiln-sync
      :*  syd=(slav %tas i.way)
          her=(slav %p i.t.way)
          sud=(slav %tas i.t.t.way)
      ==
  abet:abet:(mere:(auto hos) mes)
::
++  take-writ-sync                                    ::
  |=  {way/wire rot/riot}
  ?>  ?=({@ @ @ *} way)
  =+  ^-  hos/kiln-sync
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
  |=  {way/wire ~}
  ?>  ?=({@ ~} way)
  =+  tym=(slav %dr i.way)
  ~&  %wake-overload-deprecated
  abet
::
++  spam
  |=  mes/(list tank)
  ((slog mes) ..spam)
::     %-  emit :: XX not displayed/immediately
::     [%poke /kiln/spam [our %hall] (said our %kiln now eny mes)]
::
++  auto
  |=  kiln-sync
  =+  (fall (~(get by syn) syd her sud) [let=*@ud ust=ost])
  |%
  ++  abet
    ..auto(syn (~(put by syn) [syd her sud] let ust))
  ::
  ++  blab
    |=  new/(list move)
    ^+  +>
    +>.$(moz (welp new moz))
  ::
  ++  spam  |*(* %_(+> ..auto (^spam +<)))
  ++  stop
    =>  (spam (render "ended autosync" sud her syd) ~)
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (blab [ust %warp wire her sud ~] ~)
  ::  XX duplicate of start-sync? see |track
  ::
  ++  start-track
    =>  (spam (render "activated track" sud her syd) ~)
    =.  let  1
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (blab [ost %warp wire her sud `[%sing %y ud+let /]] ~)
  ::
  ++  start-sync
    =<  (spam (render "activated sync" sud her syd) ~)
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (blab [ost %warp wire her sud `[%sing %w [%da now] /]] ~)
  ::
  ++  writ
    |=  rot=riot
    ?~  rot
      %^    spam
          leaf+"bad %writ response"
        (render "on sync" sud her syd)
      ~
    =.  let  ?.  ?=($w p.p.u.rot)  let  ud:((hard cass:clay) q.q.r.u.rot)
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
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
    (blab [ost %merg wire syd her sud ud+let germ] ~)
  ::
  ++  mere
    |=  mes=(each (set path) (pair term tang))
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
          $no-ali-desk
        :~  (render "sync activated" sud her syd)
            leaf+"note: blank desk {<sud>} on {<her>}"
        ==
      ==
    =/  =wire  /kiln/sync/[syd]/(scot %p her)/[sud]
    (blab [ost %warp wire her sud `[%sing %y ud+let /]] ~)
  --
::
++  work                                              ::  state machine
  |=  syd/desk
  =+  ^-  per-desk
      %+  fall  (~(get by rem) syd)
      =+  *per-desk
      %_(- cas [%da now])
  |%
  ++  abet                                            ::  resolve
    ..work(rem (~(put by rem) syd auto gem her sud cas))
  ::
  ++  blab
    |=  new/(list move)
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
    (blab [ost %merg /kiln/[syd] syd her sud cas gem] ~)
  ::
  ++  fancy-merge                                     ::  send to self
    |=  {syd/desk her/@p sud/desk gem/?($auto germ)}
    ^+  +>
    %-  blab  :_  ~
    [ost %poke /kiln/fancy/[^syd] [our %hood] %kiln-merge [syd her sud cas gem]]
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
      [ost %merg /kiln/[syd] (cat 3 syd '-scratch') her sud cas gem]
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
        :*  ost  %build  /kiln/[syd]  live=%.n
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
            =+  ^-  for/mark
                =+  (slag (dec (lent pax)) pax)
                ?~(- %$ i.-)
            ^-  schematic
            [%mash [our tic] for [[her sud] for dali] [[our syd] for dbob]]
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
    =+  ^-  can/(list (pair path (unit miso)))
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
        [((hard path) q.q.pax) ?:(?=($null p.dif) ~ `[%dif dif])]
    :: ~&  >  kiln-made+[(turn can head) syd=syd +<.abet]
    =+  notated=(skid can |=({path a/(unit miso)} ?=(^ a)))
    =+  annotated=(turn `(list (pair path *))`-.notated head)
    =+  unnotated=(turn `(list (pair path *))`+.notated head)
    =+  `desk`(cat 3 syd '-scratch')
    =+  ^-  tan/(list tank)
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
    :*  ost  %info  /kiln/[syd]
        (cat 3 syd '-scratch')  %&
        %+  murn  can
        |=  {p/path q/(unit miso)}
        `(unit (pair path miso))`?~(q ~ `[p u.q])
    ==
  --
--
