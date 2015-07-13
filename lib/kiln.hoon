::                                                      ::  ::
::::  /hoon/kiln/lib                               ::  ::
  ::                                                    ::  ::
/?  310                                                 ::  version
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  kiln-part  ,[%kiln %0 kiln-pith]                    ::  kiln state
++  kiln-pith                                           ::
    $:  rem=(map desk kiln-desk)                        ::
        syn=(map kiln-sync ,[let=@ud ust=bone])         ::
    ==                                                  ::
++  kiln-desk                                           ::  per-desk state
    $:  auto=?                                          ::  escalate on failure
        gem=germ                                        ::  strategy
        her=@p                                          ::  from ship
        sud=@tas                                        ::  from desk
        cas=case                                        ::  at case
    ==                                                  ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  kiln-mount                                          ::
    $:  pax=path                                        ::
        pot=term                                        ::
    ==                                                  ::
++  kiln-unmount                                        ::
    $|(term [span path])                                ::
++  kiln-sync                                           ::
    $:  syd=desk                                        ::
        her=ship                                        ::
        sud=desk                                        ::
    ==                                                  ::
++  kiln-unsync                                         ::
    $:  syd=desk                                        ::
        her=ship                                        ::
        sud=desk                                        ::
    ==                                                  ::
++  kiln-merge                                          ::
    $:  syd=desk                                        ::
        ali=ship                                        ::
        sud=desk                                        ::
        gim=?(%auto germ)                               ::
    ==                                                  ::
++  kiln-cp  [input=path output=path]                   ::
++  kiln-mv  [input=path output=path]                   ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  !:                                                    ::  ::
|%                                                      ::  kiln library
++  kiln-work                                           ::  work in kiln
  |=  [bowl kiln-part]
  ?>  =(src our)
  =>  |%                                                ::  arvo structures
      ++  card                                          ::
        $%  [%exec wire @p ~ [beak silk]]               ::
            [%info wire @p @tas nori]                   ::
            [%mont wire @tas @p @tas path]              ::
            [%ogre wire $|(@tas beam)]                  ::
            [%merg wire @p @tas @p @tas germ]           ::
            [%poke wire dock pear]                      ::
            [%warp wire sock riff]                      ::
        ==                                              ::
      ++  pear                                          ::  poke fruit
        $%  [%talk-command command:talk]                ::
            [%kiln-merge kiln-merge]                    ::
        ==                                              ::
      ++  move  (pair bone card)                        ::  user-level move
      --
  |_  moz=(list move)
  ++  abet                                              ::  resolve
    [(flop moz) `kiln-part`+>+>->]
  ::
  ++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
  ++  emil                                              ::  return cards
    |=  (list card) 
    ^+  +>
    ?~(+< +> $(+< t.+<, +> (emit i.+<)))
  ::
  ++  poke-mount
    |=  kiln-mount
    =+  bem=(tome pax)
    ?~  bem
      =+  "can't mount bad path: {<pax>}"
      abet:(spam leaf/- ~)
    abet:(emit %mont /mount pot p.u.bem q.u.bem (flop s.u.bem))
  ::
  ++  poke-unmount
    |=  mon=kiln-unmount
    ?^  mon
      =+  bem=(tome mon)
      ?~  bem
        =+  "can't unmount bad path: {<mon>}"
        abet:(spam leaf/- ~)
      abet:(emit %ogre /unmount-beam [[p q %ud 0] s]:u.bem)
    abet:(emit %ogre /unmount-point mon)
  ::
  ++  poke-sync                                         ::
    |=  hos=kiln-sync
    ?:  (~(has by syn) hos)
      =+  "already syncing from {<sud.hos>} on {<her.hos>} to {<syd.hos>}"
      abet:(spam leaf/- ~)
    abet:abet:start:(auto hos)
  ::
  ++  poke-unsync                                         ::
    |=  hus=kiln-unsync
    ?.  (~(has by syn) hus)
      =+  "not syncing from {<sud.hus>} on {<her.hus>} to {<syd.hus>}"
      abet:(spam leaf/- ~)
    %*  .  abet:abet:stop:(auto hus)
      syn  (~(del by syn) hus)
    ==
  ::
  ++  poke-merge                                        ::
    |=  kiln-merge
    abet:abet:(merge:(work syd) ali sud gim)
  ::
  ++  do-info
    |=  [mez=tape tor=toro]
    abet:(emit:(spam leaf/mez ~) %info /kiln our tor)
  ::
  ++  poke-rm  |=(a=path (do-info "removed" (fray a)))
  ++  poke-cp
    |=  [input=path output=path]
    %+  do-info  "copied"
    ?>  =(-:(flop input) -:(flop output))
    (foal output -:(flop input) %noun .^(%cx input))    ::  XX type
  ::
  ++  poke-mv
    |=  [input=path output=path] 
    %+  do-info  "moved"
    ?>  =(-:(flop input) -:(flop output))
    %+  furl  (fray output)
    (foal output -:(flop input) %noun .^(%cx input))
  ::
  ++  poke-label
    |=  [syd=desk lab=@tas]
    =+  pax=/(scot %p our)/[syd]/[lab]
    (do-info "labeled {(spud pax)}" [syd %| lab])
  ::
  ++  poke-schedule
    |=  [where=path tym=@da eve=@t]
    =.  where  (welp where /sched)
    %+  do-info  "scheduled"
    =+  old=;;((map ,@da cord) (fall (file where) ~))
    (foal where %sched !>((~(put by old) tym eve)))
  ::
  ++  take  |=(way=wire ?>(?=([@ ~] way) (work i.way))) ::  general handler
  ++  take-mere                                         ::
    |=  [way=wire are=(each (set path) (pair term tang))]
    abet:abet:(mere:(take way) are)
  ::
  ++  take-made                                         ::
    |=  [way=wire dep=@uvH reg=gage]
    abet:abet:(made:(take way) dep reg) 
  ::
  ++  take-coup-fancy                                   ::
    |=  [way=wire saw=(unit tang)]
    abet:abet:(coup-fancy:(take way) saw)
  ::
  ++  take-mere-sync                                    ::
    |=  [way=wire mes=(each (set path) (pair term tang))]
    ?>  ?=([@ @ @ ~] way)
    =+  ^-  hos=kiln-sync
        :*  syd=(slav %tas i.way)
            her=(slav %p i.t.way)
            sud=(slav %tas i.t.t.way)
        ==
    abet:abet:(mere:(auto hos) mes)
  ::
  ++  take-writ                                         ::
    |=  [way=wire rot=riot]
    ?>  ?=([@ @ @ ~] way)
    =+  ^-  hos=kiln-sync
        :*  syd=(slav %tas i.way)
            her=(slav %p i.t.way)
            sud=(slav %tas i.t.t.way)
        ==
    abet:abet:(writ:(auto hos) rot)
  ::
  ++  spam
    |=  mes=(list tank)
    %-  emit
    [%poke /kiln/spam [our %talk] (said our %kiln now eny mes)]
  ::
  ++  auto
    |=  kiln-sync
    =+  (fall (~(get by syn) syd her sud) [let=*@ud ust=ost])
    |%
    ++  abet
      ..auto(syn (~(put by syn) [syd her sud] let ust))
    ::
    ++  blab
      |=  new=(list move)
      ^+  +>
      +>.$(moz (welp new moz))
    ::
    ++  spam  |*(* %_(+> ..auto (^spam +<)))
    ++  stop
      =>  (spam leaf/"ended autosync from {<sud>} on {<her>} to {<syd>}" ~)
      %-  blab  :_  ~
      :*  ust  %warp
          /kiln/sync/[syd]/(scot %p her)/[sud]
          [our her]  sud  ~
      ==
    ::
    ++  start
      ::  XX remove feedback for demo
      ::  =.  .  %-  spam
      ::         [leaf/"activated sync from {<sud>} on {<her>} to {<syd>}" ~]
      %-  blab
      :~  [ost %mont /mount syd our syd /]
          :*  ost  %warp
              /kiln/sync/[syd]/(scot %p her)/[sud]
              [our her]  sud  ~  %sing  %w  [%da now]  /
      ==  ==
    ::
    ++  writ
      |=  rot=riot
      ?~  rot
        %^    spam
            leaf/"bad %writ response on sync"
          leaf/"from {<sud>} on {<her>} to {<syd>}"
        ~
      =.  let  ?.  ?=(%w p.p.u.rot)  let  ((hard ,@ud) q.q.r.u.rot)
      %-  blab  :_  ~
      :*  ost  %merg
          /kiln/sync/[syd]/(scot %p her)/[sud]
          our  syd  her  sud
          ?:  =(0 .^(%cw /(scot %p our)/[syd]/(scot %da now)))
            %init
          %mate
      ==
    ::
    ++  mere
      |=  mes=(each (set path) (pair term tang))
      =.  let  +(let)
      =.  +>.$
        %-  spam
        ?:  ?=(%& -.mes)
          [leaf/"sync succeeded from {<sud>} on {<her>} to {<syd>}" ~]
        ?+  p.p.mes
          :*  leaf/"sync failed from {<sud>} on {<her>} to {<syd>}"
              leaf/"please manually merge the desks with"
              leaf/":+merge %{(trip syd)} {(scow %p her)} %{(trip sud)}"
              leaf/""
              leaf/"error code: {<p.p.mes>}"
              q.p.mes
          ==
        ::
            %no-ali-desk
          :~  leaf/"sync activated from {<sud>} on {<her>} to {<syd>}"  
              leaf/"note: {<sud>} on {<her>} is a blank desk"
          ==
        ==
      %-  blab  :_  ~
      :*  ost  %warp
          /kiln/sync/[syd]/(scot %p her)/[sud]
          [our her]  sud  ~  %sing  %y  [%ud let]  /
      ==
    --
  ::
  ++  work                                              ::  state machine
    |=  syd=desk
    =+  ^-  kiln-desk
        %+  fall  (~(get by rem) syd)
        =+  *kiln-desk
        %_(- cas [%da now])
    |%
    ++  abet                                            ::  resolve
      ..work(rem (~(put by rem) syd auto gem her sud cas))
    ::
    ++  blab
      |=  new=(list move)
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
      |=(tan=tang ~|(%ford-fail (mean tan)))
    ::
    ++  unwrap-tang
      |*  res=(each ,* tang)
      ?:  ?=(%& -.res)
        p.res
      (ford-fail p.res)
    ::
    ++  gage-to-cages
      |=  gag=gage  ^-  (list (pair cage cage))
      (unwrap-tang (gage-to-tage gag))
    ::
    ++  gage-to-tage
      |=  gag=gage
      ^-  (each (list (pair cage cage)) tang)
      ?.  ?=(%tabl -.gag)
        (mule |.(`~`(ford-fail >%strange-gage< ~)))
      =<  ?+(. [%& .] [@ *] .)
      |-  ^-  ?((list ,[cage cage]) (each ,~ tang))
      ?~  p.gag  ~
      =*  hed  i.p.gag
      ?-  -.p.hed
        %tabl  (mule |.(`~`(ford-fail >%strange-gage< ~)))
        %|     (mule |.(`~`(ford-fail p.p.hed)))
        %&     ?-  -.q.hed
          %tabl  (mule |.(`~`(ford-fail >%strange-gage< ~)))
          %|     (mule |.(`~`(ford-fail p.q.hed)))
          %&     =+  $(p.gag t.p.gag)
                 ?+(- [[p.p p.q]:hed -] [@ *] -)
      ==       ==
    ::
    ++  perform                                         ::  
      ^+  .
      (blab [ost %merg /kiln/[syd] our syd her sud gem] ~)
    ::
    ++  fancy-merge                                     ::  send to self
      |=  [syd=desk her=@p sud=desk gem=?(%auto germ)]
      ^+  +>
      %-  blab  :_  ~
      [ost %poke /kiln/fancy/[^syd] [our %hood] %kiln-merge [syd her sud gem]]
    ::
    ++  spam  ::|=(tang ((slog 0 +<) ..spam))
              |*(* +>(..work (^spam +<)))
    ++  merge
      |=  [her=@p sud=@tas gim=?(%auto germ)]
      ^+  +>
      =.  cas  [%da now]
      ?.  ?=(%auto gim)
        perform(auto |, gem gim, her her, sud sud)
      ?:  =(0 .^(%cw /(scot %p our)/[syd]/(scot %da now)))
        =>  $(gim %init)
        .(auto &)
      =>  $(gim %fine)
      .(auto &)
    ::
    ++  coup-fancy
      |=  saw=(unit tang) 
      ?~  saw
        =>  (spam leaf/"%melding %{(trip sud)} into scratch space" ~)
        %-  blab  :_  ~
        [ost %merg /kiln/[syd] our (cat 3 syd '-scratch') her sud gem]
      =+  :-  "failed to set up conflict resolution scratch space"
          "I'm out of ideas"
      lose:(spam leaf/-< leaf/-> u.saw)
    ::
    ++  mere
      |=  are=(each (set path) (pair term tang)) 
      ^+  +>
      ?:  =(%meld gem)
        ?:  ?=(%& -.are)
          ?.  auto
            =+  "merged with strategy {<gem>}"
            win:(spam leaf/- ?~(p.are ~ [>`(set path)`p.are< ~]))
          :: ~?  >  =(~ p.are)  [%mere-no-conflict syd]
          =+  "mashing conflicts"
          =>  .(+>.$ (spam leaf/- ~))
          =+  tic=(cat 3 syd '-scratch')
          %-  blab  :_  ~
          :*  ost  %exec  /kiln/[syd]
              our  ~  [our tic %da now]  %tabl
              ^-  (list (pair silk silk))
              :: ~&  >  kiln-mashing/[p.are syd=syd +<.abet]
              %+  turn  (~(tap in p.are))
              |=  pax=path
              ^-  (pair silk silk)
              :-  [%$ %path -:!>(*path) pax]
              =+  base=[%file [our tic %da now] (flop pax)]
              =+  alis=[%file [her sud cas] (flop pax)]
              =+  bobs=[%file [our syd %da now] (flop pax)]
              =+  dali=[%diff base alis]
              =+  dbob=[%diff base bobs]
              =+  ^-  for=mark
                  =+  (slag (dec (lent pax)) pax)
                  ?~(- %$ i.-)
              [%mash for [her sud dali] [our syd dbob]]
          ==
        =+  "failed to merge with strategy meld"
        lose:(spam leaf/- >p.p.are< q.p.are)
      ?:  ?=(%& -.are)
        =+  "merged with strategy {<gem>}"
        win:(spam leaf/- ?~(p.are ~ [>`(set path)`p.are< ~]))
      ?.  auto
        =+  "failed to merge with strategy {<gem>}"
        lose:(spam leaf/- >p.p.are< q.p.are)
      ?+    gem
        (spam leaf/"strange auto" >gem< ~)
      ::
          %init
        =+  :-  "auto merge failed on strategy %init"
            "I'm out of ideas"
        lose:(spam leaf/-< leaf/-> [>p.p.are< q.p.are])
      ::
          %fine
        ?.  ?=(%bad-fine-merge p.p.are)
          =+  "auto merge failed on strategy %fine"
          lose:(spam leaf/- >p.p.are< q.p.are)
        =>  (spam leaf/"%fine merge failed, trying %meet" ~)
        perform(gem %meet)
      ::
          %meet
        ?.  ?=(%meet-conflict p.p.are)
          =+  "auto merge failed on strategy %meet"
          lose:(spam leaf/- >p.p.are< q.p.are)
        =>  (spam leaf/"%meet merge failed, trying %mate" ~)
        perform(gem %mate)
      ::
          %mate
        ?.  ?=(%mate-conflict p.p.are)
          =+  "auto merge failed on strategy %mate"
          lose:(spam leaf/- >p.p.are< q.p.are)
        =>  .(gem %meld)
        =+  tic=(cat 3 syd '-scratch')
        =>  =+  :-  "%mate merge failed with conflicts,"
                "setting up scratch space at %{(trip tic)}"
            [tic=tic (spam leaf/-< leaf/-> q.p.are)]
        (fancy-merge tic our syd %that)
      ==
    ::
    ++  made
      |=  [dep=@uvH reg=gage]
      ^+  +>
      ?:  ?=(%| -.reg)
        =+  "failed to mash"
        lose:(spam leaf/- p.reg)
      =+  ^-  can=(list (pair path (unit miso)))
          %+  turn  (gage-to-cages reg)
          |=  [pax=cage dif=cage]
          ^-  (pair path (unit miso))
          ?.  ?=(%path p.pax)
            ~|  "strange path mark: {<p.pax>}"
            !!
          [((hard path) q.q.pax) ?:(?=(%null p.dif) ~ `[%dif dif])]
      :: ~&  >  kiln-made/[(turn can head) syd=syd +<.abet]
      =+  notated=(skid can |=([path a=(unit miso)] ?=(^ a)))
      =+  annotated=(turn `(list (pair path ,*))`-.notated head)
      =+  unnotated=(turn `(list (pair path ,*))`+.notated head)
      =+  (trip (cat 3 syd '-scratch'))
      =+  ^-  tan=(list tank)
          %-  zing
          ^-  (list (list tank))
          :~  :~  leaf/""
                  leaf/"done setting up scratch space in %{-}"
                  leaf/"please resolve the following conflicts and run"
                  leaf/":+merge %{(trip syd)} {<our>} %{-}"
              ==
              ?~  annotated
                ~
              :~  leaf/""
                  leaf/"annotated conflicts in:"
                  >`(list path)`annotated<
              ==
              ?~  unnotated
                ~
              :~  leaf/""
                  leaf/"some conflicts could not be annotated."
                  leaf/"for these, the scratch space contains"
                  leaf/"the most recent common ancestor of the"
                  leaf/"conflicting content."
                  leaf/""
                  leaf/"unannotated conflicts in:"
                  >`(list path)`unnotated<
              ==
          ==
      =<  win
      %-  blab:(spam tan)
      :_  ~
      :*  ost  %info  /kiln/[syd]
          our  (cat 3 syd '-scratch')
          %&  *cart
          %+  murn  can
          |=  [p=path q=(unit miso)]
          `(unit (pair path miso))`?~(q ~ `[p u.q])
      ==
    --
  --
--
