::                                                      ::  ::
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::
::
::TODO  master changes
::TODO  =/ instead of =+ ^= where possible
::TODO  avoid lark where possible
::TODO  remove old/unused code
::TODO  improve naming. way->wir, rad->rep, etc.
::TODO  tidiness, remove unnecessary ~&, etc.
::TODO  maybe ensure every arm has a mini-description at :57 too?
::TODO  maybe prefix all printfs and other errors with %talk?
::
::TODO  sending remotes and mirrors alongside locals and shape no longer makes
::      any sense, does it?
::TODO  crash on pokes/peers we do not expect
::TODO  rename cores. ra->ta (transaction), pa->to (story).
::
/?    310                                               ::  hoon version
/-    talk, sole                                        ::  structures
/+    talk, sole, time-to-id, twitter                   ::  libraries
/=    seed  /~  !>(.)
!:
::
::::
  ::
::x  include talk and sole cores from the /+ include into our subject,
::x  so we can do some-arm instead of some-arm:talk.
[. talk sole]
=>  |%                                                  ::  data structures
    ++  house                                           ::
      $:  stories/(map knot story)                      ::  conversations
          ::TODO  maybe also store locals/shapes for easier syncing with readers
          remotes/(map partner atlas)                   ::  remote presence
          mirrors/(map station config)                  ::  remote config
          ::TODO  rename to readers?
          general/(set bone)                            ::  our message readers
          outbox/(pair @ud (map @ud thought))           ::  urbit outbox
          log/(map knot @ud)                            ::  logged to clay
          folks/(map ship human)                        ::  human identities
          nik/(map (set partner) char)                  ::  bound station glyphs
          nak/(jug char (set partner))                  ::  station glyph lookup
      ==                                                ::
    ++  story                                           ::  wire content
      $:  count/@ud                                     ::  (lent grams)
          grams/(list telegram)                         ::  all history
          locals/atlas                                  ::  local presence
          sequence/(map partner @ud)                    ::  partners heard
          shape/config                                  ::  configuration
          known/(map serial @ud)                        ::  messages heard
          followers/(map bone river)                    ::  subscribers
      ==                                                ::
    ++  river  (pair point point)                       ::  stream definition
    ++  point                                           ::  stream endpoint
      $%  {$ud p/@ud}                                   ::  by number
          {$da p/@da}                                   ::  by date
      ==                                                ::
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  {$talk-report report}                         ::
          {$talk-lowdown lowdown}                       ::
          {$talk-reaction reaction}                     ::
          {$sole-effect sole-effect}                    ::
      ==                                                ::
    ++  pear                                            ::  poke fruit
      $%  {$talk-command command}                       ::
          {$write-comment spur ship cord}               ::
          {$write-fora-post spur ship cord cord}        ::
      ==                                                ::
    ++  card                                            ::  general card
      $%  {$diff lime}                                  ::
          {$info wire @p @tas nori}                     ::
          {$peer wire dock path}                        ::
          {$poke wire dock pear}                        ::
          {$pull wire dock $~}                          ::
          {$quit $~}                                    ::
      ==                                                ::
    ++  weir                                            ::  parsed wire
      $%  {$repeat p/@ud q/@p r/knot}                   ::
          {$friend p/knot q/station}                    ::
      ==                                                ::
    ++  strap  |*({k/* v/*} (~(put by *(map _k _v)) k v))
    --
::
|_  {hid/bowl house}
::
++  ra                                                  ::  per transaction
  ::x  gets called when talk gets poked or otherwise prompted/needs to perform
  ::x  an action.
  ::x  arms generally modify state, and store moves in ++ra's moves. these get
  ::x  produced when calling ++ra-abet.
  ::x  in applying commands and making reports, it uses ++pa for story work.
  ::
  ::x  moves: moves storage, added to by ++ra-emit and -emil, produced by -abed.
  |_  moves/(list move)
  ::
  ++  ra-abet                                           ::  resolve core
    ::x  produces the moves stored in ++ra's moves.
    ::x  sole-effects get special treatment to become a single move.
    ::TODO  this shouldn't ever make sole-effects anymore, so remove that logic.
    ::TODO  instead, we want to squash lowdown %names and %glyphs,
    ::      but figure out if that'll ever be needed first.
    ::
    ^+  [*(list move) +>]
    :_  +>
    ::x  seperate our sole-effects from other moves.
    =+  ^=  yop
        |-  ^-  (pair (list move) (list sole-effect))
        ?~  moves  [~ ~]
        =+  mor=$(moves t.moves)
        ?:  ?&  =(ost.hid p.i.moves)
                ?=({$diff $sole-effect *} q.i.moves)
            ==
          [p.mor [+>.q.i.moves q.mor]]
        [[i.moves p.mor] q.mor]
    ::x  flop moves, flop and squash sole-effects into a %mor.
    =+  :*  moz=(flop p.yop)
            ^=  foc  ^-  (unit sole-effect)
            ?~  q.yop  ~
            ?~(t.q.yop `i.q.yop `[%mor (flop `(list sole-effect)`q.yop)])
        ==
    ::x  produce moves or sole-effects and moves.
    ?~(foc moz [[ost.hid %diff %sole-effect u.foc] moz])
  ::
  ++  ra-emil                                           ::  ra-emit move list
    ::x  adds multiple moves to the core's list. flops to emulate ++ra-emit.
    ::
    |=  mol/(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ra-emit                                           ::  emit a move
    ::x  adds a move to the core's list.
    ::
    |=  mov/move
    %_(+> moves [mov moves])
  ::
  ++  ra-evil                                           ::  emit error
    ::x  stack trace and crash.
    ::
    |=  msg/cord
    ~|  [%ra-evil msg]
    !!
  ::
  ++  ra-init                                           ::  initialize talk
    ::x  populate state on first boot. creates our main and public stories.
    ::
    ~&  %ra-init
    %+  roll
      ^-  (list {posture knot cord})
      :~  [%brown (main our.hid) 'default home']
          [%green ~.public 'visible activity']
      ==
    |:  [[typ=*posture man=*knot des=*cord] ..ra-init]  ^+  ..ra-init
    %+  ra-apply  our.hid
    :+  %design  man
    :-  ~  :-  ~
    [des [typ ~]]
  ::
  ++  ra-apply                                          ::  apply command
    ::x  applies the command sent by her.
    ::
    |=  {her/ship cod/command}
    ^+  +>
    ?-    -.cod
      ::x  the $design command is used for modifying channel configs,
      ::x  which is done when joining, leaving or creating channels.
      ::x  this may only be done by ourselves.
        $design
      ?.  (team our.hid her)
        (ra-evil %talk-no-owner)
      ?~  q.cod
        ?.  (~(has by stories) p.cod)
          (ra-evil %talk-no-story)
        ::x  $design with ~ for config signals delete
        ::TODO  be sure to send a deletion %tales lowdown!
        (ra-unconfig p.cod)
      (ra-config p.cod u.q.cod)
    ::
      ::x  used for relaying messages (as a station host).
        $review   (ra-think | her +.cod)
    ::
      ::x  used for sending messages (as their author).
        $publish
      ?.  (team our.hid her)  +>.$
      (ra-think & her +.cod)
    ==
  ::
  ++  ra-inform                                         ::x  new lowdown
    ::x  send talk-lowdown to all readers.
    ::
    |=  low/lowdown
    %-  ra-emil
    %-  ~(rep in general)
    |=  {b/bone l/(list move)}
    [[b %diff %talk-lowdown low] l]
  ::
  ++  ra-react
    ::x  send a talk-reaction to a reader.
    ::
    |=  {red/bone rac/reaction}
    %-  ra-emit
    [red %diff %talk-reaction rac]
  ::
  ++  ra-action
    ::x  peforms action sent by a reader.
    ::
    |=  {red/bone act/action}
    ^+  +>
    ?-  -.act
      ::
      ::  station configuration
      ::
        $create                                         ::  create station
      ?.  (~(has in stories) p.act)
        (ra-config p.act [[%& our.hid p.act] ~ ~] q.act [r.act ~])
      (ra-react red %fail (crip "{(trip p.act)}: already exists") `act)
      ::
        $permit                                         ::  invite/banish
      %-  (ra-affect p.act red `act)  |=  par/_pa  =<  pa-abet
      (pa-permit:par q.act r.act)
      ::
      ::  personal metadata
      ::
        $status                                         ::  our status update
      ::x  for every knot (story) in the set, update our status.
      |-  ^+  +>.^$
      ?~  p.act  +>.^$
      =.  +>.^$  $(p.act l.p.act)
      =.  +>.^$  $(p.act r.p.act)
      %-  (ra-affect n.p.act red `act)  |=  par/_pa  =<  pa-abet
      (pa-notify:par our.hid q.act)
      ::
      ::  changing shared ui
      ::
        $human                                          ::  new identity
      ?.  =((~(get by folks) p.act) `q.act)  +>         ::  no change
      =.  folks
        ?~  hand.q.act  (~(del by folks) p.act)
        (~(put by folks) p.act q.act)
      %+  ra-inform  %names
      ::TODO  think long and hard, do we need unit for delition or is a human
      ::      with [~ ~] good enough? if the latter, agent's $names will change.
      (strap p.act ?~(hand.q.act ~ `q.act))
      ::
        $glyph                                          ::  bind a glyph
      =.  nik  (~(put by nik) q.act p.act)
      =.  nak  (~(put ju nak) p.act q.act)
      (ra-inform %glyph nak)
    ==
  ::
  ++  ra-config                                         ::  configure story
    ::x  (re)configures story man. if it's a new story, emit our stories.
    ::
    |=  {man/knot con/config}
    ^+  +>
    =+  :-  neu=!(~(has by stories) man)
        pur=(fall (~(get by stories) man) *story)
    ::x  if we just created the story, and it's invite only, make sure we're in.
    =.  con  ::TODO  =?
      ?.  &(neu ?=(?($white $green) p.cordon.con))  con
      con(q.cordon [our.hid ~ ~])
    pa-abet:(~(pa-reform pa man ~ pur) con)
  ::
  ++  ra-unconfig
    |=  man/knot
    ^+  +>
    =+  soy=(~(get by stories) man)
    ?~  soy  +>.$
    =.  stories  (~(del by stories) man)
    pa-abet:~(pa-reform-gone pa man ~ u.soy)
  ::
  ++  ra-base-hart
    ::x  produces our ship's host desk's web address as a hart.
    ::
    .^(hart %e /(scot %p our.hid)/host/(scot %da now.hid))
  ::
  ++  ra-fora-post
    ::x  sends a fora post. if we don't have a channel for posts yet, create one
    ::
    |=  {pax/path sup/spur hed/@t txt/@t}
    ::x  tell %hood to submit a fora post.
    =.  ..ra-emit
      %+  ra-emit  ost.hid
      :*  %poke
          /fora-post
          [our.hid %hood]
          [%write-fora-post sup src.hid hed txt]
      ==
    =+  man=%posts
    ::x  if we have a %posts story, go ahead and consume.
    ?:  (~(has by stories) man)
      (ra-consume-fora-post man pax hed txt)
    ::x  if we have no %posts story, first create it, then consume.
    =;  new  (ra-consume-fora-post:new man pax hed txt)
    =.  ..ra-apply
      %+  ra-apply  our.hid
      :+  %design  man
      :-  ~  :-  ~               ::x  sources
      :-  'towards a community'  ::x  caption
      [%brown ~]                 ::x  cordon
    ::x  send informative message to our mailbox.
    %^  ra-consume  &  our.hid
    :^    (shaf %init eny.hid)  ::x  serial
        (my [[%& our.hid (main our.hid)] *envelope %pending] ~)  ::x  audience
    ::x  statement
      now.hid
    [~ %app %tree 'receiving forum posts, ;join %posts for details']
  ::
  ++  ra-consume-fora-post
    ::x  add a message for a fora post to the man story.
    ::
    |=  {man/knot pax/path hed/@t txt/@t}  ^+  +>
    =.  pax  (welp pax /posts/(crip "{<now.hid>}~"))
    %^  ra-consume  |
      src.hid
    :*  (shaf %comt eny.hid)
        (my [[%& our.hid man] *envelope %pending] ~)
        now.hid
        (sy /fora-post eyre+pax ~)
      :-  %mor  :~
        [%fat text+(lore txt) [%url [ra-base-hart `pax ~] ~]]
        [%app %tree (crip "forum post: '{(trip hed)}'")]
      ==
    ==
  ::
  ++  ra-comment
    ::x  sends a comment. if we don't have a channel for them yet, creates one.
    ::
    |=  {pax/path sup/spur txt/@t}
    =.  ..ra-emit
      %+  ra-emit  ost.hid
      :*  %poke
          /comment
          [our.hid %hood]
          [%write-comment sup src.hid txt]
      ==
    =+  man=%comments
    ?:  (~(has by stories) man)
      (ra-consume-comment man pax sup txt)
    =;  new  (ra-consume-comment:new man pax sup txt)
    =.  ..ra-apply
      %+  ra-apply  our.hid
      :+  %design  man
      :-  ~  :-  ~
      :-  'letters to the editor'
      [%brown ~]
    %^  ra-consume  &  our.hid
    :^    (shaf %init eny.hid)
        (my [[%& our.hid (main our.hid)] *envelope %pending] ~)
      now.hid
    [~ %app %tree 'receiving comments, ;join %comments for details']
  ::
  ++  ra-consume-comment
    ::x  adds a message for a comment to the man story.
    ::
    |=  {man/knot pax/path sup/spur txt/@t}  ^+  +>
    =+  nam=?~(sup "" (trip i.sup))                     :: file name
    =+  fra=(crip (time-to-id now.hid))                 :: url fragment
    %^  ra-consume  |
      src.hid
    :*  (shaf %comt eny.hid)
        (my [[%& our.hid man] *envelope %pending] ~)
        now.hid
        (sy /comment eyre+pax ~)
      :-  %mor  :~
        [%fat text+(lore txt) [%url [ra-base-hart `pax ~] `fra]]
        [%app %tree (crip "comment on /{nam}")]
      ==
    ==
  ::
  ++  ra-know                                           ::  story monad
    ::x  produces a wet core that takes a gate that takes a story core and
    ::x  produces updated state.
    ::
    |=  man/knot
    |*  fun/$-(_pa _+>)
    ^+  +>+>
    =+  pur=(~(get by stories) man)
    ?~  pur
      ~&  [%ra-know-not man]                            ::  XX should crash
      +>+>.$
    ::x  call the sample gate with a ++pa core.
    (fun ~(. pa man ~ u.pur))
  ::
  ++  ra-affect
    ::x  attempt to retrieve a story and call a sample gate with it.
    ::x  if no such story, react.
    ::
    |=  {man/knot red/bone act/(unit action)}
    |*  fun/$-(_pa _+>)
    ^+  +>+>
    =+  pur=(~(get by stories) man)
    ?~  pur
      %+  ra-react  red
      [%fail (crip "no story {(trip man)}") act]
    (fun ~(. pa man ~ u.pur))
  ::
  ++  ra-diff-talk-report                               ::  subscription update
    ::x  process a talk report from cuz into story man.
    ::
    |=  {man/knot cuz/station rad/report}
    %-  (ra-know man)  |=  par/_pa  =<  pa-abet
    (pa-diff-talk-report:par cuz rad)
  ::
  ++  ra-quit                                           ::  subscription quit
    ::x  removes cuz from the subscribers of story man.
    ::
    |=  {man/knot cuz/station}
    %-  (ra-know man)  |=  par/_pa  =<  pa-abet
    (pa-quit:par %& cuz)
  ::
  ++  ra-retry                                          ::  subscription resend
    ::x  produce a %peer/subscribe move for cuz to story man.
    ::
    |=  {man/knot cuz/station}
    %-  (ra-know man)  |=  par/_pa  =<  pa-abet
    (pa-acquire:par [%& cuz]~)
  ::
  ++  ra-coup-repeat                                    ::
    ::x  assemble partner and call ++ra-repeat.
    ::
    |=  {{num/@ud her/@p man/knot} saw/(unit tang)}
    (ra-repeat num [%& her man] saw)
  ::
  ++  ra-repeat                                         ::  remove from outbox
    ::x  take message out of outbox, mark it as received or rejected.
    ::x  crashes if pan is not in message's audience.
    ::
    |=  {num/@ud pan/partner saw/(unit tang)}
    =+  oot=(~(get by q.outbox) num)
    ?~  oot  ~|([%ra-repeat-none num] !!)
    =.  q.outbox  (~(del by q.outbox) num)
    =.  q.u.oot
      =+  olg=(~(got by q.u.oot) pan)
      %+  ~(put by q.u.oot)  pan
      :-  -.olg
      ?~  saw  %received
      ~>  %slog.[0 u.saw]
      %rejected
    (ra-think | our.hid u.oot ~)
  ::
  ++  ra-cancel                                         ::  drop a bone
    ::x  removes a bone from the story in pax.
    ::
    |=  {src/ship pax/path}
    ^+  +>
    ?.  ?=({@ @ *} pax)
      ::x  if story is not in path, just delete the bone from general.
      +>(general (~(del in general) ost.hid))
    %-  (ra-know i.pax)  |=  par/_pa  =<  pa-abet
    ::x  delete bone from all follower groups and set src's status to %gone.
    (pa-notify:pa-cancel:par src %gone *human)
  ::
  ++  ra-human                                          ::  look up person
    ::x  get her identity. if she has none, make her one.
    ::
    |=  her/ship
    ^-  {human _+>}
    =^  who  folks
        =+  who=(~(get by folks) her)
        ?^  who  [u.who folks]
        =+  who=`human`[~ `(scot %p her)]               ::  XX do right
        [who (~(put by folks) her who)]
    [who +>.$]
  ::
  ++  ra-subscribe                                      ::  listen to
    ::x  subscribe her at pax.
    ::
    |=  {her/ship pax/path}
    ^+  +>
    ::x  empty path, meta-subscribe and send report with all our stories.
    ?:  ?=($~ pax)
      ?.  (team our.hid her)
        ~&  [%foreign-reader her]
        +>
      (ra-welcome(general (~(put in general) ost.hid)) ost.hid)
    ::?.  ?=({@ *} pax)
    ::  (ra-evil %talk-bad-path)
    ::TODO  ^ mint-vains?
    =+  pur=(~(get by stories) i.pax)
    ?~  pur
      ~&  [%bad-subscribe-story-c i.pax]
      (ra-evil %talk-no-story)
    =+  soy=~(. pa i.pax `(list command)`~ u.pur)  ::TODO  nest-fail if no cast
    ::x  she needs read permissions to subscribe.
    ?.  (pa-visible:soy her)
      (ra-evil %talk-no-story)
      ::TODO?  or (pa-sauce ost.hid [%quit ~]~) ?
    =^  who  +>.$  (ra-human her)
    ::x  send current data to bring her up to date.
    =.  soy  (pa-report-cabal:soy ost.hid ~ ~)
    ::=.  soy  (pa-report-group:soy ost.hid ~ ~)  ::TODO  reenable if pa-not=dif
    =.  soy  (pa-first-grams:soy her t.pax)  ::x  also adds new sub to followers
    ::x  add her status to presence map.
    =.  soy  (pa-notify:soy her %hear who)
    ::x  apply changes to story.
    pa-abet:soy
  ::
  ++  ra-welcome
    ::x  brings reader new up to date.
    ::
    ::TODO  just like weld somehow
    |=  new/bone
    =.  +>  %-  ra-emil
      %-  zing
      %+  turn  (~(tap by stories))
      |=  {k/knot s/story}
      ^-  (list move)
      :~  ::x  story configurations
          [new %diff %talk-lowdown %tales k `shape.s]
          ::x  local presences
          [new %diff %talk-lowdown %precs k locals.s]
          ::x  telegrams
          [new %diff %talk-lowdown %grams k count.s grams.s]
      ==
    %-  ra-emil
    :~  ::x  remote configurations
        [new %diff %talk-lowdown %remco mirrors]
        ::x  remote presences
        [new %diff %talk-lowdown %rempe remotes]
        ::x  bound glyphs
        [new %diff %talk-lowdown %glyph nak]
        ::x  nicknames
        [new %diff %talk-lowdown %names (~(run by folks) some)]
    ==
  ::
  ++  ra-think                                          ::  publish+review
    ::x  consumes each thought.
    ::
    |=  {pub/? her/ship tiz/(list thought)}
    ^+  +>
    ?~  tiz  +>
    $(tiz t.tiz, +> (ra-consume pub her i.tiz))
  ::
  ++  ra-normal                                         ::  normalize
    ::x  sanitize %lin speech, enforce lowercase and no special characters.
    ::
    |=  tip/thought
    ^-  thought
    ?.  ?=({$lin *} r.r.tip)  tip
    %_    tip
        q.r.r
      %-  crip
      %+  scag  64
      %-  tufa
      %+  turn  (tuba (trip q.r.r.tip))
      |=  a/@c
      ?:  &((gte a 'A') (lte a 'Z'))
        (add a 32)
      ?:  |((lth a 32) (gth a 126))
        `@`'?'
      a
    ==
  ::
  ++  ra-consume                                        ::  consume thought
    ::x  if pub is true, sends the thought to each partner in the audience.
    ::x  if false, updates the thought in our store.
    ::
    |=  {pub/? her/ship tip/thought}
    =.  tip  (ra-normal tip)
    =+  aud=(~(tap by q.tip) ~)  ::x  why ~ ?
    |-  ^+  +>.^$
    ?~  aud  +>.^$
    $(aud t.aud, +>.^$ (ra-conduct pub her p.i.aud tip))
  ::
  ++  ra-conduct                                        ::  thought to partner
    ::x  record a message or sends it.
    ::
    |=  {pub/? her/ship tay/partner tip/thought}
    ^+  +>
    ?-  -.tay
      $&  ?:  pub
            =.  her  our.hid                            ::  XX security!
            ?:  =(her p.p.tay)
              (ra-record q.p.tay p.p.tay tip)
            (ra-transmit p.tay tip)
          ?.  =(our.hid p.p.tay)
            +>
          (ra-record q.p.tay her tip)
      $|  !!
    ==
  ::
  ++  ra-record                                         ::  add to story
    ::x  add or update a telegram in story man.
    ::
    |=  {man/knot gam/telegram}
    %-  (ra-know man)  |=  par/_pa  =<  pa-abet
    (pa-learn:par gam)
  ::
  ++  ra-transmit                                       ::  send to neighbor
    ::x  sends a thought to a station, adds it to the outbox.
    ::
    |=  {cuz/station tip/thought}
    ^+  +>
    =.  +>
        %+  ra-emit  ost.hid
        :*  %poke
            /repeat/(scot %ud p.outbox)/(scot %p p.cuz)/[q.cuz]
            [p.cuz %talk-guardian]
            [%talk-command `command`[%review tip ~]]
        ==
    +>(p.outbox +(p.outbox), q.outbox (~(put by q.outbox) p.outbox tip))
  ::
  ++  pa                                                ::  story core
    ::x  story core, used for doing work on a story.
    ::x  as always, an -abet arms is used for applying changes to the state.
    ::x  ++pa-watch- arms get called by ++ra-subscribe to add a subscriber.
    ::x  bones are used to identify subscribers (source event identifiers).
    ::
    |_  ::x  man: the knot identifying the story in stories.
        ::x  coz: talk commands issued due to changes.
        ::x  story doesn't get a face because ease of use.
        ::
        $:  man/knot
            coz/(list command)
            story
        ==
    ++  pa-abet
      ::x  apply/fold changes back into the stories map.
      ::
      ^+  +>
      =.  +>  +>(stories (~(put by stories) man `story`+<+>))
      =.  coz  (flop coz)
      |-  ^+  +>+
      ?~  coz  +>+
      =.  +>+  (ra-apply our.hid i.coz)
      $(coz t.coz)
    ::
    ++  pa-tell
      ::x  stores a talk command.
      ::
      |=  cod/command
      ^+  +>
      +>(coz [cod coz])
    ::
    ++  pa-followers
      ^-  (set bone)
      %-  ~(gas in *(set bone))
      %+  turn  (~(tap by followers))
      |=  {b/bone *}  b
    ::
    ++  pa-admire                                       ::  accept from
      ::x  should be checking her write permissions, but defaults to allowed.
      ::x  commented code seems to use an older control structure.
      ::x?  this seems like an easy fix, why was this ever disabled?
      ::
      |=  her/ship
      ^-  ?
      ?-  p.cordon.shape
        $black  !(~(has in q.cordon.shape) her)         ::x  channel, blacklist
        $green  (~(has in q.cordon.shape) her)          ::x  journal, whitelist
        $brown  !(~(has in q.cordon.shape) her)         ::x  mailbox, blacklist
        $white  (~(has in q.cordon.shape) her)          ::x  village, whitelist
      ==
    ::
    ++  pa-visible                                      ::  display to
      ::x  checks her read permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  p.cordon.shape
        $black  !(~(has in q.cordon.shape) her)         ::x  channel, blacklist
        $green  &                                       ::x  journal, all
        $brown  (team our.hid her)                      ::x  mailbox, our
        $white  (~(has in q.cordon.shape) her)          ::x  village, whitelist
      ==
    ::
    ++  pa-report                                       ::  update
      ::x  sends report to all bones.
      ::
      |=  {wac/(set bone) caw/report}
      ::  ~&  [%pa-report man -.caw]
      ^+  +>
      ?~  wac  +>
      =.  +>  $(wac l.wac)
      =.  +>  $(wac r.wac)
      ::  ~&  [%pa-report-cabal man shape]
      (pa-sauce n.wac [%diff %talk-report caw]~)
    ::
    ++  pa-inform
      ::x  sends lowdown to all readers.
      ::
      |=  low/lowdown
      =.  moves
        |-  ^-  (list move)
        ?~  general  moves
        =+  lef=$(general l.general)
        =+  rit=$(general r.general)
        :_  (welp lef rit)
        [n.general %diff %talk-lowdown low]
      +>.$
    ::
    ++  pa-remotes
      ::x  produces remotes, with all our local presences replaced by their
      ::x  versions from their stories.
      ::
      %-  ~(urn by remotes)           ::  XX performance
      |=  {pan/partner atl/atlas}  ^-  atlas
      ?.  &(?=($& -.pan) =(our.hid p.p.pan))  atl
      =+  soy=(~(get by stories) q.p.pan)
      ?~  soy  atl
      locals.u.soy
    ::
    ++  pa-report-group                                  ::  update presence
      ::x  build a group report, containing our different presence maps, and
      ::x  send it to all bones.
      ::x  we send remote presences to facilitate federation. aka "relay"
      ::
      |=  vew/(set bone)
      (pa-report vew %group locals pa-remotes)
    ::
    ++  pa-report-cabal                                 ::  update config
      ::x  a cabal report, containing our and remote configs, to all bones.
      ::
      |=  vew/(set bone)
      (pa-report vew %cabal shape mirrors)
    ::
    ++  pa-cabal
      ::x  add station's config to our remote config map.
      ::
      ::TODO  if web frontend doesn't use ham, remove it (also from sur/talk)
      |=  {cuz/station con/config ham/(map station config)}
      ^+  +>
      =+  old=mirrors
      =.  mirrors  (~(put by mirrors) cuz con)
      ?:  =(mirrors old)  +>.$
      =.  +>.$  (pa-inform %remco mirrors)
      (pa-report-cabal pa-followers)
    ::
    ++  pa-diff-talk-report                             ::  subscribed update
      ::x  process a talk report from cuz.
      ::
      |=  {cuz/station rad/report}
      ^+  +>
      ::x  verify we are supposed to receive reports from cuz.
      ?.  (~(has in sources.shape) [%& cuz])
        ~&  [%pa-diff-unexpected cuz -.rad]
        +>
      ?-  -.rad
        $cabal  (pa-cabal cuz +.rad)
        $group  (pa-remind [%& cuz] +.rad)
        $grams  (pa-lesson q.+.rad)
      ==
    ::
    ++  pa-quit                                         ::  stop subscription
      ::x  delete tay from our subscriptions, then send an updated capal report.
      ::
      |=  tay/partner
      =.  +>
        %.  pa-followers
        pa-report-cabal(sources.shape (~(del in sources.shape) tay))
      (pa-inform %tales man `shape)
    ::
    ++  pa-sauce                                        ::  send backward
      ::x  turns cards into moves, reverse order, prepend to existing moves.
      ::
      |=  {ost/bone cub/(list card)}
      %_    +>.$
          moves
        (welp (flop (turn cub |=(a/card [ost a]))) moves)
      ==
    ::
    ++  pa-abjure                                       ::  unsubscribe move
      ::x  for each partner, produce a %pull/unsubscribe move.
      ::
      |=  tal/(list partner)
      %+  pa-sauce  0  ::x  subscription is caused by this app
      %-  zing
      %+  turn  tal
      |=  tay/partner
      ^-  (list card)
      ?-  -.tay
        $|  ~&  tweet-abjure+p.p.tay
            !!
      ::
        $&  ~&  [%pa-abjure /[man]/(scot %p p.p.tay)/[q.p.tay]]
            :_  ~
            :*  %pull
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk-guardian]
                ~
            ==
      ==
    ::
    ++  pa-acquire                                      ::  subscribe to
      ::x  for each partner, produce a %peer/subscribe move.
      ::
      |=  tal/(list partner)
      %+  pa-sauce  0  ::x  subscription is caused by this app
      %-  zing
      %+  turn  tal
      |=  tay/partner
      ^-  (list card)
      =+  num=(~(get by sequence) tay)
      =+  old=(sub now.hid ~d1)                         :: XX full backlog
      ::x  subscribe starting at the last message we read,
      ::x  or if we haven't read any yet, messages from up to a day ago.
      =+  ini=?^(num (scot %ud u.num) (scot %da old))
      ?-  -.tay
        $|  !!
        $&  ::  ~&  [%pa-acquire [our.hid man] [p.p.tay q.p.tay]]
            :_  ~
            :*  %peer
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk-guardian]
                /[q.p.tay]/[ini]
            ==
      ==
    ::
    ++  pa-reform                                       ::  reconfigure, ugly
      ::x  change config of current story, subscribe/unsubscribe to/from the
      ::x  partners we gained/lost, and send out an updated cabal report.
      ::
      |=  cof/config
      =.  +>.$  (pa-inform %tales man `cof)
      =+  ^=  dif  ^-  (pair (list partner) (list partner))
          =+  old=`(list partner)`(~(tap in sources.shape) ~)
          =+  new=`(list partner)`(~(tap in sources.cof) ~)
          :-  (skip new |=(a/partner (~(has in sources.shape) a)))
          (skip old |=(a/partner (~(has in sources.cof) a)))
      =.  +>.$  (pa-acquire p.dif)
      =.  +>.$  (pa-abjure q.dif)
      =.  shape  cof
      (pa-report-cabal pa-followers)
    ::
    ++  pa-reform-gone
      =.  .  (pa-inform %tales man ~)
      =.  .  (pa-report-cabal pa-followers)
      (pa-abjure (~(tap in sources.shape)))
    ::
    ++  pa-cancel                                       ::  unsubscribe from
      ::x  deletes the current ost.hid from all follower groups.
      ::
      ::  ~&  [%pa-cancel ost.hid]
      .(followers (~(del by followers) ost.hid))
    ::
    ++  pa-notify                                       ::  local presence
      ::x  add her status to our presence map. if this changes it, send report.
      ::
      |=  {her/ship saz/status}
      ^+  +>
      =/  nol  (~(put by locals) her saz)
      ?:  =(nol locals)  +>.$
      =.  +>.$  (pa-inform %precs man nol)
      (pa-report-group(locals nol) pa-followers)
    ::
    ++  pa-remind                                       ::  remote presence
      ::x  adds tay's loc to our remote presence map, after merging with rem.
      ::x  if this changes anything, send update report.
      ::
      |=  {tay/partner loc/atlas rem/(map partner atlas)}
      ::x  remove this story from the presence map, since it's in local already.
      =.  rem  (~(del by rem) %& our.hid man)  :: superceded by local data
      =/  buk  (~(uni by remotes) rem)  ::TODO  drop?
      =.  buk  (~(put by buk) tay loc)
      ?:  =(buk remotes)  +>.$
      =.  +>.$  (pa-inform %rempe buk)
      (pa-report-group(remotes buk) pa-followers)
    ::
    ++  pa-start                                        ::  start stream
      ::x  grab all telegrams that fall within the river and send them in a
      ::x  grams report to ost.hid.
      ::
      |=  riv/river
      ^+  +>
      ::TODO  use =; lab/{dun/? end/@u zeg/(list telegram)}
      =-  ::  ~&  [%pa-start riv lab]
          =.  +>.$
            (pa-sauce ost.hid [[%diff %talk-report %grams q.lab r.lab] ~])
          ?:  p.lab  ::x?  dun never gets changed, so always | ?
            (pa-sauce ost.hid [[%quit ~] ~])
          +>.$(followers (~(put by followers) ost.hid riv))
      ^=  lab
      =+  [end=count gaz=grams dun=| zeg=*(list telegram)]
      |-  ^-  (trel ? @ud (list telegram))
      ?~  gaz  [dun end zeg]
      ?:  ?-  -.q.riv                                   ::  after the end
            $ud  (lte p.q.riv end)
            $da  (lte p.q.riv p.r.q.i.gaz)
          ==
        ::x  if we're past the river, continue browsing back.
        $(end (dec end), gaz t.gaz)  ::TODO  dun &
      ?:  ?-  -.p.riv                                   ::  before the start
            $ud  (lth end p.p.riv)
            $da  (lth p.r.q.i.gaz p.p.riv)
          ==
        ::x  if we're before the river, we're done.
        [dun end zeg]
      ::x  if we're in the river, add this gram and continue.
      $(end (dec end), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ++  pa-first-grams                                  ::  subscribe messages
      ::x  (called upon subscribe) send backlog of grams to her.
      ::x  deduces which messages to send from pax.
      ::
      |=  {her/ship pax/path}
      ^+  +>
      ?.  (pa-visible her)
        ~&  [%pa-first-grams-visible ~]
        (pa-sauce ost.hid [%quit ~]~)
      ::x  find the range of grams to send.
      =+  ^=  ruv  ^-  (unit river)
          %+  biff  ::x  collapse unit list.
            (zl:jo (turn pax ;~(biff slay |=(a/coin `(unit dime)`?~(-.a a ~)))))
          |=  paf/(list dime)
          ?~  paf
            $(paf [%ud (sub (max 64 count) 64)]~)
          ?~  t.paf
            $(t.paf [%da (dec (bex 128))]~)
          ?.  ?=({{?($ud $da) @} {?($ud $da) @} $~} paf)
            ~
          `[[?+(- . $ud .)]:i.paf [?+(- . $ud .)]:i.t.paf]  ::XX arvo issue #366
      ::  ~&  [%pa-first-grams her pax ruv]
      ?~  ruv
        ~&  [%pa-first-grams-malformed pax]
        (pa-sauce ost.hid [%quit ~]~)
      (pa-start u.ruv)
    ::
    ++  pa-refresh                                      ::  update to listeners
      ::x  called when grams get added or changed. calculates the changes and
      ::x  sends them to all followers. if we run into any followers that are
      ::x  no longer interested in this story, remove them.
      ::
      |=  {num/@ud gam/telegram}
      ^+  +>
      ::x  notify all of our readers.
      =.  +>  (pa-inform %grams man num gam ~)
      ::x  notify only the followers who are currently interested.
      =+  ^=  moy
          |-  ^-  (pair (list bone) (list move))
          ?~  followers  [~ ~]
          ::  ~&  [%pa-refresh num n.followers]
          =+  lef=$(followers l.followers)
          =+  rit=$(followers r.followers)
          =+  old=[p=(welp p.lef p.rit) q=(welp q.lef q.rit)]
          ?:  ?-  -.q.q.n.followers                        ::  after the end
                $ud  (lte p.q.q.n.followers num)
                $da  (lte p.q.q.n.followers p.r.q.gam)
              ==
            [[p.n.followers p.old] [[p.n.followers %quit ~] q.old]]
          ?:  ?-  -.p.q.n.followers                        ::  before the start
                $ud  (gth p.p.q.n.followers num)
                $da  (gth p.p.q.n.followers p.r.q.gam)
              ==
            old
          :-  p.old
          [[p.n.followers %diff %talk-report %grams num gam ~] q.old]
      =.  moves  (welp q.moy moves)
      |-  ^+  +>.^$
      ?~  p.moy  +>.^$
      $(p.moy t.p.moy, followers (~(del by followers) i.p.moy))
    ::
    ++  pa-lesson                                       ::  learn multiple
      ::x  learn all telegrams in a list.
      ::
      |=  gaz/(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (pa-learn i.gaz))
    ::
    ++  pa-learn                                        ::  learn message
      ::x  store an incoming telegram, modifying audience to say we received it.
      ::x  update existing telegram if it already exists.
      ::
      |=  gam/telegram
      ^+  +>
      ::x  if author isn't allowed to write here, reject.
      ?.  (pa-admire p.gam)
        +>.$
      =.  q.q.gam
        ::x  if we are in the audience, mark us as having received it.
        =+  ole=(~(get by q.q.gam) [%& our.hid man])
        ?^  ole  (~(put by q.q.gam) [%& our.hid man] -.u.ole %received)
        ::  for fedearted stations, pretend station src/foo is also our/foo
        ::  XX pass src through explicitly instead of relying on implicit
        ::     value in hid from the subscription to src/foo
        =+  ole=(~(get by q.q.gam) [%& src.hid man])
        ?~  ole  q.q.gam
        ::x  as described above, fake src into our.
        =.  q.q.gam  (~(del by q.q.gam) [%& src.hid man])
        (~(put by q.q.gam) [%& our.hid man] -.u.ole %received)
      =+  old=(~(get by known) p.q.gam)
      ?~  old
        (pa-append gam)      ::x  add
      (pa-revise u.old gam)  ::x  modify
    ::
    ++  pa-append                                       ::  append new
      ::x  add gram to our story, and update our subscribers.
      ::
      |=  gam/telegram
      ^+  +>
      %+  %=  pa-refresh
            grams  [gam grams]
            count  +(count)
            known  (~(put by known) p.q.gam count)
          ==
        count
      gam
    ::
    ++  pa-revise                                       ::  revise existing
      ::x  modify a gram in our story, and update our subscribers.
      ::
      |=  {num/@ud gam/telegram}
      =+  way=(sub count num)
      ?:  =(gam (snag (dec way) grams))
        +>.$                                            ::  no change
      =.  grams  (welp (scag (dec way) grams) [gam (slag way grams)])
      (pa-refresh num gam)
    ::
    ++  pa-unearth
      ::x  find the bones in our follower list than belong to a ship in sis.
      ::
      |=  sis/(set ship)
      ^-  (set bone)
      %-  ~(rep in sup.hid)
      |=  {{b/bone s/ship p/path} c/(set bone)}
      ?.  ?&  (~(has in sis) s)
              (~(has by followers) b)
              ?=({@tas *} p)
              =(i.p man)
          ==
        c
      (~(put in c) b)
    ::
    ++  pa-permit
      ::x  invite/banish ships to/from this station.
      ::
      |=  {inv/? sis/(set ship)}
      ^+  +>
      =/  white/?  ?=(?($white $green) p.cordon.shape)  ::  whitelist?
      =/  add/?  =(inv white)                           ::  add to list?
      =.  followers  ::TODO  =?
        ?:  inv  followers
        %-  ~(rep in (pa-unearth sis))
        |=  {b/bone f/_followers}
        =.  f  ?~  f  followers  f  ::TODO  =?
        (~(del by f) b)
      =.  +>.$
        %-  pa-tell
        :-  %publish
        %-  ~(rep in sis)
        |=  {s/ship t/(list thought)}
        :_  t
        =^  sir  eny.hid  (uniq eny.hid)
        :+  sir                                         ::  serial
          [[[%& s (main s)] [*envelope %pending]] ~ ~]  ::  audience
        :+  now.hid  ~                                  ::  statement
        [%inv inv [our.hid man]]
      %-  pa-reform
      %=  shape
          q.cordon
        %.  sis
        ?:  add
          ~&  [%new-shape (~(uni in q.cordon.shape) sis)]
          ~(uni in q.cordon.shape)
        ~&  [%new-shape (~(dif in q.cordon.shape) sis)]
        ~(dif in q.cordon.shape)
      ==
    --
  --
::
++  peer                                                ::  accept subscription
  ::x  incoming subscription on pax.
  ::
  |=  pax/path
  ~&  [%b-peer pax src.hid ost.hid]
  ^+  [*(list move) +>]
  ~?  !(team src.hid our.hid)  [%peer-talk-stranger src.hid]
  ?:  ?=({$sole *} pax)  ~&(%broker-no-sole !!)
  ra-abet:(ra-subscribe:ra src.hid pax)
::
++  poke-talk-command                                   ::  accept command
  ::x  incoming talk command. process it and update logs.
  ::
  |=  cod/command
  ^+  [*(list move) +>]
  ::  ~&  [%talk-poke-command src.hid cod]
  =^  mos  +>.$
      ra-abet:(ra-apply:ra src.hid cod)
  =^  mow  +>.$  log-all-to-file
  [(welp mos mow) +>.$]
::
++  poke-talk-action                                    ::  accept action
  ::x  incoming talk action. process it.
  ::
  |=  act/action
  ?.  (team src.hid our.hid)
    ~&  [%talk-action-stranger src.hid]
    [~ +>]
  ra-abet:(ra-action:ra ost.hid act)
::
++  diff-talk-report                                    ::  accept report
  ::x  incoming talk-report. process it and update logs.
  ::
  |=  {way/wire rad/report}
  ^-  (quip move +>)
  =^  mos  +>.$
      %+  etch-friend  way  |=  {man/knot cuz/station}
      ra-abet:(ra-diff-talk-report:ra man cuz rad)
  =^  mow  +>.$  log-all-to-file
  [(welp mos mow) +>.$]
::
++  coup-repeat                                         ::
  ::x  ack from ++ra-transmit. mark the message as received or rejected.
  ::
  |=  {way/wire saw/(unit tang)}
  %+  etch-repeat  [%repeat way]  |=  {num/@ud src/@p man/knot}
  ra-abet:(ra-coup-repeat:ra [num src man] saw)
::
++  etch                                                ::  parse wire
  ::x  parse wire to obtain either %friend with story and station or %repeat
  ::x  with message number, source ship and story.
  ::
  |=  way/wire
  ^-  weir
  ?+    -.way  !!
      $friend
    ?>  ?=({$show @ @ @ $~} t.way)
    [%friend i.t.t.way (slav %p i.t.t.t.way) i.t.t.t.t.way]
  ::
      $repeat
    ?>  ?=({@ @ @ $~} t.way)
    [%repeat (slav %ud i.t.way) (slav %p i.t.t.way) i.t.t.t.way]
  ==
::
++  etch-friend                                         ::
  ::x  parse a /friend wire, call gate with resulting data.
  ::
  |=  {way/wire fun/$-({man/knot cuz/station} {(list move) _.})}
  =+  wer=(etch way)
  ?>(?=($friend -.wer) (fun p.wer q.wer))
::
++  etch-repeat                                         ::
  ::x  parse a /repeat wire, call gate with resulting data.
  ::
  |=  {way/wire fun/$-({num/@ud src/@p man/knot} {(list move) _.})}
  =+  wer=(etch way)
  ?>(?=($repeat -.wer) (fun p.wer q.wer r.wer))
::
++  reap-friend                                         ::
  ::x  subscription n/ack. if it failed, remove their subscription from state.
  ::
  |=  {way/wire saw/(unit tang)}
  ^-  (quip move +>)
  ?~  saw  [~ +>]
  %+  etch-friend  [%friend way]  |=  {man/knot cuz/station}
  =.  u.saw  [>%reap-friend-fail man cuz< u.saw]
  %-  (slog (flop u.saw))
  ra-abet:(ra-quit:ra man cuz)
::
++  quit-friend                                         ::
  ::x  resubscribe.
  ::
  |=  way/wire
  %+  etch-friend  [%friend way]  |=  {man/knot cuz/station}
  ra-abet:(ra-retry:ra man cuz)
::
++  pull                                                ::
  ::x  unsubscribe.
  ::
  |=  pax/path
  ^+  [*(list move) +>]
  ::  ~&  [%talk-pull src.hid ost.hid pax]
  ra-abet:(ra-cancel:ra src.hid pax)
::
++  log-all-to-file
  ::x  for every story we're logging, (over)write all their grams to log files,
  ::x  if new ones have arrived.
  ::
  ^-  (quip move .)
  ?:  &  [~ .]  ::  XXX!!!!
  :_  %_  .
        log   %-  ~(urn by log)
              |=({man/knot len/@ud} count:(~(got by stories) man))
      ==
  %+  murn  (~(tap by log))
  |=  {man/knot len/@ud}
  ^-  (unit move)
  ?:  (gte len count:(~(got by stories) man))
    ~
  `(log-to-file man)
::
++  log-to-file
  ::x  log all grams of story man to a file.
  ::
  |=  man/knot
  ^-  move
  =+  ^-  paf/path
      =+  day=(year %*(. (yore now.hid) +.t +:*tarp))
      %+  tope  [our.hid %home da+now.hid]
      /talk-telegrams/(scot %da day)/[man]/talk
  =+  grams:(~(got by stories) man)
  [ost.hid %info /jamfile our.hid (foal paf [%talk-telegrams !>(-)])]
::
++  poke-talk-comment
  ::x  send a comment.
  ::
  |=  {pax/path sup/spur txt/@t}  ^-  (quip move +>)
  ra-abet:(ra-comment:ra pax sup txt)
::
++  poke-talk-fora-post
  ::x  send a fora post.
  ::
  |=  {pax/path sup/spur hed/@t txt/@t}  ^-  (quip move +>)
  ra-abet:(ra-fora-post:ra pax sup hed txt)
::
++  poke-talk-save
  ::x  store the talk telegrams of story man in a log file.
  ::
  |=  man/knot
  ^-  (quip move +>)
  =+  paf=/(scot %p our.hid)/home/(scot %da now.hid)/talk/[man]/talk-telegrams
  =+  grams:(~(got by stories) man)
  [[ost.hid %info /jamfile our.hid (foal paf [%talk-telegrams !>(-)])]~ +>.$]
::
++  poke-talk-load
  ::x  load/update the story man into our state, as saved in ++poke-talk-save.
  ::
  |=  man/knot
  =+  ^=  grams
      .^  (list telegram)
          %cx
          /(scot %p our.hid)/home/(scot %da now.hid)/talk/[man]/talk-telegrams
      ==
  =+  toy=(~(got by stories) man)
  [~ +>.$(stories (~(put by stories) man toy(grams grams, count (lent grams))))]
::
++  poke-talk-log
  ::x  start logging story man.
  ::
  |=  man/knot
  ~&  %poke-log
  ^-  (quip move +>)
  :-  [(log-to-file man) ~]
  +>.$(log (~(put by log) man count:(~(got by stories) man)))
::
++  poke-talk-unlog
  ::x  stop logging story man.
  ::
  |=  man/knot
  ^-  (quip move +>)
  :-  ~
  +>.$(log (~(del by log) man))
::
++  prep
  ::x  state adapter.
  ::
  |=  old/(unit house)
  ::^-  (quip move ..prep)
  ::?~  old
    ra-abet:ra-init:ra
  ::[~ ..prep(+<+ u.old)]
--
