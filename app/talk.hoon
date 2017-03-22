::                                                      ::  ::  
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::   
::
::TODO  master changes
::TODO  =/ instead of =+ ^= where possible
::TODO  avoid lark where possible
::TODO  remove old/unused code
::TODO  improve naming
::TODO  tidiness
::
/?    310                                               ::  hoon version
/-    talk, sole                                        ::  structures
/+    talk, sole, time-to-id, twitter                   ::  libraries
/=    seed  /~  !>(.)
::
::::
  ::
::x  include talk and sole cores from the /+ include into our subject,
::x  so we can do some-arm instead of some-arm:talk.
[. talk sole]
=>  |%                                                  ::  data structures
    ++  house  {$6 house-6}                             ::  full state
    ++  house-any                                       ::  app history
      $%  {$3 house-3}                                  ::  3: talk
          {$4 house-4}                                  ::  4: talk
          {$5 house-5}                                  ::  5: talk
          {$6 house-6}                                  ::  5: talk
      ==                                                ::
    ++  house-3                                         ::
      %+  cork  house-4  |=  house-4                    ::  modern house with
      +<(stories (~(run by stories) story-3))           ::  old stories
    ++  house-4                                         ::
      %+  cork  house-5  |=  house-5                    ::  modern house with
      +<(shells (~(run by shells) shell-4))             ::  no settings
    ++  house-5                                         ::
      %+  cork  house-6  |=  house-6                    ::  modern house with
      +<(shells (~(run by shells) shell-5))             ::  auto-audience
    ++  house-6                                         ::
      $:  stories/(map knot story)                      ::  conversations
          general/(set bone)                            ::  meta-subscribe
          outbox/(pair @ud (map @ud thought))           ::  urbit outbox
          folks/(map ship human)                        ::  human identities
          shells/(map bone shell)                       ::  interaction state
          log/(map knot @ud)                            ::  logged to clay
          nik/(map (set partner) char)                  ::  bound station glyphs
          nak/(jug char (set partner))                  ::  station glyph lookup
      ==                                                ::
    ++  story-3  (cork story |=(story +<(|10 &11.+<)))  ::  missing glyphers
    ++  story                                           ::  wire content
      $:  count/@ud                                     ::  (lent grams)
          grams/(list telegram)                         ::  all history
          locals/(map ship (pair @da status))           ::  local presence
          remotes/(map partner atlas)                   ::  remote presence
          mirrors/(map station config)                  ::  remote config
          sequence/(map partner @ud)                    ::  partners heard
          shape/config                                  ::  configuration
          known/(map serial @ud)                        ::  messages heard
          gramsers/(map bone river)                     ::  message followers
          groupers/(set bone)                           ::  presence followers
          cabalers/(set bone)                           ::  config followers
          glyphers/(set bone)                           ::  glyph followers
      ==                                                ::
    ++  shell                                           ::  console session
      $:  her/ship                                      ::  client identity
          man/knot                                      ::  mailbox
          count/@ud                                     ::  messages shown
          say/sole-share                                ::  console state
          active/{$~ u/(set partner)}                   ::  active targets
          $passive-deprecated                           ::  passive targets
          owners/register                               ::  presence mirror
          harbor/(map knot (pair posture cord))         ::  stations mirror
          system/cabal                                  ::  config mirror
          settings/(set knot)                           ::  frontend settings
      ==                                                ::
    ++  shell-5                                         ::  has passive
      %+  cork  shell  |=  shell                        ::
      %=  +<                                            ::
        &6      passive=*(set partner)                  ::
        active  *(unit (set partner))                   ::
      ==                                                ::
    ++  shell-4  (cork shell-5 |=(shell-5 +<(|8 &9.+<)))::  missing settings
    ++  river  (pair point point)                       ::  stream definition
    ++  point                                           ::  stream endpoint
      $%  {$ud p/@ud}                                   ::  by number
          {$da p/@da}                                   ::  by date
      ==                                                ::
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  {$talk-report report}                         ::
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
    ++  work                                            ::  interface action
      $%  {$number p/$@(@ud {@u @ud})}                  ::  relative/absolute
          {$help $~}                                    ::  print usage info
          {$who p/where}                                ::  presence
          {$what p/$@(char (set partner))}              ::  show bound glyph
          {$bind p/char q/(unit where)}                 ::
          {$join p/where}                               ::  
          {$leave p/where}                              ::  
          {$say p/(list speech)}                        ::
          {$eval p/cord q/twig}                         ::
          {$invite p/knot q/(list partner)}             ::  whitelist add
          {$banish p/knot q/(list partner)}             ::  blacklist add
          {$block p/knot q/(list partner)}              ::  blacklist add
          {$author p/knot q/(list partner)}             ::  whitelist add
          {$nick p/(unit ship) q/(unit cord)}           ::
          {$set p/knot}                                 ::
          {$unset p/knot}                               ::
          {$target p/where q/(unit work)}               ::  set active targets
          ::  {$destroy p/knot}                         ::
          {$create p/posture q/knot r/cord}             ::
          {$probe p/station}                            ::
      ==                                                ::
    ++  where  (set partner)                            ::  non-empty audience 
    ++  sigh                                            ::  assemble label
      ::x?  why is this not in ++ta?
      ::
      |=  {len/@ud pre/tape yiz/cord}
      ^-  tape
      =+  nez=(trip yiz)
      =+  lez=(lent nez)
      ?>  (gth len (lent pre))
      =.  len  (sub len (lent pre))
      ?.  (gth lez len)  
        =.  nez  (welp pre nez)
        ?.  (lth lez len)  nez
        (runt [(sub len lez) '-'] nez)
      :(welp pre (scag (dec len) nez) "+")  
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]     :: station char pool'
    ++  peer-type                                       ::  stream requests
      ::x  helper functions for determining/specifying from/in a path, what kind
      ::x  of subscription our peer wants/what they're interested in.
      ::
      =<  apex
      |%
      ++  apex  ?($a-group $f-grams $v-glyph $x-cabal)  ::  options
      ++  encode  |=(a/apex ^-(char (end 3 1 a)))       ::  by first char
      ++  decode                                        ::  discriminate
        |=  a/char  ^-  apex
        ?+  a  ~|(bad-subscription-designator+a !!)
          $a  %a-group
          $f  %f-grams
          $v  %v-glyph
          $x  %x-cabal
        ==
      --
    --
|%
::  old protocol workaround door
++  timed
  ::x?  seems hacky. if old, should be removed in "new talk", right?
  ::x?  seems like it's used for adding/dealing with "fake"/workaround ships
  ::x?  with datetimes in their status. but why?
  ::x  looking at ++pa-remind, this can safely be deleted when breaching state.
  ::
  ::x  a: stations with ships and their status.
  |_  a/(map partner atlas) :: XX (map partner (pair @da atlas))
  ++  strip
    ::x  removes workaround ships from all partner's status lists.
    ::
    (~(run by a) |=(b/atlas (~(del by b) `@p`%timed-sub)))
  ::
  ++  put  ::  XX put:by
    ::x  adds workaround ship to d with pretty-printed date c in its status,
    ::x  then adds it to a with key/partner b.
    ::
    |=  {b/partner c/@da d/atlas}
    =/  sta/status  [%gone [~ (some (scot %da c))]]
    (~(put by a) b (~(put by d) `@p`%timed-sub sta))
  ::
  ++  decode-status
    ::x  attempts to retrieve datetime from status (as inserted by put:timed).
    ::
    |=  a/status  ^-  (unit @da)
    ?.  ?=({$gone $~ $~ tym/@t} a)  ~
    =>  .(a `{$gone $~ $~ tym/@t}`a)
    (slaw %da tym.a)
  ::
  ++  uni
    ::x  union of two station-shipstatus maps.
    ::
    |=  b/_a  ^+  a
    :: XX efficiency
    %-  ~(uni by a)
    %-  ~(urn by b)
    |=  nb/{p/partner q/atlas}
    ?.  (~(has by a) p.nb)  q.nb
    =/  qna  (~(got by a) p.nb)
    :: XX p.qna p.q.nb
    =/  pqna  (biff (~(get by qna) `@p`%timed-sub) decode-status)
    ?~  pqna  q.nb
    =/  pqnb  (biff (~(get by q.nb) `@p`%timed-sub) decode-status)
    ?~  pqnb  qna
    ?:  (gth u.pqna u.pqnb)  qna
    ?:  (gth u.pqnb u.pqna)  q.nb
    ::  unfortunately, multiple reports on the same channel can
    ::  be sent on the same event, necessitating last-wins
    :: ~|  uni-timed+[n.a n.b]
    :: ?>  =(n.a n.b)
    q.nb
  --
--
|_  {hid/bowl house}
++  ra                                                  ::  per transaction
  ::x  gets called when talk gets poked or otherwise prompted/needs to perform
  ::x  an action.
  ::x  arms generally modify state, and store moves in ++ra's moves. these get
  ::x  produced when calling ++ra-abet.
  ::x  in applying commands and making reports, it uses ++pa for story work.
  ::
  ::x  moves: moves storage, added to by ++ra-emit and -emil, produced by -abed.
  |_  moves/(list move)
  ++  sh                                                ::  per console
    ::x  shell core, responsible for doing things with console sessions,
    ::x  like parsing input, acting based on input, showing output, keeping
    ::x  track of settings and other frontend state.
    ::x  important arms include ++sh-repo which is used to apply reports, and
    ::x  ++sh-sole which gets called upon cli prompt interaction.
    ::x  any talk commands the core's arms want to have executed get put into
    ::x  coz. the stored commands get applied upon calling ++sh-abet. 
    ::
    |_  $:  ::x  coz: talk commands storage, applied by ++sh-abet.
            ::x  she: console session state used in this core.
            ::
            coz/(list command)                          ::  talk actions
            she/shell
        ==
    ++  sh-scad                                         ::  command parser
      ::x  builds a core with parsers for talk-cli, and produces its work arm.
      ::x  ++work uses those parsers to parse the current talk-cli prompt input
      ::x  and produce a work item to be executed by ++sh-work.
      ::
      =<  work
      |%
      ++  expr                                          ::  [cord twig]
        |=  tub/nail  %.  tub
        %+  stag  (crip q.tub)
        wide:(vang & [&1:% &2:% (scot %da now.hid) |3:%])
      ::
      ++  dare                                          ::  @dr
        %+  sear
          |=  a/coin
          ?.  ?=({$$ $dr @} a)  ~
          (some `@dr`+>.a)
        nuck:so
      ::
      ++  ship  ;~(pfix sig fed:ag)                     ::  ship
      ++  shiz                                          ::  ship set
        %+  cook
          |=(a/(list ^ship) (~(gas in *(set ^ship)) a))
        (most ;~(plug com (star ace)) ship)
      ::
      ++  pasp                                          ::  passport
        ;~  pfix  pat
          ;~  pose
            (stag %twitter ;~(pfix (jest 't') col urs:ab))
          ==
        ==
      ::
      ++  stan                                          ::  station
        ;~  pose
          (cold [our.hid man.she] col)
          ;~(pfix cen (stag our.hid sym))
          ;~(pfix fas (stag (sein our.hid) sym))
        ::
          %+  cook
            |=  {a/@p b/(unit term)}
            [a ?^(b u.b (main a))]
          ;~  plug
            ship
            (punt ;~(pfix fas urs:ab))
          ==
        ==
      ::
      ++  parn                                          ::  partner
        ;~  pose
          (stag %& stan)
          (stag %| pasp)
        ==
      ++  partners-flat                                 ::  collapse mixed list
        |=  a/(list (each partner (set partner)))
        ^-  (set partner)
        ?~  a  ~
        ?-  -.i.a
          $&  (~(put in $(a t.a)) p.i.a)
          $|  (~(uni in $(a t.a)) p.i.a)
        ==
      ::
      ++  para                                          ::  partners alias
        %+  cook  partners-flat
        %+  most  ;~(plug com (star ace))
        (pick parn (sear sh-glyf glyph))
      ::
      ++  parz                                          ::  non-empty partners
        %+  cook  ~(gas in *(set partner))
        (most ;~(plug com (star ace)) parn)
      ::
      ++  nump                                          ::  number reference
        ;~  pose
          ;~(pfix hep dem:ag)
          ;~  plug
            (cook lent (plus (just '0')))
            ;~(pose dem:ag (easy 0))
          ==
          (stag 0 dem:ag)
        ==
      ::
      ++  pore                                          ::  posture
        ;~  pose
          (cold %black (jest %channel))
          (cold %white (jest %village))
          (cold %green (jest %journal))
          (cold %brown (jest %mailbox))
        ==
      ::
      ++  message
        ;~  pose
          ;~(plug (cold %eval hax) expr)
        ::
          %+  stag  %say
          %+  most  (jest '•')
          ;~  pose
            (stag %url aurf:urlp)
            :(stag %lin | ;~(pfix pat text))
            :(stag %lin & ;~(less sem hax text))
          ==
        ==
      ::
      ++  nick  (cook crip (stun [1 14] low))           ::  nickname
      ++  text  (cook crip (plus (shim ' ' '~')))       ::  bullets separating
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      ::  station postfix
      ++  setting
        %-  perk  :~
          %noob
          %quiet
          %showtime
        ==
      ++  work
        %+  knee  *^work  |.  ~+
        =-  ;~(pose ;~(pfix sem -) message)
        ;~  pose
          ;~  (glue ace)  (perk %create ~)
            pore
            ;~(pfix cen sym)
            qut
          ==
        ::
          ;~(plug (perk %who ~) ;~(pose ;~(pfix ace para) (easy ~)))
          ;~(plug (perk %bind ~) ;~(pfix ace glyph) (punt ;~(pfix ace para)))
          ;~((glue ace) (perk %join ~) para)
          ;~((glue ace) (perk %leave ~) para)
          ;~((glue ace) (perk %what ~) ;~(pose parz glyph))
        ::
          ;~  plug  (perk %nick ~)
            ;~  pose
              ;~  plug
                (cook some ;~(pfix ace ship))
                (cold (some '') ;~(pfix ace sig))
              ==
              ;~  plug
                ;~  pose
                  (cook some ;~(pfix ace ship))
                  (easy ~)
                ==
                ;~  pose
                  (cook some ;~(pfix ace nick))
                  (easy ~)
                ==
              ==
            ==
          ==
        ::
          ;~(plug (perk %set ~) ;~(pose ;~(pfix ace setting) (easy %$)))
          ;~(plug (perk %unset ~) ;~(pfix ace setting))
        ::
          ;~(plug (perk %help ~) (easy ~))
          (stag %number nump)
          (stag %target ;~(plug para (punt ;~(pfix ace message))))
          (stag %number (cook lent (star sem)))
        ==
      --
    ++  sh-abet
      ::x  applies talk commands (in reverse order, because ++sh-tell adds them
      ::x  to coz as [new coz], but you want to apply them in the order
      ::x  [coz new]). produces an updated context for the ++sh core.
      ::
      ^+  +>  ::x  points to ++sh's |_ core's context.
      =+  zoc=(flop coz)
      |-  ^+  +>+>  ::x  +> would point to |-'s context. +>+> goes to ++sh |_'s.
      ::x  produce context with this shell updated.
      ?~  zoc  +>+>.$(shells (~(put by shells) ost.hid she))
      ::x  recurse, with context (of |-?) modified.
      $(zoc t.zoc, +>.$ (sh-deal i.zoc))
    ::
    ++  sh-deal                                         ::  apply from shell
      ::x  used by ++sh-abet, applies an individual talk command.
      ::
      |=  cod/command
      ^+  +>
      ?-    -.cod
        ::x  the $design command is used for modifying channel configs,
        ::x  which is done when joining, leaving or creating channels.
          $design
        ?~  q.cod
          ::x  updates context with new config state.
          =.  +>+>.$  (ra-config p.cod *config)
          ::x  produces context with story p.cod deleted.
          +>.$(stories (~(del by stories) p.cod))
        ::x  produces +> with its +> (so, +>+>) updated by ++ra-think.
        +>(+> (ra-config p.cod u.q.cod))
      ::
        ::x  used for relaying messages (as a station host).
          $review   +>(+> (ra-think | her.she +.cod))
        ::x  used for sending messages (as their author).
          $publish  +>(+> (ra-think & her.she +.cod))
      ==
    ::
    ++  sh-fact                                         ::  send console effect
      ::x  adds a console effect to ++ra's moves.
      ::
      |=  fec/sole-effect
      ^+  +>
      +>(moves :_(moves [ost.hid %diff %sole-effect fec]))
    ::
    ++  sh-peep                                         ::  peer to path
      ::x?  unused?
      ::
      |=  pax/path
      ^+  +>
      +>(+> (ra-subscribe her.she pax))
    ::
    ++  sh-peer                                         ::  subscribe shell
      ::x  create a shell, subscribe to default stories.
      ::
      =<  sh-prod
      %_    .
          +>
        =/  typ
          =+  (ly ~[%a-group %f-grams %x-cabal])
          (rap 3 (turn - encode:peer-type))
        ::x  subscriptions to the shell's ship's default channels.
        (ra-subscribe:(ra-subscribe her.she ~) her.she [typ man.she ~])
      ==
    ::
    ++  sh-prod                                         ::  show prompt
      ::x  make and store a move to modify the cli prompt, displaying audience.
      ::
      ^+  .
      %+  sh-fact  %pro
      :+  &  %talk-line
      ^-  tape
      =/  rew/(pair (pair @t @t) (set partner))
          [['[' ']'] u.active.she]
      =+  cha=(~(get by nik) q.rew)
      ?^  cha  ~[u.cha ' ']
      :: ~&  [rew nik nak]
      =+  por=~(te-prom te man.she q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-pact                                         ::  update active aud
      ::x  change currently selected audience to lix, updating prompt.
      ::
      |=  lix/(set partner)
      ^+  +>
      =+  act=(sh-pare lix)  ::x  ensure we can see what we send.
      ?~  act  ~|(%no-audience !!)  ::x?  this can't actually happen, right?
      ?:  =(active.she `act)  +>.$
      sh-prod(active.she `act)
    ::
    ++  sh-pare                                         ::  adjust target list
      ::x  if the audience paz does not contain a partner we're subscribed to,
      ::x  add our mailbox to the audience (so that we can see our own message).
      ::
      |=  paz/(set partner)
      ?:  (sh-pear paz)  paz
      (~(put in paz) [%& our.hid man.she])
    ::
    ++  sh-pear                                         ::  hearback
      ::x  produces true if any partner is included in our subscriptions,
      ::x  aka, if we hear messages sent to paz.
      ::
      |=  paz/(set partner)
      ?~  paz  |
      ?|  $(paz l.paz) 
          $(paz r.paz)
          (~(has in sources.shape:(~(got by stories) man.she)) `partner`n.paz)
      ==
    ::
    ++  sh-pest                                         ::  report listen
      ::x  updates audience to be tay, only if tay is not a village/%white.
      ::x?  why exclude village (invite-only?) audiences from this?
      ::
      |=  tay/partner
      ^+  +>
      ?.  ?=($& -.tay)  +>  ::x  if partner is a passport, do nothing.
      =+  sib=(~(get by ham.system.she) `station`p.tay)  ::x  get config for tay
      ?.  |(?=($~ sib) !?=($white p.cordon.u.sib))
        +>.$
      (sh-pact [tay ~ ~])
    ::
    ++  sh-rend                                         ::  print on one line
      ::x  renders a telegram as a single line, adds it as a console move,
      ::x  and updates the selected audience to match the telegram's.
      ::
      |=  gam/telegram
      =+  lin=~(tr-line tr man.she settings.she gam)
      (sh-fact %txt lin)
    ::
    ++  sh-numb                                         ::  print msg number
      ::x  does as it says on the box.
      ::
      |=  num/@ud
      ^+  +>
      =+  bun=(scow %ud num)
      %+  sh-fact  %txt
      (runt [(sub 13 (lent bun)) '-'] "[{bun}]")
    ::
    ++  sh-glyf                                         ::  decode glyph
      ::x  gets the partner(s) that match a glyph.
      ::x?  why (set partner)? it seems like it only ever returns a single one.
      ::TODO should produce a set when ambiguous.
      ::
      |=  cha/char  ^-  (unit (set partner))
      =+  lax=(~(get ju nak) cha)
      ?:  =(~ lax)  ~  ::x  no partner.
      ?:  ?=({* $~ $~} lax)  `n.lax  ::x  single partner.
      ::x  in case of multiple partners, pick the most recently active one.
      =+  grams=grams:(~(got by stories) man.she)
      |-  ^-  (unit (set partner))
      ?~  grams  ~
      ::x  get first partner from a telegram's audience.
      =+  pan=(silt (turn (~(tap by q.q.i.grams)) head))
      ?:  (~(has in lax) pan)  `pan
      $(grams t.grams)
    ::
    ::TODO  we have a stdlib set diff now!
    ++  sh-repo-house-diff
      ::x  calculates difference between two shelves (channel definitions).
      ::
      |=  {one/shelf two/shelf}
      =|  $=  ret
          $:  old/(list (pair knot (pair posture cord)))
              new/(list (pair knot (pair posture cord)))
              cha/(list (pair knot (pair posture cord)))
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret 
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has by one) p.i.owt)
          ret
        ret(new [i.owt new.ret])
      ret 
    ::
    ++  sh-repo-atlas-diff
      ::x  calculates the difference between two atlasses (presence lists).
      ::
      |=  {one/atlas two/atlas}
      =|  $=  ret
          $:  old/(list (pair ship status))
              new/(list (pair ship status))
              cha/(list (pair ship status))
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        ?:  =(%gone p.q.i.eno)  ret
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(%gone p.u.unt)
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  =(%gone p.q.i.owt)  ret
        ?.  (~(has by one) p.i.owt)
          ret(new [i.owt new.ret])
        ?:  =(%gone p:(~(got by one) p.i.owt))
          ret(new [i.owt new.ret])
        ret
      ret 
    ::
    ++  sh-repo-cabal-diff
      ::x  calculates the difference between two cabals (station configurations)
      ::
      |=  {one/(map station config) two/(map station config)}
      =|  $=  ret
          $:  old/(list (pair station config))
              new/(list (pair station config))
              cha/(list (pair station config))
          ==
      ^+  ret
      =.  ret  
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret 
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has by one) p.i.owt)
          ret
        ret(new [i.owt new.ret])
      ret
    ::
    ++  sh-repo-rogue-diff
      ::x  calculates the difference between two maps of stations and their
      ::x  presence lists.
      ::
      |=  {one/(map partner atlas) two/(map partner atlas)}
      =|  $=  ret
          $:  old/(list (pair partner atlas))
              new/(list (pair partner atlas))
              cha/(list (pair partner atlas))
          ==
      =.  one  ~(strip timed one)
      =.  two  ~(strip timed two)
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        =+  unt=(~(get by two) p.i.eno)
        ?~  unt
          ret(old [i.eno old.ret])
        ?:  =(q.i.eno u.unt)  ret 
        ret(cha [[p.i.eno u.unt] cha.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has by one) p.i.owt)
          ret
        ret(new [i.owt new.ret])
      ret
    ::
    ++  sh-repo-whom-diff
      ::x  calculates the difference between two partner sets.
      ::
      |=  {one/(set partner) two/(set partner)}
      =|  $=  ret
          $:  old/(list partner)
              new/(list partner)
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        ?:  (~(has in two) i.eno)
          ret
        ret(old [i.eno old.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has in one) i.owt)
          ret
        ret(new [i.owt new.ret])
      ret
    ::
    ++  sh-repo-ship-diff
      ::x  calculates the difference between two ship sets.
      ::
      |=  {one/(set ship) two/(set ship)}
      =|  $=  ret
          $:  old/(list ship)
              new/(list ship)
          ==
      ^+  ret
      =.  ret
        =+  eno=(~(tap by one))
        |-  ^+  ret
        ?~  eno  ret
        =.  ret  $(eno t.eno)
        ?:  (~(has in two) i.eno)
          ret
        ret(old [i.eno old.ret])
      =.  ret
        =+  owt=(~(tap by two))
        |-  ^+  ret
        ?~  owt  ret
        =.  ret  $(owt t.owt)
        ?:  (~(has in one) i.owt)
          ret
        ret(new [i.owt new.ret])
      ret 
    ::
    ++  sh-puss
      ::x  posture as text.
      ::
      |=  a/posture  ^-  tape
      ?-  a
        $black  "channel"
        $brown  "mailbox"
        $white  "village"
        $green  "journal"
      ==
    ::
    ++  sh-repo-config-exceptions
      ::x  used by ++sh-repo-config-show to aid in printing info to cli.
      ::
      |=  {pre/tape por/posture old/(list ship) new/(list ship)}
      =+  out=?:(?=(?($black $brown) por) "try " "cut ")
      =+  inn=?:(?=(?($black $brown) por) "ban " "add ")
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old  +>.^$
          =.  +>.^$  $(old t.old)
          (sh-note :(weld pre out " " (scow %p i.old)))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          (sh-note :(weld pre out " " (scow %p i.new)))
      +>.$
    ::
    ++  sh-repo-config-sources
      ::x  used by ++sh-repo-config-show to aid in printing info to cli,
      ::x  pertaining to the un/subscribing to partners.
      ::
      |=  {pre/tape old/(list partner) new/(list partner)}
      ^+  +>
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old  +>.^$
          =.  +>.^$  $(old t.old)
          (sh-note (weld pre "off {~(ta-full ta man.she i.old)}"))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          (sh-note (weld pre "hey {~(ta-full ta man.she i.new)}"))
      +>.$
    ::
    ++  sh-repo-config-show
      ::x  prints config changes to the cli.
      ::
      |=  {pre/tape laz/config loc/config}
      ^+  +>
      =.  +>.$
        ?:  =(caption.loc caption.laz)  +>.$
        (sh-note :(weld pre "cap " (trip caption.loc)))
      =.  +>.$
          %+  sh-repo-config-sources
            (weld (trip man.she) ": ")
          (sh-repo-whom-diff sources.laz sources.loc)
      ?:  !=(p.cordon.loc p.cordon.laz)
        =.  +>.$  (sh-note :(weld pre "but " (sh-puss p.cordon.loc)))
        %^    sh-repo-config-exceptions  
            (weld (trip man.she) ": ")  
          p.cordon.loc
        [~ (~(tap in q.cordon.loc))]
      %^    sh-repo-config-exceptions  
          (weld (trip man.she) ": ")
        p.cordon.loc
      (sh-repo-ship-diff q.cordon.laz q.cordon.loc)
    ::
    ++  sh-repo-cabal-changes
      ::x  used by ++sh-repo-cabal for printing cabal config changes to cli.
      ::
      |=  $:  laz/(map station config)
              old/(list (pair station config))
              new/(list (pair station config))
              cha/(list (pair station config))
          ==
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new  +>.^$
          =.  +>.^$  $(new t.new)
          =.  +>.^$  (sh-pest [%& p.i.new])
          %+  sh-repo-config-show  
            (weld ~(sn-phat sn man.she p.i.new) ": ")
          [*config q.i.new]
      =.  +>.$
          |-  ^+  +>.^$
          ?~  cha  +>.^$
          =.  +>.^$  $(cha t.cha)
          %+  sh-repo-config-show  
            (weld ~(sn-phat sn man.she p.i.cha) ": ")
          [(~(got by laz) `station`p.i.cha) q.i.cha]
      +>.$
    ::
    ++  sh-repo-cabal
      ::x  updates the current shell's cabal and prints changes to cli.
      ::
      |=  bal/cabal
      ^+  +>
      =+  laz=system.she
      =.  system.she  bal
      =.  +>.$
          %+  sh-repo-cabal-changes  ham.laz
          (sh-repo-cabal-diff ham.laz ham.bal)
      (sh-repo-config-show "" loc.laz loc.bal)
    ::
    ++  sh-repo-house
      ::x  applies new shelf ("house"?) and prints changes to cli.
      ::
      |=  awl/(map knot (pair posture cord))
      ^+  +>
      =+  dif=(sh-repo-house-diff harbor.she awl) 
      =.  harbor.she  awl
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old.dif  +>.^$
          =.  +>.^$  $(old.dif t.old.dif)
          (sh-note "cut {(sh-puss p.q.i.old.dif)} %{(trip p.i.old.dif)}")
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new.dif  +>.^$
          =.  +>.^$  $(new.dif t.new.dif)
          =+  :*  nam=(trip p.i.new.dif)
                  por=(sh-puss p.q.i.new.dif)
                  des=(trip q.q.i.new.dif)
              ==
          (sh-note "new {por} %{nam}: {des}")
      =.  +>.$ 
          |-  ^+  +>.^$
          ?~  cha.dif  +>.^$
          =.  +>.^$  $(cha.dif t.cha.dif)
          =+  :*  nam=(trip p.i.cha.dif)
                  por=(sh-puss p.q.i.cha.dif)
                  des=(trip q.q.i.cha.dif)
              ==
          (sh-note "mod %{nam}: {por}, {des}")
      +>.$
    ::
    ++  sh-note                                         ::  shell message
      ::x  prints a txt to cli in talk's format.
      ::
      |=  txt/tape
      ^+  +>
      (sh-fact %txt (runt [14 '-'] `tape`['|' ' ' (scag 64 txt)]))
    ::
    ++  sh-spaz                                         ::  print status
      ::x  gets the presence of a status.
      ::
      |=  saz/status
      ^-  tape
      ['%' (trip p.saz)]
    ::
    ++  sh-repo-group-diff-here                         ::  print atlas diff
      ::x  prints presence notifications.
      ::
      |=  $:  pre/tape 
            $=  cul
            $:  old/(list (pair ship status))
                new/(list (pair ship status))
                cha/(list (pair ship status))
            ==
          ==
      ?:  (~(has in settings.she) %quiet)
        +>.$
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old.cul  +>.^$
          =.  +>.^$  $(old.cul t.old.cul)
          (sh-note (weld pre "bye {(scow %p p.i.old.cul)}"))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new.cul  +>.^$
          =.  +>.^$  $(new.cul t.new.cul)
          %-  sh-note
          (weld pre "met {(scow %p p.i.new.cul)} {(sh-spaz q.i.new.cul)}")
      =.  +>.$ 
          |-  ^+  +>.^$
          ?~  cha.cul  +>.^$
          %-  sh-note 
          (weld pre "set {(scow %p p.i.cha.cul)} {(sh-spaz q.i.cha.cul)}")
      +>.$
    ::
    ++  sh-repo-group-here                              ::  update local
      ::x  updates local presence store and prints changes.
      ::
      |=  loc/atlas
      ^+  +>
      =+  cul=(sh-repo-atlas-diff p.owners.she loc)
      =.  p.owners.she  loc
      (sh-repo-group-diff-here "" cul)
    ::
    ++  sh-repo-group-there                             ::  update foreign
      ::x  updates remote presences(?) and prints changes.
      ::
      |=  yid/(map partner atlas)
      =+  day=(sh-repo-rogue-diff q.owners.she yid)
      =+  dun=q.owners.she
      =.  q.owners.she  yid
      ?:  (~(has in settings.she) %quiet)
        +>.$
      =.  +>.$
          |-  ^+  +>.^$
          ?~  old.day  +>.^$
          =.  +>.^$  $(old.day t.old.day)
          (sh-note (weld "not " (~(ta-show ta man.she p.i.old.day) ~)))
      =.  +>.$
          |-  ^+  +>.^$
          ?~  new.day  +>.^$
          =.  +>.^$  $(new.day t.new.day)
          =.  +>.^$
              (sh-note (weld "new " (~(ta-show ta man.she p.i.new.day) ~)))
          (sh-repo-group-diff-here "--" ~ (~(tap by q.i.new.day)) ~)
      =.  +>.$ 
          |-  ^+  +>.^$
          ?~  cha.day  +>.^$
          =.  +>.^$  $(cha.day t.cha.day)
          =.  +>.^$
              (sh-note (weld "for " (~(ta-show ta man.she p.i.cha.day) ~)))
          =+  yez=(~(got by dun) p.i.cha.day)
          %+  sh-repo-group-diff-here  "--"
          (sh-repo-atlas-diff yez q.i.cha.day)
      +>.$
    ::
    ++  sh-repo-group
      ::x  update local and remote presences.
      ::
      |=  ges/register
      ^+  +>
      =.  +>  (sh-repo-group-here p.ges)
      =.  +>  (sh-repo-group-there q.ges)
      +>
    ::
    ++  sh-repo-gram
      ::x  renders telegram: increase gram count and print the gram.
      ::x  every fifth gram, prints the number.
      ::
      |=  {num/@ud gam/telegram}
      ^+  +>
      ?:  =(num count.she)
        =.  +>  ?:(=(0 (mod num 5)) (sh-numb num) +>)
        (sh-rend(count.she +(num)) gam)
      ?:  (gth num count.she)
        =.  +>  (sh-numb num)
        (sh-rend(count.she +(num)) gam)
      +> 
    ::
    ++  sh-repo-grams                                   ::  apply telegrams
      ::x  renders telegrams.
      ::
      |=  {num/@ud gaz/(list telegram)}
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, num +(num), +> (sh-repo-gram num i.gaz))
    ::
    ++  sh-repo-glyph                                   ::  apply binding
      ::x  updates glyph bindings and lookup, and updates selected audience.
      ::
      |=  nac/(jug char (set partner))
      ^+  +>
      %_  sh-prod
        nak  nac
        nik  %-  ~(gas by *(map (set partner) char))
             =-  (zing `(list (list {(set partner) char}))`-)
             %+  turn  (~(tap by nac))
             |=  {a/char b/(set (set partner))}
             (turn (~(tap by b)) |=(c/(set partner) [c a]))
      ==
    ::
    ++  sh-repo                                         ::  apply report
      ::x  applies the different kinds of reports using their handler arms above
      ::
      |=  rad/report
      ^+  +>
      ::  ~&  [%sh-repo rad]
      ?-  -.rad
        $cabal   (sh-repo-cabal +.rad)
        $grams   (sh-repo-grams +.rad)
        $glyph   (sh-repo-glyph +.rad)                  ::  XX ever happens?
        $group   (sh-repo-group +.rad)
        $house   (sh-repo-house +.rad)
      ==
    ::
    ++  sh-sane-chat                                    ::  sanitize chatter
      ::x  (for chat messages) sanitizes the input buffer and splits it into
      ::x  multiple lines ('•').
      ::
      |=  buf/(list @c)
      ^-  (list sole-edit)
      ?~  buf  ~
      =+  isa==(i.buf (turf '@'))
      =+  [[pre=*@c cur=i.buf buf=t.buf] inx=0 brk=0 len=0 new=|]
      =*  txt  -<
      |^  ^-  (list sole-edit)
          ?:  =(cur (turf '•'))
            ?:  =(pre (turf '•'))
              [[%del inx] ?~(buf ~ $(txt +.txt))]
            ?:  new
              [(fix ' ') $(cur `@c`' ')] 
            newline
          ?:  =(cur `@`' ')
            =.  brk  ?:(=(pre `@`' ') brk inx)
            ?.  =(64 len)  advance
            :-  (fix(inx brk) (turf '•'))
            ?:  isa
              [[%ins +(brk) (turf '@')] newline(new &)]
            newline(new &)
          ?:  =(64 len)
            =+  dif=(sub inx brk)
            ?:  (lth dif 64)
              :-  (fix(inx brk) (turf '•'))
              ?:  isa
                [[%ins +(brk) (turf '@')] $(len dif, new &)]
              $(len dif, new &)
            [[%ins inx (turf '•')] $(len 0, inx +(inx), new &)]
          ?:  |((lth cur 32) (gth cur 126))
            [(fix '?') advance]
          ?:  &((gte cur 'A') (lte cur 'Z'))
            [(fix (add 32 cur)) advance]
          advance
      ::
      ++  advance  ?~(buf ~ $(len +(len), inx +(inx), txt +.txt))
      ++  newline  ?~(buf ~ $(len 0, inx +(inx), txt +.txt))
      ++  fix  |=(cha/@ [%mor [%del inx] [%ins inx `@c`cha] ~])  
      --
    ::
    ++  sh-sane                                         ::  sanitize input
      ::x  parses cli prompt input using ++sh-scad and sanitizes when invalid.
      ::
      |=  {inv/sole-edit buf/(list @c)}
      ^-  {lit/(list sole-edit) err/(unit @u)}
      =+  res=(rose (tufa buf) sh-scad)
      ?:  ?=($| -.res)  [[inv]~ `p.res]
      :_  ~
      ?~  p.res  ~
      =+  wok=u.p.res
      |-  ^-  (list sole-edit)
      ?+  -.wok  ~
        $target  ?~(q.wok ~ $(wok u.q.wok))
        $say  |-  ::  XX per line
              ?~  p.wok  ~
              ?:  ?=($lin -.i.p.wok)
                (sh-sane-chat buf)
              $(p.wok t.p.wok)
      ==
    ::
    ++  sh-slug                                         ::  edit to sanity
      ::x  corrects invalid prompt input.
      ::
      |=  {lit/(list sole-edit) err/(unit @u)}
      ^+  +>
      ?~  lit  +>
      =^  lic  say.she
          (~(transmit sole say.she) `sole-edit`?~(t.lit i.lit [%mor lit]))
      (sh-fact [%mor [%det lic] ?~(err ~ [%err u.err]~)])
    ::
    ++  sh-stir                                         ::  apply edit
      ::x  called when typing into the talk prompt. applies the change and does
      ::x  sanitizing.
      ::
      |=  cal/sole-change
      ^+  +>
      =^  inv  say.she  (~(transceive sole say.she) cal)
      =+  fix=(sh-sane inv buf.say.she)
      ?~  lit.fix
        +>.$
      ?~  err.fix
        (sh-slug fix)                 :: just capital correction
      ?.  &(?=($del -.inv) =(+(p.inv) (lent buf.say.she)))
        +>.$                          :: allow interior edits, deletes
      (sh-slug fix)
    ::
    ++  sh-lame                                         ::  send error
      ::x  just puts some text into the cli.
      ::
      |=  txt/tape
      (sh-fact [%txt txt])
    ::
    ++  sh-whom                                         ::  current audience
      ::x  produces the currently selected audience for this shell.
      ::
      ^-  audience
      %-  ~(gas by *audience)
      %+  turn  (~(tap in u.active.she))
      |=(a/partner [a *envelope %pending])
    ::
    ++  sh-tell                                         ::  add command
      ::x  adds talk command to core state. these get applied with ++sh-abet.
      ::
      |=  cod/command
      %_(+> coz [cod coz])
    ::
    ++  sh-twig-head  ^-  vase                          ::  eval data
      ::x  makes a vase of environment data to evaluate against (#-messages).
      ::
      !>(`{our/@p now/@da eny/@uvI}`[our.hid now.hid (shas %eny eny.hid)])
    ::
    ++  sh-work                                         ::  do work
      ::x  implements worker arms for different talk commands.
      ::x  all worker arms must produce updated state/context.
      ::
      |=  job/work
      ^+  +>
      =+  roy=(~(got by stories) man.she)
      =<  work
      |%
      ++  work
        ?-  -.job
          $number  (number +.job)
          $leave   (leave +.job)
          $join    (join +.job)
          $eval    (eval +.job)
          $who     (who +.job)
          $what    (what +.job)
          $bind    (bind +.job)
          $invite  (invite +.job)
          $banish  (banish +.job)
          $author  (author +.job)
          $block   (block +.job)
          $create  (create +.job)
          $nick    (nick +.job)
          $set     (wo-set +.job)
          $unset   (unset +.job)
          $target  (target +.job)
          $probe   (probe +.job)
          $help    help
          $say     (say +.job)
        ==
      ::
      ++  activate                                      ::  from %number
        |=  gam/telegram
        ^+  ..sh-work
        =+  tay=~(. tr man.she settings.she gam)
        =.  ..sh-work  (sh-fact tr-fact:tay)
        sh-prod(active.she `tr-pals:tay)
      ::
      ++  help  
        (sh-fact %txt "see http://urbit.org/docs/using/messaging/")
      ::
      ++  glyph
        |=  idx/@
        =<  cha
        %+  reel  glyphs
        |=  {all/tape ole/{cha/char num/@}}
        =+  new=(snag (mod idx (lent all)) all)
        =+  num=~(wyt in (~(get ju nak) new))
        ?~  cha.ole  [new num]
        ?:  (lth num.ole num)
          ole
        [new num]
      ::
      ++  set-glyph
        |=  {cha/char lix/(set partner)}
        =:  nik  (~(put by nik) lix cha)
            nak  (~(put ju nak) cha lix)
          ==
        %_    ..sh-work
            ..pa
          %-  (ra-know man.she) 
          |=(_pa pa-abet:(pa-report glyphers %glyph nak))
        ==
      ::
      ++  join                                          ::  %join
        |=  pan/(set partner)
        ^+  ..sh-work
        =.  ..sh-work
          =+  (~(get by nik) pan)
          ?^  -  (sh-note "has glyph {<u>}")
          =+  cha=(glyph (mug pan))
          (sh-note:(set-glyph cha pan) "new glyph {<cha>}")
        =+  loc=loc.system.she
        ::x  change local mailbox config to include subscription to pan.
        %^  sh-tell  %design  man.she
        `loc(sources (~(uni in sources.loc) pan))
      ::
      ++  leave                                          ::  %leave
        |=  pan/(set partner)
        ^+  ..sh-work
        =+  loc=loc.system.she
        ::x  change local mailbox config to exclude subscription to pan.
        %^  sh-tell  %design  man.she
        `loc(sources (~(dif in sources.loc) pan))
      ::
      ++  what                                          ::  %what
        |=  qur/$@(char (set partner))  ^+  ..sh-work
        ?^  qur
          =+  cha=(~(get by nik) qur)
          (sh-fact %txt ?~(cha "none" [u.cha]~))
        =+  pan=(~(tap in (~(get ju nak) qur)))
        ?:  =(~ pan)  (sh-fact %txt "~")
        =<  (sh-fact %mor (turn pan .))
        |=(a/(set partner) [%txt <a>]) ::  XX ~(te-whom te man.she a)
      ::
      ++  who                                          ::  %who  
        |=  pan/(set partner)  ^+  ..sh-work  
        =<  (sh-fact %mor (murn (sort (~(tap by q.owners.she) ~) aor) .))
        |=  {pon/partner alt/atlas}  ^-  (unit sole-effect)
        ?.  |(=(~ pan) (~(has in pan) pon))  ~
        =-  `[%tan rose+[", " `~]^- leaf+~(ta-full ta man.she pon) ~]
        =<  (murn (sort (~(tap by alt)) aor) .)
        |=  {a/ship b/presence c/human}  ^-  (unit tank) :: XX names
        ?-  b
          $gone  ~
          $hear  `>a<
          $talk  `>a<      ::  XX difference
        ==
      :: 
      ++  bind                                          ::  %bind
        |=  {cha/char pan/(unit (set partner))}  ^+  ..sh-work
        ?~  pan  $(pan [~ u.active.she])
        =+  ole=(~(get by nik) u.pan)
        ?:  =(ole [~ cha])  ..sh-work
        (sh-note:(set-glyph cha u.pan) "bound {<cha>} {<u.pan>}")
      ::
      ++  invite                                        ::  %invite
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  block                                         ::  %block
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  author                                        ::  %author
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  banish                                        ::  %banish
        |=  {nom/knot tal/(list partner)}
        ^+  ..sh-work
        !!
      ::
      ++  create                                        ::  %create
        |=  {por/posture nom/knot txt/cord}
        ^+  ..sh-work
        ?:  (~(has in stories) nom) 
          (sh-lame "{(trip nom)}: already exists")
        =.  ..sh-work
            ::x  create new config for channel.
            %^  sh-tell  %design  nom
            :-  ~
            :+  *(set partner)
              (end 3 64 txt)
            [por ~]
        (join [[%& our.hid nom] ~ ~])
      ::
      ++  reverse-folks
        |=  nym/knot
        ^-  (list ship)
        %+  murn  (~(tap by folks))
        |=  {p/ship q/human}
        ?~  hand.q  ~
        ?.  =(u.hand.q nym)  ~
        [~ u=p]
      ::
      ++  nick                                          ::  %nick
        |=  {her/(unit ship) nym/(unit cord)}
        ^+  ..sh-work
        ?:  ?=({$~ $~} +<)
          %+  sh-fact  %mor
          %+  turn  (~(tap by folks))
          |=  {p/ship q/human}
          :-  %txt
          ?~  hand.q
            "{<p>}:"
          "{<p>}: {<u.hand.q>}"
        ?~  nym
          ?>  ?=(^ her)
          =+  asc=(~(get by folks) u.her)
          %+  sh-fact  %txt
          ?~  asc  "{<u.her>} unbound"
          ?~  hand.u.asc  "{<u.her>}:"
          "{<u.her>}: {<u.hand.u.asc>}"
        ?~  her
          %+  sh-fact  %mor
          %+  turn  (reverse-folks u.nym)
          |=  p/ship
          [%txt "{<p>}: {<u.nym>}"]
        %=  ..sh-work
          folks  ?~  u.nym
                   (~(del by folks) u.her)
                 (~(put by folks) u.her [true=~ hand=nym])
        ==
      ::
      ++  wo-set                                        ::  %set
        |=  seg/knot
        ^+  ..sh-work
        ?~  seg
          %+  sh-fact  %mor
          %+  turn  (~(tap in settings.she))
          |=  s/knot
          [%txt (trip s)]
        %=  ..sh-work
          settings.she  (~(put in settings.she) seg)
        ==
      ::
      ++  unset                                         ::  %unset
        |=  neg/knot
        ^+  ..sh-work
        %=  ..sh-work
          settings.she  (~(del in settings.she) neg)
        ==
      ::
      ++  target                                        ::  %target
        |=  {pan/(set partner) woe/(unit ^work)}
        ^+  ..sh-work
        =.  ..sh-pact  (sh-pact pan)
        ?~(woe ..sh-work work(job u.woe))
      ::
      ++  number                                        ::  %number
        |=  num/$@(@ud {p/@u q/@ud})
        ^+  ..sh-work
        =+  roy=(~(got by stories) man.she)
        |-
        ?@  num
          ?:  (gte num count.roy)
            (sh-lame "{(scow %s (new:si | +(num)))}: no such telegram")
          =.  ..sh-fact  (sh-fact %txt "? {(scow %s (new:si | +(num)))}")
          (activate (snag num grams.roy))
        ?.  (gth q.num count.roy)
          ?~  count.roy
            (sh-lame "0: no messages")
          =+  msg=(deli (dec count.roy) num)
          =.  ..sh-fact  (sh-fact %txt "? {(scow %ud msg)}")
          (activate (snag (sub count.roy +(msg)) grams.roy))
        (sh-lame "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram")
      ::
      ++  deli                                          ::  find number
        |=  {max/@ud nul/@u fin/@ud}  ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  probe                                         ::  inquire
        |=  cuz/station
        ^+  ..sh-work
        ~&  [%probe cuz]
        ..sh-work
      ::
      ++  eval                                          ::  run
        |=  {txt/cord exe/twig}
        =>  |.([(sell (slap (slop sh-twig-head seed) exe))]~)
        =+  tan=p:(mule .)
        (say [%fat tank+tan exp+txt] ~)
      ::
      ++  say                                           ::  publish
        |=  sep/(list speech)
        ^+  ..sh-work
        =-  ..sh-work(coz ?~(tot coz :_(coz [%publish tot])))
        |-  ^-  tot/(list thought)
        ?~  sep  ~
        =^  sir  ..sh-work  sh-uniq
        [[sir sh-whom [now.hid ~ i.sep]] $(sep t.sep)]
      --
    ::
    ++  sh-done                                         ::  apply result
      ::x  called upon hitting return in the prompt. if input is invalid,
      ::x  ++sh-slug is called. otherwise, the appropriate work is done
      ::x  and the entered command (if any) gets displayed to the user.
      ::
      =+  fix=(sh-sane [%nop ~] buf.say.she)
      ?^  lit.fix
        (sh-slug fix)
      =+  jub=(rust (tufa buf.say.she) sh-scad)
      ?~  jub  (sh-fact %bel ~)
      %.  u.jub
      =<  sh-work
      =+  buf=buf.say.she
      =^  cal  say.she  (~(transmit sole say.she) [%set ~])
      %-  sh-fact
      :*  %mor
          [%nex ~]
          [%det cal]
          ?.  ?=({$';' *} buf)  ~ 
          :_  ~
          [%txt (runt [14 '-'] `tape`['|' ' ' (tufa `(list @)`buf)])]  
      ==
    ::
    ++  sh-sole                                         ::  apply edit
      ::x  applies sole action.
      ::
      |=  act/sole-action
      ^+  +>
      ?-  -.act
        $det  (sh-stir +.act)
        $clr  ..sh-sole :: (sh-pact ~) :: XX clear to PM-to-self?
        $ret  sh-done
      ==
    ::
    ++  sh-uniq
      ::x  generates a new serial.
      ::
      ^-  {serial _.}
      [(shaf %serial eny.hid) .(eny.hid (shax eny.hid))]
    --
  ++  ra-abed                                           ::  resolve core
    ::x  produces the moves stored in ++ra's moves.
    ::x  sole-effects get special treatment to become a single move.
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
  ++  ra-abet                                           ::  complete core
    ::x  applies talk reports, then produces moves and updated state.
    ::
    ra-abed:ra-axel
  ::
  ++  ra-axel                                           ::  rebound reports
    ::x  extracts and applies the talk-reports in moves.
    ::
    ^+  .
    ::x  separate our talk-reports from other moves.
    =+  ^=  rey
        |-  ^-  (pair (list move) (list (pair bone report)))
        ?~  moves
          [~ ~]
        =+  mor=$(moves t.moves)
        ?.  ?&  (~(has by shells) `bone`p.i.moves)
                ?=({$diff $talk-report *} q.i.moves)
            ==
          [[i.moves p.mor] q.mor]
        [p.mor [[p.i.moves +>.q.i.moves] q.mor]]
    ::x  update moves to exclude talk-reports.
    =.  moves  p.rey
    =.  q.rey  (flop q.rey)
    ?:  =(q.rey ~)  +
    |-  ^+  +>
    ?~  q.rey  ra-axel
    ::x  apply reports.
    =+  bak=(ra-back(ost.hid p.i.q.rey) q.i.q.rey)
    $(q.rey t.q.rey, +> bak(ost.hid ost.hid))
  ::
  ++  ra-back
    ::x  applies report.
    ::
    |=  rad/report
    ^+  +>
    sh-abet:(~(sh-repo sh ~ (~(got by shells) ost.hid)) rad)
  ::
  ++  ra-sole
    ::x  applies sole-action.
    ::
    |=  act/sole-action
    ^+  +>
    =+  shu=(~(get by shells) ost.hid)
    ?~  shu
      ~|  :+  %ra-console-broken  ost.hid 
          ?:((~(has by sup.hid) ost.hid) %lost %unknown)
      !!
    sh-abet:(~(sh-sole sh ~ u.shu) act)
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
  ++  ra-house                                          ::  emit partners
    ::x  emits a talk-report move containing all our stories?
    ::x?  this is for showing people what they can subscribe to, right?
    ::x?  but this also shows invite-only stories, aren't they secret clubs?
    ::
    |=  ost/bone
    %+  ra-emit  ost.hid
    :+  %diff  %talk-report
    :-  %house
    %-  ~(gas in *(map knot (pair posture cord)))
    %+  turn  (~(tap by stories)) 
    |=({a/knot b/story} [a p.cordon.shape.b caption.shape.b])
  ::
  ++  ra-homes                                          ::  update partners
    ::x  send a list of our stories to all general subscribers.
    ::
    =+  gel=general
    |-  ^+  +>
    ?~  gel  +>
    =.  +>  $(gel l.gel)
    =.  +>  $(gel r.gel)
    (ra-house n.gel)
  ::
  ++  ra-init                                           ::  initialize talk
    ::x  populate state on first boot. creates our main and public stories.
    ::
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
      ::x?  shouldn't this be team-only too?
        $design
      ?.  =(her our.hid)
        (ra-evil %talk-no-owner)
      ?~  q.cod
        ?.  (~(has by stories) p.cod)
          (ra-evil %talk-no-story)
        ::x?  why delete story if we got no config? can't we overwrite?
        (ra-config(stories (~(del by stories) p.cod)) p.cod *config)
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
  ++  ra-config                                         ::  configure story
    ::x  (re)configures story man. if it's a new story, emit our stories.
    ::
    |=  {man/knot con/config}
    ^+  +>
    =+  :-  neu=(~(has by stories) man)
        pur=(fall (~(get by stories) man) *story)
    =.  +>.$  pa-abet:(~(pa-reform pa man pur) con)
    ?:(neu +>.$ ra-homes)
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
    (fun ~(. pa man u.pur))
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
    %-  (ra-know i.t.pax)  |=  par/_pa  =<  pa-abet
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
  ++  ra-console                                        ::  console subscribe
    ::x  make a shell for her, subscribe her to it.
    ::
    |=  {her/ship pax/path}
    ^+  +>
    ::x  get story from the path, default to standard mailbox.
    =/  man/knot
      ?+  pax  !!
        $~        (main her)
        {@ta $~}  i.pax
      ==
    =/  she/shell
      %*(. *shell her her, man man, active `(sy [%& our.hid man] ~))
    sh-abet:~(sh-peer sh ~ `shell`she)
  ::
  ++  ra-subscribe                                      ::  listen to
    ::x  subscribe her at pax.
    ::
    |=  {her/ship pax/path}
    ^+  +>
    ::  ~&  [%ra-subscribe ost.hid her pax]
    ::x  empty path, meta-subscribe and send report with all our stories.
    ?:  ?=($~ pax)
      (ra-house(general (~(put in general) ost.hid)) ost.hid)
    ?.  ?=({@ @ *} pax)
      (ra-evil %talk-bad-path)
    =+  vab=(~(gas in *(set peer-type)) (turn (rip 3 i.pax) decode:peer-type))
    =+  pur=(~(get by stories) i.t.pax)
    ?~  pur
      ~&  [%bad-subscribe-story-c i.t.pax]
      (ra-evil %talk-no-story)
    =+  soy=~(. pa i.t.pax u.pur)
    ::x  check her read permissions.
    ?.  (pa-visible:soy her)
      (ra-evil %talk-no-story)
    =^  who  +>.$  (ra-human her)
    ::x  for each stream type she is interested in, add her to the followers.
    =.  soy  ?.((~(has in vab) %a-group) soy (pa-watch-group:soy her))
    =.  soy  ?.((~(has in vab) %v-glyph) soy (pa-watch-glyph:soy her))
    =.  soy  ?.((~(has in vab) %x-cabal) soy (pa-watch-cabal:soy her))
    =.  soy  ?.((~(has in vab) %f-grams) soy (pa-watch-grams:soy her t.t.pax))
    ::x  add her status to presence map.
    =.  soy  (pa-notify:soy her %hear who)
    ::x  apply changes to story.
    pa-abet:soy
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
    ::  ~&  [%ra-conduct pub her tay]
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
            [p.cuz %talk]
            [%talk-command `command`[%review tip ~]]
        ==
    +>(p.outbox +(p.outbox), q.outbox (~(put by q.outbox) p.outbox tip))
  ::
  ++  pa                                                ::  story core
    ::x  story core, used for doing work on a story.
    ::x  as always, an -abet arms is used for applying changes to the state.
    ::x  ++pa-watch- arms get called by ++ra-subscribe to add a subscriber.
    ::x  bones are used to identify subscribers (source event identifiers)
    ::
    |_  ::x  man: the knot identifying the story in stories.
        ::x  story doesn't get a face because ease of use
        ::
        $:  man/knot
            story
        ==
    ++  pa-abet
      ::x  apply/fold changes back into the stories map.
      ::
      ^+  +>
      +>(stories (~(put by stories) man `story`+<+))
    ::
    ++  pa-admire                                       ::  accept from
      ::x  should be checking her write permissions, but defaults to allowed.
      ::x  commented code seems to use an older control structure.
      ::x?  this seems like an easy fix, why was this ever disabled?
      ::
      |=  her/ship
      ^-  ?
      ::?-  -.cordon.shape
      ::  %&  (~(has in p.cordon.shape) her)
      ::  %|  !(~(has in p.cordon.shape) her)
      ::==
      &
    ::
    ++  pa-visible                                      ::  display to
      ::x  checks her read permissions.
      ::
      |=  her/ship
      ^-  ?
      ?-  p.cordon.shape
        $black  &                                       ::x  channel, all
        $green  &                                       ::x  journal, all
        $brown  (team our.hid her)                      ::x  mailbox, our
        $white  (~(has in q.cordon.shape) her)          ::x  village, invite
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
    ++  pa-watch-group                                  ::  subscribe presence
      ::x  if she may, add her bone to presence followers and send her a group
      ::x  (presence) report.
      ::
      |=  her/ship
      ?.  (pa-admire her)
        (pa-sauce ost.hid [%quit ~]~)
      =.  groupers  (~(put in groupers) ost.hid)
      (pa-report-group ost.hid ~ ~)
    ::
    ++  pa-watch-cabal                                  ::  subscribe config
      ::x  if she may, add her bone to config followers and send her an updated
      ::x  cabal (config) report.
      ::
      |=  her/ship
      ?.  (pa-admire her)
        ~&  [%pa-admire-not her]
        (pa-sauce ost.hid [%quit ~]~)
      =.  cabalers  (~(put in cabalers) ost.hid)
      ::  ~&  [%pa-watch-cabal her man shape]
      (pa-sauce ost.hid [[%diff %talk-report %cabal shape mirrors] ~])
    ::
    ++  pa-watch-glyph                                  ::  subscribe config
      ::x  if she may, add her bone to glyph followers and send an updated glyph
      ::x  report.
      ::
      |=  her/ship
      ?.  (pa-admire her)
        ~&  [%pa-admire-not her]
        (pa-sauce ost.hid [%quit ~]~)
      =.  glyphers  (~(put in glyphers) ost.hid)
      (pa-report [ost.hid ~ ~] %glyph nak)
    ::
    ++  pa-report-group                                  ::  update presence
      ::x  build a group report, containing our different presence maps, and
      ::x  send it to all bones.
      ::x?  why should we be responsible for sending remotes presences?
      ::
      |=  vew/(set bone)
      %^  pa-report  vew  %group
      :-  %-  ~(run by locals)
          |=({@ a/status} a)
      %-  ~(urn by remotes)           ::  XX preformance
      |=  {pan/partner atl/atlas}  ^-  atlas
      ?.  &(?=($& -.pan) =(our.hid p.p.pan))  atl
      =+  (~(get by stories) q.p.pan)
      ?~  -  atl
      %-  ~(run by locals.u)
      |=({@ a/status} a)
    ::
    ++  pa-report-cabal                                 ::  update config
      ::x  a cabal report, containing our and remote configs, to all config
      ::x  followers.
      ::
      (pa-report cabalers %cabal shape mirrors)
    ::
    ++  pa-cabal
      ::x  add station's config to our remote config map.
      ::x?  ham is unused, not even when calling this.
      ::
      |=  {cuz/station con/config ham/(map station config)}
      ^+  +>
      =+  old=mirrors
      =.  mirrors  (~(put by mirrors) cuz con)
      ?:  =(mirrors old)
        +>.$
      pa-report-cabal 
    ::
    ++  pa-diff-talk-report                             ::  subscribed update
      ::x  process a talk report from cuz.
      ::
      |=  {cuz/station rad/report}
      ^+  +>
      ::x  verify we are supposed to receive reports from cuz.
      ?.  (~(has in sources.shape) [%& cuz])
        ~&  [%pa-diff-unexpected cuz rad]
        +>
      ?+  -.rad  ~|([%talk-odd-friend rad] !!)
        $cabal  (pa-cabal cuz +.rad)
        $group  (pa-remind [%& cuz] +.rad)
        $grams  (pa-lesson q.+.rad)
      ==
    ::
    ++  pa-quit                                         ::  stop subscription
      ::x  delete tay from our subscriptions, then send an updated capal report.
      ::
      |=  tay/partner
      pa-report-cabal(sources.shape (~(del in sources.shape) tay))
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
      %+  pa-sauce  0  ::x  why bone 0?
      %-  zing
      %+  turn  tal
      |=  tay/partner
      ^-  (list card)
      ?-  -.tay
        $|  ~&  tweet-abjure+p.p.tay
            !!
      ::
        $&  ~&  [%pa-abjure [our.hid man] [p.p.tay q.p.tay]]
            :_  ~
            :*  %pull
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk]
                ~
            ==
      ==
    ::
    ++  pa-acquire                                      ::  subscribe to
      ::x  for each partner, produce a %peer/subscribe move.
      ::
      |=  tal/(list partner)
      %+  pa-sauce  0
      %-  zing
      %+  turn  tal
      |=  tay/partner
      ^-  (list card)
      =+  num=(~(get by sequence) tay)
      =+  old=(sub now.hid ~d1)                         :: XX full backlog
      ::x  subscribe starting at the last message we read,
      ::x  or if we haven't read any yet, messages from up to a day ago.
      =+  ini=?^(num (scot %ud u.num) (scot %da old))
      =/  typ
        =+  (ly ~[%a-group %f-grams %x-cabal])
        (rap 3 (turn - encode:peer-type))
      ?-  -.tay
        $|  !!
        $&  ::  ~&  [%pa-acquire [our.hid man] [p.p.tay q.p.tay]]
            :_  ~
            :*  %peer
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk] 
                /[typ]/[q.p.tay]/[ini]
            ==
      ==
    ::
    ++  pa-reform                                       ::  reconfigure, ugly
      ::x  change config of current story, subscribe/unsubscribe to/from the
      ::x  partners we gained/lost, and send out an updated cabal report.
      ::
      |=  cof/config
      =+  ^=  dif  ^-  (pair (list partner) (list partner))
          =+  old=`(list partner)`(~(tap in sources.shape) ~)
          =+  new=`(list partner)`(~(tap in sources.cof) ~)
          :-  (skip new |=(a/partner (~(has in sources.shape) a)))
          (skip old |=(a/partner (~(has in sources.cof) a)))
      =.  +>.$  (pa-acquire p.dif)
      =.  +>.$  (pa-abjure q.dif)
      =.  shape  cof
      pa-report-cabal
    ::
    ++  pa-cancel                                       ::  unsubscribe from
      ::x  deletes the current ost.hid from all follower groups.
      ::
      ::  ~&  [%pa-cancel ost.hid]
      %_  .
        gramsers  (~(del by gramsers) ost.hid)
        groupers  (~(del in groupers) ost.hid)
        glyphers  (~(del in glyphers) ost.hid)
        cabalers  (~(del in cabalers) ost.hid)
      ==
    ::
    ++  pa-notify                                       ::  local presence
      ::x  add her status to our presence map. if this changes it, send report.
      ::
      |=  {her/ship saz/status}
      ^+  +>
      =/  nol  (~(put by locals) her now.hid saz)
      ?:  =(nol locals)  +>.$
      (pa-report-group(locals nol) groupers)
    ::
    ++  pa-remind                                       ::  remote presence
      ::x  adds tay's loc to our remote presence map, after merging with rem.
      ::x  if this changes anything, send update report.
      ::
      |=  {tay/partner loc/atlas rem/(map partner atlas)}
      ::x  remove this story from the presence map, since it's in local already.
      =.  rem  (~(del by rem) %& our.hid man)  :: superceded by local data
      =/  buk  (~(uni timed remotes) rem)  ::  XX drop?
      =.  buk  (~(put timed buk) tay now.hid loc)
      ?:  =(~(strip timed buk) ~(strip timed remotes))  +>.$
      (pa-report-group(remotes buk) groupers)
    ::
    ++  pa-start                                        ::  start stream
      ::x  grab all telegrams that fall within the river and send them in a
      ::x  grams report to ost.hid.
      ::
      |=  riv/river
      ^+  +>
      =-  ::  ~&  [%pa-start riv lab]
          =.  +>.$
          (pa-sauce ost.hid [[%diff %talk-report %grams q.lab r.lab] ~])
          ?:  p.lab  ::x?  dun never gets changed, so always | ?
            (pa-sauce ost.hid [[%quit ~] ~])
          +>.$(gramsers (~(put by gramsers) ost.hid riv))
      ^=  lab
      =+  [end=count gaz=grams dun=| zeg=*(list telegram)]
      |-  ^-  (trel ? @ud (list telegram))
      ?~  gaz  [dun end zeg]
      ?:  ?-  -.q.riv                                   ::  after the end
            $ud  (lte p.q.riv end)
            $da  (lte p.q.riv p.r.q.i.gaz)
          ==
        ::x  if we're past the river, continue browsing back.
        $(end (dec end), gaz t.gaz)
      ?:  ?-  -.p.riv                                   ::  before the start
            $ud  (lth end p.p.riv)
            $da  (lth p.r.q.i.gaz p.p.riv)
          ==
        ::x  if we're before the river, we're done.
        [dun end zeg]
      ::x  if we're in the river, add this gram and continue.
      $(end (dec end), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ++  pa-watch-grams                                  ::  subscribe messages
      ::x  (called upon subscribe) send backlog of grams to her.
      ::x  deduces which messages to send from pax.
      ::
      |=  {her/ship pax/path}
      ^+  +>
      ?.  (pa-admire her)
        ~&  [%pa-watch-grams-admire ~]
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
          ::x?  the switches, they do nothing!
          `[[?+(- . $ud .)]:i.paf [?+(- . $ud .)]:i.t.paf]  ::  XX types
      ::  ~&  [%pa-watch-grams her pax ruv]
      ?~  ruv
        ~&  [%pa-watch-grams-malformed pax]
        (pa-sauce ost.hid [%quit ~]~)
      (pa-start u.ruv)
    ::
    ++  pa-refresh                                      ::  update to listeners
      ::x  called when grams get added or changed. calculates the changes and
      ::x  sends them to all message followers. if we run into any followers
      ::x  that are no longer interested in this story, remove them.
      ::
      |=  {num/@ud gam/telegram}
      ^+  +>
      =+  ^=  moy
          |-  ^-  (pair (list bone) (list move))
          ?~  gramsers  [~ ~]
          ::  ~&  [%pa-refresh num n.gramsers]
          =+  lef=$(gramsers l.gramsers)
          =+  rit=$(gramsers r.gramsers)
          =+  old=[p=(welp p.lef p.rit) q=(welp q.lef q.rit)]
          ?:  ?-  -.q.q.n.gramsers                        ::  after the end
                $ud  (lte p.q.q.n.gramsers num)
                $da  (lte p.q.q.n.gramsers p.r.q.gam)
              ==
            [[p.n.gramsers p.old] [[p.n.gramsers %quit ~] q.old]]
          ?:  ?-  -.p.q.n.gramsers                        ::  before the start
                $ud  (gth p.p.q.n.gramsers num)
                $da  (gth p.p.q.n.gramsers p.r.q.gam)
              ==
            old
          :-  p.old
          [[p.n.gramsers %diff %talk-report %grams num gam ~] q.old]
      =.  moves  (welp q.moy moves)
      |-  ^+  +>.^$
      ?~  p.moy  +>.^$
      $(p.moy t.p.moy, gramsers (~(del by gramsers) i.p.moy))
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
        ~&  %pa-admire-rejected
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
    --
  --
::
++  sn                                                  ::  station render core
  ::x  used in both station and ship rendering.
  ::
  ::x  man: mailbox.
  ::x  one: the station.
  |_  {man/knot one/station} 
  ++  sn-best                                           ::  best to show
    ::x  returns true if one is better to show, false otherwise.
    ::x  prioritizes: our > main > size.
    ::
    |=  two/station
    ^-  ?
    ::x  the station that's ours is better.
    ?:  =(our.hid p.one)
      ?:  =(our.hid p.two)
        ?<  =(q.one q.two)
        ::x  if both stations are ours, the main story is better.
        ?:  =((main p.one) q.one)  %&
        ?:  =((main p.two) q.two)  %|
        ::x  if neither are, pick the "larger" one.
        (lth q.one q.two)
      %&
    ::x  if one isn't ours but two is, two is better.
    ?:  =(our.hid p.two)
      %|
    ?:  =(p.one p.two)
      ::x  if they're from the same ship, pick the "larger" one.
      (lth q.one q.two)
    ::x  when in doubt, pick one if its ship is "smaller" than its channel.
    ::x?  i guess you want this to be consistent across (a b) and (b a), but
    ::x  this still seems pretty arbitrary.
    (lth p.one q.one)
  ::
  ++  sn-curt                                           ::  render name in 14
    ::x  prints a ship name in 14 characters. left-pads with spaces.
    ::x?  mup is unused, what is it even for? ++ta-show still uses it.
    ::
    |=  mup/?
    ^-  tape
    =+  raw=(cite p.one)
    (runt [(sub 14 (lent raw)) ' '] raw)
  ::
  ++  sn-nick
    ::x  get nick for ship, or shortname if no nick. left-pads with spaces.
    ::
    |.  ^-  tape
    =+  nym=(~(get by folks) p.one)
    ?~  nym
      (sn-curt |)
    ?~  hand.u.nym
      (sn-curt |)
    =+  raw=(trip u.hand.u.nym)
    =+  len=(sub 14 (lent raw))
    (weld (reap len ' ') raw)
  ::
  ++  sn-phat                                           ::  render accurately
    ::x  prints a station fully, but still taking "shortcuts" where possible:
    ::x  ":" for local mailbox, "~ship" for foreign mailbox,
    ::x  "%channel" for local station, "/channel" for parent station.
    ::
    ^-  tape
    ?:  =(p.one our.hid)
      ?:  =(q.one man)
        ":"
      ['%' (trip q.one)]
    ?:  =(p.one (sein our.hid))
      ['/' (trip q.one)]
    =+  wun=(scow %p p.one)
    ?:  =(q.one (main p.one))
      wun
    :(welp wun "/" (trip q.one))
  --
::
++  ta                                                  ::  partner core
  ::x  used primarily for printing partners.
  ::
  ::x  man: mailbox.
  ::x  one: the partner.
  |_  {man/knot one/partner}
  ++  ta-beat                                           ::  more relevant
    ::x  returns true if one is better to show, false otherwise.
    ::x  prefers stations over passports. if both are stations, sn-best. if both
    ::x  are passports, pick the "larger" one, if they're equal, content hash.
    ::
    |=  two/partner  ^-  ?
    ?-    -.one
        $&
      ?-  -.two
        $|  %&
        $&  (~(sn-best sn man p.one) p.two)
      ==
    ::
        $|
      ?-  -.two
        $&  %|
        $|  ?:  =(-.p.two -.p.one)
              (lth (mug +.p.one) (mug +.p.two))
            (lth -.p.two -.p.one)
      ==
    ==
  ++  ta-best                                           ::  most relevant
    ::x  picks the most relevant partner.
    ::
    |=(two/partner ?:((ta-beat two) two one))
  ::
  ++  ta-full  (ta-show ~)                              ::  render full width
  ++  ta-show                                           ::  render partner
    ::x  renders a partner as text.
    ::
    |=  moy/(unit ?)
    ^-  tape
    ?-    -.one
    ::x render station as glyph if we can.
        $&
      ?~  moy
        =+  cha=(~(get by nik) one ~ ~)
        =-  ?~(cha - "'{u.cha ~}' {-}")
        ~(sn-phat sn man p.one)
      (~(sn-curt sn man p.one) u.moy)
    ::
    ::x  render passport.
        $|
      =+  ^=  pre  ^-  tape
          ?-  -.p.one
            $twitter  "@t:"
          ==
      ?~  moy
        (weld pre (trip p.p.one))
      =.  pre  ?.(=(& u.moy) pre ['*' pre])
      (sigh 14 pre p.p.one)
    ==
  --
::
++  te                                                  ::  audience renderer
  ::x  used for representing audiences (sets of partners) as tapes.
  ::
  ::  man: mailbox.
  ::  lix: members of the audience.
  |_  {man/knot lix/(set partner)}
  ++  te-best  ^-  (unit partner)
    ::x  pick the most relevant partner.
    ::
    ?~  lix  ~
    :-  ~
    |-  ^-  partner
    =+  lef=`(unit partner)`te-best(lix l.lix)
    =+  rit=`(unit partner)`te-best(lix r.lix)
    =.  n.lix  ?~(lef n.lix (~(ta-best ta man n.lix) u.lef))
    =.  n.lix  ?~(rit n.lix (~(ta-best ta man n.lix) u.rit))
    n.lix
  ::
  ++  te-deaf  ^+  .                                    ::  except for self
    ::x  remove ourselves from the audience.
    ::
    .(lix (~(del in lix) `partner`[%& our.hid man]))
  ::
  ++  te-maud  ^-  ?                                    ::  multiple audience
    ::x  checks if there's multiple partners in the audience via pattern match.
    ::
    =.  .  te-deaf
    !?=($@($~ {* $~ $~}) lix)
  ::
  ++  te-prom  ^-  tape                                 ::  render targets
    ::x  render all partners, ordered by relevance.
    ::
    =.  .  te-deaf
    =+  ^=  all
        %+  sort  `(list partner)`(~(tap in lix))
        |=  {a/partner b/partner}
        (~(ta-beat ta man a) b)
    =+  fir=&
    |-  ^-  tape
    ?~  all  ~
    ;:  welp
      ?:(fir "" " ")
      (~(ta-show ta man i.all) ~)
      $(all t.all, fir |)
    ==
  ::
  ++  te-whom                                           ::  render sender
    ::x  render sender as the most relevant partner.
    ::
    (~(ta-show ta man (need te-best)) ~ te-maud)
  ::
  ++  ta-dire                                           ::  direct message
    ::x  returns true if partner is a mailbox of ours.
    ::
    |=  pan/partner  ^-  ?
    ?&  ?=($& -.pan)
        =(p.p.pan our.hid)
    ::
        =+  sot=(~(get by stories) q.p.pan)
        &(?=(^ sot) ?=($brown p.cordon.shape.u.sot))
    ==
  ::
  ++  te-pref                                           ::  audience glyph
    ::x  get the glyph that corresponds to the audience, with a space appended.
    ::x  if it's a dm to us, use :. if it's a dm by us, use ;. complex, use *.
    ::
    ^-  tape
    =+  cha=(~(get by nik) lix)
    ?^  cha  ~[u.cha ' ']
    ?.  (lien (~(tap by lix)) ta-dire)
      "* "
    ?:  ?=({{$& ^} $~ $~} lix)
      ": "
    "; "
  --
::
++  tr                                                  ::  telegram renderer
  ::x  responsible for converting telegrams and everything relating to them to
  ::x  text to be displayed in the cli.
  ::
  |_  $:  ::x  man: story.
          ::x  sef: settings flags.
          ::x  telegram:
          ::x   who: author.
          ::x   thought:
          ::x    sen: unique identifier.
          ::x    aud: audience.
          ::x    statement:
          ::x     wen: timestamp.
          ::x     bou: complete aroma.
          ::x     sep: message contents.
          ::
          man/knot
          sef/(set knot)
          who/ship
          sen/serial
          aud/audience
          wen/@da
          bou/bouquet
          sep/speech
      ==
  ++  tr-fact  ^-  sole-effect                          ::  activate effect
    ::x  produce sole-effect for printing message details.
    ::
    ~[%mor [%tan tr-meta] tr-body]
  ::
  ++  tr-line  ^-  tape                                 ::  one-line print
    ::x  crams a telegram into a single line by displaying a short ship name,
    ::x  a short representation of the gram, and an optional timestamp.
    ::
    =+  txt=(tr-text =(who our.hid))
    ?:  =(~ txt)  ""
    =+  ^=  baw
        ::  ?:  oug 
        ::  ~(te-whom te man tr-pals)
        ?.  (~(has in sef) %noob)
          (~(sn-curt sn man [who (main who)]) |)
        (~(sn-nick sn man [who (main who)]))
    ?:  (~(has in sef) %showtime)
      =+  dat=(yore now.hid)
      =+  ^=  t
        |=  a/@  ^-  tape
        %+  weld
          ?:  (lth a 10)  "0"  ~
          (scow %ud a)
      =+  ^=  time  :(weld "~" (t h.t.dat) "." (t m.t.dat) "." (t s.t.dat))
      :(weld baw txt (reap (sub 67 (lent txt)) ' ') time)
    (weld baw txt)
  ::
  ++  tr-meta  ^-  tang
    ::x  build strings that display metadata, including message serial,
    ::x  timestamp, author and audience.
    ::
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))     :: round
    =+  hed=leaf+"{(scow %uv sen)} at {(scow %da wen)}"
    =+  =<  paz=(turn (~(tap by aud)) .)
        |=({a/partner *} leaf+~(ta-full ta man a))
    =+  bok=(turn (sort (~(tap in bou)) aor) smyt)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] paz] bok]]~
  ::
  ++  tr-body
    ::x  long-form display of message contents, specific to each speech type.
    ::
    |-  ^-  sole-effect
    ?+  -.sep  tan+[>sep<]~
      $exp  tan+~[leaf+"# {(trip p.sep)}"]
      $lin  tan+~[leaf+"{?:(p.sep "" "@ ")}{(trip q.sep)}"]
      $non  tan+~
      $app  tan+~[rose+[": " ~ ~]^~[leaf+"[{(trip p.sep)}]" leaf+(trip q.sep)]]
      $url  url+(crip (earf p.sep))
      $mor  mor+(turn p.sep |=(speech ^$(sep +<)))
      $fat  [%mor $(sep q.sep) tan+(tr-rend-tors p.sep) ~]
      $api
        :-  %tan
        :_  ~
        :+  %rose
          [": " ~ ~]
        :~  leaf+"[{(trip id.sep)} on {(trip service.sep)}]"
            leaf+(trip body.sep)
            leaf+(earf url.sep)
        ==
    ==
  ::
  ++  tr-rend-tors
    ::x  render an attachment.
    ::
    |=  a/torso  ^-  tang
    ?-  -.a
      $name  (welp $(a q.a) leaf+"={(trip p.a)}" ~)
      $tank  +.a
      $text  (turn (flop +.a) |=(b/cord leaf+(trip b)))
    ==
  ::
  ++  tr-pals
    ::x  strip delivery info from audience, producing a set of partners.
    ::
    ^-  (set partner)
    %-  ~(gas in *(set partner))
    (turn (~(tap by aud)) |=({a/partner *} a))
  ::
  ++  tr-chow
    ::x  truncate the txt to be of max len characters. if it does truncate,
    ::x  indicates it did so by appending a character.
    ::
    |=  {len/@u txt/tape}  ^-  tape
    ?:  (gth len (lent txt))  txt
    =.  txt  (scag len txt)
    |-
    ?~  txt  txt
    ?:  =(' ' i.txt)
      |-(['_' ?.(?=({$' ' *} t.txt) t.txt $(txt t.txt))])
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  ::
  ++  tr-both
    ::x  try to fit two tapes into a single line.
    ::
    |=  {a/tape b/tape}  ^-  tape
    ?:  (gth (lent a) 62)  (tr-chow 64 a)
    %+  weld  a
    (tr-chow (sub 64 (lent a)) "  {b}")
  ::
  ++  tr-text
    ::x  gets a tape representation of a message that fits within a single line.
    ::
    |=  oug/?
    ^-  tape
    ?+    -.sep  ~&(tr-lost+sep "")
        $mor
      ?~  p.sep  ~&(%tr-mor-empty "")
      |-  ^-  tape
      ?~  t.p.sep  ^$(sep i.p.sep)
      (tr-both ^$(sep i.p.sep) $(p.sep t.p.sep))
    ::
        $fat
      %+  tr-both  $(sep q.sep)
      ?+  -.p.sep  "..."
        $tank  ~(ram re %rose [" " `~] +.p.sep)
      ==
    ::
        $exp  (tr-chow 66 '#' ' ' (trip p.sep))
        $url  =+  ful=(earf p.sep)
              ?:  (gth 64 (lent ful))  ['/' ' ' ful]
              :+  '/'  '_' 
              =+  hok=r.p.p.p.sep
              ~!  hok
              =-  (swag [a=(sub (max 64 (lent -)) 64) b=64] -)
              ^-  tape
              =<  ?:(?=($& -.hok) (reel p.hok .) +:(scow %if p.hok))
              |=({a/knot b/tape} ?~(b (trip a) (welp b '.' (trip a))))
    ::
        $lin
      =+  txt=(trip q.sep)
      ?:  p.sep
        =+  pal=tr-pals
        =.  pal  ?:  =(who our.hid)  pal
                 (~(del in pal) [%& who (main who)])
        (weld ~(te-pref te man pal) txt)
      (weld " " txt)
    ::
        $app
      (tr-chow 64 "[{(trip p.sep)}]: {(trip q.sep)}")
    ::
        $api
      (tr-chow 64 "[{(trip id.sep)}@{(trip service.sep)}]: {(trip summary.sep)}")
    ==
  -- 
::
++  peer                                                ::  accept subscription
  ::x  incoming subscription on pax.
  ::
  |=  pax/path
  ^+  [*(list move) +>]
  ~?  !=(src.hid our.hid)  [%peer-talk-stranger src.hid]
  :: ~&   [%talk-peer src.hid ost.hid pax]
  ?:  ?=({$sole *} pax)
    ?>  (team our.hid src.hid)
    ~?  (~(has by shells) ost.hid)  [%talk-peer-replaced ost.hid pax]
    ra-abet:(ra-console:ra src.hid t.pax)
  ::  ~&  [%talk-peer-data ost.hid src.hid pax]
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
++  poke-sole-action                                    ::  accept console
  ::x  incoming sole action. process it.
  ::
  |=  act/sole-action
  ra-abet:(ra-sole:ra act)
::
++  diff-talk-report                                    ::
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
  ::x  unsubscribe. remove from story and shells.
  ::
  |=  pax/path
  ^+  [*(list move) +>]
  ::  ~&  [%talk-pull src.hid ost.hid pax]
  =^  moz  +>.$  ra-abet:(ra-cancel:ra src.hid pax)
  [moz +>.$(shells (~(del by shells) ost.hid))]
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
  |=  old/(unit house-any)
  ^-  (quip move ..prep)
  ?~  old
    ra-abet:ra-init:ra
  |-
  ?-  -.u.old
    $6  [~ ..prep(+<+ u.old)]
    $5  =<  ^$(-.u.old %6, shells.u.old (~(run by shells.u.old) .))
        |=  shell-5  ^-  shell
        +<(passive %passive-deprecated, active ?^(active active `passive))
    $4  =<  ^$(-.u.old %5, shells.u.old (~(run by shells.u.old) .))
        |=(shell-4 `shell-5`+<(system [system settings=*(set knot)]))
    $3  =<  ^$(-.u.old %4, stories.u.old (~(run by stories.u.old) .))
        |=(story-3 `story`+<(cabalers [cabalers glyphers=*(set bone)]))
  ==
--
