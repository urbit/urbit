::                                                      ::  ::
::::  /hoon/talk/app                                    ::  ::
  ::                                                    ::  ::   
/?    314
/-    talk, sole
/+    talk, sole
/=    seed    /~    !>(.)
/=    talk-doc    
      /;  |=(a=wain (turn a |=(b=cord [%txt "? {(trip b)}"])))
      /:  /===/pub/doc/talk/help  /txt/
::
::::
  ::
[. talk sole]
=>  |%                                                  ::  data structures
    ++  house  ,[%3 house-3]                            ::  full state
    ++  house-any                                       ::  app history
      $%  [%1 house-1]                                  ::  1: talk
          [%2 house-2]                                  ::  2: talk
          [%3 house-3]                                  ::  3: talk
      ==                                                ::
    ++  house-1                                         ::
      $:  stories=(map span story)                      ::  conversations
          general=(set bone)                            ::  meta-subscribe
          outbox=(pair ,@ud (map ,@ud thought))         ::  urbit outbox
          folks=(map ship human)                        ::  human identities
          shells=(map bone shell)                       ::  interaction state
      ==                                                ::
    ++  house-2                                         ::
      $:  stories=(map span story)                      ::  conversations
          general=(set bone)                            ::  meta-subscribe
          outbox=(pair ,@ud (map ,@ud thought))         ::  urbit outbox
          folks=(map ship human)                        ::  human identities
          shells=(map bone shell)                       ::  interaction state
          log=(map span ,@ud)                           ::  logged to clay
      ==                                                ::
    ++  house-3                                         ::
      $:  stories=(map span story)                      ::  conversations
          general=(set bone)                            ::  meta-subscribe
          outbox=(pair ,@ud (map ,@ud thought))         ::  urbit outbox
          folks=(map ship human)                        ::  human identities
          shells=(map bone shell)                       ::  interaction state
          log=(map span ,@ud)                           ::  logged to clay
          nik=(map (set partner) char)                  ::
          nak=(jug char (set partner))                  ::
      ==                                                ::
    ++  story                                           ::  wire content
      $:  count=@ud                                     ::  (lent grams)
          grams=(list telegram)                         ::  all history
          locals=(map ship (pair ,@da status))          ::  local presence
          remotes=(map partner atlas)                   ::  remote presence
          mirrors=(map station config)                  ::  remote config
          sequence=(map partner ,@ud)                   ::  partners heard
          shape=config                                  ::  configuration
          known=(map serial ,@ud)                       ::  messages heard
          guests=(map bone river)                       ::  message followers
          viewers=(set bone)                            ::  presence followers
          owners=(set bone)                             ::  config followers
      ==                                                ::
    ++  shell                                           ::  console session
      $:  her=ship                                      ::  client identity
          man=span                                      ::  mailbox
          count=@ud                                     ::  messages shown
          say=sole-share                                ::  console state
          active=(unit (set partner))                   ::  active targets
          passive=(set partner)                         ::  passive targets
          guests=register                               ::  presence mirror
          harbor=(map span (pair posture cord))         ::  stations mirror
          system=cabal                                  ::  config mirror
      ==                                                ::
    ++  river  (pair point point)                       ::  stream definition
    ++  point                                           ::  stream endpoint
      $%  [%ud p=@ud]                                   ::  by number
          [%da p=@da]                                   ::  by date
      ==                                                ::
    ++  move  (pair bone card)                          ::  all actions
    ++  lime                                            ::  diff fruit
      $%  [%talk-report report]                         ::
          [%sole-effect sole-effect]                    ::
      ==                                                ::
    ++  pear                                            ::  poke fruit
      $%  [%talk-command command]                       ::
      ==                                                ::
    ++  card                                            ::  general card
      $%  [%diff lime]                                  ::
          [%info wire @p @tas nori]                     ::
          [%peer wire dock path]                        ::
          [%poke wire dock pear]                        ::
          [%pull wire dock ~]                           ::
          [%pass wire note]                             ::
          [%quit ~]                                     ::
      ==                                                ::
    ++  weir                                            ::  parsed wire
      $%  [%repeat p=@ud q=@p r=span]                   ::
          [%friend p=span q=station]                    ::
      ==                                                ::
    ++  work                                            ::  interface action
      $%  [%number p=$|(@ud [@u @ud])]                  ::  relative/absolute
          [%help ~]                                     ::  print usage info
          [%who p=?((set partner) char)]                ::  presence
          [%what p=$|(char (set partner))]              ::  show bound glyph
          [%bind p=char q=(unit (set partner))]         ::
          [%join p=(set partner)]                       ::  
          [%say p=(list speech)]                        ::
          [%eval p=cord q=twig]                         ::
          [%invite p=span q=(list partner)]             ::  whitelist add
          [%banish p=span q=(list partner)]             ::  blacklist add
          [%block p=span q=(list partner)]              ::  blacklist add
          [%author p=span q=(list partner)]             ::  whitelist add
          [%nick p=(unit ship) q=(unit cord)]           ::
          [%target p=where q=(unit work)]               ::  set active targets
          ::  [%destroy p=span]                         ::
          [%create p=posture q=span r=cord]             ::
          [%probe p=station]                            ::
      ==                                                ::
    ++  where  ?((set partner) char)                    ::  audience/shorthand
    ++  sigh                                            ::  assemble label
      |=  [len=@ud pre=tape yiz=cord]
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
    ++  glyphs  `wall`~[">=+-" "}),." "\"'`^" "$%&@"]     :: station char pool
    --
|_  [hid=bowl house]
++  ra                                                  ::  per transaction
  |_  moves=(list move)
  ++  sh                                                ::  per console
    |_  $:  coz=(list command)                          ::  talk actions
            she=shell
        ==
    ++  sh-scad                                         ::  command parser
      =<  work
      |%
      ++  expr                                          ::  [cord twig]
        |=  tub=nail  %.  tub
        %+  stag  (crip q.tub)
        wide:(vang & [&1:% &2:% (scot %da now.hid) |3:%])
      ::
      ++  dare                                          ::  @dr
        %+  sear
          |=  a=coin
          ?.  ?=([%$ %dr @] a)  ~
          (some `@dr`+>.a)
        nuck:so
      ::
      ++  ship  ;~(pfix sig fed:ag)                     ::  ship
      ++  shiz                                          ::  ship set
        %+  cook
          |=(a=(list ^ship) (~(gas in *(set ^ship)) a))
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
          (cold [our.hid man.she] tis)
          ;~(pfix cen (stag our.hid sym))
          ;~(pfix fas (stag (sein our.hid) sym))
        ::
          %+  cook
            |=  [a=@p b=(unit term)]
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
      ++  parz                                          ::  non-empty partners
        %+  cook  ~(gas in *(set partner))
        (most ;~(plug com (star ace)) parn)
      ::
      ++  nump                                          ::  number reference
        ;~  pose
          ;~(pfix hep dem:ag)
          ;~(plug (cook lent (plus (just '0'))) dem:ag)
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
      ++  text  (cook crip (star (shim ' ' '~')))       ::  bullets separating
      ++  glyph  (mask "/\\\{(<!?{(zing glyphs)}")      ::  station postfix
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
          ;~  plug  (perk %who ~)
            ;~  pose
              ;~(pfix ace parz)
              ;~(pfix ace glyph)
              (easy ~)
            ==
          ==
        ::
          ;~(plug (perk %bind ~) ;~(pfix ace glyph) (punt ;~(pfix ace parz)))
          ;~((glue ace) (perk %join ~) parz)
          ;~((glue ace) (perk %what ~) ;~(pose parz glyph))
        ::
          ;~  plug  (perk %nick ~)
            ;~  pose
              (cook some ;~(pfix ace ship))
              (easy ~)
            ==
            ;~  pose
              (cook some ;~(pfix ace nick))
              (easy ~)
            ==
          ==
        ::
          ;~(plug (perk %help ~) (easy ~))
          (stag %number nump)
          =+  (punt ;~(pfix ace message))
          (stag %target ;~(plug ;~(pose parz glyph) -))
          (stag %number (cook lent (star sem)))
        ==
      --
    ++  sh-abet
      ^+  +>
      =+  zoc=(flop coz)
      |-  ^+  +>+>
      ?~  zoc  +>+>.$(shells (~(put by shells) ost.hid she))
      $(zoc t.zoc, +>.$ (sh-deal i.zoc))
    ::
    ++  sh-deal                                         ::  apply from shell
      |=  cod=command
      ^+  +>
      ?-    -.cod
          %design
        ?~  q.cod
          =.  +>+>.$  (ra-config p.cod *config)
          +>.$(stories (~(del by stories) p.cod))
        +>(+> (ra-config p.cod u.q.cod))
      ::
          %review   +>(+> (ra-think | her.she +.cod))
          %publish  +>(+> (ra-think & her.she +.cod))
      ==
    ::
    ++  sh-fact                                         ::  send console effect
      |=  fec=sole-effect
      ^+  +>
      +>(moves :_(moves [ost.hid %diff %sole-effect fec]))
    ::
    ++  sh-peep                                         ::  peer to path
      |=  pax=path
      ^+  +>
      +>(+> (ra-subscribe her.she pax))
    ::
    ++  sh-peer                                         ::  subscribe shell
      =<  sh-prod
      %_    .
          +>
        (ra-subscribe:(ra-subscribe her.she ~) her.she [%afx man.she ~])
      ==
    ::
    ++  sh-prod                                         ::  show prompt
      ^+  .
      %+  sh-fact  %pro
      :+  &  %talk-line
      ^-  tape
      =+  ^=  rew  ^-  (pair (pair ,@t ,@t) (set partner))
          ?~  active.she
            [['(' ')'] passive.she]
          [['[' ']'] u.active.she]
      =+  cha=(~(get by nik) q.rew)
      ?^  cha  ~[u.cha ' ']
      =+  por=~(te-prom te man.she q.rew)
      (weld `tape`[p.p.rew por] `tape`[q.p.rew ' ' ~])
    ::
    ++  sh-pact                                         ::  update active aud
      |=  lix=(set partner)
      ^+  +>
      =+  act=?~(lix ~ `(sh-pare lix))
      ?:  =(active.she act)  +>.$
      sh-prod(active.she act)
    ::
    ++  sh-pare                                         ::  adjust target list
      |=  paz=(set partner)
      ?:  (sh-pear paz)  paz
      (~(put in paz) [%& our.hid man.she])
    ::
    ++  sh-pear                                         ::  hearback
      |=  paz=(set partner)
      ?~  paz  |
      ?|  $(paz l.paz) 
          $(paz r.paz)
          (~(has in sources.shape:(~(got by stories) man.she)) `partner`n.paz)
      ==
    ::
    ++  sh-pass                                         ::  passive from aud
      |=  aud=audience
      %-  sh-poss
      %-  ~(gas in *(set partner))
      (turn (~(tap by aud)) |=([a=partner *] a))
    ::
    ++  sh-poss                                         ::  passive update
      |=  lix=(set partner)
      ?^  buf.say.she
        +>.$
      =+  sap=(sh-pare lix)
      ?:  =(sap passive.she)
        +>.$
      sh-prod(passive.she sap)
    ::
    ++  sh-pest                                         ::  report listen
      |=  tay=partner
      ^+  +>
      ?.  ?=(%& -.tay)  +>
      =+  sib=(~(get by ham.system.she) `station`p.tay)
      ?.  |(?=(~ sib) !?=(%white p.cordon.u.sib))
        +>.$
      (sh-poss [tay ~ ~])
    ::
    ++  sh-rend                                         ::  print on one line
      |=  gam=telegram
      =+  lin=~(tr-line tr man.she gam)
      (sh-pass:(sh-fact %txt lin) q.q.gam) 
    ::
    ++  sh-numb                                         ::  print msg number
      |=  num=@ud
      ^+  +>
      =+  bun=(scow %ud num)
      ::  =+  pad=(sub 36 (lent bun))
      ::  =+  now=`@da`(dis now.hid 0xffff.ffff.ffff.ffff.0000.0000.0000.0000)
      %+  sh-fact  %txt
      (runt [(sub 15 (lent bun)) '-'] "[{bun}]")
    ::
    ++  sh-repo-house-diff
      |=  [one=shelf two=shelf]
      =|  $=  ret
          $:  old=(list (pair span (pair posture cord)))
              new=(list (pair span (pair posture cord)))
              cha=(list (pair span (pair posture cord)))
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
      |=  [one=atlas two=atlas]
      =|  $=  ret
          $:  old=(list (pair ship status))
              new=(list (pair ship status))
              cha=(list (pair ship status))
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
    ++  sh-repo-cabal-diff
      |=  [one=(map station config) two=(map station config)]
      =|  $=  ret
          $:  old=(list (pair station config))
              new=(list (pair station config))
              cha=(list (pair station config))
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
      |=  [one=(map partner atlas) two=(map partner atlas)]
      =|  $=  ret
          $:  old=(list (pair partner atlas))
              new=(list (pair partner atlas))
              cha=(list (pair partner atlas))
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
    ++  sh-repo-whom-diff
      |=  [one=(set partner) two=(set partner)]
      =|  $=  ret
          $:  old=(list partner)
              new=(list partner)
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
      |=  [one=(set ship) two=(set ship)]
      =|  $=  ret
          $:  old=(list ship)
              new=(list ship)
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
      |=  a=posture  ^-  tape
      ?-  a
        %black  "channel"
        %brown  "mailbox"
        %white  "village"
        %green  "journal"
      ==
    ::
    ++  sh-repo-config-exceptions
      |=  [pre=tape por=posture old=(list ship) new=(list ship)]
      =+  out=?:(?=(?(%black %brown) por) "try " "cut ")
      =+  inn=?:(?=(?(%black %brown) por) "ban " "add ")
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
      |=  [pre=tape old=(list partner) new=(list partner)]
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
      |=  [pre=tape laz=config loc=config]
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
      |=  $:  laz=(map station config)
              old=(list (pair station config))
              new=(list (pair station config))
              cha=(list (pair station config))
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
      |=  bal=cabal
      ^+  +>
      =+  laz=system.she
      =.  system.she  bal
      =.  +>.$
          %+  sh-repo-cabal-changes  ham.laz
          (sh-repo-cabal-diff ham.laz ham.bal)
      (sh-repo-config-show "" loc.laz loc.bal)
    ::
    ++  sh-repo-house
      |=  awl=(map span (pair posture cord))
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
      |=  txt=tape
      ^+  +>
      (sh-fact %txt (runt [14 '-'] `tape`['|' ' ' (scag 64 txt)]))
    ::
    ++  sh-spaz                                         ::  print status
      |=  saz=status
      ^-  tape
      ['%' (trip p.saz)]
    ::
    ++  sh-repo-group-diff-here                         ::  print atlas diff
      |=  $:  pre=tape 
            $=  cul
            $:  old=(list (pair ship status))
                new=(list (pair ship status))
                cha=(list (pair ship status))
            ==
          ==
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
      |=  loc=atlas
      ^+  +>
      =+  cul=(sh-repo-atlas-diff p.guests.she loc)
      =.  p.guests.she  loc
      (sh-repo-group-diff-here "" cul)
    ::
    ++  sh-repo-group-there                             ::  update foreign
      |=  yid=(map partner atlas)
      =+  day=(sh-repo-rogue-diff q.guests.she yid)
      =+  dun=q.guests.she
      =.  q.guests.she  yid
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
      |=  ges=register
      ^+  +>
      =.  +>  (sh-repo-group-here p.ges)
      =.  +>  (sh-repo-group-there q.ges)
      +>
    ::
    ++  sh-repo-gram
      |=  [num=@ud gam=telegram]
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
      |=  [num=@ud gaz=(list telegram)]
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, num +(num), +> (sh-repo-gram num i.gaz))
    ::
    ++  sh-repo                                         ::  apply report
      |=  rad=report
      ^+  +>
      ::  ~&  [%sh-repo rad]
      ?-  -.rad
        %cabal   (sh-repo-cabal +.rad)
        %grams   (sh-repo-grams +.rad)
        %group   (sh-repo-group +.rad)
        %house   (sh-repo-house +.rad)
      ==
    ::
    ++  sh-sane-chat                                    ::  sanitize chatter
      |=  buf=(list ,@c)
      ^-  (list sole-edit)
      ?~  buf  ~
      =+  [inx=0 sap=0 con=0]
      |^  ^-  (list sole-edit)
          ?:  =(i.buf (turf '•'))
            ?.  =(0 con)  newline
            [[%del inx] ?~(t.buf ~ $(buf t.buf))]
          ?:  =(i.buf ' ')
            ?.  =(64 con)  advance(sap inx)
            [(fix (turf '•')) newline]
          ?:  =(64 con)
            =+  dif=(sub inx sap)
            ?:  (lth dif 64)
              [(fix(inx sap) (turf '•')) $(con dif)]
            [[%ins inx (turf '•')] $(con 0, inx +(inx))]
          ?:  |((lth i.buf 32) (gth i.buf 126))
            [(fix '?') advance]
          ?:  &((gte i.buf 'A') (lte i.buf 'Z'))
            [(fix (add 32 i.buf)) advance]
          advance
      ::
      ++  advance  ?~(t.buf ~ $(con +(con), inx +(inx), buf t.buf))
      ++  newline  ?~(t.buf ~ $(con 0, inx +(inx), buf t.buf))
      ++  fix  |=(cha=@ [%mor [%del inx] [%ins inx `@c`cha] ~])  
      --
    ::
    ++  sh-sane                                         ::  sanitize input
      |=  [inv=sole-edit buf=(list ,@c)]
      ^-  (list sole-edit)
      =+  res=(rose (tufa buf) sh-scad)
      ?:  ?=(| -.res)  [inv ~]
      =+  wok=`(unit work)`p.res
      |-  ^-  (list sole-edit)
      ?~  wok  ~
      ?+  -.u.wok  ~
        %target  $(wok q.u.wok)
        %say  |-  ::  XX per line
              ?~  p.u.wok  ~
              ?:  ?=(%lin -.i.p.u.wok)
                (sh-sane-chat buf)
              $(p.u.wok t.p.u.wok)
      ==
    ::
    ++  sh-slug                                         ::  edit to sanity
      |=  lit=(list sole-edit)
      ^+  +>
      ?~  lit  +>
      =^  lic  say.she
          (~(transmit sole say.she) `sole-edit`?~(t.lit i.lit [%mor lit]))
      (sh-fact [%mor [%det lic] ~])
    ::
    ++  sh-stir                                         ::  apply edit
      |=  cal=sole-change
      ^+  +>
      =^  inv  say.she  (~(transceive sole say.she) cal)
      =+  lit=(sh-sane inv buf.say.she)
      ?~  lit
        +>.$
      (sh-slug lit)
    ::
    ++  sh-lame                                         ::  send error
      |=  txt=tape
      (sh-fact [%txt txt])
    ::
    ++  sh-whom                                         ::  current audience
      ^-  audience
      %-  ~(gas by *audience)
      %+  turn  (~(tap in ?~(active.she passive.she u.active.she)))
      |=(a=partner [a *envelope %pending])
    ::
    ++  sh-tell                                         ::  add command
      |=  cod=command
      %_(+> coz [cod coz])
    ::
    ++  sh-twig-head  ^-  vase                          ::  eval data
      !>(`[our=@p now=@da eny=@uvI]`[our.hid now.hid (shas %eny eny.hid)])
    ::
    ++  sh-work                                         ::  do work
      |=  job=work
      ^+  +>
      =+  roy=(~(got by stories) man.she)
      =<  work
      |%
      ++  work
        ?-  -.job
          %number  (number +.job)
          %join    (join +.job)
          %eval    (eval +.job)
          %who     (who +.job)
          %what    (what +.job)
          %bind    (bind +.job)
          %invite  (invite +.job)
          %banish  (banish +.job)
          %author  (author +.job)
          %block   (block +.job)
          %create  (create +.job)
          %nick    (nick +.job)
          %target  (target +.job)
          %probe   (probe +.job)
          %help    (help)
          %say     (say +.job)
        ==
      ::
      ++  activate                                      ::  from %number
        |=  gam=telegram
        ^+  ..sh-work
        =+  tay=~(. tr man.she gam)
        =.  ..sh-work  (sh-fact %tan tr-tang:tay)
        sh-prod(active.she `tr-pals:tay)
      ::
      ++  help  |=(~ (sh-fact %mor talk-doc))           ::  %help
      ++  glyph
        |=  idx=@
        =<  cha.ole
        %+  reel  glyphs
        |=  [all=tape ole=[cha=char num=@]]
        =+  new=(snag (mod idx (lent all)) all)
        =+  num=~(wyt in (~(get ju nak) new))
        ?~  cha.ole  [new num]
        ?:  (lth num.ole num)
          ole
        [new num]
      ::
      ++  join                                          ::  %join
        |=  lix=(set partner)
        ^+  ..sh-work
        =.  ..sh-work
          =+  (~(get by nik) lix)
          ?^  -  (sh-note "has glyph {<u>}")
          =+  cha=(glyph (mug lix))
          =:  nik  (~(put by nik) lix cha)
              nak  (~(put ju nak) cha lix)
            ==
          (sh-note "new glyph {<cha>}")
        =+  loc=loc.system.she
        %^  sh-tell  %design  man.she
        :-  ~
        =+  tal=(~(tap in lix))
        %_    loc
            sources
          |-  ^-  (set partner)
          ?~  tal  sources.loc
          ?:  (~(has in sources.loc) i.tal)
            $(tal t.tal, sources.loc (~(del in sources.loc) i.tal))
          $(tal t.tal, sources.loc (~(put in sources.loc) i.tal))
        ==
      ::
      ++  what                                          ::  %what
        |=  qur=$|(char (set partner))  ^+  ..sh-work
        ?^  qur
          =+  cha=(~(get by nik) qur)
          (sh-fact %txt ?~(cha "none" [u.cha]~))
        =+  pan=(~(tap in (~(get ju nak) qur)))
        ?:  =(~ pan)  (sh-fact %txt "~")
        =<  (sh-fact %mor (turn pan .))
        |=(a=(set partner) [%txt <a>]) ::  XX ~(te-whom te man.she a)
      ::
      ++  who                                          ::  %who
        |=  lix=?((set partner) char)  ^+  ..sh-work
        =<  ?~(lix (. lix) ?^(lix (. lix) (fetch-nik lix .)))
        |=  pan=(set partner)
        =<  (sh-fact %mor (murn (sort (~(tap by q.guests.she)) aor) .))
        |=  [pon=partner alt=atlas]  ^-  (unit sole-effect)
        ?.  |(=(~ pan) (~(has in pan) pon))  ~
        =-  `[%tan rose/[", " `~]^- leaf/~(ta-full ta man.she pon) ~]
        =<  (murn (sort (~(tap by alt)) aor) .)
        |=  [a=ship b=presence c=human]  ^-  (unit tank) :: XX names
        ?-  b
          %gone  ~
          %hear  `>a<
          %talk  `>a<      ::  XX difference
        ==
      :: 
      ++  bind                                          ::  %bind
        |=  [cha=char pan=(unit (set partner))]  ^+  ..sh-work
        ~&  bind/[cha pan nik nak]
        ?~  pan  $(pan [~ ?~(active.she passive.she u.active.she)])
        =+  ole=(~(get by nik) u.pan)
        ?:  =(ole [~ cha])  ..sh-work
        =.  nak  ?~(ole nak (~(del ju nak) u.ole u.pan))
        =:  nak  (~(put ju nak) cha u.pan)
            nik  (~(put by nik) u.pan cha)
          ==
        ..sh-work
      ::
      ++  invite                                        ::  %invite
        |=  [nom=span tal=(list partner)]
        ^+  ..sh-work
        !!
      ::
      ++  block                                         ::  %block
        |=  [nom=span tal=(list partner)]
        ^+  ..sh-work
        !!
      ::
      ++  author                                        ::  %author
        |=  [nom=span tal=(list partner)]
        ^+  ..sh-work
        !!
      ::
      ++  banish                                        ::  %banish
        |=  [nom=span tal=(list partner)]
        ^+  ..sh-work
        !!
      ::
      ++  create                                        ::  %create
        |=  [por=posture nom=span txt=cord]
        ^+  ..sh-work
        ?:  (~(has in stories) nom) 
          (sh-lame "{(trip nom)}: already exists")
        =.  ..sh-work
            %^  sh-tell  %design  nom
            :-  ~
            :+  *(set partner)
              (end 3 64 txt)
            [por ~]
        (join [[%& our.hid nom] ~ ~])
      ::
      ++  reverse-folks
        |=  nym=span
        ^-  (list ship)
        %+  murn  (~(tap by folks))
        |=  [p=ship q=human]
        ?~  hand.q  ~
        ?.  =(u.hand.q nym)  ~
        [~ u=p]
      ::
      ++  nick                                          ::  %nick
        |=  [her=(unit ship) nym=(unit cord)]
        ^+  ..sh-work
        ?:  ?=([~ ~] +<)
          %+  sh-fact  %mor
          %+  turn  (~(tap by folks))
          |=  [p=ship q=human]
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
          |=  p=ship
          [%txt "{<p>}: {<u.nym>}"]
        %=  ..sh-work
          folks  (~(put by folks) u.her [true=~ hand=nym])
        ==
      ::
      ++  target                                        ::  %target
        |=  [lix=?((set partner) char) woe=(unit ^work)]
        =-  ?~(woe end work(job u.woe, ..sh-pact end))
        ^-  end=_..sh-work
        ?~  lix  (sh-pact lix) 
        ?^  lix  (sh-pact lix) 
        (fetch-nik lix sh-pact)
      ::
      ++  fetch-nik
        |=  [cha=char sh-fun=$+((set partner) _..sh-work)]
        =+  lax=(~(get ju nak) cha)
        ?:  =(~ lax)  (sh-lame "unknown {<cha>}")
        ?:  ?=([* ~ ~] lax)  (sh-fun n.lax)
        |-  ^-  sh-fun
        ?~  grams.roy  
          (sh-lame:(what cha) "ambiguous {<cha>}")
        =+  pan=(sa (turn :_(head (~(tap by q.q.i.grams.roy)))))
        ?:  (~(has in lax) pan)
          (sh-fun pan)
        $(grams.roy t.grams.roy)
      ::
      ++  number                                        ::  %number
        |=  num=$|(@ud [p=@u q=@ud])
        ^+  ..sh-work
        =+  roy=(~(got by stories) man.she)
        |-
        ?@  num
          ?:  (gte num count.roy)
            (sh-lame "{(scow %s (new:si | +(num)))}: no such telegram")
          =.  ..sh-fact  (sh-fact %txt "? {(scow %s (new:si | +(num)))}")
          (activate (snag num grams.roy))
        ?.  (gth q.num count.roy)
          =+  msg=(deli count.roy num)
          =.  ..sh-fact  (sh-fact %txt "? {(scow %ud msg)}")
          (activate (snag (sub count.roy +(msg)) grams.roy))
        (sh-lame "…{(reap p.num '0')}{(scow %ud q.num)}: no such telegram")
      ::
      ++  deli                                          ::  find number
        |=  [max=@ud nul=@u fin=@ud]  ^-  @ud
        =+  dog=|-(?:(=(0 fin) 1 (mul 10 $(fin (div fin 10)))))
        =.  dog  (mul dog (pow 10 nul))
        =-  ?:((lte - max) - (sub - dog))
        (add fin (sub max (mod max dog)))
      ::
      ++  probe                                         ::  inquire
        |=  cuz=station
        ^+  ..sh-work
        ~&  [%probe cuz]
        ..sh-work
      ::
      ++  eval                                          ::  run
        |=  [txt=cord exe=twig]
        =>  |.([(sell (slap (slop sh-twig-head seed) exe))]~)
        =+  tan=p:(mule .)
        (say [%fat tank/tan exp/txt] ~)
      ::
      ++  say                                           ::  publish
        |=  sep=(list speech)
        ^+  ..sh-work
        ?~  sep    ..sh-work
        =^  sir  ..sh-work  sh-uniq
        %_    $
            sep  t.sep
            coz  :_  coz
          [%publish [[sir sh-whom [now.hid ~ i.sep]] ~]]
        ==
      --
    ::
    ++  sh-done                                         ::  apply result
      =+  lit=(sh-sane [%nop ~] buf.say.she)
      ?^  lit
        (sh-slug lit)
      =+  jub=(rust (tufa buf.say.she) sh-scad)
      ?~  jub  (sh-fact %bel ~)
      =.  +>  (sh-work u.jub)
      =+  buf=buf.say.she
      =^  cal  say.she  (~(transmit sole say.she) [%set ~])
      %-  sh-fact
      :*  %mor
          [%nex ~]
          [%det cal]
          ?.  ?=([%';' *] buf)  ~ 
          :_  ~
          [%txt (weld "----------------| " (tufa buf))]
      ==
    ::
    ++  sh-sole                                         ::  apply edit
      |=  act=sole-action
      ^+  +>
      ?-  -.act
        %det  (sh-stir +.act)
        %clr  (sh-pact ~)
        %ret  sh-done
      ==
    ::
    ++  sh-uniq
      ^-  [serial _.]
      [(shaf %serial eny.hid) .(eny.hid (shax eny.hid))]
    --
  ++  ra-abed                                           ::  resolve core
    ^-  [(list move) _+>]
    :_  +>
    =+  ^=  yop  
        |-  ^-  (pair (list move) (list sole-effect))
        ?~  moves  [~ ~]
        =+  mor=$(moves t.moves)
        ?:  ?&  =(ost.hid p.i.moves) 
                ?=([%diff %sole-effect *] q.i.moves)
            ==
          [p.mor [+>.q.i.moves q.mor]]
        [[i.moves p.mor] q.mor]
    =+  :*  moz=(flop p.yop)
            ^=  foc  ^-  (unit sole-effect)
            ?~  q.yop  ~ 
            ?~(t.q.yop `i.q.yop `[%mor (flop `(list sole-effect)`q.yop)])
        ==
    ?~(foc moz [[ost.hid %diff %sole-effect u.foc] moz])
  ::
  ++  ra-abet                                           ::  complete core
    ra-abed:ra-axel
  ::
  ++  ra-axel                                           ::  rebound reports
    ^+  .
    =+  ^=  rey
        |-  ^-  (pair (list move) (list (pair bone report)))
        ?~  moves
          [~ ~]
        =+  mor=$(moves t.moves)
        ?.  ?&  (~(has by shells) `bone`p.i.moves)
                ?=([%diff %talk-report *] q.i.moves)
            ==
          [[i.moves p.mor] q.mor]
        [p.mor [[p.i.moves +>.q.i.moves] q.mor]]
    =.  moves  p.rey
    ?:  =(q.rey ~)  +
    =.  q.rey  (flop q.rey)
    |-  ^+  +>
    ?~  q.rey  ra-axel
    $(q.rey t.q.rey, +> (ra-back(ost.hid p.i.q.rey) q.i.q.rey))
  ::
  ++  ra-back
    |=  rad=report
    ^+  +>
    sh-abet:(~(sh-repo sh ~ (~(got by shells) ost.hid)) rad)
  ::
  ++  ra-sole
    |=  act=sole-action
    ^+  +>
    =+  shu=(~(get by shells) ost.hid)
    ?~  shu
      ~&  [%ra-console-broken ost.hid]
      +>.$
    sh-abet:(~(sh-sole sh ~ u.shu) act)
  ::  
  ++  ra-emil                                           ::  ra-emit move list
    |=  mol=(list move)
    %_(+> moves (welp (flop mol) moves))
  ::
  ++  ra-emit                                           ::  emit a move
    |=  mov=move
    %_(+> moves [mov moves])
  ::
  ++  ra-evil                                           ::  emit error
    |=  msg=cord
    ~|  [%ra-evil msg]
    !!
  ::
  ++  ra-house                                          ::  emit partners
    |=  ost=bone
    %+  ra-emit  ost.hid
    :+  %diff  %talk-report
    :-  %house
    %-  ~(gas in *(map span (pair posture cord)))
    %+  turn  (~(tap by stories)) 
    |=([a=span b=story] [a p.cordon.shape.b caption.shape.b])
  ::
  ++  ra-homes                                          ::  update partners
    =+  gel=general
    |-  ^+  +>
    ?~  gel  +>
    =.  +>  $(gel l.gel)
    =.  +>  $(gel r.gel)
    (ra-house n.gel)
  ::
  ++  ra-init                                           ::  initialize talk
    %+  ra-apply  our.hid
    :+  %design  (main our.hid)
    :-  ~  :-  ~
    :-  'default home'
    [%brown ~] 
  ::
  ++  ra-apply                                          ::  apply command
    |=  [her=ship cod=command]
    ^+  +>
    ?-    -.cod
        %design
      ?.  =(her our.hid)
        (ra-evil %talk-no-owner)
      ?~  q.cod
        ?.  (~(has by stories) p.cod)
          (ra-evil %talk-no-story)
        (ra-config(stories (~(del by stories) p.cod)) p.cod *config)
      (ra-config p.cod u.q.cod)
    ::
        %review   (ra-think | her +.cod)
        %publish  (ra-think & her +.cod)
    ==
  ::
  ++  ra-config                                         ::  configure story
    |=  [man=span con=config]
    ^+  +>
    =+  :-  neu=(~(has by stories) man)
        pur=(fall (~(get by stories) man) *story)
    =.  +>.$  pa-abet:(~(pa-reform pa man pur) con)
    ?:(neu +>.$ ra-homes)
  ::
  ++  ra-know                                           ::  story monad
    |=  man=span
    |*  fun=$+(_pa _+>+>)
    ^+  +>+>
    =+  pur=(~(get by stories) man)
    ?~  pur
      ~&  [%ra-know-not man]                            ::  XX should crash
      +>+>.$
    (fun ~(. pa man u.pur))
  ::
  ++  ra-diff-talk-report                               ::  subscription update
    |=  [man=span cuz=station rad=report]
    %-  (ra-know man)  |=  par=_pa  =<  pa-abet
    (pa-diff-talk-report:par cuz rad)
  ::
  ++  ra-quit                                           ::  subscription quit
    |=  [man=span cuz=station]
    %-  (ra-know man)  |=  par=_pa  =<  pa-abet
    (pa-quit:par %& cuz)
  ::
  ++  ra-retry                                          ::  subscription resend
    |=  [man=span cuz=station]
    %-  (ra-know man)  |=  par=_pa  =<  pa-abet
    (pa-acquire:par [%& cuz]~)
  ::
  ++  ra-coup-repeat                                    ::
    |=  [[num=@ud her=@p man=span] saw=(unit tang)]
    (ra-repeat num [%& her man] saw)
  ::
  ++  ra-repeat                                         ::  remove from outbox 
    |=  [num=@ud pan=partner saw=(unit tang)]
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
    |=  [src=ship pax=path]
    ^+  +>
    ?.  ?=([@ @ *] pax)
      +>(general (~(del in general) ost.hid))
    %-  (ra-know i.t.pax)  |=  par=_pa  =<  pa-abet
    (pa-notify:pa-cancel:par src %gone *human)
  ::
  ++  ra-human                                          ::  look up person
    |=  her=ship
    ^-  [human _+>]
    =^  who  folks
        =+  who=(~(get by folks) her)
        ?^  who  [u.who folks]
        =+  who=`human`[~ `(scot %p her)]               ::  XX do right
        [who (~(put by folks) her who)]
    [who +>.$]
  ::
  ++  ra-console                                        ::  console subscribe
    |=  [her=ship pax=path]
    ^+  +>
    =+  man=`span`?~(pax (main her) ?>(?=(~ t.pax) i.pax))
    =+  ^=  she  ^-  shell
        [her man 0 *sole-share ~ [[%& our.hid man] ~ ~] [~ ~] ~ *cabal]
    sh-abet:~(sh-peer sh ~ she)
  ::
  ++  ra-subscribe                                      ::  listen to
    |=  [her=ship pax=path]
    ^+  +>
    ::  ~&  [%ra-subscribe ost.hid her pax]
    ?:  ?=(~ pax)
      (ra-house(general (~(put in general) ost.hid)) ost.hid)
    ?.  ?=([@ @ *] pax)
      (ra-evil %talk-bad-path)
    =+  ^=  vab  ^-  (set ,@tas)
        =|  vab=(set ,@tas)
        |-  ^+  vab
        ?:  =(0 i.pax)  vab
        $(i.pax (rsh 3 1 i.pax), vab (~(put in vab) (end 3 1 i.pax)))
    =+  pur=(~(get by stories) i.t.pax)
    ?~  pur
      ~&  [%bad-subscribe-story-c i.t.pax]
      (ra-evil %talk-no-story)
    =+  soy=~(. pa i.t.pax u.pur)
    =^  who  +>.$  (ra-human her)
    =.  soy  ?.((~(has in vab) %a) soy (pa-watch:soy her))
    =.  soy  ?.((~(has in vab) %x) soy (pa-master:soy her))
    =.  soy  (pa-notify:soy her %hear who)
    =.  soy  ?.((~(has in vab) %f) soy (pa-listen:soy her t.t.pax))
    pa-abet:soy
  ::
  ++  ra-think                                          ::  publish/review
    |=  [pub=? her=ship tiz=(list thought)]
    ^+  +>
    ?~  tiz  +>
    $(tiz t.tiz, +> (ra-consume pub her i.tiz))
  ::
  ++  ra-normal                                         ::  normalize
    |=  tip=thought
    ^-  thought
    ?.  ?=([%lin *] r.r.tip)  tip
    %_    tip
        q.r.r
      %-  crip
      %+  scag  64
      %-  tufa
      %+  turn  (tuba (trip q.r.r.tip))
      |=  a=@c
      ?:  &((gte a 'A') (lte a 'Z'))
        (add a 32)
      ?:  |((lth a 32) (gth a 126))
        `@`'?'
      a
    ==
  ::
  ++  ra-consume                                        ::  consume thought
    |=  [pub=? her=ship tip=thought]
    =.  tip  (ra-normal tip)
    =+  aud=(~(tap by q.tip) ~)
    |-  ^+  +>.^$
    ?~  aud  +>.^$
    $(aud t.aud, +>.^$ (ra-conduct pub her p.i.aud tip))
  ::
  ++  ra-conduct                                        ::  thought to partner
    |=  [pub=? her=ship tay=partner tip=thought]
    ^+  +>
    ::  ~&  [%ra-conduct pub her tay]
    ?-  -.tay
      %&  ?:  pub
            =.  her  our.hid                            ::  XX security!
            ?:  =(her p.p.tay)
              (ra-record q.p.tay p.p.tay tip)
            (ra-transmit p.tay tip)
          ?.  =(our.hid p.p.tay)
            +>
          (ra-record q.p.tay her tip)
      %|  !!
    ==
  ::
  ++  ra-record                                         ::  add to story
    |=  [man=span gam=telegram]
    %-  (ra-know man)  |=  par=_pa  =<  pa-abet
    (pa-learn:par gam)
  ::
  ++  ra-transmit                                       ::  send to neighbor
    |=  [cuz=station tip=thought]
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
    |_  $:  man=span
            story
        ==
    ++  pa-abet
      ^+  +>
      +>(stories (~(put by stories) man `story`+<+))
    ::
    ++  pa-admire                                       ::  accept from
      |=  her=ship
      ^-  ?
      ::?-  -.cordon.shape
      ::  %&  (~(has in p.cordon.shape) her)
      ::  %|  !(~(has in p.cordon.shape) her)
      ::==
      &
    ::
    ++  pa-watch                                        ::  watch presence
      |=  her=ship
      ?.  (pa-admire her)
        (pa-sauce ost.hid [%quit ~]~)
      =.  viewers  (~(put in viewers) ost.hid)
      (pa-display ost.hid ~ ~)
    ::
    ++  pa-master                                       ::  hear config
      |=  her=ship
      ?.  (pa-admire her)
        ~&  [%pa-admire-not her]
        (pa-sauce ost.hid [%quit ~]~)
      =.  owners  (~(put in owners) ost.hid)
      ::  ~&  [%pa-master her man shape]
      (pa-sauce ost.hid [[%diff %talk-report %cabal shape mirrors] ~])
    ::
    ++  pa-display                                      ::  update presence
      |=  vew=(set bone)
      =+  ^=  reg
          :_  remotes
          |-  ^-  atlas
          ?~  locals  ~
          [[p.n.locals q.q.n.locals] $(locals l.locals) $(locals r.locals)]
      ::  ~&  [%pa-display man reg]
      |-  ^+  +>.^$
      ?~  vew  +>.^$
      =.  +>.^$  $(vew l.vew)
      =.  +>.^$  $(vew r.vew)
      (pa-sauce n.vew [[%diff %talk-report %group reg] ~])
    ::
    ++  pa-monitor                                      ::  update config
      =+  owe=owners
      |-  ^+  +>
      ?~  owe  +>
      =.  +>  $(owe l.owe)
      =.  +>  $(owe r.owe)
      ::  ~&  [%pa-monitor man shape]
      (pa-sauce n.owe [[%diff %talk-report %cabal shape mirrors] ~])
    ::
    ++  pa-cabal
      |=  [cuz=station con=config ham=(map station config)]
      ^+  +>
      =+  old=mirrors
      =.  mirrors  (~(put by mirrors) cuz con)
      ?:  =(mirrors old)
        +>.$
      pa-monitor 
    ::
    ++  pa-diff-talk-report                             ::  subscribed update
      |=  [cuz=station rad=report]
      ^+  +>
      ?.  (~(has in sources.shape) [%& cuz])
        ~&  [%pa-diff-unexpected cuz rad]
        +>
      ?+  -.rad  ~|([%talk-odd-friend rad] !!)
        %cabal  (pa-cabal cuz +.rad)
        %group  (pa-remind [%& cuz] +.rad)
        %grams  =.  q.+.rad
                  ?.  ?=(%pawn (clan our.hid))   q.+.rad 
                  ?.  (gth (lent q.+.rad) 200)   q.+.rad
                  (slag (sub (lent q.+.rad) 200) q.+.rad)
                (pa-lesson q.+.rad)
      ==
    ::
    ++  pa-quit                                         ::  stop subscription
      |=  tay=partner
      pa-monitor(sources.shape (~(del in sources.shape) tay))
    ::
    ++  pa-sauce                                        ::  send backward
      |=  [ost=bone cub=(list card)]
      %_    +>.$
          moves
        (welp (flop (turn cub |=(a=card [ost a]))) moves)
      ==
    ::
    ++  pa-abjure                                       ::  unsubscribe move
      |=  tal=(list partner)
      %+  pa-sauce  0
      %-  zing
      %+  turn  tal
      |=  tay=partner
      ^-  (list card)
      ?-  -.tay
        %|  ~&  tweet-abjure/p.p.tay
            !!
      ::
        %&  ~&  [%pa-abjure [our.hid man] [p.p.tay q.p.tay]]
            :_  ~
            :*  %pull
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk]
                ~
            ==
      ==
    ::
    ++  pa-acquire                                      ::  subscribe to
      |=  tal=(list partner)
      %+  pa-sauce  0
      %-  zing
      %+  turn  tal
      |=  tay=partner
      ^-  (list card)
      =+  num=(fall (~(get by sequence) tay) 0)
      ?-  -.tay
        %|  !!
        %&  ::  ~&  [%pa-acquire [our.hid man] [p.p.tay q.p.tay]]
            :_  ~
            :*  %peer
                /friend/show/[man]/(scot %p p.p.tay)/[q.p.tay]
                [p.p.tay %talk] 
                /afx/[q.p.tay]/(scot %ud num)
            ==
      ==
    ::
    ++  pa-reform                                       ::  reconfigure, ugly
      |=  cof=config
      =+  ^=  dif  ^-  (pair (list partner) (list partner))
          =+  old=`(list partner)`(~(tap in sources.shape) ~)
          =+  new=`(list partner)`(~(tap in sources.cof) ~)
          :-  (skip new |=(a=partner (~(has in sources.shape) a)))
          (skip old |=(a=partner (~(has in sources.cof) a)))
      =.  +>.$  (pa-acquire p.dif)
      =.  +>.$  (pa-abjure q.dif)
      =.  shape  cof
      pa-monitor
    ::
    ++  pa-cancel                                       ::  unsubscribe from
      ~&  [%pa-cancel ost.hid]
      %_  .
        guests  (~(del by guests) ost.hid)
        viewers  (~(del in viewers) ost.hid)
        owners  (~(del in owners) ost.hid)
      ==
    ::
    ++  pa-notify                                       ::  local presence
      |=  [her=ship saz=status]
      ^+  +>
      =+  ^=  nol
          ?:  =(%gone p.saz) 
            (~(del by locals) her)
          (~(put by locals) her now.hid saz)
      ?:  =(nol locals)  +>.$
      (pa-display(locals nol) viewers)
    ::
    ++  pa-remind                                       ::  remote presence
      |=  [tay=partner loc=atlas rem=(map partner atlas)]
      =+  ^=  buk
          =+  mer=(turn (~(tap by rem) ~) |=([* a=atlas] a))
          |-  ^-  atlas
          ?~  mer  loc
          =.  loc  $(mer t.mer)
          =+  dur=`(list (pair ship status))`(~(tap by i.mer) ~)
          |-  ^-  atlas
          ?~  dur  loc
          =.  loc  $(dur t.dur)
          =+  fuy=(~(get by loc) p.i.dur)
          ?~  fuy  (~(put by loc) p.i.dur q.i.dur)
          ?:  =(`presence`p.q.i.dur `presence`p.u.fuy)
            loc
          ?-  p.u.fuy
            %gone  (~(del by loc) p.i.dur q.i.dur)
            %talk  loc
            %hear  (~(put by loc) p.i.dur q.i.dur)
          ==
      =+  gub=(~(get by remotes) tay)
      ::  ~&  [%pa-remind tay gub buk]
      ?.  |(?=(~ gub) !=(buk u.gub))
        +>.$
      =.  remotes  (~(put by remotes) tay buk)
      (pa-display viewers)
    ::
    ++  pa-start                                        ::  start stream
      |=  riv=river
      ^+  +>
      =-  ::  ~&  [%pa-start riv lab]
          =.  +>.$  (pa-sauce ost.hid [[%diff %talk-report %grams q.lab r.lab] ~])
          ?:  p.lab
            (pa-sauce ost.hid [[%quit ~] ~])
          +>.$(guests (~(put by guests) ost.hid riv))
      ^=  lab
      =+  [end=count gaz=grams dun=| zeg=*(list telegram)]
      |-  ^-  (trel ,? ,@ud (list telegram))
      ?~  gaz  [dun end zeg]
      ?:  ?-  -.q.riv                                   ::  after the end
            %ud  (lte p.q.riv end)
            %da  (lte p.q.riv p.r.q.i.gaz)
          ==
        $(end (dec end), gaz t.gaz)
      ?:  ?-  -.p.riv                                   ::  before the start
            %ud  (lth end p.p.riv)
            %da  (lth p.r.q.i.gaz p.p.riv)
          ==
        [dun end zeg]
      $(end (dec end), gaz t.gaz, zeg [i.gaz zeg])
    ::
    ++  pa-listen                                       ::  subscribe
      |=  [her=ship pax=path]
      ^+  +>
      ?.  (pa-admire her)
        ~&  [%pa-listen-admire ~]
        (pa-sauce ost.hid [%quit ~]~)
      =+  ^=  ruv  ^-  (unit river)
          ?:  ?=(~ pax)
            `[[%ud ?:((lth count 64) 0 (sub count 64))] [%da (dec (bex 128))]]
          ?:  ?=([@ ~] pax)
            =+  say=(slay i.pax)
            ?.  ?=([~ %$ ?(%ud %da) @] say)  ~
            `[(point +>.say) [%da (dec (bex 128))]]
          ?.  ?=([@ @ ~] pax)  ~
          =+  [say=(slay i.pax) den=(slay i.t.pax)]
          ?.  ?=([~ %$ ?(%ud %da) @] say)  ~
          ?.  ?=([~ %$ ?(%ud %da) @] den)  ~
          `[(point +>.say) (point +>.den)]
      ::  ~&  [%pa-listen her pax ruv]
      ?~  ruv
        ~&  [%pa-listen-malformed pax]
        (pa-sauce ost.hid [%quit ~]~)
      (pa-start u.ruv)
    ::
    ++  pa-refresh                                      ::  update to guests
      |=  [num=@ud gam=telegram]
      ^+  +>
      =+  ^=  moy
          |-  ^-  (pair (list bone) (list move))
          ?~  guests  [~ ~]
          ::  ~&  [%pa-refresh num n.guests]
          =+  lef=$(guests l.guests)
          =+  rit=$(guests r.guests)
          =+  old=[p=(welp p.lef p.rit) q=(welp q.lef q.rit)]
          ?:  ?-  -.q.q.n.guests                        ::  after the end
                %ud  (lte p.q.q.n.guests num)
                %da  (lte p.q.q.n.guests p.r.q.gam)
              ==
            [[p.n.guests p.old] [[p.n.guests %quit ~] q.old]]
          ?:  ?-  -.p.q.n.guests                        ::  before the start
                %ud  (gth p.p.q.n.guests num)
                %da  (gth p.p.q.n.guests p.r.q.gam)
              ==
            old
          :-  p.old
          [[p.n.guests %diff %talk-report %grams num gam ~] q.old]
      =.  moves  (welp q.moy moves)
      |-  ^+  +>.^$
      ?~  p.moy  +>.^$
      $(p.moy t.p.moy, guests (~(del by guests) i.p.moy))
    ::
    ++  pa-lesson                                       ::  learn multiple
      |=  gaz=(list telegram)
      ^+  +>
      ?~  gaz  +>
      $(gaz t.gaz, +> (pa-learn i.gaz))
    ::
    ++  pa-learn                                        ::  learn message
      |=  gam=telegram
      ^+  +>
      ?.  (pa-admire p.gam)
        ~&  %pa-admire-rejected
        +>.$
      =.  q.q.gam  
        =+  ole=(~(get by q.q.gam) [%& our.hid man])
        ?~  ole  q.q.gam
        (~(put by q.q.gam) [%& our.hid man] -.u.ole %received)
      =+  old=(~(get by known) p.q.gam)
      ?~  old
        (pa-append gam)
      (pa-revise u.old gam)
    ::
    ++  pa-append                                       ::  append new
      |=  gam=telegram
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
      |=  [num=@ud gam=telegram]
      =+  way=(sub count num)
      =.  grams  (welp (scag (dec way) grams) [gam (slag way grams)])
      (pa-refresh num gam)
    --
  --
::
++  sn                                                  ::  station render core
  |_  [man=span one=station] 
  ++  sn-best                                           ::  best to show
    |=  two=station
    ^-  ?
    ?:  =(our.hid p.one)
      ?:  =(our.hid p.two)
        ?<  =(q.one q.two)
        ?:  =((main p.one) q.one)  %&
        ?:  =((main p.two) q.two)  %|
        (lth q.one q.two)
      %&
    ?:  =(our.hid p.two)
      %|
    ?:  =(p.one p.two)
      (lth q.one q.two)
    (lth p.one q.one)
  ::
  ++  sn-curt                                           ::  render name in 14
    |=  mup=?
    ^-  tape
    =+  rac=(clan p.one) 
    =+  raw=(scow %p p.one)
    =.  raw  ?.(mup raw ['*' (slag 2 raw)])
    ?-    rac
        %czar  (weld "          " raw)
        %king  (weld "       " raw)
        %duke  raw
        %earl  ;:  welp
                 (scag 1 raw)
                 (scag 6 (slag 15 raw))
                 "^"
                 (scag 6 (slag 22 raw))
               ==
        %pawn  :(welp (scag 7 raw) "_" (scag 6 (slag 51 raw)))
    ==
  ::
  ++  sn-phat                                           ::  render accurately
    ^-  tape
    ?:  =(p.one our.hid)
      ?:  =(q.one man)
        "="
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
  |_  [man=span one=partner]
  ++  ta-beat                                           ::  more relevant
    |=  two=partner  ^-  ?
    ?-    -.one
        %&  
      ?-  -.two
        %|  %&
        %&  (~(sn-best sn man p.one) p.two)
      ==
    ::
        %|
      ?-  -.two
        %&  %|
        %|  ?:  =(-.p.two -.p.one)
              (lth (mug +.p.one) (mug +.p.two))
            (lth -.p.two -.p.one)
      ==
    ==
  ++  ta-best                                           ::  most relevant 
    |=(two=partner ?:((ta-beat two) two one))
  ::
  ++  ta-full  (ta-show ~)                              ::  render full width
  ++  ta-show                                           ::  render partner
    |=  moy=(unit ,?)
    ^-  tape
    ?-    -.one
        %&  
      ?~  moy
        =+  cha=(~(get by nik) one ~ ~)
        =-  ?~(cha - "'{u.cha ~}' {-}")
        ~(sn-phat sn man p.one)
      (~(sn-curt sn man p.one) u.moy)
    ::
        %|
      =+  ^=  pre  ^-  tape
          ?-  -.p.one
            %twitter  "@t:"
          ==
      ?~  moy
        (weld pre (trip p.p.one))
      =.  pre  ?.(=(& u.moy) pre ['*' pre])
      (sigh 14 pre p.p.one)
    ==
  --
::
++  te                                                  ::  audience renderer
  |_  [man=span lix=(set partner)]
  ++  te-best  ^-  (unit partner)
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
    .(lix (~(del in lix) `partner`[%& our.hid man]))
  ::
  ++  te-maud  ^-  ?                                    ::  multiple audience
    =.  .  te-deaf
    !?=($|(~ [* ~ ~]) lix)
  ::
  ++  te-prom  ^-  tape                                 ::  render targets
    =.  .  te-deaf
    =+  ^=  all
        %+  sort  `(list partner)`(~(tap in lix))
        |=  [a=partner b=partner]
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
    (~(ta-show ta man (need te-best)) ~ te-maud)
  ::
  ++  te-pref                                           ::  audience glyph
    ^-  tape
    =+  cha=(~(get by nik) lix)
    ?^  cha  ~[u.cha ' ']
    ?.  ?=([[& ^] ~ ~] lix)  "* "
    =+  sta=p.n.lix
    =+  sot=(~(get by stories) q.sta)
    ?:  &(=(p.sta our.hid) ?=(^ sot) ?=(%brown p.cordon.shape.u.sot))
      ": "
    "* "
  --
::
++  tr                                                  ::  telegram renderer
  |_  $:  man=span
          who=ship
          sen=serial
          aud=audience
          wen=@da
          bou=bouquet
          sep=speech
      ==
  ++  tr-line  ^-  tape                                 ::  one-line print
    =+  txt=(tr-text =(who our.hid))
    ?:  =(~ txt)  ""
    =+  ^=  baw
        ::  ?:  oug 
        ::  ~(te-whom te man tr-pals)
        (~(sn-curt sn man [who (main who)]) |)
    (weld baw txt)
  ::
  ++  tr-tang  ^-  tang
    %+  welp  tr-sep-tang
    =.  wen  (sub wen (mod wen (div wen ~s0..0001)))     :: round
    =+  hed=leaf/"{(scow %uv sen)} at {(scow %da wen)}"
    =+  =<  paz=(turn (~(tap by aud)) .)
        |=([a=partner *] leaf/~(ta-full ta man a))
    =+  bok=(turn (sort (~(tap in bou)) aor) smyt)
    [%rose [" " ~ ~] [hed >who< [%rose [", " "to " ~] paz] bok]]~
  ::
  ++  tr-sep-tang
    |-  ^-  tang
    =<  ?+(. . [@ *] [.]~)  ^-  ?(tank tang)            ::  wrap single tanks
    ?+  -.sep  [>sep<]~
      %exp  leaf/"# {(trip p.sep)}"
      %lin  leaf/"{?:(p.sep "" "@ ")}{(trip q.sep)}"
      %non  ~
      %app  rose/[": " ~ ~]^~[leaf/"[{(trip p.sep)}]" leaf/(trip q.sep)]
      %tax  leaf/(rend-work-duty p.sep)
      %url  leaf/"/ {(earf p.sep)}"
      %mor  ?~(p.sep ~ (weld $(p.sep t.p.sep) $(sep i.p.sep)))
      %fat  (welp (tr-rend-tors p.sep) $(sep q.sep))
    ==
  ::
  ++  tr-rend-tors
    |=  a=torso  ^-  tang
    ?-  -.a
      %name  (welp $(a q.a) leaf/"={(trip p.a)}" ~)
      %tank  +.a
      %text  (turn (flop +.a) |=(b=cord leaf/(trip b)))
    ==
  ::
  ++  tr-pals
    ^-  (set partner)
    %-  ~(gas in *(set partner))
    (turn (~(tap by aud)) |=([a=partner *] a))
  ::
  ++  chow
    |=  [len=@u txt=tape]
    ?:  (gth len (lent txt))  txt
    =.  txt  (scag len txt)
    |-
    ?~  txt  txt
    ?:  =(' ' i.txt)
      |-(['_' ?.(?=([%' ' *] t.txt) t.txt $(txt t.txt))])
    ?~  t.txt  "…"
    [i.txt $(txt t.txt)]
  ::
  ++  tr-text
    |=  oug=?
    ^-  tape
    ?+    -.sep  ~&(tr-lost/sep "")
        %fat
      =+  rem=$(sep q.sep)
      ?:  (gth (lent rem) 62)  (chow 64 rem)
      =-  "{rem}{(chow (sub 64 (lent rem)) "  {-}")}"
      ?+  -.p.sep  "..."
        %tank  ~(ram re %rose [" " `~] +.p.sep)
      ==
    ::
        %exp  (chow 66 '#' ' ' (trip p.sep))
        %url  =+  ful=(earf p.sep)
              ?:  (gth 64 (lent ful))  ['/' ' ' ful]
              :+  '/'  '_' 
              =+  hok=r.p.p.p.sep
              ~!  hok
              =<  ?~(-.hok (reel p.hok .) +:(scow %if p.hok))
              |=([a=span b=tape] ?~(b (trip a) (welp b '.' (trip a))))
    ::
        %lin
      =+  txt=(trip q.sep)
      ?:  p.sep
        (weld ~(te-pref te man tr-pals) txt)
      ?:  oug
        (weld "@ " txt)
      (weld " " txt)
    ::
        %app
      (chow 64 "[{(trip p.sep)}]: {(trip q.sep)}")
    ::
        %tax
      (chow 64 " {(rend-work-duty p.sep)}")
    ==
  -- 
::
++  peer                                                ::  accept subscription
  |=  [pax=path]
  ^-  [(list move) _+>]
  ~?  !=(src.hid our.hid)  [%peer-talk-stranger src.hid]
  ::  ~&   [%talk-peer src ost.hid pax]
  ?:  ?=([%sole *] pax)
    ?>  =(our.hid src.hid)
    ~?  (~(has by shells) ost.hid)  [%talk-peer-replaced ost.hid pax]
    ra-abet:(ra-console:ra src.hid t.pax)
  ::  ~&  [%talk-peer-data ost.hid src.hid pax]
  ra-abet:(ra-subscribe:ra src.hid pax)
::
++  poke-talk-command                                   ::  accept command
  |=  [cod=command]
  ^-  [(list move) _+>]
  ::  ~&  [%talk-poke-command src.hid cod]
  =^  mos  +>.$
      ra-abet:(ra-apply:ra src.hid cod)
  =^  mow  +>.$  log-all-to-file
  [(welp mos mow) +>.$]
::
++  poke-sole-action                                    ::  accept console
  |=  [act=sole-action]
  ra-abet:(ra-sole:ra act)
::
++  diff-talk-report                                    ::
  |=  [way=wire rad=report]
  ^-  (quip move +>)
  =^  mos  +>.$
      %+  etch-friend  way  |=  [man=span cuz=station]
      ra-abet:(ra-diff-talk-report:ra man cuz rad)
  =^  mow  +>.$  log-all-to-file
  [(welp mos mow) +>.$]
::
++  coup-repeat                                         ::
  |=  [way=wire saw=(unit tang)]
  %+  etch-repeat  [%repeat way]  |=  [num=@ud src=@p man=span]
  ra-abet:(ra-coup-repeat:ra [num src man] saw)
::
++  etch                                                ::  parse wire
  |=  way=wire
  ^-  weir
  ?+    -.way  !!
      %friend 
    ?>  ?=([%show @ @ @ ~] t.way)
    [%friend i.t.t.way (slav %p i.t.t.t.way) i.t.t.t.t.way]
  ::
      %repeat
    ?>  ?=([@ @ @ ~] t.way)
    [%repeat (slav %ud i.t.way) (slav %p i.t.t.way) i.t.t.t.way]
  ==
::
++  etch-friend                                         ::
  |=  [way=wire fun=$+([man=span cuz=station] [(list move) _+>])]
  =+  wer=(etch way)
  ?>(?=(%friend -.wer) (fun p.wer q.wer))
::
++  etch-repeat                                         ::
  |=  [way=wire fun=$+([num=@ud src=@p man=span] [(list move) _+>])]
  =+  wer=(etch way)
  ?>(?=(%repeat -.wer) (fun p.wer q.wer r.wer))
::
++  reap-friend                                         ::
  |=  [way=wire saw=(unit tang)]
  ^-  (quip move +>)
  ?~  saw  [~ +>]
  %+  etch-friend  [%friend way]  |=  [man=span cuz=station]
  ~&  [%reap-friend-fail man cuz u.saw]
  ra-abet:(ra-quit:ra man cuz)
::
++  quit-friend                                         ::
  |=  way=wire
  %+  etch-friend  [%friend way]  |=  [man=span cuz=station]
  ra-abet:(ra-retry:ra man cuz)
::
++  pull                                                ::
  |=  [pax=path]
  ^-  [(list move) _+>]
  ~&  [%talk-pull src.hid ost.hid pax]
  =^  moz  +>.$  ra-abet:(ra-cancel:ra src.hid pax)
  [moz +>.$(shells (~(del by shells) ost.hid))]
::
++  log-all-to-file
  ^-  (quip move .)
  ?:  &  [~ .]  ::  XXX!!!!
  :_  %_  .
        log   %-  ~(urn by log)
              |=([man=span len=@ud] count:(~(got by stories) man))
      ==
  %+  murn  (~(tap by log))
  |=  [man=span len=@ud]
  ^-  (unit move)
  ?:  (gte len count:(~(got by stories) man))
    ~
  `(log-to-file man)
::
++  log-to-file
  |=  man=span
  ^-  move
  =+  ^-  paf=path
      =+  day=(year %*(. (yore now.hid) +.t +:*tarp))
      %+  tope  [our.hid %home da/now.hid]
      /talk-telegrams/(scot %da day)/[man]/talk
  =+  grams:(~(got by stories) man)
  [ost.hid %info /jamfile our.hid (foal paf [%talk-telegrams !>(-)])]
::
++  poke-save
  |=  man=span
  ^-  (quip move +>)
  =+  paf=/(scot %p our.hid)/home/(scot %da now.hid)/talk/[man]/talk-telegrams
  =+  grams:(~(got by stories) man)
  [[ost.hid %info /jamfile our.hid (foal paf [%talk-telegrams !>(-)])]~ +>.$]
::
++  poke-load
  |=  man=span
  =+  ^=  grams
      %-  (hard (list telegram))
      .^  %cx
          /(scot %p our.hid)/home/(scot %da now.hid)/talk/[man]/talk-telegrams
      ==
  =+  toy=(~(got by stories) man)
  [~ +>.$(stories (~(put by stories) man toy(grams grams, count (lent grams))))]
::
++  poke-log
  |=  man=span
  ~&  %poke-log

  ^-  (quip move +>)
  :-  [(log-to-file man) ~]
  +>.$(log (~(put by log) man count:(~(got by stories) man)))
::
++  poke-unlog
  |=  man=span
  ^-  (quip move +>)
  :-  ~
  +>.$(log (~(del by log) man))
::
++  prep
  |=  [old=(unit house-any)]
  ^-  (quip move +>)
  ?~  old
    ra-abet:ra-init:ra
  |-
  ?-  -.u.old
    %1  $(u.old [%2 stories general outbox folks shells ~]:u.old)
    %2  $(u.old [%3 stories general outbox folks shells log ~ ~]:u.old)
    %3  [~ +>.^$(+<+ u.old)]
  ==
--
