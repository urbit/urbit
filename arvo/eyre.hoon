!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
^-  vane                                                ::  kernel instrument
=>  =~
|%                                                      ::  structures
++  bolo                                                ::  eyre state
  $:  gub=@t                                            ::  random identity
      ged=duct                                          ::  client interface
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      own=(map ship serf)                               ::  live servers
      ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]         ::  outgoing by number
      kes=(map duct ,@ud)                               ::  outgoing by duct
  ==                                                    ::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      rey=[p=@ud q=(map ,@ud pimp)]                     ::  live requests
  ==                                                    ::
++  dude  ,[p=@tas q=@]                                 ::  client identity
++  loco  ,[p=? q=(unit ,@tas) r=path]                  ::  logical construct
++  pest                                                ::  request in progress
  $|  $?  %new                                          ::  virgin
      ==                                                ::  
  $%  [%err p=@ud q=(list tank)]                        ::  error report
      [%fin p=love]                                     ::  ready to send
      [%raw p=hiss]                                     ::  wild url
  ==                                                    ::
++  pimp                                                ::  traced request
  $:  ful=?                                             ::  | === HEAD
      hen=duct                                          ::  event trace
      sam=seam                                          ::  logical request
      pez=pest                                          ::  request state
  ==                                                    ::
++  serf                                                ::  servant per ship
  $:  pef=@t                                            ::  server prefix
      wup=(map hole cyst)                               ::  secure sessions
      cah=(map cash vase)                               ::  compilation cache
  ::  wez=(map duct root)                               ::  all routes
  ==                                                    ::
--                                                      ::
|%
++  colt                                                ::  prune to save
  |=  bol=bolo
  %_(bol own (~(run by own.bol) |=(a=serf a(cah ~))))
::
++  coss                                                ::  cookie search
  |=  [nam=@t mah=math]
  ^-  (unit hole)
  =+  ^=  cok  ^-  (list ,@t)
      =+  cok=(~(get by mah) 'cookie')
      ?~(cok ~ u.cok) 
  |-  ^-  (unit hole)
  ?~  cok  ~
  =+  mar=`(unit (list ,[p=@t q=@t]))`(rush i.cok cock:epur)
  ?~  mar  $(cok t.cok)
  |-  ^-  (unit hole)
  ?~  u.mar  ^$(cok t.cok)
  ?:(=(nam p.i.u.mar) [~ q.i.u.mar] $(u.mar t.u.mar))
::
++  ecco                                                ::  eat headers
  |=  hed=(list ,[p=@t q=@t])
  =+  mah=*math
  |-  ^-  math
  ?~  hed  mah
  =+  cus=(cass (rip 3 p.i.hed))
  =+  zeb=(~(get by mah) cus)
  $(hed t.hed, mah (~(put by mah) cus ?~(zeb [q.i.hed ~] [q.i.hed u.zeb])))
::
++  loft                                                ::  love to response
  |=  luv=love
  ^-  httr
  ?-  -.luv
    %mid  [200 ~[content-type/(moon p.luv)] [~ q.luv]]
    %ham  [200 ~[content-type/'text/html'] [~ (tact (xmlt p.luv ~))]]
    %raw  p.luv
    %wan  !! 
    %zap  :+  p.luv 
            ~[content-type/'text/plain']
          :-  ~
          %-  tell
          |-  ^-  wall 
          ?~  q.luv  ~ 
          (weld (~(win re i.q.luv) 0 120) $(q.luv t.q.luv))
  ==
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  $:  wru=(unit writ)                       ::  event authority 
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::  
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet
    ^-  [(list move) bolo]
    [(flop mow) bol]
  ::
  ++  adit
    .(ney (mix eny ney))
  ::
  ++  apex
    |-  ^+  +
    ?+    -.fav  
      ~|([%ye-bad -.fav] !!)
    ::
        %born  +(ged hen)                               ::  register external
        %that                                           ::  outbound response
      ?>  ?=([@ @ @ ~] tea)                             ::
        =+  :*  our=(slaw %p i.tea)                     ::  ship
                ses=i.t.tea                             ::  session
                num=(slaw %ud i.t.t.tea)                ::  request in session
          ==                                            ::
      !!
    ::
        %them                                           ::  outbound request
      ?~  p.fav
        =+  sud=(need (~(get by kes) hen))
        %=  +.$
          mow    :_(mow [~ ged [%thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      %=  +.$
        mow    :_(mow [~ ged [%thus p.ask p.fav]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.fav)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        %they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.fav))
      %=  +.$
        mow    :_(mow [~ p.kas [%thou q.fav]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        %this                                           ::  inbound request
      =*  sec  p.fav    ::  ?                           ::  https bit
      =*  heq  r.fav    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  mah=(ecco r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            &  ?>(=(sec p.p.p.ryp) p.ryp)
            |  =+  hot=(~(get by mah) %host)
               ?>  ?=([~ @ ~] hot)
               [[sec (rash i.u.hot thor:epur)] p.ryp q.ryp]
          ==
      (hell pul [p.heq mah s.heq])
    ==
  ::
  ++  doss                                              ::  host to ship
    |=  hot=host
    ^-  (unit ship)
    =+  gow=(~(get by dop) hot)
    ?^  gow  gow
    ?.  &(?=(& -.hot) ?=(^ p.hot))  ~
    (rush (cat 3 '~' i.p.hot) fed:ag)
  ::
  ++  fail                                              ::  request failed
    |=  [sas=@ud str=tape]
    ^+  +>
    %-  muff
    :-  %thou
    ^-  httr
    [sas ~[content-type/'text/plain'] [~ (tact str)]]
  ::
  ++  hell                                              ::  request, no ship
    |=  [pul=purl moh=moth]
    ^+  +>
    =^  wiq  q.q.pul
        ?~  q.q.pul  [~ ~]
        =+  nam=(cat 3 '~' i.q.q.pul)
        =+  gow=(rush i.q.q.pul fed:ag)
        ^-  [(unit ship) (list ,@t)]
        ?~(gow [~ q.q.pul] [gow t.q.q.pul])
    (huff ?^(wiq wiq (doss r.p.pul)) ?=(@ wiq) pul moh)
  ::
  ++  huff                                              ::  request by ship
    |=  [oar=(unit ship) hey=? pul=purl moh=moth]
    ^+  +>
    =*  sec  p.p.pul
    ?~  oar
      (fail 400 "urbit: url does not match a vessel")
    =+  ^=  sef  ^-  serf
        =+  suf=(~(get by own) u.oar)
        ?^  suf  u.suf
        =+  sef=*serf
        sef(pef (cat 3 gub (rsh 3 1 (scot %p u.oar))))
    =+  ^=  saw  ^-  [p=hole q=cyst]
        =+  lig=(coss pef.sef q.moh)
        ?^  lig
          =+  cyz=(need (~(get by wup.sef) u.lig))
          [u.lig cyz(cug ~)]
        =+  ses=(rsh 3 1 (scot %p (end 6 1 ney)))
        :-  ses
        ^-  cyst
        :*  ^-  cred
            :*  [sec hey r.p.pul]
                ~
                (rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
                ~
                [%& .0.0.0.0]
                ~
            ==
        ::
            :_  ~
            %^  cat  3
              (cat 3 (cat 3 pef.sef '=') ses)
            ::  (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
            '; HttpOnly'
        ::
            now
            [1 ~]
        ==
    abet:work:(~(into ya [u.oar p.saw] sef q.saw) pul moh)
  ::
  ++  muff                                              ::  return card
    |=  fav=card
    +>(mow :_(mow [wru hen fav]))
  ::
  ++  ya                                                ::  session engine
    =|  [[our=ship ses=hole] serf cyst]
    =*  sef  ->-
    =*  cyz  ->+
    |%
    ++  abet                                            ::  resolve engine
      ^+  ..ya
      %=    ..ya
          own
        (~(put by own) our sef(wup (~(put by wup) ses cyz)))
      ==
    ::
    ++  flux                                            ::  credential caboose
      |=  quy=quay
      ^-  coin
      :*  %many
          [%blob ced]
          |-  ^-  (list coin)
          ?~  quy  ~
          [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
      == 
    ::
    ++  funk                                            ::  functional request
      |=  [imp=? fur=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      %+  bind
        ^-  (unit ,[logo tube])
        =+  ^=  zac  ^-  (unit ,[p=@ta q=path])
            ?:  imp
              [~ (scot %da now) paw]
            ?~  paw  ~
            =+  zac=(slay i.paw)
            ?.  ?=([~ %$ ?(%ud %da %tas) *] zac)  ~
            [~ i.paw t.paw]
        ?:  ?|  ?=(~ zac) 
                ?=(~ q.u.zac) 
                !(levy t.q.u.zac (sane %ta))
            ==  ~
        :+  ~  ?~(fur %html u.fur)
        ^-  tube
        :*  (scot %p our) 
            i.q.u.zac 
            p.u.zac 
            t.q.u.zac
        ==
      |=  [for=logo toe=tube]
      ^-  seam
      [%fun for toe(s (weld s.toe `path`~[~(rent co (flux quy))]))]
    ::
    ++  holy                                            ::  structured request
      |=  [pul=purl moh=moth]
      ^-  (unit seam)
      ?~  q.q.pul  ~
      =*  nep  i.q.q.pul
      =*  paw  t.q.q.pul
      =+  [one=(end 3 1 nep) two=(cut 3 [1 1] nep) tri=(cut 3 [2 1] nep)]
      ?.  ?&  ?-  p.moh
                %conn  |                                ::  connect
                %delt  |                                ::  delete
                %get   =(%g one)                        ::  get
                %head  =(%g one)                        ::  head
                %opts  |                                ::  options
                %post  =(%p one)                        ::  post
                %put   =(%t one)                        ::  put
                %trac  |                                ::  trace
              ==
              ?+  two  ~
                %e  &                                   ::  stranger
                %g  p.p.pul                             ::  guest
                %n  !=(~ aut.ced)                       ::  neighbor
                %s  =+  urb=(~(get by aut.ced) %$)      ::  self
                    ?~(urb | (levy u.urb |=(a=@ =(our a))))
              ==
              ?=  $?  %a                                ::  application
                      %c                                ::  console
                      %f                                ::  functional
                      %l                                ::  login
                      %n                                ::  now
                  ==
                  tri
              =(3 (met 3 nep))
          ==
        ~
      ?-  tri
        ?(%f %n)     (funk =(%n tri) p.q.pul paw r.pul)
        ?(%a %c %l)  !!
      ==
    ::
    ++  lace                                            ::  load and execute
      |=  [pax=path sam=vase]
      ^-  [gank _+>]
      =^  hum  +>.$  (lack pax)
      :_  +>.$
      =+  mud=(need hum)
      ?:  ?=(| -.mud)  mud
      (mule |.((slam p.mud sam)))
    ::
    ++  lack                                            ::  probe/load
      |=  pax=path
      ^-  [(unit gank) _+>]
      =+  ans=(sky %cz pax)
      ?~  ans  [~ +>.$]
      =+  ank=((hard ankh) u.ans)
      ?~  q.ank  [~ +>.$]
      =+  huc=(~(get by cah.sef) p.u.q.ank)
      ?^  huc  
        [[~ %& u.huc] +>.$]
      =+  mud=(much pax q.u.q.ank)
      :-  [~ mud]
      ?:  ?=(| -.mud)  +>.$  
      +>.$(cah.sef (~(put by cah.sef) p.u.q.ank p.mud))
    ::
    ++  lend                                            ::  load directory node
      |=  pax=path
      ^-  arch
      ((hard arch) (need (sky %cy pax)))
    ::
    ++  liar                                            ::  load file as vase
      |=  pax=path
      ^-  vase
      =+  fil=(lick pax)
      :_(fil ?^(fil [%cell %noun %noun] [%atom %$]))
    ::
    ++  lich                                            ::  simple directory
      |=  pax=path
      ^-  (list ,@tas)
      (turn (~(tap by r:(lend pax)) ~) |=([a=@tas b=~] a))
    ::
    ++  lick                                            ::  load file
      |=  pax=path
      (need (sky %cx pax))
    ::
    ++  lily                                            ::  translation targets
      |=  [pre=path for=@tas]
      ^-  (list ,@tas)
      (lich :(weld pre `path`/tan `path`/[for]))
    ::
    ++  lion                                            ::  translation graph
      |=  [too=@tas pre=path fro=(list ,@tas)]
      ^-  (unit (list ,@tas))
      =|  war=(set ,@tas)
      =<  -:(apex fro)
      |%
      ++  apex
        |=  rof=(list ,@tas)
        ^-  [(unit (list ,@tas)) _+>]
        ?~  rof
          [~ +>]
        =^  orf  +>  (apse i.rof)
        ?^(orf [orf +>.$] $(rof t.rof))
      ::
      ++  apse
        |=  for=@tas
        ^-  [(unit (list ,@tas)) _+>]
        ?:  =(for too)  [[~ [too ~]] +>]
        ?:  (~(has in war) for)  [~ +>]
        =.  war  (~(put in war) for)
        =^  orf  +>.$  (apex (lily pre for))
        :_  +>.$
        ?~(orf ~ [~ [for u.orf]])
      --
    ::
    ++  link                                            ::  translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [(unit gank) _+>]
      ?:  =(for too)  [[~ %& vax] +>.$]
      =+  wuy=(lion too pre [for ~])
      ?~  wuy  [~ +>.$]
      ?>  ?=(^ u.wuy)
      ?>  =(for i.u.wuy)
      |-  ^-  [(unit gank) _+>.^$]
      ?~  t.u.wuy  [[~ %& vax] +>.^$]
      =^  mud  +>.^$  (lite i.t.u.wuy pre for vax)
      ?:  ?=(| -.mud)  [[~ mud] +>.^$]
      $(t.u.wuy t.t.u.wuy, for i.t.u.wuy, vax p.mud)
    ::
    ++  lino                                            ::  translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [gank _+>]
      =^  gun  +>.$  (link too pre for vax)
      :_  +>.$
      ?^  gun  u.gun
      [%| [[%leaf "can't make {<too>} from {<for>}"] ~]]
    ::
    ++  lite                                            ::  step translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [gank _+>]
      (lace :(weld pre `path`/tan `path`/[for] `path`/[too] `path`/hoon) vax)
    ::
    ++  loan                                            ::  normalize vase
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  mof  +>.$  (lack :(weld pre `path`/nor `path`/[for] `path`/hoon))
      :_  +>.$
      ?~  mof  [%& vax]
      ?:  ?=(| -.u.mof)  u.mof
      =+  pud=(mule |.((~(play ut `type`p.p.u.mof) [%cnzy %$])))
      ?:  ?=(| -.pud)  pud
      ?:  (~(nest ut `type`p.pud) | p.vax)
        [%& vax]
      (mule |.((slam `vase`p.u.mof vax)))
    ::
    ++  lobo                                            ::  vase to love
      |=  [for=logo pre=path vax=vase]
      ^-  [(each love (list tank)) _+>]
      =^  mud  +>.$  (lino %mime pre for vax)
      :_  +>.$
      ?:  ?=(| -.mud)  mud
      [%& %mid (mite -.q.p.mud) (octs +.q.p.mud)]
    :: 
    ++  loch                                            ::  validate vase
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  wav  +>.$  (lack :(weld pre `path`/val `path`/[for] `path`/hoon))
      :_  +>.$
      ?~  wav  [%& vax]
      ?:  ?=(| -.u.wav)  u.wav
      (mule |.((slam `vase`p.u.wav vax)))
    ::
    ++  loot                                            ::  load extension tree
      |=  [pax=path one=(unit logo)]
      ^-  (list path)
      =|  [tex=path all=(list path)]
      |-  ^-  (list path)
      ?^  one
        =+  don=`path`[u.one ~]
        =+  arc=(lend (weld pax don))
        ?~(q.arc ~ [[u.one tex] ~])
      =+  arc=(lend pax)
      =+  ryx=(~(tap by r.arc) ~)
      =-  ?~(q.arc orx [tex orx])
      ^=  orx
      |-  ^-  (list path)
      ?~  ryx  all
      %=  ^$
        one  [~ %hoon]
        pax  (weld pax `path`[p.i.ryx ~])
        tex  [p.i.ryx tex]
        all  $(ryx t.ryx)
      ==
    ::
    ++  lope                                            ::  normalize/validate
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (loan for pre vax)
      ?:  ?=(| -.mud)  [mud +>.$]
      (loch for pre p.mud)
    ::
    ++  loth                                            ::  direct hard
      |=  [for=logo pre=path pax=path]
      ^-  [gank _+>]
      (lope for pre (liar pax))
    ::
    ++  loti                                            ::  translated soft
      |=  [too=logo for=logo pre=path pax=path sam=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (loto for pre pax sam)
      ?:  ?=(| -.mud)  [mud +>.$]
      (lino too pre for p.mud)
    ::
    ++  loto                                            ::  direct soft
      |=  [for=logo pre=path pax=path sam=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (lace pax sam)
      ?:  ?=(| -.mud)  [mud +>.$]
      (lope for pre p.mud)
    ::
    ++  lots                                            ::  translated hard
      |=  [too=logo for=logo pre=path pax=path]
      ^-  [gank _+>]
      =^  mud  +>.$  (lope for pre (liar pax))
      ?:  ?=(| -.mud)  [mud +>.$]
      (lino too pre for p.mud)
    ::
    ++  loud                                            ::  synthesis search
      |=  [syn=? for=logo pre=path mid=path]
      ^-  (list ,[p=path q=path r=loco])
      =|  suf=path 
      |-  ^-  (list ,[p=path q=path r=loco])
      =+  pax=(weld pre (flop mid))
      =+  lot=(loot pax ?:(syn ~ [~ for]))
      =-  ?^  tol  tol
          ?~  mid  ~
          $(mid t.mid, suf [i.mid suf])
      ^=  tol
      |-  ^-  (list ,[p=path q=path r=loco])
      ?~  lot  ~
      =+  mor=$(lot t.lot)
      ?~  i.lot  mor
      =+  axp=(weld pax `path`(flop i.lot))
      ?:  &(syn ?=([%hoon @ ~] i.lot))
        :_(mor [mid suf | ?:(=(for i.t.i.lot) ~ [~ i.t.i.lot]) axp])
      ?:  ?=([@ ~] i.lot)
        :_(mor [mid suf & ?:(=(for i.i.lot) ~ [~ i.i.lot]) axp])
      mor
    ::
    ++  loup                                            ::  weak synthesis
      |=  [for=logo pre=path mid=path]
      ^-  [(unit gank) _+>]
      =+  syt=(weld pre `path`[%syn ~])
      =+  ^=  luc  ^-  (list ,[p=path q=path r=loco])
          =+  luc=(loud | for pre mid)
          ?.  ?=(~ luc)  luc
          (loud & for syt mid)
      ?:  =(~ luc)  [~ +>.$]
      =+  ^=  waz
          |-  ^-  $:  p=(list ,[p=path q=path r=path])
                      q=(list ,[p=path q=path r=path])
                      r=(list ,[p=path q=path r=[p=@tas q=path]])
                      s=(list ,[p=path q=path r=[p=@tas q=path]])
                  ==
          ?~  luc  [~ ~ ~ ~]
          =+  mor=$(luc t.luc)
          ?-  -.r.i.luc
            &  ?~  q.r.i.luc 
                 [[[p.i.luc q.i.luc r.r.i.luc] p.mor] q.mor r.mor s.mor]
               :+  p.mor  q.mor 
               [[[p.i.luc q.i.luc u.q.r.i.luc r.r.i.luc] r.mor] s.mor]
            |  ?~  q.r.i.luc 
                 [p.mor [[p.i.luc q.i.luc r.r.i.luc] q.mor] r.mor s.mor]
               :+  p.mor  q.mor
               [r.mor [[p.i.luc q.i.luc u.q.r.i.luc r.r.i.luc] s.mor]]
          ==
      =^  mud  +>.$
        ?^  p.waz                                       ::  direct hard
          (loth for pre r.i.p.waz)
        ?^  q.waz                                       ::  direct soft
          %-  loto
          :*  for
              pre
              r.i.q.waz
              !>([for pre p.i.q.waz q.i.q.waz])
          ==
        ?^  r.waz                                       ::  translated hard
          (lots for p.r.i.r.waz pre q.r.i.r.waz)
        ?^  s.waz                                       ::  translated soft
          %-  loti
          :*  for
              p.r.i.s.waz
              pre
              q.r.i.s.waz
              !>([for pre p.i.s.waz q.i.s.waz])
          ==
        !!
      [[~ mud] +>.$]
    ::
    ++  lude                                            ::  functional synth
      |=  [for=logo toe=tube]
      ^-  [(unit (each love (list tank))) _+>]
      =+  [pre mid]=[`path`[p.toe q.toe r.toe ~] `path`(flop s.toe)]
      =^  gun  +>.$  (loup for pre mid)
      ?~  gun  [~ +>.$]
      ?:  ?=(| -.u.gun)  :_(+>.$ [~ %| p.u.gun])
      =^  mun  +>.$  (lobo for pre p.u.gun)
      [[~ mun] +>.$]
    ::
    ++  step                                            ::  step in work
      |-  ^+  +
      =^  zib  +.$
          =+  yub=q.rey
          |-  ^-  [(list ,[p=@ud q=pimp]) _+.^$]
          ?~  yub  [~ +.^$]
          =^  sin  +.^$  $(yub l.yub)
          =^  dex  +.^$  $(yub r.yub) 
          =^  top  +.^$  (wink q.n.yub)
          =+  pot=`(list ,[p=@ud q=pimp])`?~(top ~ [[p.n.yub u.top] ~])
          [:(weld pot dex sin) +.^$]
      +.$(q.rey (~(gas by `_q.rey`~) zib))
    ::
    ++  wink                                            ::  advance request
      |=  pip=pimp
      ^-  [(unit pimp) _+>]
      ?-    pez.pip
          %new
        ?-    -.sam.pip
            %fun
          =^  syt  +>.$  (lude p.sam.pip q.sam.pip)
          :_  +>.$  
          :-  ~
          %=    pip
              pez
            ^-  pest
            ?~  syt
              [%err 404 [[%leaf "{<+.sam.pip>} not found"] ~]]
            ?-  -.u.syt
              |  [%err 500 (flop p.u.syt)]
              &  [%fin p.u.syt]
            ==
          ==
        ==
      ::
          [%err *]
        [~ +>.$(..ya (muff [%thou (loft `love`[%zap +.pez.pip])]))]
      ::
          [%fin *]
        =+  har=(loft p.pez.pip)
        =.  q.har  (weld (turn cug |=(a=@t ['set-cookie' a])) q.har)
        [~ +>.$(..ya (muff [%thou har]))]
      ::
          [%raw *]
        :_  +>.$
        ^-  (unit pimp)
        :-  ~
        =+  hoy=(holy p.pez.pip)
        ?~  hoy
          pip(pez [%err 404 [[%leaf "invalid request"] ~]])
        pip(sam u.hoy, pez %new)
      ==
    ::
    ++  work
      |-  ^+  +
      =+  sez=step
      ?:  =(rey.sez rey)  sez
      $(+ sez)
    ::
    ++  into
      |=  [pul=purl moh=moth]  
      ^+  +>
      =+  num=p.rey
      %=    +>.$
          p.rey  +(num)
          q.rey
        %+  ~(put by q.rey)  num
        ^-  pimp
        :*  !?=(%head p.moh)
            hen
            *seam
            `pest`[%raw pul moh]
        ==
      ==
    --
  --
--
.  ==
=|  bolo
=*  bol  -
|=  [now=@da eny=@ sky=$+(* (unit))]                    ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ?:  ?=(%crud -.fav)
    [[[wru [/d hen] %flog fav] ~] ..^$]
  ^-  [p=(list move) q=vane]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol  
    abet:apex:~(adit ye [[wru tea hen fav] [now eny sky] ~] bol)
  [mos ..^$]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  new=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`bolo`+>-.^$)) | p.new)  
    ~&  %eyre-reset
    ..^$
  ..^$(+>- (bolo q.new))
::
++  raze
  ^-  vane
  ..$(+>- *bolo)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay  
  `vase`!>((colt `bolo`+>-.$))
++  vern  [164 0]
--
