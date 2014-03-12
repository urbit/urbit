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
++  cred                                                ::  credential
  $:  sec=?                                             ::  https in url
      hut=?                                             ::  host defines ship
      aut=(map ,@ta (list ,@t))                         ::  client identities
      hot=host                                          ::  parsed host
      orx=oryx                                          ::  CSRF secret
  ==                                                    ::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      rey=[p=@ud q=(map ,@ud pimp)]                     ::  live requests
  ==                                                    ::
++  dude  ,[p=@tas q=@]                                 ::  client identity
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
      wup=(map logo cyst)                               ::  secure sessions
  ::  wez=(map duct root)                               ::  all routes
  ==                                                    ::
--                                                      ::
|%
++  coss                                                ::  cookie search
  |=  [nam=@t mah=math]
  ^-  (unit logo)
  =+  ^=  cok  ^-  (list ,@t)
      =+  cok=(~(get by mah) 'cookie')
      ?~(cok ~ u.cok) 
  |-  ^-  (unit logo)
  ?~  cok  ~
  ~&  [%cookie i.cok]
  =+  mar=`(unit (list ,[p=@t q=@t]))`(rush i.cok cock:epur)
  ?~  mar  $(cok t.cok)
  |-  ^-  (unit logo)
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
::
++  lobo                                                ::  vase to love
  |=  [ext=@tas vax=vase]
  ^-  (unit love)
  ?+    ext  ~
      %html  
    ?.  (~(nest ut [%atom %$]) | p.vax)  ~
    ?>  ?=(@ q.vax)
    [~ %mid /text/html (met 3 q.vax) q.vax]
  ==
::
++  loup                                                ::  weak synthesis
  |=  [ext=@tas toe=tube]
  ~&  [%loup ext toe]
  =+  pre=`path`[p.toe q.toe r.toe ~]
  =|  suf=path
  |-  ^-  (unit gank)
  =+  pax=:(weld pre s.toe `path`~[ext])
  =+  arc=((hard arch) .^(%cy pax))
  ?^  q.arc
    =+  fil=.^(%cx (weld pax `path`~[ext]))
    [~ %& ^-(vase [?@(fil [%atom %$] %noun) fil])]
  ?:  (~(has by r.arc) %hoon)
    (mush pax !>([ext pre s.toe `path`(flop suf)]))
  ?~  s.toe  ~
  $(s.toe t.s.toe, suf [i.s.toe suf])
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
      ~&  [%this purl pul]
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
        =+  gow=(rush (cat 3 '~' i.q.q.pul) fed:ag)
        ^-  [(unit ship) (list ,@t)]
        ?~(gow [~ q.q.pul] [gow t.q.q.pul])
    (huff ?^(wiq wiq (doss r.p.pul)) ?=(^ wiq) pul moh)
  ::
  ++  huff                                              ::  request by ship
    |=  [oar=(unit ship) hut=? pul=purl moh=moth]
    ^+  +>
    =*  sec  p.p.pul
    ?~  oar
      (fail 400 "urbit: url does not match a vessel")
    =+  ^=  sef  ^-  serf
        =+  suf=(~(get by own) u.oar)
        ?^  suf  u.suf
        =+  sef=*serf
        sef(pef (cat 3 gub (rsh 3 1 (scot %p u.oar))))
    =+  ^=  saw  ^-  [p=logo q=cyst]
        =+  lig=(coss pef.sef q.moh)
        ?^  lig  
          =+  cyz=(need (~(get by wup.sef) u.lig))
          [u.lig cyz(cug ~)]
        =+  ses=(rsh 6 1 (scot %p (end 6 1 ney)))
        :-  ses
        ^-  cyst
        :*  ^-  cred
            :*  sec
                hut
                ~            
                r.p.pul
                ?.(sec %$ (rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses)))))
            ==
        ::
            :_  ~
            %^  cat  3
              (cat 3 (cat 3 pef.sef '=') ses)
            (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
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
    =|  [[our=ship ses=logo] serf cyst]
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
    ++  funk                                            ::  functional request
      |=  [imp=? uxt=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      ?~  paw  
        ~&  %no-funk
        ~
      ?.  ((sane %tas) i.paw)  
        ~&  %mad-desk
        ~
      =^  cuz  t.paw
          ^-  [(unit ,@ta) (list ,@t)]
          ?:  imp
            [[~ (scot %da now)] t.paw]
          ?~  t.paw
            [~ ~]
          =+  zac=(slay i.t.paw)
          ?.  ?=([~ %$ ?(%ud %da %tas) *] zac)  [~ ~]
          [[~ i.t.paw] t.t.paw]
      ?~  cuz  
        ~&  %funk-case
        ~
      ?.  (levy t.paw (sane %ta))  
        ~&  %funk-path 
        ~
      :+  ~  %fun
      :-  ?~(uxt %html u.uxt)
      ^-  tube
      :*  (scot %p our)
          i.paw
          u.cuz
          %+  weld
            `path`t.paw
          ^-  path
          :_  ~
          %~  rent  co
          :~  %many
              [%blob ced]
              :-  %many
              %+  turn  quy 
              |=  [a=@t b=@t]
              `coin`[%many [%$ %t a] [%$ %t b] ~]
          == 
      ==
    ::
    ++  holy                                            ::  structured request
      |=  [pul=purl moh=moth]
      ^-  (unit seam)
      ~&  [%holy purl]
      ?~  q.q.pul  ~
      =*  nep  i.q.q.pul
      =*  paw  t.q.q.pul
      =+  [one=(end 3 1 nep) two=(cut 3 [1 1] nep) tri=(cut 3 [2 1] nep)]
      ?.  ?&  ?-  p.moh
                %conn  |
                %delt  |
                %get   =(%g one)
                %head  =(%g one)
                %opts  |
                %post  =(%p one)
                %put   =(%t one)
                %trac  |
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
        ~&  [%holy-bad nep]
        ~
      ?-  tri
        ?(%f %n)     (funk =(%n tri) p.q.pul paw r.pul)
        ?(%a %c %l)  !!
      ==
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
          :_  +>.$
          :-  ~
          %=    pip
              pez
            ^-  pest
            =+  syt=(loup p.sam.pip q.sam.pip)
            ?~  syt
              [%err 404 [[%leaf "not found"] ~]]
            ?-    -.u.syt
                |  [%err 500 p.u.syt]
                &
              =+  luy=(lobo p.sam.pip p.u.syt)
              ?~  luy  [%err 500 [[%leaf "inappropriate file"] ~]]
              [%fin u.luy]
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
  =.  gub  ?.(=(0 gub) gub (cat 3 (scot %p (end 6 1 eny)) '-'))
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
++  stay  `vase`!>(`bolo`+>-.$)
++  vern  [164 0]
--
