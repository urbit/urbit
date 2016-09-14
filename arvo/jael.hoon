!:                                                      ::  /van/jael
::                                                      ::  %orthodox
!?  150
  |=  pit/vase  ::  XX remove from arvo api
::                                                      ::::
::::  ## 1                                              ::  structures
  ::                                                    ::::
=>  |%
++  jael-state                                          ::  all crypto state
  $:  ver/$0                                            ::  %jael version 
      nav/jael-objective                                ::  all universal state
  ::  nix/jael-subjective                               ::  all derived state
  ==                                                    ::
++  jael-objective                                      ::  all universal state
  $:  urb/jael-urbit                                    ::  all urbit state
      web/(map site jael-web-domain)                    ::  all web state
  ==                                                    ::
++  jael-web-domain                                     ::  per foreign app
  $:  sec/(map @t jael-web-app)                         ::  client per api key
      usr/(map @ta jael-web-user)                       ::  direct user info
  ==                                                    ::
++  jael-web-app                                        ::  local app
  $:  key/(unit (pair @da @))                           ::  API key
      tok/(map @t (pair @da @))                         ::  token by username
  ==                                                    ::
++  jael-web-user                                       ::  per-user secrets
  $:  pas/(unit @t)                                     ::  password
      dey/(unit @t)                                     ::  display name
  ==                                                    ::
++  jael-urbit                                          ::  objective urbit
  $:  pug/gree                                          ::  all public state
      pry/(map ship jael-ship)                          ::  all private state
  ==                                                    ::
++  jael-ship                                           ::  objective by ship
  $:  rel/(map ship jael-friend)                        ::  relationships
      own/(map life ring)                               ::  private keys
      vew/(set duct)                                    ::  watchers
  ==                                                    ::
++  jael-friend                                         ::  relationship 
  $:  luf/(unit life)                                   ::  life as known to
      lab/(nap jael-right)                              ::  promises to
      vow/(set duct)                                    ::  watchers
  ==                                                    ::
++  jael-gift                                           ::  output
  $?  {$cash jael-report-cash}                          ::  asset dump
      {$clue jael-report-them}                          ::  channel dump
      {$paid jael-report-paid}                          ::  asset update
      {$self jael-report-self}                          ::  self dump
      {$well jael-web-domain}                           ::  service update
  ==                                                    ::
++  jael-right                                          ::  urbit commitment
  $%  {$block p/pile}                                   ::  reserved block
      {$email p/(set @ta)}                              ::  email addresses
      {$entry p/(map hand (pair @da code))}             ::  symmetric keys
      {$final p/(map ship @uvG)}                        ::  ticketed ships
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$guest $~}                                       ::  refugee visa
      {$lived p/life}                                   ::  PKI commitment
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$give p/ship q/(nap jael-right)}                 ::  add rights
      {$line p/ship q/@da r/code}                       ::  outbound symkey
      {$link p/ship q/@da r/code}                       ::  inbound symkey
      {$meet p/ship q/gree}                             ::  integrate pki from
      {$over p/ship q/jael-task}                        ::  mirror operation
      {$pall p/ship q/life}                             ::  our life acked
      {$ping p/ship}                                    ::  empty contact
      {$step p/lamp q/ring}                             ::  update private key
      {$take p/ship q/(nap jael-right)}                 ::  subtract rights
      {$vain $~}                                        ::  watch self
      {$vest $~}                                        ::  watch assets
      {$view p/ship}                                    ::  watch urbit
      {$vile p/site}                                    ::  watch website
      {$west p/ship q/path r/*}                         ::  remote request
      {$wink p/site q/@t r/(unit bill)}                 ::  set web API key
      {$wish p/site q/@t r/(unit @t)}                   ::  set web login
      {$wonk p/site q/@t r/@t s/(unit bill)}            ::  set web API token
  ==                                                    ::
++  jael-report-them                                    ::  report on neighbor
  $:  gur/grue                                          ::  certificate
      lab/(nap jael-right)                              ::  our promises to
      own/(nap jael-right)                              ::  our promises from
  ==                                                    ::
++  jael-report-self                                    ::  report on self
  $:  gur/grue                                          ::  certificate
      war/(map life ring)                               ::  private keys
  ==                                                    ::
++  jael-report-cash                                    ::  neighbors/assets
  $:  has/(map ship (nap jael-right))                   ::
  ==                                                    ::
++  jael-report-paid                                    ::  asset diff
  $:  dif/(list (trel ship ? (nap jael-right)))         ::  who, +/-, what
  ==                                                    ::
++  jael-note                                           ::  out request $->
  $%  {$x $mess p/ship q/path r/*}                      ::  send message
  ==                                                    ::
++  jael-message                                        ::  p2p message
  $%  {$hail p/(nap jael-right)}                        ::  re/set rights
      {$ping p/gree}                                    ::  propagate
  ==                                                    ::
++  jael-edit                                           ::  pki change
  $:  why/?($hear $make)                                ::  import or create
      gut/jael-change                                   ::  new information
  ==                                                    ::
++  jael-effect                                         ::  objective effect
  $%  {$kick p/ship}                                    ::  ship key update
      {$kids p/ship q/gree}                             ::  propagate to kids of
      {$lord p/ship q/gree}                             ::  to parent of
      {$pals p/ship q/gree}                             ::  to peers of
      {$seat p/site q/@ta}                              ::  webapp key update
      {$site p/site}                                    ::  website key update
      {$slap p/ship q/ship}                             ::  channel key update
  ==                                                    ::
++  jael-change                                         ::  pki delta
  $%  {$step p/ship q/life r/lace}                      ::  new deed
      {$sure p/ship q/life r/mind s/@}                  ::  new signature
  ==                                                    ::
++  jael-move                                           ::  output
  {p/duct q/(wind jael-note jael-gift)}
--
::                                                      ::::
::::  ## 2                                              ::  static data
  ::                                                    ::::
=>  |%
::                                                      ::  zeno
++  zeno                                                ::  boot signatures
  |=  ::  who: galaxy (0-255)
      ::
      who/ship
  ^-  pass
  !!
--
::                                                      ::::
::::   ## 3                                             ::  reactors
  ::                                                    ::::
=>  |%
=>  |%
::                                                      ::  of
++  of                                                  ::  general reactor
  =|  $:  ::  sys: system context                       ::::  #3.a
          ::
          ^=  sys
          $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now/@da
              eny/@e
          ==
          ::  all vane state
          ::
          jael-state
      ==
  ::  lex: all durable state
  ::  moz: pending actions
  ::
  =*  lex  ->+
  =|  moz/(list jael-move) 
  |%
  ::                                                    ::  abet:of
  ++  abet                                              ::  resolve
    [(flop moz) lex]
  ::                                                    ::  call:of
  ++  call                                              ::  invoke
    |=  $:  ::  hen: event cause
            ::  tac: event data
            ::
            hen/duct
            tac/jael-task
        ==
    ^+  +>
    ?-    -.tac
    ::
    ::  add rights
    ::    {$give p/ship q/(nap jael-right))}
    ::
        $give  
      (curd abet:(~(give ur urb.nav) our [p q]:tac))
    ::
    ::  outbound symmetric key
    ::    {$link p/ship q/@da r/code}
    ::
        $link
      =*  ryt  [%entry [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
      %-  curd
      abet:(~(give ur urb.nav) our p.tac ryt)
    ::
    ::  inbound symmetric key
    ::    {$line p/ship q/@da r/code}
    ::
        $line
      =*  ryt  [%entry [[(shaf %hand r.tac) q.tac r.tac] ~ ~]]
      %-  curd
      abet:(~(give ur sys urb.nav) p.tac our ryt)
    ::
    ::  public-key update
    ::    {$meet p/ship q/gree}
    ::
        $meet
      (curd abet:(~(meet ur sys urb.nav) p.tac q.tac))
    ::
    ::  learn as other ship
    ::    {$over p/ship q/jael-task}
    ::
        $over
      $(our p.tac, tac q.tac)
    ::
    ::  remote version acknowledgment
    ::    {$pall p/ship q/life}
    ::
        $pall
      abet:(pall:~(call ur urb.urb.nav) 
    ::
    ::  request incidental contact
    ::    {$ping p/ship}
    ::
        $ping
      (cure [%ping p.tac ~]~)
    ::
    ::  extend our certificate with a new private key
    ::    {$step p/lamp}
    ::
        $step 
      (curd abet:(~(step ur urb.nav) our p.tac))
    ::
    ::  remove rights
    ::    {$take p/ship q/(nap jael-right)}
    ::  
        $take
      (curd abet:(~(give ur urb.nav) our [p q]:tac))
    ::
    ::  monitor self
    ::    {$vain $~}
    ::
        $vain
      !!
    ::
    ::  monitor assets
    ::    {$vest $~}
    ::
        $vest
      !!
    ::
    ::  monitor website
    ::    {$vile p/site}
    ::
        $vile 
      !!
    ::
    ::  execute remote request
    ::    {$west p/ship q/path r/*}
    ::
        $west
      ?>  =(~ q.tac)
      =+  mes=((hard jael-message) r.tac)
      ?-    -.mes
      ::
      ::  reset remote rights
      ::    {$hail p/(nap jael-right)}
      ::
          $hail
        (curd (~(hail ur urb.nav) p.tac our p.mes))
      ::
      ::  share certificates
      ::    {$ping p/gree}
      ::
          $ping
        (curd (~(meet ur urb.nav) p.tac p.mes))
      ==

    ::
    ::  set/clear web API key
    ::    {$wink p/site q/@t r/(unit bill)}
    ::
        $wink
      !!
    ::
    ::  set/clear web password
    ::    {$wish p/site q/@t r/(unit @t)}
    ::
        $wish
      !!
    ::
    ::  set/clear web API token
    ::    {$wonk p/site q/@t r/@t s/(unit bill)}
    ::
        $wonk
      !!
    ==
  ::                                                    ::  curd:of
  ++  curd                                              ::  use urbit result
    |=  {hab/(list jael-effect) urb/jael-urbit}
    (curd(urb.nav urb) hab) 
  ::                                                    ::  cure:of
  ++  cure                                              ::  apply urbit effects
    |=  hab/(list jael-effect)
    ^+  +>
    !!
  ::                                                    ::  emil:of
  ++  emil                                              ::  effects
    |=  ::  moz: effects in order
        ::
        moz/(list jael-move)
    ^+(moz (weld (flop moz) ^moz))
  ::                                                    ::  emit:of
  ++  emit                                              ::  effect
    |=  ::  mov: effect
        ::
        mov/jael-move
    ^+(moz [mov moz])  
  --
::                                                      ::  ur
++  ur                                                  ::  urbit reactor
  =|  $:  ::  sys: system context                       ::::  #3.b
          ::
          ^=  sys
          $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now/@da
              eny/@e
          ==
          ::  all vane state
          ::
          jael-urbit
      ==
  ::  urb: all urbit state
  ::  mer: merge state
  ::    via: authenticated source
  ::    cod: propagating certificates
  ::  hab: side effects
  ::
  =*  urb  ->+
  =|  $:  mer/(unit {via/@p cod/gree})
          hab/(list jael-effect)
      ==
  |%
  ::                                                  ::  abet:ur
  ++  abet                                            ::  resolve
    [(flop hab) urb]
  ::                                                  ::  like:ur
  ++  like                                            ::  verify signature
    |=  {myn/mind ash/@ val/@}
    ^-  ?
    ::
    ::  XX crypto api needs some adjusting
    ::
    ?>  .=  ash 
        %-  need 
        =<  sure:as
        %.  [*code val]
        (com:nu:crub (look myn))
    &
  ::                                                  ::  look:ur
  ++  look                                            ::  get public key
    |=  myn/mind
    (look:(from who.myn) lyf.myn)
  ::                                                  ::  mean:ur
  ++  mean                                            ::  apply merge
    |=  {why/?($hear $make) gut/jael-change}
    ^+  +>
    ?-    -.gut
      ::  
      ::  learn new signature on existing deed
      ::    {$sure p/ship q/life r/mind s/@}
      ::
        $sure
      !!
      ::
      ::  learn new deed
      ::    {$step p/ship q/life r/lace}
      ::
        $step
      ?:  =($hear why)
        ?: : 
    ==

  ==                                                    ::


        $sure
      ?:  =($make 
    == 
        $hear
      
    ::
    ::  map from edits to 
    ::
    !!
  ::                                                  ::  meet:ur
  ++  meet                                            ::  calculate merge
    |=  $:  ::  via: authenticated source
            ::  cod: transmitted certificates
            ::
            via/@p
            cod/gree
        ==
    ^+  +>
    =-  ::  map edits to effects
        ::
        |-  ^+  +>.^$
    =.  mer  `[via cod]
    =+  lec=(~(tap by cod))
    ::
    ::  check new certs ship by ship
    ::
    |-  ^-  fex/(list jael-edit)
    ?~  lec  ~
    (weld (boat:(from p.i.lec) q.i.lec) $(lec t.lec))
  ::                                                  ::  from:ur
  ++  from                                            ::  server reactor
    |=  ::  rex: server ship
        ::
        rex/ship
    ::
    ::  shy: private state
    ::  rug: public state
    ::
    =+  :+  shy/(fall (~(get by pry) rex) *jael-ship)
            rug/(fall (~(get by pug) rex) *grue)
            gur/(fall ?~(mer ~ (~(get by cod.u.mer) rex) *grue))
    |%
    ::                                                ::  abet:from:ur
    ++  abet                                          ::  resolve
      %_(..from pry (~(put by pry) rex shy))
    ::                                                ::  
    ++  look                                          ::  get public key
      |=  lyf/life 
      ^-  @
      ::
      ::  first galaxy key is hardcoded
      ::
      ?:  &((lth rex 256) =(1 lyf))
        (zeno rex)
      ::
      ::  cascade search over old and new, new first
      ::
      ?~  mer  (need find)
      |^  ((bond |.((need find))) find(rug gur))
      ++  find
        ^-  (unit @)
        ::
        ::  crash if this life is revoked
        ::
        ?>  =(p.rug lyf)
        %+  biff  (~(get by q.rug) lyf)
        |=(lace `pub.dat)
      --
    ::
    

    ++  boat
      ?>  mer
      =+  gur=((~(get by cod.u.mer) *grue)
    ++  bonk


    --
















    ::                                                ::  unto:from:ur:of
    ++  unto                                          ::  client reactor
      |=  ::  pal: client ship
          ::
          pal/ship
      ::
      ::  cly: client state
      ::
      =+  (fall (~(get by rel) pal) *jael-friend) 
      =*  cly  -
      |%
      ::                                              ::  abet:unto:from:ur:
      ++  abet                                        ::  resolve
        ^+  ..unto
        ..unto(rel (~(put by rel) pal cly))
      ::                                              ::  give:unto:from:ur:
      ++  give                                        ::  credit
        |=  lab/(nap jael-right)
        ^+  +>
        !!
      --
    --

      ::                                                ::
      ++  boat                                          ::  merge ships
        |=  $:  ::  who: this ship
                ::  gur: new will for this ship
                ::
                rex/ship 
                gur/grue
            ==
        ^-  (list jael-edit)
        ::
        ::  rug: old will for this ship
        ::
        =+  rug=(fall (~(get by pug) rex) *grue)
        ?:  =(gur rug)  ~
        =+  :*  ::
                ::  num: life counter
                ::  end: last life in old or new ship
                ::
                num=`life`1
                end=(max p.gur p.rug)
            ==
        =|  $:  ::  pre: previous deed
                ::  fex: edits in reverse order
                ::
                pre/(unit lama)
                fex/(list jael-edit)
            ==
        |-  ^+  fex
        ::
        ::  merge all lives in :%
        ::
        ?:  (gth num end)  
          (flop fex)
        ::
        ::  lub: deed merge for this life
        ::
        =+  ^=  lub
            %-  bonk 
            :*  rex 
                num 
                pre 
                (~(get by q.rug) num) 
                (~(get by q.gur) num)
            ==
        %=  $
          num  +(num)
          pre  `p.lub
          fex  (weld (flop q.lub) fex)
        ==
      ::                                                ::
      ++  bonk                                          ::  merge lives
        |=  $:  ::  rex: ship we're merging
                ::  num: life we're merging
                ::  pre: previous deed 
                ::  lod: old deed
                ::  wan: new deed
                ::
                rex/ship
                num/@ud
                pre/(unit lama)
                lod/(unit lace)
                wan/(unit lace)
            ==
        ^-  $:  ::  p: next previous deed
                ::  q: edits in order
                ::
                p/lama
                q/(list jael-edit)
            ==
        ::
        ::  if no new information, do nothing
        ::
        ?:  |(?=($~ wan) =(wan lod))
          ?>  ?=(^ lod) 
          [dat.u.lod ~]
        ::
        ::  ash: hash of deed content
        ::  def: our default parent
        ::  dad: our declared parent
        ::  mir: our rank
        ::
        =/  ash  (sham dat.u.wan)
        =/  def  (sein rex)
        =*  dad  dad.doc.dat.u.wan
        =/  mir  (clan rex)
        ?>  ?:  |(=(num 1) =(%earl mir) =(%pawn mir))
              ::
              ::  comets and moons must stay with default parent
              ::
              =(def dad)
            ::
            ::  other ships may migrate to parent of same rank
            ::
            =((clan def) (clan dad))
        ::
        ::  if we have an old deed at this life, merge them
        ::
        ?:  ?=(^ lod)
          ::
          ::  use the old deed as the next previous
          ::
          :-  dat.u.lod
          ::
          ::  deed data must be identical
          ::
          ?>  =(dat.u.wan dat.u.lod)
          ::
          ::  sow: all new signatures
          ::
          =+  sow=`(list (trel ship life @))`(~(tap by syg.u.wan))
          |-  ^-  (list jael-edit)
          ?~  sow  ~
          ::
          ::  mor: all further edits
          ::  och: old signature for this signer
          ::
          =+  mor=$(sow t.sow)
          =+  och=(~(get by syg.u.lod) p.i.sow)
          ::
          ::  ignore obsolete or equal signature
          ::
          ?.  |(?=($~ och) (gth q.i.sow p.u.och))
            mor
          ::
          ::  check and merge new, or newer, signature
          ::
          ?>  (like [p q]:i.sow ash r.i.sow)
          :_(mor [%make %sure rex num [p q]:i.sow r.i.sow])
        ::
        ::  use the new deed as the next previous
        ::
        :-  dat.u.wan
        ::
        ::  non-initial deeds must be signed by previous
        ::
        ?>  ?|  ?=($~ pre)
                =+  laz=(~(got by syg.u.wan) rex)
                ?>  =(p.laz (dec num))
                =(ash (need (sure:as:(com:nu:crub pub.u.pre) *code q.laz)))
            ==
        ::
        ::  check the parent has signed, if necessary
        ::
        ?>  ?|  ::
                ::  no parent signature for existing, non-moon urbits
                ::
                ?&  ?=(^ pre)
                    =(dad.doc.u.pre dad)
                    !=(%earl mir)
                ==
                ::
                ::  public keys for galaxies are hardcoded
                ::
                ?&  =(%czar mir)
                    ?=($~ pre)
                    =(pub.dat.u.wan (zeno rex))
                ==
                ::
                ::  the deed's secure channel authenticates it
                ::
                =(via rex)
                ::
                ::  check valid parent signature
                ::
                =+  par=(~(got by syg.u.wan) dad)
                (like [dad p.par] ash q.par)
            ==
        ::  tep: deed update 
        ::
        =/  tep  [%hear %step rex num u.wan]
        ::
        ::  if we don't need to add a signature, report the new deed
        ::
        ?:  (~(has by syg.u.wan) dad)
          [tep ~]
        ::
        ::  lyf: life of parent
        ::  rig: secret key of parent
        ::  val: new signature
        ::
        =*  lyf  p:(~(got by pug) dad)
        =*  rig  (~(got by own:(~(got by pry) dad)) lyf)
        =*  val  (sign:as:(nol:nu:crub rig) *@ ash)
        [tep [%make %sure rex num [dad lyf] val] ~]














  --




  ++  ur                                                ::  urbit reactor
    =|  jael-urbit 
    ::
    ::  hab: effects in reverse order
    ::  urb: all urbit state
    ::
    =|  hab/(list jael-effect)
    =*  urb  ->
    |%
    ::                                                  ::  mean:ur:of
    ++  mean                                            ::  apply merge
      |=  ved/(list jael-edit)
      ^+  +>
      !!
    ::                                                  ::  meet:ur:of

      ::                                                ::
    --
  ::                                                    ::  we
  ++  we                                                ::  web reactor
    !!                                                  ::::
  --  
--
::                                                      ::::
::::                                                    ::  preamble
  ::                                                    ::::
::
::  lex: all durable %jael state
::
=|  lex/jael-state
|=  $:  :: 
        ::  now: current time
        ::  eny: unique entropy
        ::  ski: namespace resolver
        ::
        now/@da
        eny/@e                                        
        ski/sley
    ==
::                                                      ::
::::                                                    ::  interface
  ::                                                    ::::
|%
::                                                      ::  call
++  call                                                ::  request
  |=  $:  ::  hen: cause of this event
          ::  hic: event data
          ::
          hen/duct
          hic/(hypo (hobo jael-task))
      ==
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard jael-task) p.q.hic)))
  ^-  {p/(list jael-move) q/_..^$}
  =^  did  lex  abet:~(call of [now eny] ~ lex)
  [did ..^$]
::                                                      ::  doze
++  doze                                                ::  await
  |=  $:  ::  now: current time
          ::  hen: cause (XX why we need this?)
          ::
          now/@da 
          hen/duct
      ==
  ^-  (unit @da)
  ~
::                                                      ::  load
++  load                                                ::  upgrade
  |=  $:  ::  old: previous state
          ::
          old/jael-state
      ==
  ^+  ..^$
  ..^$(lex old)
::                                                      ::  scry
++  scry                                                ::  inspect
  |=  $:  ::  fur: event security
          ::  ren: access mode
          ::  why: owner
          ::  syd: desk (branch)
          ::  lot: case (version)
          ::  tyl: rest of path
          ::
          fur/(unit (set monk))
          ren/@tas 
          why/shop 
          syd/desk 
          lot/coin 
          tyl/spur
      ==
  ^-  (unit (unit cage))
  !!
::                                                      ::  stay
++  stay                                                ::  preserve
  lex
::                                                      ::  take
++  take                                                ::  accept
  |=  $:  ::  tea: order
          ::  hen: cause
          ::  hin: result
          ::
          tea/wire 
          hen/duct 
          hin/(hypo sign-arvo)
      ==
  ^-  {p/(list jael-move) q/_..^$}
  [~ ..^$]
--
