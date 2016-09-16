!:                                                      ::  /van/jael
::                                                      ::  %orthodox
!?  150
::                                                      ::::
::::                        # 1                         ::  structures
  ::                                                    ::::
|=  pit/vase
=>  |%
++  jael-state                                          ::  all crypto state
  $:  ver/$0                                            ::  %jael version 
      nav/jael-objective                                ::  all objective state
      nix/jael-subjective                               ::  all subjective state
  ==                                                    ::
++  jael-subjective                                     ::  indexes / observers
  $:  sub/jael-subjective-urbit                         ::  urbit subjective
      sew/jael-subjective-web                           ::  web subjective
  ==                                                    ::
++  jael-subjective-urbit                               ::  urbit metadata
  $:  $=  dev                                           ::  indexes
        $:  dad/ship                                    ::  our parent
            pal/(set ship)                              ::  peers
            kid/(set ship)                              ::  children
            ast/(map ship jael-purse)                   ::  assets from
        ==                                              ::
      $=  vew                                           ::  all trackers
        $:  lat/(set duct)                              ::  rights
            hin/(set duct)                              ::  channel, all
            yon/(map ship (set duct))                   ::  channel by ship
  ==    ==                                              ::
++  jael-subjective-web                                 ::  web metadata
  $~                                                    ::
++  jael-objective                                      ::  all universal state
  $:  urb/jael-urbit                                    ::  all urbit state
      web/(map site jael-web-domain)                    ::  all web state
  ==                                                    ::
++  jael-web-domain                                     ::  per foreign app
  $:  sec/(map @t jael-web-app)                         ::  client per api key
      usr/(map @ta (unit @t))                           ::  user/password
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
      pry/(map ship (map ship jael-purse))              ::  all private state
  ==                                                    ::
++  jael-friend                                         ::  relationship 
  $:  luf/(unit life)                                   ::  life as known to
      lab/jael-purse                                    ::  promises to
  ==                                                    ::
++  jael-gift                                           ::  output
  $?  {$cash jael-report-cash}                          ::  account report
      {$clue jael-report-clue}                          ::  channel dump
      {$paid jael-report-paid}                          ::  asset update
      {$self jael-report-self}                          ::  self dump
  ==                                                    ::
++  jael-right                                          ::  urbit commitment
  $%  {$email p/(set @ta)}                              ::  email addresses
      {$final p/(map ship @uvG)}                        ::  ticketed ships
      {$fungi p/(map term @ud)}                         ::  fungibles
      {$guest $~}                                       ::  refugee visa
      {$hotel p/pile}                                   ::  reserved block
      {$jewel p/(map life ring)}                        ::  private keyring
      {$lived p/life}                                   ::  PKI knowledge
      {$noble p/(map hand (pair @da code))}             ::  symmetric keys
  ==                                                    ::
++  jael-purse                                          ::  rights set
  (nap jael-right)                                      ::
::                                                      ::
++  jael-delta                                          ::  rights change
  $:  mor/jael-purse                                    ::  gain rights
      les/jael-purse                                    ::  lose rights
  ==                                                    ::
++  jael-task                                           ::  operations on
  $%  {$give p/ship q/jael-purse}                       ::  add rights
      {$line p/ship q/@da r/code}                       ::  outbound symkey
      {$link p/ship q/@da r/code}                       ::  inbound symkey
      {$meet p/ship q/gree}                             ::  integrate pki from
      {$nuke ~}                                         ::  cancel as tracker
      {$over p/ship q/jael-task}                        ::  mirror operation
      {$pall p/ship q/life}                             ::  our life acked
      {$ping p/ship}                                    ::  empty contact
      {$step p/lamp q/ring}                             ::  update private key
      {$take p/ship q/jael-purse}                       ::  subtract rights
      {$vest $~}                                        ::  watch accounts
      {$view p/ship}                                    ::  watch channel
      {$vile p/site}                                    ::  watch website
      {$vine $~}                                        ::  watch all changes
      {$west p/ship q/path r/*}                         ::  remote request
      {$wink p/site q/@t r/(unit bill)}                 ::  set web API key
      {$wish p/site q/@t r/(unit @t)}                   ::  set web login
      {$wonk p/site q/@t r/@t s/(unit bill)}            ::  set web API token
  ==                                                    ::
++  jael-report-them                                    ::  report on neighbor
  $:  gur/grue                                          ::  certificate
      lab/jael-purse                                    ::  our promises to
      own/jael-purse                                    ::  our promises from
  ==                                                    ::
++  jael-report-self                                    ::  report on self
  $:  gur/grue                                          ::  certificate
      war/(map life ring)                               ::  private keys
  ==                                                    ::
++  jael-report-cash                                    ::  neighbors/assets
  $:  has/(map ship jael-purse)                         ::
  ==                                                    ::
++  jael-report-paid                                    ::  asset diff
  $:  dif/(list (trel ship ? jael-purse))               ::  who, +/-, what
  ==                                                    ::
++  jael-note                                           ::  out request $->
  $%  {$x $mess p/ship q/path r/*}                      ::  send message
  ==                                                    ::
++  jael-message                                        ::  p2p message
  $%  {$hail p/jael-purse}                              ::  reset rights
      {$ping p/gree}                                    ::  propagate
  ==                                                    ::
++  jael-edit-fact                                      ::  certificate change
  $:  rex/ship                                          ::  owner
      via/ship                                          ::  propagated from
      lyf/life                                          ::  deed modified
      way/?($new $old)                                  ::  new/existing deed
      gur/gree                                          ::  pedigree
  ==                                                    ::
++  jael-edit-rite                                      ::  rights change
  $:  rex/ship                                          ::  server
      pal/ship                                          ::  client
      del/jael-delta                                    ::  change
  ==                                                    ::
++  jael-edit                                           ::  urbit change
  $:  {$fact jael-edit-fact}                            ::  pki change
      {$rite p/ship q/ship r/jael-delta}                ::  rights change
  ==                                                    ::
++  jael-edit                                           ::  pki change
  $:  $=  why                                           ::  cause of change
        $?  $hear                                       ::  external source
            $make                                       ::  internal source
        ==                                              ::
      gut/jael-change                                   ::  new information
  ==                                                    ::
++  jael-change                                         ::  pki change
  $%  {$sent p/ship q/ship r/jael-delta)}               ::  rights from/to
      {$step p/ship q/life r/gree}                      ::  new deed
      {$sure p/ship q/life r/gree}                      ::  new signature
  ==                                                    ::
++  jael-move                                           ::  output
  {p/duct q/(wind jael-note jael-gift)}
--
::                                                      ::::
::::                        # 2                         ::  static data
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
::::                        # 3                         ::  stateless functions
  ::                                                    ::::
=>  |%
::                                                      ::  chop:of
++  chop                                                ::  subtract rights
  |=  {les/jael-purse lab/jael-purse}
  ^-  jael-purse
  !!
::                                                      ::  clip:of
++  clip                                                ::  construct diff
  |=  {nex/jael-purse lab/jael-purse} 
  ^-  jael-delta
  !!
::                                                      ::  comb:of
++  comb                                                ::  combine
  |=  {mor/jael-purse lab/jael-purse}
  !!
::                                                      ::  come:of
++  come                                                ::  apply rights delta
  |=  {del/jal-delta lab/jael-purse}
  ^-  jael-purse
  (chop les.del (comb mor.del lab))
::                                                      ::::
::::                        # 4                         ::  reactors
  ::                                                    ::::
=>  |%
::                          ## 4.a                      ::  of
++  of                                                  ::  general reactor
  =|  $:  ::  sys: system context                       ::::
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
    ::    {$give p/ship q/jael-purse)}
    ::
        $give  
      (cure abet:(~(give ur urb.nav) our [p q]:tac))
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
      (cure abet:(~(meet ur sys urb.nav) p.tac q.tac))
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
      (cure abet:(~(pall ur sys urb.nav) our p.tac q.tac))
    ::
    ::  request incidental contact
    ::    {$ping p/ship}
    ::
        $ping
      !!
      :: (cure [%ping p.tac ~]~)
    ::
    ::  extend our certificate with a new private key
    ::    {$step p/lamp}
    ::
        $step 
      (cure abet:(~(step ur sys urb.nav) our p.tac))
    ::
    ::  remove rights
    ::    {$take p/ship q/jael-purse}
    ::  
        $take
      (cure abet:(~(give ur sys urb.nav) our [p q]:tac))
    ::
    ::  monitor self
    ::    {$vain $~}
    ::
        $vain
      =/  
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
      ::    {$hail p/jael-purse}
      ::
          $hail
        (cure (~(hail ur urb.nav) p.tac our p.mes))
      ::
      ::  share certificates
      ::    {$ping p/gree}
      ::
          $ping
        (cure (~(meet ur urb.nav) p.tac p.mes))
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
  ++  curd                                              ::  subjective moves
    |=  {moz/(list move) urb/jael-subjective-urbit}
    +>(urb.nix urb, moz (weld (flop moz) ^moz))
  ::                                                    ::  cure:of
  ++  cure                                              ::  objective edits
    |=  {hab/(list jael-edit) urb/jael-urbit}
    ^+  +>
    (curd(urb urb.nav) abet:(~(apex su sys urb.nav urb.nix) hab))
  --
::                          ## 4.b                      ::  su
++  su                                                  ::  subjective reactor
  =|  $:  ::  sys: system context                       ::::
          ::
          ^=  sys
          $:  ::  now: current time
              ::  eny: unique entropy
              ::
              now/@da
              eny/@e
          ==
          urb/jael-urbit
          jael-subjective-urbit
      ==
  ::  urb: objective urbit state
  ::  sub: subjective urbit state
  ::  moz: moves in reverse order
  ::
  =*  sub  ->+
  =|  moz/(list move)
  |%
  ::                                                    ::  abet:su
  ++  abet                                              ::  resolve
    [(flop moz) urb]
  ::                                                    ::  apex:su
  ++  apex                                              ::  apply changes
    |=  hab/(list jael-edit)
    ?~  hab  +>
    $(hab t.hab, +> (repo:(echo i.hab) i.hab))
  ::                                                    ::  cart:su
  ++  cart                                              ::  ping ship
    |=  {her/ship gur/gree} 
    ^+  +>
    +>(moz :_(moz [%pass %x ~ %mess /x %ping gur]))
  ::                                                    ::  carp:su
  ++  carp                                              ::  ping all in set
    |=  {all/(set ship) gur/gree}
    =+  lal/(~(tap by all))
    |-  ^+  +>.^$
    ?~  lal  +>.^$
    $(lal t.lal, +>.^$ (cart i.lal gur))
  ::                                                    ::  echo:su
  ++  echo                                              ::  
    |=  led/jael-edit
    ^+  +>
    ?-    -.led
    ::
    ::  new certificate state
    ::    {$fact rex/ship via/ship lyf/life way/?($new $old) gur/gree}
    ::
    ::
    ::  new certificate state
    ::    {$fact p/ship q/life r/?($new old) s/gree}
    ::    {$make p/ship q/life r/?($new old) s/gree}
    ::
        ?($hear $make)
      ::
      ::  ignore changes to existing deeds
      ::
      ?.  =(%new r.led)  +>
      =+  dad=dad.doc.dat:(~(got by q:(~(got by s.led) p.led)) q.led)
      ::
      ::  if self update, update cached parent state
      ::
      ?:  =(our p.led)
        +>.$(dad dad)
      ::
      ::  if first meeting, add to child/peer sets
      ::
      ?:  =(1 q.led)  +>
      ?:  =(our dad)
        +>.$(kid (~(put in kid) p.led))
      ?.  =((clan p.led) (clan our))
        +>.$
      +>.$(pal (~(put in pal) p.led))
    ::
    ::  new rights
    ::    {$send p/ship q/jael-delta}
    ::
        $send
      ::
      ::  record promises made to us
      ::
      ?.  =(our q.led)  +>.$
      =*  haz  (fall (~(get by ast) p.led) *jael-purse)
      +>.$(ast (~(put by ast) (come r.led haz)))
    ==
  ::                                                    ::  repo:su
  ++  repo                                              ::  report change
    |=  led/jael-edit
    ^+  +>
    ?-    -.led
    ::
    ::  new certificate state
    ::    {$fact rex/ship via/ship lyf/life way/?($new $old) gur/gree}
    ::
        $fact
      !!
    ::
    ::  new rights
    ::    {$paid p/ship q/jael-delta}
    ::
        $paid
      +>
    ==
  ::                                                    ::  prop:su
  ++  prop                                              ::  propagate pki change
    |=  led/jael-edit
    ^+  +>
    ?.  ?=(?($hear $make) -.led)  +>
    ?-    -.led
    ::
    ::  new certificate state
    ::    {$made p/ship q/life r/?($new old) s/gree}
    ::
        ?($hear $make)
      ::
      ::  if we made a cert for someone else, send it back
      ::
      =.  +>  ?.  &(=($make -.led) !=(our p.led))  +>
              (cart p.led s.led)
      ::
      ::  if this is a new child we just met, feed it 
      ::
    == 

    ::    {$hear p/ship q/life r/?($new old) s/gree}
    ::    {$make p/ship q/life r/?($new old) s/gree}
    ::
      ?:  (lth 
      ?:  =(%old r.led) 
        
    ::
    ::  new rights
    ::    {$paid p/ship q/jael-delta}
    ::
        $paid
      +>
    ==
    
::                          ## 4.c                      ::  ur
++  ur                                                  ::  urbit reactor
  =|  $:  ::  sys: system context                       ::::
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
    =.  +>  (
    ?-    -.gut
      ::  
      ::  learn new signature on existing deed
      ::    {$sure p/ship q/life r/mind s/@}
      ::
        $sure
      ?:  =($hear why)
        (emit %kick p.gut)
      (emil [%kick p.gut] [%kids p.gut] ~)
      ::
      ::  learn new deed
      ::    {$step p/ship q/life r/lace}
      ::
        $step
      ?:  =($hear why)
        (emit %kick p.gut)
      (emil [%kick p.gut] 
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
    ::
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
        |=  lab/jael-purse
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
