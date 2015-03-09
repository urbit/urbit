!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
=>  =~
|%                                                      ::  interfaces
++  chop  ,[p=@ud q=@da]                                ::  see 
++  gift                                                ::  out result <-$
          $%  [%thou p=httr]                            ::  raw http response
              [%thus p=@ud q=(unit hiss)]               ::  http request/cancel
              [%veer p=@ta q=path r=@t]                 ::  drop-through
              [%vega p=path]                            ::  drop-through
          ==                                            ::
++  gram                                                ::  inter-ship message
  $?  [[%lon ~] p=hole]                                 ::  login request
      [[%aut ~] p=hole]                                 ::  login reply
      [[%hat ~] p=hole q=hart]                          ::  login redirect
  ==                                                    ::
++  hasp  ,[p=ship q=term]                              ::  don't see %gall
++  hapt  ,[p=ship q=path]                              ::  do see %gall
++  kiss                                                ::  in request ->$
          $%  [%born ~]                                 ::  new unix process
              [%crud p=@tas q=(list tank)]              ::  XX rethink
              [%init p=@p]                              ::  report install
::               [%them p=(unit hiss)]                     ::  outbound request
::               [%they p=@ud q=httr]                      ::  inbound response
              [%this p=? q=clip r=httq]                 ::  inbound request
              [%thud ~]                                 ::  inbound cancel
              [%wart p=sack q=@tas r=_`[path *]`*gram]  ::  urbit message
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  $:  %a                                    ::  to %ames
          $%  [%want p=sock q=[path *]]                 ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%flog p=[%crud p=@tas q=(list tank)]]    ::
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit silk)]                ::
              [%wasp p=@p q=@uvH]                       ::
          ==  ==  ==                                    ::
++  rave                                                ::  see %clay
          $%  [& p=mood]                                ::
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  silk                                                ::  see %ford
          $&  [p=silk q=silk]                           ::
          $%  [%boil p=mark q=beam r=path]              ::
              [%cast p=mark q=silk]                     :: 
              [%done p=(set beam) q=cage]               ::
          ==                                            ::
++  sine                                                ::
          $?  sign                                      ::
              $:  %g                                    ::
          $%  [%veer p=@ta q=path r=@t]                 ::
              [%vega p=path]                            ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $?  $:  %f                                    ::  by %ford
          $%  [%made p=@uvH q=(each cage tang)]         ::
              [%news ~]                                 ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
          ==  ==  ==                                    ::
++  whir  $|(~ $%([%at p=hole q=whir]))                 ::  wire subset
--                                                      ::
|%                                                      ::  models
++  bolo                                                ::  eyre state
  $:  %0                                                ::  version
      gub=@t                                            ::  random identity
      hov=(unit ship)                                   ::  master for remote
      ged=duct                                          ::  client interface
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      liz=(jug beam (each duct oryx))                   ::  clay subscriptions
      wup=(map hole cyst)                               ::  secure sessions
      sop=(map hole ,[ship ?])                          ::  foreign session names
  ==                                                    ::
::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      [him=ship aut=(set ship)]                         ::  authenticated
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      way=(map ship ,[purl duct])                       ::  waiting auth
      vew=(set oryx)                                    ::  open views XX expire
  ==                                                    ::
::
++  perk                                                ::  parsed request
  $%  [%spur p=spur]
      [%beam p=beam]
      [%poll p=@uvH]
      [%auth perk-auth]
      [%away ~]
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  [%get him=ship rem=pork]
      [%xen ses=hole rem=pork]
      [%at p=pork]                               ::  inject auth
      [%js ~]
      [%json ~]
      [%try him=ship cod=cord]
      [%del p=(unit ship)]
  ==
::
++  pest                                                ::  result
  $%  [%for p=whir q=beam r=term s=cred]                ::  %f block
      [%fow p=@uvH]                                     ::  %f deps
      [%fin $|(~ pest-fin)]                             ::  done
      [%red p=purl q=@t]                                ::  redirect
      [%zap p=@ud q=(list tank)]                        ::  err
  ==
::
++  pest-fin                                            ::  response
  $%  [%code p=@ud q=pest-fin]
      [%json p=json] 
      [%html p=manx] 
      [%js p=@t] 
      [%$ p=httr]
  ==
--                                                      ::
|%
++  session-from-cookies
  |=  [nam=@t maf=math]
  ^-  (unit hole)
  =+  ^=  cok  ^-  (list ,@t)
      =+  cok=(~(get by maf) 'cookie')
      ?~(cok ~ u.cok)
  |-  ^-  (unit hole)
  ?~  cok  ~
  =+  mar=`(unit (list ,[p=@t q=@t]))`(rush i.cok cock:epur)
  ?~  mar  $(cok t.cok)
  |-  ^-  (unit hole)
  ?~  u.mar  ^$(cok t.cok)
  ?:  &(=(nam p.i.u.mar) !=('~' q.i.u.mar))
    [~ q.i.u.mar]
  $(u.mar t.u.mar)
::
++  heat                                                ::  eat headers
  |=  hed=(list ,[p=@t q=@t])  ^-  math
  %+  roll  hed
  |=  [a=[p=cord cord] b=math] 
  =.  p.a  (cass (trip p.a))
  (~(add ja b) a)
::
++  tanx                                                ::  tanks to manx
  |=  tac=(list tank)
  ^-  manx
  =+  rolt=|=(a=wall `tape`?~(a ~ :(weld i.a "\0a" $(a t.a))))
  =+  mec=(rolt (turn tac |=(a=tank (rolt (wash 0^160 a)))))
  ;html
    ;head
      ;meta(charset "utf-8");
      ;title: server error
    ==
    ;body:pre:code:"{mec}"
  ==
::
++  depo                                                ::  inject dependency
  |=  [dep=@uvH max=manx]
  ^-  manx
  ?>  ?=([[%html ~] [[%head ~] *] [[%body ~] ^] ~] max) ::  XX static
  ?~  dep  max
  max(c.i.c :_(c.i.c.max ;script@"/~/on/{<dep>}.js";))
::
++  jass                                                ::  inject window.urb
  |=  [urb=json jaz=cord]  ^-  cord
  %^  cat  3
    (crip "window.urb = {(pojo urb)}\0a")
  jaz
::
++  js                                                  ::  static javascript
  |%
  ++  poll                                              ::  dependency long-poll
    '''
    urb.tries = 0
    urb.call = function() {
      xhr = new XMLHttpRequest()
      xhr.open('GET', urb.poll, true)
      xhr.addEventListener('load', function() {
        // if(~~(this.status / 100) == 4)
        //   return document.write(xhr.responseText)
        if(this.status !== 205) {
          return urb.keep()
        }
        document.location.reload()
      })
      xhr.addEventListener('error', urb.keep)
      xhr.addEventListener('abort', urb.keep)
      xhr.send()
    }
    urb.keep = function() {
      setTimeout(urb.call,1000*urb.tries)
      urb.tries++
    }
    urb.call()
    '''
  ::
  ++  auth
    '''
    var req = function(url,dat,cb){
      var xhr = new XMLHttpRequest()
      xhr.open('POST', url, true)
      dat.oryx = urb.oryx
      xhr.send(JSON.stringify(dat))
      xhr.addEventListener('load', function(ev){
        if(this.status !== 200)
          return err.innerHTML = ":(\n" + xhr.responseText
        else if(cb) return cb(xhr.responseText,ev)
      })
    }
    
    ship.innerText = urb.ship
    urb.foreign = /^\/~\/am/.test(window.location.pathname)
    urb.submit = function(){
      req(
        "/~/auth.json?PUT", 
        {ship: ship.innerText, code: pass.value},
        function(){
          if(urb.foreign) document.location = 
            document.location.hash.match(/#[^?]+/)[0].slice(1) +
            document.location.pathname.replace(
              /^\/~\/am\/[^/]+/,
              '/~/as/~' + urb.ship) +
            document.location.search
          else document.location.reload()
      })
    }
    urb.away = function(){req("/~/auth.json?DELETE", {}, 
      function(){document.write("success!")})}
    '''
  --
++  xml
  |%
  ++  login-page
    ;html
      ;head:title:'Log in'
      ;body
        ;p: Identify yourself, ~;{span#ship(contenteditable "")}?
        ;style:'#ship {background: lightgray} #ship br {display: none}'
        ;input#pass(onchange "urb.submit()");
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
      ==
    ==
  ::
  ++  logout-page
    ;html
      ;head:title:'Log out'
      ;body
        ;p: Goodbye ~;{span#ship}.
        ;button#act(onclick "urb.away()"): Log out
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
      ==
    ==
  --
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  hen=duct                                  ::  event floor
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  our=ship                              ::  current ship
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet                                              ::  resolve moves
    ^-  [(list move) bolo]
    [(flop mow) bol]
  ::
  ++  adit  .(ney (mix eny ney))                        ::  flip entropy
  ::
  ++  anon  `@p`(add our ^~((bex 64)))                  ::  pseudo-sub
  ++  axon                                              ::  accept response
    |=  [tea=wire typ=type sih=sign]
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    -.+.sih
        %crud
      +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::
        %made
      =+  tee=((soft whir) tea)
      ?~  tee  ~&  e/ford/lost/hen  +>.$
      =.  our  (need hov)                             ::  XX
      |-  ^+  ..axon
      ?-    u.tee
          [%at ^]
        ?.  ?=([%& %js ^] q.sih)
          ~&  e/at-lost/p.u.tee
          $(u.tee q.u.tee)
        =*  cag  p.q.sih
        ?>  ?=(@ q.q.cag)
        =+  cyz=(~(got by wup) p.u.tee)
        =^  jon  ..ya  ~(stat-json ya p.u.tee cyz)
        $(u.tee q.u.tee, q.q.p.q.sih (jass jon q.q.cag))
          ~      
        :: ~&  e/ford/hen
        ?-  -.q.sih
          |  (fail 404 p.sih p.q.sih)
          &  =*  cag  p.q.sih
             ?+    p.cag  (back ~ p.sih cag)
                 %hipo                                  ::  hacks!
               ?>  ?=(@tas -.q.q.cag)
               $(p.q.sih [-.q.q.cag (slot 3 q.cag)])
             ::
                 %mime
               ~|  q.q.cag
               =+  ((hard ,[mit=mite rez=octs]) q.q.cag)  ::  XX
               (give-gift %thou 200 [content-type/(moon mit)]~ ~ rez)
             ==
        ==
      ==
    ::
        %news  (give-json 205 ~ %b &)                     ::  dependency updated
    ==
  ::
  ++  apex                                              ::  accept request
    |=  kyz=kiss
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    -.kyz
        %born  +>.$(ged hen)                            ::  register external
        %crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
        %init                                           ::  register ownership
      %_    +>.$
          hov  ?~(hov [~ p.kyz] [~ (min u.hov p.kyz)])
      ==
    ::
        %this                                           ::  inbound request
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  maf=(heat r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            &  ?>(=(sec p.p.p.ryp) p.ryp)
            |  =+  hot=(~(get ja maf) %host)
               ?>  ?=([@ ~] hot)
               [[sec (rash i.hot thor:epur)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul ?=(hoke r.p.pul))
      abet:~(handle rq pul q.+.kyz [p.heq maf s.heq])
    ::
        %thud                                           ::  cancel request
      ~&  e/gone/hen
      +>.$
    ::
        %wart                                           ::  remote request
      =+  mez=((soft gram) r.kyz)
      ?~  mez
        ~&  [%strange-wart p.kyz q.kyz]
        +>.$
      ?-  -<.u.mez
        %aut  abet:(logon:(ses-ya p.u.mez) q.p.kyz)
        %hat  (foreign-hat:(ses-ya p.u.mez) q.p.kyz q.u.mez)
        %lon  
          ~&  ses-ask/[p.u.mez sop (~(run by wup) ,~)]
          ?:  (ses-authed p.u.mez)  
            (ames-gram q.p.kyz aut/~ p.u.mez)
          =.  sop  (~(put by sop) p.u.mez q.p.kyz |)
          (ames-gram q.p.kyz hat/~ p.u.mez our-host)
      ==
    ==
  ::
  ++  ses-authed 
    |=  ses=hole
    =+  sap=(~(get by sop) ses)
    ?:  ?=([~ @ %&] sap)  &
    =+  cyz=(~(get by wup) ses)
    ?~  cyz  |
    (~(has in aut.u.cyz) our)
  ::
  ++  ses-ya  |=(ses=hole ~(. ya ses (~(got by wup) ses)))
  ++  our-host  `hart`[| [~ 8.445] `/localhost]       :: XX testing
  ++  fail                                            ::  request failed
    |=  [sas=@ud dep=@uvH mez=tang]
    ^+  +>
    ::  (back ~ dep %tang !>(mez))  ::  XX broken tang->mime door in ford
    (give-html sas ~ (depo dep (tanx mez)))
  ::
  ++  give-html                                       ::  request failed
    |=  [sas=@ud cug=(list ,@t) max=manx]
    %-  give-gift
    %+  add-cookies  cug
    (make-resp-gift sas text//html (crip (poxo max)))
  ::
  ++  give-json                                       ::  success json
    |=  [sas=@uG cug=(list ,@t) jon=json]
    %-  give-gift
    %+  add-cookies  cug
    (make-resp-gift sas application//json (crip (pojo jon)))
  ::
  ++  add-cookies
    |=  [cug=(list ,@t) git=[%thou httr]]
    ?~  cug  git
    =+  cuh=(turn `(list ,@t)`cug |=(a=@t set-cookie/a))
    git(q (weld cuh q.git))
  ::
  ++  give-gift                                       ::  done card
    |=  gef=gift
    +>(mow :_(mow [hen %give gef]))
  ::
  ++  resp                                            ::  mime response
    |=  [sas=@uG mit=mite bod=cord]
    (give-gift (make-resp-gift sas mit bod))
  ::
  ++  pass-note  |=(noe=[whir note] +>(mow :_(mow [hen %pass noe])))
  ++  ames-gram
    |=([him=ship gam=gram] (pass-note ~ %a %want [our him] [%e -.gam] +.gam))
  ::
  ++  ford-req
    |=  [tea=whir our=ship kas=silk]
    ::  ~&  [%ford-req our num ses -.kas]
    (pass-note tea %f [%exec our `kas])
  ::
  ++  back                                              ::  %ford bounce
    |=  [tea=whir dep=@uvH cag=cage]                
    (ford-req tea our [%cast %mime %done ~ cag])        ::  XX deps
  ::
  ++  make-resp-gift                                            ::  mimed response
    |=  [sas=@uG mit=mite rez=@]
    ::  (weld (turn cug |=(a=@t ['set-cookie' a]))
    [%thou `httr`[sas ~[content-type/(moon mit)] [~ (taco rez)]]]
  ::
  ++  host-to-ship                                              ::  host to ship
    |=  hot=host
    ^-  (unit ship)
    =+  gow=(~(get by dop) hot)
    ?^  gow  gow
    ?.  ?=(& -.hot)  ~
    =+  dom=(flop p.hot)                                ::  domain name
    ?~  dom  ~
    (rush i.dom fed:ag)
  ::
  ++  rq
    |_  $:  [hat=hart pok=pork quy=quay]                ::  purl, parsed url
            cip=clip                                    ::  client ip
            [mef=meth maf=math bod=(unit octs)]         ::  method/headers/body
        ==
    ++  done  .
    ++  abet  ..rq
    ++  teba  |*(a=$+(* ..rq) |*(b=* %_(done ..rq (a b))))
    ++  fail  (teba ^fail)
    ++  resp  (teba ^resp)
    ++  give-html  (teba ^give-html)
    ++  give-gift  (teba ^give-gift)
    ++  give-json  (teba ^give-json)
    ++  pass-note  (teba ^pass-note)
    ++  ford-req   (teba ^ford-req)
    ++  back  (teba ^back)
    ::
    ++  ford-kill  (pass-note ~ %f [%exec our ~])       :: XX unused
    ++  ford-wasp
      |=  [tea=whir dep=@uvH]
      (pass-note tea %f [%wasp our dep])
    ::
    ++  beam-into-ford
      |=  [tea=whir bem=beam ext=term ced=cred]
      =:  s.bem  [%web ~(rent co (flux:ya quy ced)) s.bem]
          r.bem  ?+(r.bem r.bem [%ud %0] da/now)
      ==
      (ford-req tea our [%boil ext bem ~])
    ::
    ::
    ++  as-beam
      ^-  (unit beam)
      |-
      ?~  q.pok
        $(q.pok /index)
      ?.  ((sane %tas) i.q.pok)
        (tome q.pok)
      `[[our i.q.pok da/now] (flop t.q.pok)]
    ::
    ++  as-magic-filename
      ^-  (unit gift)
      ?+    [(fall p.pok %$) q.pok]  ~
          [?(%ico %png) %favicon ~]
        :-  ~
        %^  make-resp-gift  200  image//png
        0w89wg.GV4jA.l9000.00dPb.YzBT6.giO00.o100d.wZcqc.a9tg-.VTG0b.
        AUIvE.HBM3g.cK4SE.0aagi.l090p.I1P5g.Y-80r.y1YS9.1xE~Y.qgpFY.
        vKN1V.905y0.2UwvL.43TUw.uL406.0-31h.xwoJF.Ul454.ilk00.00Yps.
        BNumh.xpl9B.pS5Ji.i1BoC.ZAgg1.BsC5T.t6pLk.Thohn.gp000.0ov~P.
        7M000.0o840.00010.0001i.h4x93.g0000.Eq2wR.7jB29
      ::
          [%txt %robots ~]
        :-  ~
        %^  make-resp-gift  200  text//plain
        %-  role
        :~  'User-agent: *'
            'Disallow: /'
        ==
      ==
    ::
    ++  as-aux-request                                ::  /~/... req parser
      ^-  (unit perk)
      |-
      ?:  ?=([%'~~' *] q.pok)                            ::  auth shortcut
        $(q.pok ['~' %as %own t.q.pok])
      ?.  ?=([%'~' @ *] q.pok)  ~
      :-  ~
      =*  pef  i.t.q.pok
      =+  but=t.t.q.pok                 ::  XX  =*
      ?+    pef  ~|(pfix-lost/`path`/~/[pef] !!)
          %on
        :-  %poll
        ~|  on/bad-path/but
        ?>  ?=([@ ~] but)
        (slav %uv i.but)
          %as
        :+  %auth  %get
        ~|  bad-ship/?~(but ~ i.but)
        ?~  but  !!
        :_  pok(q t.but)
        ?+  i.but  (slav %p i.but)
          %anon  anon
          %own   our
        ==
          %am  ?~(but !! [%auth %xen i.but pok(q t.but)])
          %at  [%auth %at pok(q but)]
          %away  [%away ~]
          %auth
        :-  %auth
        |-  ^-  perk-auth
        ?+    p.pok  !!
            ~          $(p.pok [~ %json])
            [~ %js]    [%js ~]
            [~ %json]
          ?+    mef  ~|(%bad-meth !!)
              %get   [%json ~]
              %post
            ?+    quy  ~|(bad-quy/'"PUT" or "DELETE"' !!)
                [[%'PUT' ~] ~]
              =+  paz=(ot ship/(su fed:ag) code/so ~):jo
              ~|  parsing/bod
              [%try (need (paz (need (poja q:(need bod)))))]
            ::
                [[%'DELETE' ~] ~]
              ~|  parsing/bod
              =+  jon=(need (poja q:(need bod)))
              ?>  ?=(%o -.jon)
              =+  sip=(~(get by p.jon) %ship)
              [%del ?~(sip ~ [~ (need ((su:jo fed:ag) u.sip))])]
        ==  ==
      ==  ==
    ::
    ++  handle
      ^+  done
      =+  oar=(host-to-ship r.hat)
      =.  our  ?~(oar our u.oar)  ::  XX
      =+  pez=process
      ?:  ?=(%| -.pez)  p.pez  ::  XX transitional
      =+  status=200
      |-  ^+  done
      ?-  -.p.pez
        %for  (beam-into-ford +.p.pez)
        %fow  (ford-wasp ~ p.p.pez)
        %zap  (fail p.p.pez 0v0 q.p.pez)
        %red  =+  fra=?~(q.p.pez "" ['#' (trip q.p.pez)])
              =+  url=(weld (earn p.p.pez) `tape`fra)
              $(p.pez [%fin ~ 307 [location/(crip url)]~ ~])
        %fin  ?~  +.p.pez  done
              ?-  &2.p.pez
                ~  (give-gift %thou p.p.pez)
                %js  (resp status text//javascript p.p.pez)
                %html  (give-html status ~ p.p.pez)
                %json  (give-json status ~ p.p.pez)
                %code  $(+.p.pez q.p.pez, status p.p.pez)
      ==      ==
    ::
    ++  process
      ^-  (each pest ,_done)
      =+  git=as-magic-filename
      ?^  git  [%| (give-gift u.git)]
      %-  |=  a=(each (each pest ,_done) tang) 
          ?~(-.a p.a [%& %zap 404 >%exit< p.a])
      %-  mule  |.  ^-  (each pest ,_done)
      ~|  [mef maf bod]
      =+  bem=as-beam
      ?^  bem  (process-parsed %beam u.bem)
      ?>  check-oryx
      =+  hem=as-aux-request
      ?^  hem  (process-parsed u.hem)
      ~|(strange-path/q.pok !!)
    ::
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=([~ %json] p.pok) ?=(%post mef) ?=(^ bod))  &
      =+  oxe=(parse-to-oryx q.u.bod)
      ?~  oxe  |
      & ::(~(has in vew.cyz:for-client) u.oxe) ::XX
    ::
    ++  parse-to-oryx  ;~(biff poja (ot oryx/so ~):jo)
    ++  root-beak  `beak`[our %main ud/0]               ::  XX
    ++  process-parsed
      |=  hem=perk
      ^-  (each pest ,_done)
      ?-    -.hem
          ?(%spur %beam)  
        =+  ext=(fall p.pok %urb)
        =+  bem=?-(-.hem %beam p.hem, %spur [root-beak p.hem])
        [%& %for ~ bem ext ced.cyz:for-client]
          %poll
        ?:  ?=([~ %js] p.pok)  ::  XX treat non-json cases?
          =+  polling-url=['/' (apex:earn %| pok(u.p %json) quy)]
          :^  %&  %fin  %js
          (jass (joba %poll (jape polling-url)) poll:js)
        ?~  p.hem  [%| done]
        [%& %fow p.hem]
          %away  [%& %fin %html logout-page:xml]
          %auth
        =+  yac=for-client
        ?-    &2.hem
            %json
          :-  %|
          =^  jon  ..ya  stat-json.yac
          (give-json 200 ~ jon)
            %js  [%& %fin %js auth:js]
            %at
          =.  ..ya  abet.yac
          =+  pez=process(pok p.hem)
          ?.  ?=(%& -.pez)  ~|(no-inject/p.hem !!)
          ?-  -.p.pez
            ?(%fow %zap %red)  pez
            %for  pez(p.p [%at ses.yac p.p.pez])
            %fin  
              ~|  %not-script
              ?>  ?=(%js &2.p.pez)
              =^  jon  ..ya  stat-json:for-client       ::  XX  state lost
              pez(p.p (jass jon p.p.pez))
          ==
        ::
            %try
          ~&  ses-try/ses.yac
          :-  %|
          ?.  =(our him.hem)
            ~|(stub-foreign/him.hem !!)
          ?.  =(load-secret cod.hem)
            ~|(try/`@t`load-secret !!)
          =^  jon  ..ya  stat-json:(logon:yac him.hem)
          (give-json 200 cug.yac jon)
        ::  
            %del  
          =.  ..ya  (logoff:yac p.hem)
          =+  cug=[(cat 3 cookie-prefix '=~; Path=/')]~
          [%| (give-json 200 cug (joba %ok %b &))]
            %get
          ~|  aute/+.hem
          ?:  |(=(anon him.hem) (~(has in aut.yac) him.hem))
            =+  pez=process(pok rem.hem)
            ?.  ?=([%& %for ^] pez)
              pez
            pez(aut.s.p (~(put ju aut.s.p.pez) %$ (scot %p him.hem)))
          ?.  =(our him.hem)
            [%| ((teba foreign-auth:for-client) him.hem hat rem.hem quy)]
          (show-login-page ~)
            %xen
          (show-login-page ~ ses.hem)
        ==
      ==
    ++  show-login-page  
      |=  ses=(unit hole)  ^-  (each pest ,_done)
      ?~  ses
        [%& %fin %code 401 %html login-page:xml]
      =+  yac=~(. ya u.ses (ses-cyst u.ses))
      =.  ..ya  abet.yac
      [%| (give-html 401 cug.yac login-page:xml)]
    ::
    ++  nice-json  ,_(give-json 200 ~ (joba %ok %b &))
    ::
    ++  load-secret
      ^-  @ta
      =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
      %^  rsh  3  1
      (scot %p (,@ (need (sky %a pax))))
    ::
    ++  cookie-prefix  (rsh 3 1 (scot %p our))
    ++  for-client                        ::  stateful per-session engine
      ^+  ya
      =+  pef=cookie-prefix
      =+  lig=(session-from-cookies pef maf)
      ?^  lig
        =+  cyz=(~(got by wup) u.lig)
        ~(. ya u.lig cyz(cug ~))
      =+  ses=(rsh 3 1 (scot %p (end 6 1 ney)))
      ~(. ya ses (ses-cyst ses))
    ::
    ++  ses-cyst
      |=  ses=hole
      =*  sec  p.hat
      =+  pef=cookie-prefix
      ^-  cyst
      :*  ^-  cred
          :*  hat(p sec)
              ~
              'not-yet-implemented' ::(rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
          ::
              =+  lag=(~(get by maf) %accept-language)
              ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
          ::
              cip
              ~
          ==
          [anon ~]
      ::
          :_  ~
          %^  cat  3
            (cat 3 (cat 3 pef '=') ses)
          ::  (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
          '; Path=/; HttpOnly'
      ::
          now
          ~
          ~
        ::  [1 ~]
      ==
    --
  ++  ya                                                ::  session engine
    =|  [ses=hole cyst]
    =*  cyz  ->
    |%
    ++  abet  ..ya(wup (~(put by wup) ses cyz))
    ++  abut  ..ya(wup (~(del by wup) ses))
    ++  logon     
      |=  her=ship
      %_  +>
        him   her
        aut   (~(put in aut) her)
        ..ya
          ~&  logon/[our her ses]
          ?.  =(our her)
            ..ya
          =+  sap=(~(get by sop) ses)
          ~&  sap
          ?.  ?=([~ @ %|] sap)
            ..ya
          (ames-gram -.u.sap aut/~ ses)
      ==
    ++  logoff    
      |=  her=(unit ship)  ^+  ..ya
      ?~  her  abut
      =.  aut  (~(del in aut) u.her)
      ?~  aut  abut
      abet(him ?.(=(her him) him n.aut))
    ::
    ++  foreign-auth
      |=  [him=ship pul=purl]  ^+  ..ya
      =.  way  (~(put by way) him pul hen)
      (ames-gram:abet him [lon/~ ses])
    ::
    ++  foreign-hat
      |=  [him=ship hat=hart]  ^+  ..ya
      ~|  way
      =^  pul  hen  (~(got by way) him)
      =.  way  (~(del by way) him)
      =.  q.q.pul  ['~' %am ses q.q.pul]
      =+  url=(welp (earn pul(p hat)) '#' (head:earn p.pul))
      %-  give-gift
      %+  add-cookies  cug
      :+  %thou  307 
      [[location/(crip url)]~ ~]
    ::
    ++  stat-json
      ^+  [*json ..ya]
      =+  orx=(rsh 3 1 (scot %p (shaf %orx eny)))
      =.  vew  (~(put in vew) orx)
      :_  abet
      %-  jobe  :~
        oryx/s/orx
        ship/(jape +:<our>)
        user/(jape +:<him>)
        auth/a/(turn (~(tap in aut)) |=(a=@p (jape +:<a>)))
      ==
    ::
    ++  flux                                            ::  credential caboose
      |=  [quy=quay ced=cred]  ^-  coin
      :*  %many
          [%$ %ta ~]
          [%blob ced]
          |-  ^-  (list coin)
          ?~  quy  ~
          [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
      ==
    ++  inte
      ^-  (unit $&([%lon purl] gram))
      ~ 
    --
  --
--
.  ==
=|  bolo
=*  bol  -
|=  [now=@da eny=@ ski=sled]                            ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%eyre-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  ^-  [p=(list move) q=_..^$]
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    abet:(apex:~(adit ye [hen [now eny our sky] ~] bol) q.hic)
  [mos ..^$]
::
++  doze                                                ::  require no timer
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  clam previous state
  |=  old=_[%0 gub hov ged ney dop liz wup=wup sop=sop]
  ^+  ..^$
  ..^$(+>- (bolo old))
::
++  scry
  |=  [our=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  ~
::
++  stay  `bolo`+>-.$
++  take                                                ::  accept response
  |=  [tea=wire hen=duct hin=(hypo sine)]
  ^-  [p=(list move) q=_..^$]
  ?:  ?=(%veer +<.q.hin)                                ::  vomit
    [[hen %give +.q.hin]~ ..^$]
  ?:  ?=(%vega +<.q.hin)                                ::  vomit
    [[hen %give +.q.hin]~ ..^$]
  =+  our=`@p`0x100  ::  XX  sentinel
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ^-  [p=(list move) q=_..^$]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    =<  abet
    %^  axon:~(adit ye [hen [now eny our sky] ~] bol)  tea 
      (~(peek ut p.hin) %free 3) 
    q.hin
  [mos ..^$]
--
