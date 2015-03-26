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
      [[%get ~] p=@uvH q=[? clip httq]]                 ::  remote request
      [[%got ~] p=@uvH q=httr]                          ::  remote response
  ==                                                    ::
++  hasp  ,[p=ship q=term]                              ::  don't see %gall
++  hapt  ,[p=ship q=path]                              ::  do see %gall
++  kiss                                                ::  in request ->$
          $%  [%born ~]                                 ::  new unix process
              [%crud p=@tas q=(list tank)]              ::  XX rethink
              [%init p=@p]                              ::  report install
              [%them p=(unit hiss)]                     ::  outbound request
              [%they p=@ud q=httr]                      ::  inbound response
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
              $:  %e                                    ::  to self
          $%  [%this p=? q=clip r=httq]                 ::  proxied request
          ==  ==                                        ::
              $:  %f                                    ::  to %ford
          $%  [%exec p=@p q=(unit silk)]                ::
              [%wasp p=@p q=@uvH]                       ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%mess p=hapt q=ship r=cage]              ::
              [%nuke p=hapt q=ship]                     ::
              [%show p=hapt q=ship r=path]              ::
              [%took p=hapt q=ship]                     ::  
          ==  ==                                        ::
              $:  %t                                    ::  to  %temp
          $%  [%wait p=@da]                             ::
              [%rest p=@da]                             ::
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
          $?  $:  %a                                    ::  by %ames
          $%  [%went p=ship q=cape]                     ::
          ==  ==                                        ::
              $:  %e                                    ::  by self
          $%  [%thou p=httr]                            ::  response for proxy
          ==  ==                                        ::
              $:  %f                                    ::  by %ford
          $%  [%made p=@uvH q=(each cage tang)]         ::
              [%news ~]                                 ::
          ==  ==                                        ::
              $:  %g                                    ::  by %gall
          $%  [%dumb ~]                                 ::
              [%mean p=ares]                            ::
              [%nice ~]                                 ::
              [%rush p=mark q=*]                        ::
              [%rust p=mark q=*]                        ::
          ==  ==                                        ::           
              $:  %t                                    ::  by %time
          $%  [%wake ~]                                 ::  timer activate
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
          ==  ==  ==                                    ::
++  ixor  ,@t                                           ::  oryx hash
++  whir  $|  ~                                         ::  wire subset
          $%  [%at p=hole q=whir]                       ::  authenticated
              [%ay p=span:ship q=span:,@uvH ~]          ::  remote duct
              [%of p=ixor ~]                            ::  associated view
              [%on p=span:,@uvH ~]                      ::  dependency
              [%to p=span:hasp q=span:ship ~]           ::  associated poke
              [%is p=ixor q=span:hasp r=path]           ::  subscription
          ==                                            ::
--                                                      ::
|%                                                      ::  models
++  bolo                                                ::  eyre state
  $:  %0                                                ::  version
      gub=@t                                            ::  random identity
      hov=(unit ship)                                   ::  master for remote
      ged=duct                                          ::  client interface
      pox=(map ,@uvH duct)                              ::  proxied sessions
      ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]         ::  outgoing by number
      kes=(map duct ,@ud)                               ::  outgoing by duct
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      liz=(jug ,@uvH (each duct ixor))                  ::  ford depsets
      wup=(map hole cyst)                               ::  secure sessions
      sop=(map hole ,[ship ?])                          ::  foreign session names
      wix=(map ixor stem)                               ::  open views
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
++  stem                                                ::  client view
  $:  ore=oryx
      ude=(unit ,[p=duct q=?])                          ::  stream, long-poll?
      era=@da                                           ::  next wake
      eve=[p=@u q=(map ,@u even)]
  ==
::
++  even                                                ::  client event
  $%  [%news p=@uv]
      [%rush p=[hasp path] q=json]
      [%mean p=[hasp path] q=ares]
  ==
::
++  perk                                                ::  parsed request
  $%  [%spur p=spur]
      [%beam p=beam]
      [%poll p=@uvH]
      [%auth perk-auth]
      [%away ~]
      [%bugs p=?(%as %to) ~]
      [%mess p=hasp q=mark r=json]
      [%subs p=hasp %json q=path]
      [%deps p=@uvH q=?(%del %put)]
      [%view p=ixor q=[~ u=@ud]]
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
      [%fot p=whir q=mark r=cage]                       ::  %f translate
      [%gap p=hapt q=ship r=cage]                       ::  %g message
      [%gas p=whir q=hapt r=ship s=path]                ::  %g subscription
      [%fin pest-fin]                                   ::  done
      [%red %html]                                      ::  redirect
      [%zap p=@ud q=(list tank)]                        ::  err
  ==
::
++  pest-fin                                            ::  response
  $|  ~
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
++  wush
  |=  [wid=@u tan=tang]
  ^-  tape
  =+  rolt=|=(a=wall `tape`?~(a ~ :(weld i.a "\0a" $(a t.a))))
  (rolt (turn tan |=(a=tank (rolt (wash 0^wid a)))))
::
++  tanx                                                ::  tanks to manx
  |=  tan=tang
  ;html
    ;head
      ;meta(charset "utf-8");
      ;title: server error
    ==
    ;body:pre:code:"{(wush 160 tan)}"
  ==
::
++  depo                                                ::  inject dependency
  |=  [dep=@uvH max=[[%html ~] [[%head ~] hed=marl] [[%body ~] manx marl] ~]]
  ^-  manx
  ?~  dep  max
  max(hed :_(hed.max ;script@"/~/on/{<dep>}.js";))
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
          return err.innerHTML = ":( " + Date.now() + "\n" + xhr.responseText
        else if(cb) return cb(xhr.responseText,ev)
      })
    }
    
    if(window.ship) ship.innerText = urb.ship
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
      function(){document.write("success!")}
    )}
    '''
  --
++  xml
  |%
  ++  login-page
    %+  titl  'Log in'
    ;=  ;p: Identify yourself, ~;{span#ship(contenteditable "")}?
        ;style:'#ship {background: lightgray} #ship br {display: none}'
        ;input#pass(onchange "urb.submit()");
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
    ==
  ::
  ++  logout-page
    %+  titl  'Log out'
    ;=  ;p: Goodbye ~;{span#ship}.
        ;button#act(onclick "urb.away()"): Log out
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
    ==
  ::
  ++  poke-test
    %+  titl  'Poke'
    ;=  ;button(onclick "urb.testPoke('/~/to/hi/txt.json')"): Hi anonymous
        ;button(onclick "urb.testPoke('/~/as/own/~/to/hi/txt.json')"): Hi
        ;pre:code#err;
        ;script@"/~/at/~/auth.js";
        ;script:'''
                show = function(t){err.innerText = ":) " + Date.now() + "\n" + t}
                urb.testPoke = function(url){
                  req(url,{xyro:{test:true}}, show)
                }
                '''
    ==
  ++  titl  |=([a=cord b=marl] ;html:(head:title:"{(trip a)}" body:"*{b}"))
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
    |=  [tee=whir typ=type sih=sign]
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    &2.sih
        %crud  +>.$(mow [[hen %slip %d %flog +.sih] mow])
        %dumb  ~|(%gall-stub !!)
        %went  +>.$
        %thou
      ?>  ?=([%ay ^] tee)
      (ames-gram (slav %p p.tee) got/~ (slav %uv q.tee) |2.sih)
    ::
        ?(%rush %rust)
      ?>  ?=([%is ^] tee)
      ?.  ?=(%json p.sih)
        =-  (ford-req tee our [%cast %json %done ~ -])
        `cage`[p.sih (slot 3 (spec (slot 3 [typ +.sih])))]
      %-  ~(get-rush ix p.tee (~(got by wix) p.tee))
      [[(pick-hasp q.tee) r.tee] ((hard json) q.sih)]
    ::
        %nice  ~|(tee ?>(?=($|(~ [%is ^]) tee) (nice-json)))
        %mean  ~|(tee ?>(?=($|(~ [%is ^]) tee) (mean-json 500 p.sih)))
        %wake
      ?>  ?=([%of ^] tee)
      =>  ~(wake ix p.tee (~(got by wix) p.tee))
      (give-json 200 ~ (joba %beat %b &))
    ::
        %news                                         ::  dependency updated
      ?.  ?=([%on ^] tee)
        ~&(e/lost/[tee hen] +>.$)
      =+  dep=(slav %uv p.tee)
      %+  roll  (~(tap in (~(get ju liz) dep)))
      =<  .(con ..axon(liz (~(del by liz) dep)))
      |=  [sus=(each duct ixor) con=_..axon]
      =.  ..axon  con
      ?-  -.sus
        %&  (give-json(hen p.sus) 205 ~ %b &) 
        %|  %-  ~(get-even ix p.sus (~(got by wix) p.sus))
            [%news dep]
      ==
    ::
        %made
      =.  our  (need hov)                             ::  XX
      |-  ^+  ..axon
      ?-    tee
          [?(%of %on %ay) *]  ~|(e/ford/lost/-.tee !!)
          [%is ^]
        %+  ~(get-rush ix p.tee (~(got by wix) p.tee))
          [(pick-hasp q.tee) r.tee]
        ?>  ?=([%& %json ^] q.sih)                    ::  XX others
        ((hard json) |3.q.sih)
      ::
          [%to ^]
        ?:  ?=(%| -.q.sih)
          (mean-json 500 ~ %cast-fail p.q.sih)
        ~|  tee
        =+  [[her app]=(pick-hasp p.tee) him=(slav %p q.tee)]
        (pass-note ~ %g [%mess [her app ~] him p.q.sih])
      ::
          [%at ^]
        ?.  ?=([%& %js ^] q.sih)
          ~&  e/at-lost/p.tee
          $(tee q.tee)
        =*  cag  p.q.sih
        ?>  ?=(@ q.q.cag)
        =+  cyz=(~(got by wup) p.tee)
        =^  jon  ..ya  ~(stat-json ya p.tee cyz)
        $(tee q.tee, q.q.p.q.sih (jass jon q.q.cag))
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
      ==     ==
    ==  ==
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
      =+  her=(host-to-ship r.p.pul)
      ?:  |(?=(~ her) =(our u.her))
        abet:~(handle rq pul [anon q.+.kyz] [p.heq maf s.heq])
      =+  han=(sham hen)
      =.  pox  (~(put by pox) han hen)
      (ames-gram u.her [%get ~] han +.kyz)
    ::
        %them                                           ::  outbound request
      ?~  p.kyz
        =+  sud=(need (~(get by kes) hen))
        %=  +>.$
          mow    :_(mow [ged [%give %thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      ::  ~&  eyre-them/(earn p.u.p.kyz)
      %=  +>.$
        mow    :_(mow [ged [%give %thus p.ask p.kyz]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.kyz)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        %they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.kyz))
      ::  ~&  >  eyre-they/[p.q.kyz (earn p.q.kas)]
      %=  +>.$
        mow    :_(mow [p.kas [%give %thou q.kyz]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        %thud                                           ::  cancel request
      :: ~&  e/gone/hen
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
      ::
        %get
          %+  pass-note  ay//(scot %p q.p.kyz)/(scot %uv p.u.mez)
          [%e %this q.u.mez]
      ::
        %got
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          (give-gift %thou q.u.mez)
      ==
    ==
  ::
  ++  pack-hasp  |=(a=hasp `span`(pack /(scot %p p.a)/[q.a]))
  ++  pick-hasp  |=(a=span (raid (need (pick a)) [%p %tas ~]))
  ++  ses-authed 
    |=  ses=hole
    =+  sap=(~(get by sop) ses)
    ?:  ?=([~ @ %&] sap)  &
    =+  cyz=(~(get by wup) ses)
    ?~  cyz  |
    (~(has in aut.u.cyz) our)
  ::
  ++  ses-ya  |=(ses=hole ~(. ya ses (~(got by wup) ses)))
  ++  our-host  `hart`[& ~ `/com/urbit/(rsh 3 1 (scot %p our))]
  ::                  [| [~ 8.445] `/localhost]       :: XX testing
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
  ++  nice-json  |=(* (give-json 200 ~ (joba %ok %b &)))
  ++  mean-json  |=([sas=@uG err=ares] (give-json sas ~ (ares-to-json err)))
  ++  ares-to-json
    |=  err=ares
    =-  (jobe fail/s/typ mess/(jape mez) ~)
    ^-  [typ=term mez=tape]
    ?~  err  [%fail "Unknown Error"]
    [p.u.err (wush 160 q.u.err)]
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
            [him=ship cip=clip]                         ::  client ip
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
    ++  nice-json  (teba ^nice-json)
    ++  pass-note  (teba ^pass-note)
    ++  ford-req   (teba ^ford-req)
    ++  back  (teba ^back)
    ::
    ++  ford-kill  (pass-note ~ %f [%exec our ~])       :: XX unused
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
      :-  ~  ^-  perk
      =*  pef  i.t.q.pok
      =+  but=t.t.q.pok                 ::  XX  =*
      ~|  [pef %bad-path but quy]
      ?+    pef  ~|(pfix-lost/`path`/~/[pef] !!)
          %on  [%poll (raid but %uv ~)]
          %of
        :+  %view  ?>(?=([@ ~] but) i.but)
        ?>  ?=([[%poll @] ~] quy)     ::  XX eventsource
        [~ (rash q.i.quy dem)]
      ::
          %as
        :+  %auth  %get
        ~|  bad-ship/?~(but ~ i.but)
        ?~  but  !!
        :_  pok(q t.but)
        ?+  i.but  (slav %p i.but)
          %anon  anon
          %own   our
        ==
      ::
          %am  ?~(but !! [%auth %xen i.but pok(q t.but)])
          %at  [%auth %at pok(q but)]
          %away  [%away ~]
          %debug  ;;(perk [%bugs but])
          %to
        =-  [%mess [- +<]:dir +>.dir (grab-body (ot:jo xyro/some ~))]
        ^=  dir
        =+  ful=(read but %p %tas %tas ~)
        ?^  ful  u.ful
        ~|  bad-mess/but
        [our (raid but %tas %tas ~)]
      ::
          %in
        ~|  expect/[%post 'application/json' /'@uv' '?PUT/DELETE']
        ?>  ?=([%post $|(~ [~ %json])] [mef p.pok])
        :+    %deps
          (raid but %uv ~)
        ?+(quy !! [[%'DELETE' ~] ~] %del, [[%'PUT' ~] ~] %put)
      ::
          %is
        ?~  but
          ~|(no-app/but=but !!)
        |-  ^-  perk
        ?~  p.pok  $(p.pok [~ %json])
        ?>  ?=(%json u.p.pok)         ::  XX marks
        ?:  ((sane %tas) i.but)
          $(but [(scot %p our) but])
        [%subs [(slav %p i.but) (slav %tas -.t.but)] u.p.pok +.t.but]
      ::
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
              [%try (grab-body paz)]
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
    ++  grab-body  |*(a=fist:jo (need (parse-body a)))
    ++  parse-body
      |*  a=fist:jo  ^+  *a
      ?.  &(?=(%post mef) ?=(^ bod))
        ~
      %.(q.u.bod ;~(biff poja a))
    ::
    ++  handle
      ^+  done
      =+  oar=(host-to-ship r.hat)
      =.  our  ?~(oar our u.oar)  ::  XX
      =+  pez=process
      ?:  ?=(%| -.pez)  p.pez  ::  XX transitional
      (handle-pest p.pez)
    ::
    ++  handle-pest
      |=  pez=pest  ^+  done
      ?-  -.pez
          %for  (beam-into-ford +.pez)
          %fot  (ford-req p.pez our [%cast q.pez %done ~ r.pez])
          %gap  (pass-note ~ %g [%mess +.pez])
          %gas  (pass-note p.pez %g [%show +>.pez])
          %zap  (fail p.pez 0v0 q.pez)
          %fin  (finish +.pez)
          %red
        =+  url=(earn hat pok(p [~ %html]) quy)
        ?+    p.pok  (fail 404 0v0 leaf/"bad redirect" leaf/<p.pok> leaf/url ~)
            [~ %js]
          $(pez [%fin %js (crip "document.location = '{url}'")])
            [~ %json]
          $(pez [%fin %json (jobe ok/b/| red/(jape url) ~)])
        ==
      ==
    ::
    ++  finish
      =+  status=200
      |=  pef=pest-fin  ^+  done
      ?~  pef  done
      ?-  -.pef
        ~  (give-gift %thou p.pef)
        %js  (resp status text//javascript p.pef)
        %html  (give-html status ~ p.pef)
        %json  (give-json status ~ p.pef)
        %code  $(pef q.pef, status p.pef)
      ==
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
      ?.  check-oryx
        ~|(%bad-oryx ~|([(parse-body to-oryx) vew.cyz:for-client] !!))
      =+  hem=as-aux-request
      ?^  hem  (process-parsed u.hem)
      ~|(strange-path/q.pok !!)
    ::
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=([~ %json] p.pok) ?=(%post mef) ?=(^ bod))  &
      =+  oxe=(parse-body to-oryx)
      ?~  oxe  |
      (~(has in vew.cyz:for-client) u.oxe)
    ::
    ++  to-oryx  (ot oryx/so ~):jo
    ++  root-beak  `beak`[our %main ud/0]               ::  XX
    ++  add-depend
      |=  [a=@uvH b=(each duct ixor)]  ^+  done
      ?~  a  done
      =+  had=(~(has by liz) a)
      =.  liz  (~(put ju liz) a b)
      ?:  had  done
      (pass-note on//(scot %uv a) %f [%wasp our a])
    ::
    ++  process-parsed
      |=  hem=perk
      ^-  (each pest ,_done)
      ?-    -.hem
          %away  [%& %fin %html logout-page:xml]
          ?(%spur %beam)  
        =+  ext=(fall p.pok %urb)
        =+  bem=?-(-.hem %beam p.hem, %spur [root-beak p.hem])
        [%& %for ~ bem ext ced.cyz:for-client]
      ::
          %deps
        =+  ire=(oryx-to-ixor (grab-body to-oryx))
        ?>  (~(has by wix) ire)  ::  XX made redundant by oryx checking
        =<  [%| (nice-json)]
        ?-  q.hem
          %del  done(liz (~(del ju liz) p.hem %| ire))
          %put  (add-depend p.hem %| ire)
        ==
      ::
          %poll
        ?.  ?=([~ %js] p.pok)  ::  XX treat non-json cases?
          [%| (add-depend p.hem %& hen)]
        =+  polling-url=['/' (apex:earn %| pok(u.p %json) quy)]
        :^  %&  %fin  %js
        (jass (joba %poll (jape polling-url)) poll:js)
      ::
          %bugs  
        ?-  p.hem
         %as  (show-login-page)
         %to  [%& %fin %html poke-test:xml]
        ==
      ::
          %mess
        =+  cay=[%json !>(`json`r.hem)]
        ?:  ?=(%json q.hem)
          [%& %gap [- + ~]:p.hem him cay]
        =+  wir=to//(pack-hasp p.hem)/(scot %p him)
        [%& %fot wir q.hem cay]
      ::
          %subs
        =+  ire=(oryx-to-ixor (grab-body to-oryx))
        :^  %&  %gas
          [%is ire (pack-hasp p.hem) q.hem]
        [[- + ~]:p.hem him q.hem]
      ::
          %view
        ~|  lost-ixor/p.hem
        =+  sem=(~(got by wix) p.hem)
        [%| ((teba ~(poll ix p.hem sem)) u.q.hem)]
      ::
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
          ?+    -.p.pez  ~&(bad-inject/p.pez !!)
              ?(%zap %red)
            pez
              %for
            =.  ..ya  abet:for-client                     
            [%| (handle-pest p.pez(p [%at ses.yac p.p.pez]))]  ::  XX better storage?
              %fin  
            ~|  %not-script
            ?>  ?=(%js &2.p.pez)
            =^  jon  ..ya  stat-json:for-client
            [%| (finish +.p.pez(p (jass jon p.p.pez)))] ::  XX better storage?
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
            =.  him  him.hem
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
      ?.  ?=($|(~ [~ %html]) p.pok)
        [%& %red %html]
      ?~  ses
        [%& %fin %code 401 %html login-page:xml]
      =+  yac=~(. ya u.ses (ses-cyst u.ses))
      =.  ..ya  abet.yac
      [%| (give-html 401 cug.yac login-page:xml)]
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
  ::
  ++  oryx-to-ixor  |=(a=oryx (rsh 3 1 (scot %p (end 6 1 (shas %ire a)))))
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
      =:  way       (~(del by way) him)
          dop       (~(put by dop) r.hat him)  
          q.q.pul   ['~' %am ses q.q.pul]
        ==
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
      =+  ire=(oryx-to-ixor orx)
      =.  wix  (~(put by wix) ire [orx ~ now [1 ~]])
      :_  abet
      %-  jobe  :~
        oryx/s/orx
        ixor/s/ire
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
  ::
  ++  ix
    =|  [ire=ixor stem]
    =*  sem  ->
    |%
    ++  done  .
    ++  abet  ..ix(wix (~(put by wix) ire sem))
    ++  teba  |*(a=$+(* ..ix) |*(b=* %_(done ..ix (a b))))
    ++  give-json  (teba ^give-json)
    ++  pass-note  (teba ^pass-note)
    ::
    ++  poll
      |=  a=@u  ^+  ..ix
      ?:  =(a p.eve)
        =.  +>  
          ?^  ude  ~&(e/ix/wait/%replaced done)
          wait-era(era (add ~s30 now))
        abet(ude [~ hen &])
      ?:  (gth a p.eve)  ~|(seq-high/cur=p.eve !!)
      =+  ven=~|(seq-low/cur=p.eve (~(got by q.eve) a))
      =.  q.eve  (~(del by q.eve) (dec a))              ::  TODO ponder a-2
      abet:(give-even & a ven)
    ::
    ++  get-rush
      |=  [a=[hasp path] b=json]  ^+  ..ix
      (get-even [%rush a (joba %json b)])
    ::
    ++  get-even
      |=  ven=even  ^+  ..ix
      =+  num=p.eve
      =.  eve  (add-even ven)
      =<  abet
      ?~  ude  done
      (give-even(hen p.u.ude, ude ~) q.u.ude num ven)
    ::
    ++  add-even
      |=  a=even  ^+  eve
      [+(p.eve) (~(put by q.eve) p.eve a)]
    ::
    ++  give-even
      |=  [pol=? num=@u ven=even]  ^+  done
      ?>  pol                         ::  XX eventstream
      %^  give-json  200  ~
      %^  jobe  id/(jone num)  type/[%s -.ven]
      ?-  -.ven
        %news  ~[from/[%s (scot %uv p.ven)]]
        %rush  ~[from/(subs-to-json p.ven) data/q.ven]
        %mean  ~[from/(subs-to-json p.ven) data/(ares-to-json q.ven)]
      ==
    ::
    ++  subs-to-json
      |=  [a=hasp b=path]
      %-  jobe  :~
        ship/[%s (rsh 3 1 (scot %p p.a))]
        appl/[%s q.a] 
        path/(jape (spud b))
      ==
    ::
    ::  XX unused
    ++  print-subs  |=([a=hasp b=path] "{<p.a>}/{(trip q.a)}{(spud b)}")
    ++  wait-era  (pass-note of//[ire] [%t %wait era])
    ++  wake  ^+(..ix abet(ude ~))  ::  XX other effects?
--  --
  --
.   ==
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
  |=  old=bolo ::_[%0 gub hov ged ney dop liz wup sop wix=**]
  ^+  ..^$
  ..^$(+>- old)
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
  =+  tee=((soft whir) tea)
  ?~  tee  ~&  [%e %lost -.q.hin hen]  [~ ..^$]
  =^  mos  bol
    =<  abet
    %^  axon:~(adit ye [hen [now eny our sky] ~] bol)  u.tee
      (~(peek ut p.hin) %free 3) 
    q.hin
  [mos ..^$]
--
