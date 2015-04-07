!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
=>  =~
|%                                                      ::  interfaces
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
              [%of p=ixor q=$|(~ whir-of)]              ::  associated view
              [%on p=span:,@uvH ~]                      ::  dependency
              [%to p=ixor q=span:ship r=term s=wire]    ::  associated app
          ==                                            ::
++  whir-of  ,[p=span:ship q=term r=wire]               ::  path in hasp
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
  $:  him=ship                                          ::  static identity
      ude=(unit ,[p=duct q=?])                          ::  stream, long-poll?
      era=@da                                           ::  next wake
      eve=[p=@u q=(map ,@u even)]                       ::  queued events
      med=(qeu duct)                                    ::  waiting /~/to/
  ==
::
++  even                                                ::  client event
  $%  [%mean p=[hasp path] q=ares]
      [%news p=@uv]
      [%rush p=[hasp path] q=json]
  ==
::
++  perk                                                ::  parsed request
  $%  [%auth p=perk-auth]
      [%away ~]
      [%bugs p=?(%as %to) ~]
      [%beam p=beam]
      [%deps p=?(%put %delt) q=@uvH]
      [%mess p=hasp q=mark r=wire s=json]
      [%poll p=@uvH]
      [%spur p=spur]
      [%subs p=?(%put %delt) q=[hasp %json wire path]]
      [%view p=ixor q=[~ u=@ud]]
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  [%at p=pork]                               ::  inject auth
      [%del p=(unit ship)]
      [%get him=ship rem=pork]
      [%js ~]
      [%json ~]
      [%try him=ship cod=cord]
      [%xen ses=hole rem=pork]
  ==
::
++  pest                                                ::  result
  $|  ~
  $%  [%| p=whir q=note]                                ::  further request
      [%$ p=httr]                                       ::  direct response
      [%red ~]                                          ::  parent redirect
  ::
      [%js p=@t]                                        ::  script
      [%json p=json]                                    ::  data
      [%html p=manx]                                    ::  successful page
      [%htme p=manx]                                    ::  authentication failure
  ==
--                                                      ::
|%
++  eat-headers
  |=  hed=(list ,[p=@t q=@t])  ^-  math
  %+  roll  hed
  |=  [a=[p=cord cord] b=math] 
  =.  p.a  (cass (trip p.a))
  (~(add ja b) a)
::
++  fcgi                                                ::  credential caboose
  |=  [quy=quay ced=cred]  ^-  coin
  :*  %many
      [%$ %ta ~]
      [%blob ced]
      |-  ^-  (list coin)
      ?~  quy  ~
      [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
  ==
::
++  gsig  |=([a=hasp b=path] [(scot %p p.a) q.a b])
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
++  wush
  |=  [wid=@u tan=tang]
  ^-  tape
  =+  rolt=|=(a=wall `tape`?~(a ~ :(weld i.a "\0a" $(a t.a))))
  (rolt (turn tan |=(a=tank (rolt (wash 0^wid a)))))
::
::
++  add-cookies
  |=  [cug=(list ,@t) hit=httr]  ^-  httr
  ?~  cug  hit
  =+  cuh=(turn `(list ,@t)`cug |=(a=@t set-cookie/a))
  hit(q (weld cuh q.hit))
::
++  add-poll                                            ::  inject dependency
  |=  [dep=@uvH max=[[%html ~] [[%head ~] hed=marl] [[%body ~] manx marl] ~]]
  ^-  manx
  ?~  dep  max
  max(hed :_(hed.max ;script@"/~/on/{<dep>}.js";))
::
++  add-json                                            ::  inject window.urb
  |=  [urb=json jaz=cord]  ^-  cord
  (cat 3 (crip "window.urb = {(pojo urb)}\0a") jaz)
::
++  ares-to-json
  |=  err=ares  ^-  json
  =-  (jobe fail/s/typ mess/(jape mez) ~)
  ^-  [typ=term mez=tape]
  ?~  err  [%fail "Unknown Error"]
  [p.u.err (wush 160 q.u.err)]
::
++  resp                                                ::  mimed response
  |=  [sas=@uG mit=mite rez=@]  ^-  httr
  ::  (weld (turn cug |=(a=@t ['set-cookie' a]))
  [sas ~[content-type/(moon mit)] [~ (taco rez)]]
::
++  render-tang                                         ::  tanks to manx
  |=  tan=tang
  ;html
    ;head
      ;meta(charset "utf-8");
      ;title: server error
    ==
    ;body:pre:code:"{(wush 160 tan)}"
  ==
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
  ++  auth-redir
    'document.location.pathname = "/~~"+document.location.pathname'
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
  ++  apex                                              ::  accept request
    |=  kyz=kiss
    ^+  +>
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    -.kyz
        %born  +>.$(ged hen)                            ::  register external
        %crud
      +>.$(mow [[hen %slip %d %flog kyz] mow])
    ::
        %init                                           ::  register ownership
      +>.$(hov ?~(hov [~ p.kyz] [~ (min u.hov p.kyz)]))
    ::
        %this                                           ::  inbound request
      %-  emule  |.  ^+  ..apex
      =*  sec  p.kyz    ::  ?                           ::  https bit
      =*  heq  r.kyz    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  maf=(eat-headers r.heq)
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
        (handle pul q.+.kyz [p.heq maf s.heq])
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
      ::  ford-kill  ::  XX discriminate
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
        %get  (pass-note ay/(dray p/uv/~ q.p.kyz p.u.mez) [%e %this q.u.mez])
        %got
          =:  hen  (~(got by pox) p.u.mez)
              pox  (~(del by pox) p.u.mez)
            ==
          (give-thou q.u.mez)
      ::
        %lon  
          ~&  ses-ask/[p.u.mez sop (~(run by wup) ,~)]
          ?:  (ses-authed p.u.mez)  
            (ames-gram q.p.kyz aut/~ p.u.mez)
          =.  sop  (~(put by sop) p.u.mez q.p.kyz |)
          (ames-gram q.p.kyz hat/~ p.u.mez our-host)
      ==
    ==
  ::
  ++  axon                                              ::  accept response
    |=  [tee=whir typ=type sih=sign]
    ^+  +>
    :: %-  emule  |.  ^+  ..axon
    =.  our  ?~(hov our u.hov)  ::  XX
    ?-    &2.sih
        %crud  +>.$(mow [[hen %slip %d %flog +.sih] mow])
        %dumb  
      =.  +>  ?+(tee +> [%of ^] pop-duct:(ire-ix p.tee))
      (emule |.(~|(gall-dumb/tee !!)))
    ::
        %went  +>.$
        %thou
      ?>  ?=([%ay ^] tee)
      (ames-gram (slav %p p.tee) got/~ (slav %uv q.tee) |2.sih)
    ::
        ?(%rush %rust)
      ?>  ?=([%of @ ^] tee)
      ?.  ?=(%json p.sih)
        =-  (back tee 0v0 %json -)
        `cage`[p.sih (slot 3 (spec (slot 3 [typ +.sih])))]
      ~!  [q.tee *whir-of]
      (get-rush:(ire-ix p.tee) q.tee ((hard json) q.sih))
    ::
        ?(%nice %mean)
      ::  ~&  [tee `@tas`&2.sih]
      ?+  tee  !!
        ~   ~&  empty-ack/&2.sih
            ?-(&2.sih %nice (nice-json), %mean (mean-json 500 p.sih))
        [%of @ ^]  (get-ack:(ire-ix p.tee) q.tee +.sih)
      ==
    ::
        %wake
      ?>  ?=([%of @ ~] tee)
      =>  wake:(ire-ix p.tee)
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
        %|  (get-even:(ire-ix p.sus) [%news dep])
      ==
    ::
        %made
      =.  our  (need hov)                             ::  XX
      |-  ^+  ..axon
      ?-    tee
          [?(%on %ay) *]  ~|(e/ford/lost/-.tee !!)
          [%of ^]
        ?~  q.tee  ~|(e/ford/lost/tee !!)
        %+  get-rush:(ire-ix p.tee)  q.tee
        ?>  ?=([%& %json ^] q.sih)                    ::  XX others
        ((hard json) |3.q.sih)
      ::
          [%to ^]
        ?:  ?=(%| -.q.sih)
          (mean-json 500 ~ %cast-fail p.q.sih)
        ~|  tee
        (new-mess:(ire-ix p.tee) [(slav %p q.tee) r.tee] s.tee p.q.sih)
      ::
          [%at ^]
        ?.  ?=([%& %js ^] q.sih)
          ~&  e/at-lost/p.tee
          $(tee q.tee)
        =*  cay  p.q.sih
        ?>  ?=(@ q.q.cay)
        =+  cyz=(~(got by wup) p.tee)
        =^  jon  ..ya  ~(stat-json ya p.tee cyz)
        $(tee q.tee, q.q.p.q.sih (add-json jon q.q.cay))
      ::
          ~      
        :: ~&  e/ford/hen
        ?.  ?=(%& -.q.sih)
          (fail 404 p.sih p.q.sih)
        =*  cay  p.q.sih
        ?.  ?=(%mime p.cay)
          (back tee p.sih %mime cay)
        ~|  q.q.cay
        =+  ((hard ,[mit=mite rez=octs]) q.q.cay)
        (give-thou 200 [content-type/(moon mit)]~ ~ rez)
      ==
    ==
  ::
  ++  emule
    |=  a=_|?(..emule)  ^+  ..emule
    =+  mul=(mule a)
    ?~  -.mul  p.mul
    (fail 500 0v0 >%exit< p.mul)
  ::
  ++  ire-ix  |=(ire=ixor ~(. ix ire (~(got by wix) ire)))
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
  ::
  ++  ames-gram
    |=([him=ship gam=gram] (pass-note ~ %a %want [our him] [%e -.gam] +.gam))
  ::
  ++  back                                              ::  %ford bounce
    |=  [tea=whir dep=@uvH mar=mark cay=cage]                
    (pass-note tea (ford-req [%cast mar %done ~ cay])) ::  XX deps
  ::
  ++  ford-kill  (pass-note ~ %f [%exec our ~])        :: XX unused
  ++  ford-req  |=(kas=silk [%f [%exec our `kas]])
  ::
  ++  fail
    |=  [sas=@ud dep=@uvH mez=tang]
    ^+  +>
    ::  (back ~ dep %tang !>(mez))  ::  tang->urb chain may be source of failure
    (give-html sas ~ (add-poll dep (render-tang mez)))
  ::
  ++  give-html
    |=  [sas=@ud cug=(list ,@t) max=manx]
    %-  give-thou
    %+  add-cookies  cug
    (resp sas text//html (crip (poxo max)))
  ::
  ++  give-json
    |=  [sas=@uG cug=(list ,@t) jon=json]
    %-  give-thou
    %+  add-cookies  cug
    (resp sas application//json (crip (pojo jon)))
  ::
  ++  give-thou                                       ::  done request
    |=  hit=httr
    +>(mow :_(mow [hen %give %thou hit]))
  ::
  ++  mean-json  |=([sas=@uG err=ares] (give-json sas ~ (ares-to-json err)))
  ++  nice-json  |=(* (give-json 200 ~ (joba %ok %b &)))
  ::
  ++  pass-note  |=(noe=[whir note] +>(mow :_(mow [hen %pass noe])))
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
  ++  load-secret
    ^-  @ta
    =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
    %^  rsh  3  1
    (scot %p (,@ (need (sky %a pax))))
  ::
  ::
  ++  handle
    |=  $:  [hat=hart pok=pork quy=quay]                ::  purl, parsed url
            cip=clip                                    ::  client ip
            [mef=meth maf=math bod=(unit octs)]         ::  method/headers/body
        ==
    =<  apex
    |%
    ++  abet  ..handle
    ++  done  .
    ++  teba  |*(a=$+(* ..handle) |*(b=* %_(done ..handle (a b))))
    ++  give-html  (teba ^give-html)
    ++  give-thou  (teba ^give-thou)
    ++  give-json  (teba ^give-json)
    ++  nice-json  (teba ^nice-json)
    ++  pass-note  (teba ^pass-note)
    ::
    ++  ford-get-beam
      |=  [bem=beam ext=term]
      =:  s.bem  [%web ~(rent co (fcgi quy fcgi-cred:for-client)) s.bem]
          r.bem  ?+(r.bem r.bem [%ud %0] da/now)
        ==
      (ford-req [%boil ext bem ~])
    ::
    ::
    ++  apex                                              
      =<  abet
      ^+  done
      =+  oar=(host-to-ship r.hat)
      =.  our  ?~(oar our u.oar)  ::  XX
      =+  pez=process
      ?:  ?=(%| -.pez)  p.pez
      (resolve ~ p.pez)
    ::
    ++  resolve
      |=  [cug=(list ,@t) pez=pest]  ^+  done
      ?~  pez  done
      ?-  -.pez
          ~  (give-thou (add-cookies cug p.pez))
          |  (pass-note +.pez)
          %js  $(pez [~ (resp 200 text//javascript p.pez)])
          %json  (give-json 200 cug p.pez)
          %html  (give-html 200 cug p.pez)
          %htme  (give-html 401 cug p.pez)
          %red
        =+  url=(earn hat pok(p [~ %html]) quy)
        ?+    p.pok  ~|(bad-redirect/[p.pok url] !!)
            [~ %js]
          $(pez [%js auth-redir:js])
            [~ %json]
          $(pez [%json (jobe ok/b/| red/(jape url) ~)])
        ==
      ==
    ::
    ::
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=([~ %json] p.pok) ?=(%post mef) ?=(^ bod))  &
      =+  oxe=(grab-body to-oryx)
      ?~  oxe  |
      ?:  (~(has in vew.cyz:for-client) u.oxe)
        &
      ~&(bad-oryx/[u.oxe vew.cyz:for-client] &)         ::  XX security
    ::
    ++  grab-body
      |*  a=fist:jo  ^+  *a
      ?.  &(?=(^ bod) ?=(?(%post %put %delt) mef))
        ~
      %.(q.u.bod ;~(biff poja a))
    ::
    ++  need-body  |*(a=fist:jo (need (grab-body a)))
    ++  root-beak  `beak`[our %main ud/0]               ::  XX
    ++  to-oryx  (ot oryx/so ~):jo
    ::
    ::
    ++  new-dependency
      |=  [a=@uvH b=(each duct ixor)]  ^+  done
      ?~  a  done
      =+  had=(~(has by liz) a)
      =.  liz  (~(put ju liz) a b)
      ?:  had  done
      (pass-note on//(scot %uv a) %f [%wasp our a])
    ::
    ++  parse
      ^-  (each perk httr)
      |^  =+  hit=as-magic-filename
          ?^  hit  [%| u.hit]
          =+  bem=as-beam
          ?^  bem  [%& %beam u.bem]
          ?.  check-oryx
            ~|(%bad-oryx ~|([(grab-body to-oryx) vew.cyz:for-client] !!))
          =+  hem=as-aux-request
          ?^  hem  [%& u.hem]
          ~|(strange-path/q.pok !!)
      ::
      ++  as-magic-filename
        ^-  (unit httr)
        ?+    [(fall p.pok %$) q.pok]  ~
            [?(%ico %png) %favicon ~]
          :-  ~
          %^  resp  200  image//png
          0w89wg.GV4jA.l9000.00dPb.YzBT6.giO00.o100d.wZcqc.a9tg-.VTG0b.
          AUIvE.HBM3g.cK4SE.0aagi.l090p.I1P5g.Y-80r.y1YS9.1xE~Y.qgpFY.
          vKN1V.905y0.2UwvL.43TUw.uL406.0-31h.xwoJF.Ul454.ilk00.00Yps.
          BNumh.xpl9B.pS5Ji.i1BoC.ZAgg1.BsC5T.t6pLk.Thohn.gp000.0ov~P.
          7M000.0o840.00010.0001i.h4x93.g0000.Eq2wR.7jB29
        ::
            [%txt %robots ~]
          :-  ~
          %^  resp  200  text//plain
          %-  role
          :~  'User-agent: *'
              'Disallow: /'
          ==
        ==
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
      ++  as-aux-request                                ::  /~/... req parser
        ^-  (unit perk)
        =.  mef
          ?.  ?=(%post mef)  mef
          ?+  quy  ~|(bad-quy/[req='"?PUT" or "?DELETE"' quy] !!)
            ~   mef
            [[%'DELETE' ~] ~]  %delt
            [[%'PUT' ~] ~]     %put
          ==
        |-
        ?:  ?=([%'~~' *] q.pok)                            ::  auth shortcut
          $(q.pok ['~' %as %own t.q.pok])
        ?.  ?=([%'~' @ *] q.pok)  ~
        :-  ~  ^-  perk
        =*  pef  i.t.q.pok
        =+  but=t.t.q.pok                 ::  XX  =*
        ?+    pef  ~|(pfix-lost/`path`/~/[pef] !!)
            %debug  ;;(perk [%bugs but])
            %away  [%away ~]
            %at  [%auth %at pok(q but)]
            %am  ?~(but !! [%auth %xen i.but pok(q t.but)])
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
            %on  [%poll (raid but %uv ~)]
            %of
          :+  %view  ?>(?=([@ ~] but) i.but)
          ?>  ?=([[%poll @] ~] quy)     ::  XX eventsource
          [~ (rash q.i.quy dem)]
        ::
            %to
          ?>  =('/' (need-body (ot wire/so ~):jo))  ::  XX custom
          =-  :^  %mess  [- +<]:dir  +>.dir 
              (need-body (ot wire/(cu stab so) xyro/some ~):jo)
          ^=  dir
          =+  ful=(read but %p %tas %tas ~)
          ?^  ful  u.ful
          ~|  bad-mess/but
          [our (raid but %tas %tas ~)]
        ::
            %in
          ~|  expect/[%post 'application/json' /'@uv' '?PUT/DELETE']
          ?>  &(?=(?(%delt %put) mef) ?=($|(~ [~ %json]) p.pok))
          [%deps mef (raid but %uv ~)]
        ::
            %is
          ?~  but
            ~|(no-app/but=but !!)
          |-  ^-  perk
          ?~  p.pok  $(p.pok [~ %json])
          ?.  ?=(%json u.p.pok)
            ~|(is/stub/u.p.pok !!)      ::  XX marks
          ?:  ((sane %tas) i.but)
            $(but [(scot %p our) but])
          ?>  ?=(?(%delt %put) mef)
          =+  :-  hap=[(slav %p i.but) (slav %tas -.t.but)]
              wir=(need-body (ot wire/(cu stab so) ~):jo)
          [%subs mef hap u.p.pok wir +.t.but]
        ::
            %auth
          :-  %auth
          |-  ^-  perk-auth
          ?+    p.pok  !!
              ~          $(p.pok [~ %json])
              [~ %js]    [%js ~]
              [~ %json]
            ?+    mef  ~|(bad-meth/mef !!)
                %get   [%json ~]
                %put
              ~|  parsing/bod
              [%try (need-body (ot ship/(su fed:ag) code/so ~):jo)]
            ::
                %delt
              ~|  parsing/bod
              =+  jon=(need (poja q:(need bod)))
              ?>  ?=(%o -.jon)
              =+  sip=(~(get by p.jon) %ship)
              [%del ?~(sip ~ [~ (need ((su:jo fed:ag) u.sip))])]
        ==  ==
        ==
      --
    ::
    ++  process
      ^-  (each pest ,_done)
      =+  pet=parse
      ?:  ?=(%| -.pet)
        [%& ~ p.pet]
      (process-parsed p.pet)
    ::
    ++  process-parsed
      |=  hem=perk  ^-  (each pest ,_done)
      ?-    -.hem
          %auth  (process-auth p.hem)
          %away  [%& %html logout-page:xml]
          ?(%beam %spur)
        =+  ext=(fall p.pok %urb)
        =+  bem=?-(-.hem %beam p.hem, %spur [root-beak p.hem])
        [%& %| ~ (ford-get-beam bem ext)]
      ::
          %bugs  
        ?-  p.hem
          %as  (show-login-page)
          %to  [%& %html poke-test:xml]
        ==
      ::
          %deps
        =+  ire=(oryx-to-ixor (need-body to-oryx))
        ?>  (~(has by wix) ire)  ::  XX made redundant by oryx checking
        =<  [%| (nice-json)]
        ?-  p.hem
          %put   (new-dependency q.hem %| ire)
          %delt  done(liz (~(del ju liz) q.hem %| ire))
        ==
      ::
          %mess
        =+  [him=him:for-view cay=[%json !>(`json`s.hem)]]
        ?:  ?=(%json q.hem)
          [%| ((teba new-mess:for-view) p.hem r.hem cay)]
        :^  %&  %|  [%to need-ixor (scot %p p.p.hem) q.p.hem r.hem]
        (ford-req [%cast q.hem %done ~ cay])
      ::
          %poll
        ?.  ?=([~ %js] p.pok)  ::  XX treat non-json cases?
          [%| (new-dependency p.hem %& hen)]
        =+  polling-url=['/' (apex:earn %| pok(u.p %json) quy)]
        [%& %js (add-json (joba %poll (jape polling-url)) poll:js)]
      ::
          %subs
        ?-  p.hem
          %put   [%| ((teba add-subs:for-view) q.hem)]
          %delt  [%| ((teba del-subs:for-view) q.hem)]
        ==
      ::
          %view
        ~|  lost-ixor/p.hem
        [%| ((teba poll:(ire-ix p.hem)) u.q.hem)]
      ==
    ::
    ++  process-auth
      |=  ham=perk-auth  ^-  (each pest ,_done)
      =+  yac=for-client
      ?-    -.ham
          %js    [%& %js auth:js]
          %json  =^  jon  ..ya  stat-json.yac
                 [%| (give-json 200 ~ jon)]
          %xen   (show-login-page ~ ses.ham)
      ::
          %at
        =.  ..ya  abet.yac
        =+  pez=process(pok p.ham)
        ?.  ?=(%& -.pez)  ~|(no-inject/p.ham !!)
        ?~  p.pez  pez
        ?+    -.p.pez  ~&(bad-inject/p.pez !!)
            %red  pez
            %|
          =.  ya  abet.yac 
          [%| (resolve ~ p.pez(p [%at ses.yac p.p.pez]))]
        ::
            %js
          =^  jon  ..ya  stat-json.yac
          [%| (resolve cug.yac p.pez(p (add-json jon p.p.pez)))]
        ==
      ::
          %del  
        =.  ..ya  (logoff:yac p.ham)
        =+  cug=[(cat 3 cookie-prefix '=~; Path=/')]~
        [%| (give-json 200 cug (joba %ok %b &))]
      ::
          %get
        ~|  aute/ham
        ?:  |(=(anon him.ham) (~(has in aut.yac) him.ham))
          process(pok rem.ham, ..ya abet.yac(him him.ham))
        ?.  =(our him.ham)
          [%| ((teba foreign-auth.yac) him.ham hat rem.ham quy)]
        (show-login-page ~)
      ::
          %try
        ~&  ses-try/ses.yac
        :-  %|
        ?.  =(our him.ham)
          ~|(stub-foreign/him.ham !!)
        ?.  =(load-secret cod.ham)
          ~|(try/`@t`load-secret !!)  ::  XX security
        =^  jon  ..ya  stat-json:(logon:yac him.ham)
        (give-json 200 cug.yac jon)
      ==
    ::
    ++  show-login-page
      |=  ses=(unit hole)  ^-  (each pest ,_done)
      ?.  ?=($|(~ [~ %html]) p.pok)
        [%& %red ~]
      ?~  ses
        [%& %htme login-page:xml]
      ?:  (~(has by wup) u.ses)
        [%& %htme login-page:xml]
      =+  yac=(new-ya u.ses)
      =.  ..ya  abet.yac
      [%| (give-html 401 cug.yac login-page:xml)]
    ::
    ++  cookie-prefix  (rsh 3 1 (scot %p our))
    ++  need-ixor  (oryx-to-ixor (need-body to-oryx))
    ++  for-view  ^+(ix (ire-ix need-ixor))
    ::
    ++  for-client                        ::  stateful per-session engine
      ^+  ya
      =+  pef=cookie-prefix
      =+  lig=(session-from-cookies pef maf)
      ?~  lig
        (new-ya (rsh 3 1 (scot %p (end 6 1 ney))))
      ~|  bad-cookie/u.lig
      =+  cyz=(~(got by wup) u.lig)
      ~(. ya u.lig cyz(cug ~))
    ::
    ++  new-ya  |=(ses=hole ~(. ya ses (new-cyst ses)))
    ++  new-cyst
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
      %-  give-thou:abet
      (add-cookies cug [307 [location/(crip url)]~ ~])
    ::
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
    ++  fcgi-cred  %_(ced aut (~(put ju aut.ced) %$ (scot %p him)))
    ++  stat-json
      ^+  [*json ..ya]
      =+  orx=`@t`(rsh 3 1 (scot %p (shaf %orx eny)))
      =.  vew  (~(put in vew) orx)
      =+  [ire=(oryx-to-ixor orx) sem=*stem]
      =.  wix  (~(put by wix) ire sem(him him, era now, p.eve 1))
      ::  ~&  stat-ire/`@t`ire
      :_  abet
      %-  jobe  :~
        oryx/s/orx
        ixor/s/ire
        ship/(jape +:<our>)
        user/(jape +:<him>)
        auth/a/(turn (~(tap in aut)) |=(a=@p (jape +:<a>)))
      ==
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
    ++  hurl-note 
      |=  [a=[hasp path] b=note]  ^+  ..ix
      =:  med  (~(put to med) hen)
          hen  `~
        ==
      :: ~&  >  hurl/[&2.b ire a]
      (pass-note:abet [%of ire (gsig a)] b)
    ::
    ++  add-even
      |=  a=even  ^+  eve
      [+(p.eve) (~(put by q.eve) p.eve a)]
    ::
    ++  new-mess
      |=  [a=hasp b=wire c=cage]  ^+  ..ix
      (hurl-note [a b] [%g %mess [- + ~]:a him c])
    ::
    ++  add-subs
      |=  [a=hasp %json b=wire c=path]  ^+  ..ix
      (hurl-note [a b] [%g %show [- + ~]:a him c])
    ::
    ++  del-subs                      ::  XX per path?
      |=  [a=hasp %json b=wire c=path]  ^+  ..ix
      (hurl-note [a b] [%g %nuke [- + ~]:a him])
    ::
    ++  get-rush
      |=  [a=whir-of b=json]  ^+  ..ix
      (get-even [%rush [[(slav %p p.a) q.a] r.a] (joba %json b)])
    ::
    ++  get-ack
      |=  [a=whir-of b=$&([%nice ~] [%mean p=ares])]  ^+  ..ix
      ?-  -.b
          %mean
        ?~  p.b                       ::  XX  actually a yawn-told-full
          (get-even %mean [[(slav %p p.a) q.a] r.a] p.b)
        (mean-json:pop-duct 500 p.b)
      ::
          %nice
        ?:  =(~ med)  ~&  resp-lost/ire  ..ix
        (nice-json:pop-duct)
      ==
    ::
    ++  get-even
      |=  ven=even  ^+  ..ix
      =+  num=p.eve
      =.  eve  (add-even ven)
      =<  abet
      ?~  ude  done
      :: ~&  got-even/ude
      (give-even(hen p.u.ude, ude ~) q.u.ude num ven)
    ::
    ++  give-even
      |=  [pol=? num=@u ven=even]  ^+  done
      =:  q.eve  (~(del by q.eve) (dec num))              ::  TODO ponder a-2
          mow    ?.(?=(%rush -.ven) mow mow:(pass-took p.ven))
        ==
      ?>  pol                         ::  XX eventstream  
      %^  give-json  200  ~
      %^  jobe  id/(jone num)  type/[%s -.ven]
      ?-  -.ven
        %news  ~[from/[%s (scot %uv p.ven)]]
        %rush  ~[from/(subs-to-json p.ven) data/q.ven]
        %mean  ~[from/(subs-to-json p.ven) data/(ares-to-json q.ven)]
      ==
    ::
    ++  pass-took
      |=  a=[p=hasp wire]
      (pass-note(hen `~) [%of ire (gsig a)] [%g %took [- + ~]:p.a him])
    ::
    ++  pop-duct  =^(ned med ~(get to med) abet(hen ned))
    ++  poll
      |=  a=@u  ^+  ..ix
      ?:  =(a p.eve)
        ?^  ude  ~&(e/ix/wait/%replaced abet(u.ude [hen &]))
        =.  era  (add ~s30 now)
        (pass-note:abet(ude [~ hen &]) of//[ire] [%t %wait era])
      ?:  (gth a p.eve)  ~|(seq-high/cur=p.eve !!)
      =+  ven=~|(seq-low/cur=p.eve (~(got by q.eve) a))
      abet:(give-even & a ven)
    ::
    ++  subs-to-json
      |=  [a=hasp b=path]
      %-  jobe  :~
        ship/[%s (rsh 3 1 (scot %p p.a))]
        appl/[%s q.a] 
        path/(jape (spud b))
      ==
    ++  wake  ^+(..ix abet(ude ~))  ::  XX other effects?
    ::  XX unused
    ++  print-subs  |=([a=hasp b=path] "{<p.a>}/{(trip q.a)}{(spud b)}")
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
  |=  old=bolo ::_[.(wix **)]:*bolo
  ^+  ..^$
  ..^$(+>- old) ::(wix ~))
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
