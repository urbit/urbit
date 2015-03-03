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
              [%wart p=sock q=@tas r=_`[path *]`*gram]  ::  urbit message
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note                                                ::  out request $->
          $%  
::           $:  %a                                    ::  to %ames
::           $%  [%want p=sock q=path r=*]                 ::
::           ==  ==                                        ::
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
  ==                                                    ::
::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      [him=ship authed=(set ship)]                         ::  authenticated
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      vew=(set oryx)                                    ::  open views
  ==                                                    ::
::
++  perk                                                ::  parsed request
  $%  [%spur p=spur]
      [%beam p=beam]
      [%poll p=@uvH]
      [%auth perk-auth]
  ==
::
++  perk-auth                                           ::  parsed auth
  $%  [%get him=ship rem=pork]
      [%js ~]
      [%stat ~]    ::  json with authentication status
      [%try him=ship cod=cord]
      [%del p=(unit ship)]
  ==
--                                                      ::
|%
++  sesh                                                ::  session from cookies
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
  ?:(=(nam p.i.u.mar) [~ q.i.u.mar] $(u.mar t.u.mar))
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
    window.urb = {}
    window.urb.ship = 'zod' // XX
    window.urb.submit = function(){
        xhr = new XMLHttpRequest()
        xhr.open('POST', "/~/auth.json?PUT", true)
        var dat = {oryx:'hi', ship: ship.value, code: pass.value}
        xhr.send(JSON.stringify(dat))
        xhr.addEventListener('load', function(){
          if(this.status !== 200)
            return err.innerHTML = ":(\n" + xhr.responseText
          else return document.location.reload()
        })
    }
    '''
  --
++  xml
  |%
  ++  login-page
    ;html
      ;head:title:'Hello World'
      ;body
        ;p: Identify yourself, ~;{input#ship(value "zod")}?
        ;input#pass(onchange "urb.submit()");
        ;pre:code#err;
        ;script@"/~/auth.js";
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
    ?-    -.+.sih
        %crud
      +>.$(mow [[hen %slip %d %flog +.sih] mow])
    ::
        %made
      ?+    tea  +>.$
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
      ?-  -<.u.mez                                      ::  XX handle
        %lon  !!
        %aut  !!
        %hat  !!
      ==
    ==
  ::
  ++  fail                                            ::  request failed
    |=  [sas=@ud dep=@uvH mez=tang]
    ^+  +>
    ::  (back ~ dep %tang !>(mez))  ::  XX broken tang->mime door in ford
    (give-html sas (depo dep (tanx mez)))
  ::
  ++  give-html                                       ::  request failed
    |=([sas=@ud max=manx] (resp sas text//html (poxo max)))
  ::
  ++  give-json                                       ::  success json
    |=  [sas=@uG cug=(list ,@t) jon=json]
    %-  give-gift
    =+  git=(tuff sas application//json (crip (pojo jon)))
    ?~  cug  git
    =+  cuh=(turn `(list ,@t)`cug |=(a=@t set-cookie/a))
    git(q (weld cuh q.git))
  ::
  ++  give-gift                                       ::  done card
    |=  gef=gift
    +>(mow :_(mow [hen %give gef]))
  ::
  ++  resp                                            ::  mime response
    |=  [sas=@uG mit=mite bod=tape]
    (give-gift (tuff sas mit (crip bod)))
  ::
  ++  pass-note  |=(noe=[wire note] +>(mow :_(mow [hen %pass noe])))
  ++  ford-req
    |=  [tea=wire our=ship kas=silk]
    ::  ~&  [%ford-req our num ses -.kas]
    (pass-note tea %f [%exec our `kas])
  ::
  ++  back                                              ::  %ford bounce
    |=  [tea=wire dep=@uvH cag=cage]                
    (ford-req tea our [%cast %mime %done ~ cag])        ::  XX deps
  ::
  ++  tuff                                            ::  mimed response
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
    =|  (unit ,@t)                                      ::  cookie given?
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
      |=  [tea=wire dep=@uvH]
      (pass-note tea %f [%wasp our dep])
    ::
    ++  beam-into-ford
      |=  [bem=beam ext=term ced=cred]
      =:  s.bem  [%web ~(rent co (flux:ya quy ced)) s.bem]
          r.bem  ?+(r.bem r.bem [%ud %0] da/now)
      ==
      (ford-req ~ our [%cast %mime [%boil ext bem ~]])
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
        %^  tuff  200  image//png
        0w89wg.GV4jA.l9000.00dPb.YzBT6.giO00.o100d.wZcqc.a9tg-.VTG0b.
        AUIvE.HBM3g.cK4SE.0aagi.l090p.I1P5g.Y-80r.y1YS9.1xE~Y.qgpFY.
        vKN1V.905y0.2UwvL.43TUw.uL406.0-31h.xwoJF.Ul454.ilk00.00Yps.
        BNumh.xpl9B.pS5Ji.i1BoC.ZAgg1.BsC5T.t6pLk.Thohn.gp000.0ov~P.
        7M000.0o840.00010.0001i.h4x93.g0000.Eq2wR.7jB29
      ::
          [%txt %robots ~]
        :-  ~
        %^  tuff  200  text//plain
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
          %auth
        :-  %auth
        |-
        ?+    p.pok  !!
            ~          $(p.pok [~ %json])
            [~ %js]    [%js ~]
            [~ %json]
          ?+    mef  ~|(%bad-meth !!)
              %get   [%stat ~]
              %post
            ?+    quy  ~|(bad-quy/'"PUT" or "DELETE"' !!)
                [[%'PUT' ~] ~]
              =+  paz=(ot ship/(su fed:ag) code/so ~):jo
              ~|  parsing/bod
              [%try (need (paz (need (poja q:(need bod)))))]
            ::
                [[%'DELETE' ~] ~]
              !!
        ==  ==
      ==  ==
    ::
    ++  handle
      ^+  done
      =+  git=as-magic-filename
      ?^  git  (give-gift u.git)
      =+  oar=(fall (host-to-ship r.hat) (need hov))
      =.  our  oar  ::  XX
      %-  |=(a=(each ,_done tang) ?~(-.a p.a (fail 404 0v0 >%exit< p.a)))
      %-  mule  |.  ^+  done    ~|  [mef maf bod]
      =+  ext=(fall p.pok %urb)
      =+  bem=as-beam
      ?^  bem
        ::  abet:(~(into-ford ya (get-sess..)) ..$)
        =+  [ses cyz]=get-session
        (beam-into-ford u.bem ext ced.cyz)
      ?>  check-oryx
      =+  hem=as-aux-request
      ?^  hem  (handle-parsed u.hem)
      ~|(strange-path/q.pok !!)
    ::
    ++  check-oryx                    ::  | if json with bad oryx
      ^-  ?
      ?.  &(?=([~ %json] p.pok) ?=(%post mef) ?=(^ bod))  &
      =|  cyz=cyst  ::  XX
      =+  oxe=(parse-to-oryx q.u.bod)
      ?~  oxe  |
      &  ::  XX
    ::     (~(has in vew.cyz) u.oxe)
    ::
    ++  parse-to-oryx  ;~(biff poja (ot oryx/so ~):jo)
    ++  continue-with-request  |=(rem=pork handle(pok rem))
    ++  foreign-auth  ,_!!
    ++  handle-parsed
      |=  hem=perk
      ^+  +>
      ?-    -.hem
          ?(%spur %beam)  !!
          %poll
        ?.  ?=([~ %js] p.pok)  ::  XX treat non-json cases?
          ?~  p.hem  done
          (ford-wasp ~ p.hem)
        %^  resp  200  text//javascript
        """
        window.urb = \{poll: "/{(apex:earn %| pok(u.p %json) quy)}"}
        {(trip poll:js)}
        """
          %auth  
        =+  `[ses=hole cyz=cyst]`get-session
        ?-    &2.hem
            %js    (resp 200 text//javascript (trip auth:js))
            %stat  (give-json 200 ~ (get-auth-status ses))
            %try
          ?.  =(our him.hem)
            ~|(stub-foreign/him.hem !!)
          ?.  =(load-secret cod.hem)
            ~|(try/`@t`load-secret !!)
          =.  authed.cyz  (~(put in authed.cyz) our)
          =.  wup  (~(put by wup) ses cyz)
          (give-json 200 cug.cyz (get-auth-status ses))
        ::  
            %del
          =<  (nice-json)
          ?~  p.hem
            .(wup (~(del by wup) ses))
          =<  .(wup (~(put by wup) ses cyz))
          =.  authed.cyz  (~(del in authed.cyz) u.p.hem)
          ?.  =(u.p.hem him.cyz)  .
          .(him.cyz anon)
        ::
            %get
          ~|  aute/+.hem
          ?:  =(anon him.hem)
            (continue-with-request rem.hem)
          ?:  (~(has in authed.cyz) him.hem)
            (continue-with-request rem.hem)
          ?.  =(our him.hem)
            (foreign-auth)
          (show-login-page)
        ==
  ::       =.  q.cez
  ::         ?.  =(anon p.p.hem)
  ::           q.cez
  ::         (~(put in q.cez) p.p.hem)
  ::       =+  hez=(get-session our cip pul moh)
  ::       =.  wup  (~(put by wup) hez)
  ::       ?^  cug.q.hez
  ::         =+  rel=;html:script:"document.location.reload()"
  ::         =+  tuv=(tuff 200 text//html (crip (poxo rel)))
  ::         =.  q.tuv
  ::           =-  (weld :_(q.tuv -))
  ::           (turn `(list ,@t)`cug.q.hez |=(a=cord set-cookie/a))))
  ::         (give-gift tuv)
  ::       ~|  hez  !!
      ==
    ++  show-login-page  ,_(give-html 200 login-page:xml)
    ++  get-auth-status  
      |=  ses=hole
      =+  [cyz=(~(get by wup) ses) orx=(rsh 3 1 (scot %p eny))]
      %-  jobe  :~
        oryx/s/orx
        user/(jape +:<?~(cyz anon him.u.cyz)>)
        auth/a/?~(cyz ~ (turn (~(tap in authed.u.cyz)) |=(a=@p (jape +:<a>))))
      ==
    ++  nice-json  ,_(give-json 200 ~ (joba %ok %b &))
    ::
    ++  load-secret
      ^-  @ta
      =+  pax=/(scot %p our)/code/(scot %da now)/(scot %p our)
      %^  rsh  3  1
      (scot %p (,@ (need (sky %a pax))))
    ::
    ++  get-session                        ::  get request state
      ^-  [hole cyst]
      =*  sec  p.hat
      =+  pef=(rsh 3 1 (scot %p our))
      =+  lig=(sesh pef maf)
      ?^  lig
        =+  cyz=(need (~(get by wup) u.lig))
        [u.lig cyz(cug ~)]
      =+  ses=(rsh 3 1 (scot %p (end 6 1 ney)))
      :-  ses
      ^-  cyst
      :*  ^-  cred
          :*  hat(p sec)
              ~
              (rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
          ::
              =+  lag=(~(get by maf) %accept-language)
              ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
          ::
              cip
              ~
          ==
          [anon ~]
      ::
        ::  ~
      ::
          :_  ~
          %^  cat  3
            (cat 3 (cat 3 pef '=') ses)
          ::  (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
          '; Path=/; HttpOnly'
      ::
          now
          ~
        ::  [1 ~]
      ==
    --
  ++  ya                                                ::  session engine
    :: =|  [[our=ship ses=hole] cyst] ::serf cyst]
    :: =*  sef  ->-
    :: =*  cyz  ->
    |%
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
  |=  old=_[%0 gub hov ged ney dop **]
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
