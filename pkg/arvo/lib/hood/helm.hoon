=*  card  card:agent:gall
|%
+$  state  state-2
+$  any-state
 $~  *state
 $%  state-2
     state-1
     state-0
 ==
+$  state-2  [%2 =mass-timer]
+$  state-1  [%1 =mass-timer]
+$  state-0  [%0 hoc=(map bone session-0)]
+$  session-0
  $:  say=*
      mud=*
      =mass-timer
  ==
::
+$  mass-timer  [way=wire nex=@da tim=@dr]
::
++  state-0-to-1
  |=  s=state-0
  ^-  state-1
  [%1 mass-timer:(~(got by hoc.s) 0)]
::
++  state-1-to-2
  |=  s=state-1
  ^-  state-2
  [%2 +.s]
--
|=  [=bowl:gall sat=state]
=|  moz=(list card)
|%
++  this  .
+$  state      ^state      ::  proxy
+$  any-state  ^any-state  ::  proxy
++  abet  [(flop moz) sat]
++  flog  |=(=flog:dill (emit %pass /di %arvo %d %flog flog))
++  emit  |=(card this(moz [+< moz]))
::  +emil: emit multiple cards
::
++  emil
  |=  caz=(list card)
  ^+  this
  ?~(caz this $(caz t.caz, this (emit i.caz)))
::
++  on-init
  (poke-serve [~ /who] %base /gen/who/hoon ~)
::
++  on-load
  |=  [hood-version=@ud old=any-state]
  =<  abet
  =?  old   ?=(%0 -.old)  (state-0-to-1 old)
  =?  this  ?=(%1 -.old)
    (emil -:(poke-serve [~ /who] %base /gen/who/hoon ~))
  =?  old   ?=(%1 -.old)  (state-1-to-2 old)
  ?>  ?=(%2 -.old)
  this(sat old)
::
++  poke-rekey                                        ::  rotate private keys
  |=  des=@t
  =/  fud=(unit feed:jael)
    %+  biff
      (bind (slaw %uw des) cue)
    (soft feed:jael)
  =<  abet
  ?~  fud
    ~&  %invalid-private-key
    this
  =/  fed  (need fud)
  ?@  -.fed
    ?.  =(our.bowl who.fed)
      ~&  [%wrong-private-key-ship who.fed]
      this
    (emit %pass / %arvo %j %rekey lyf.fed key.fed)
  ?.  =(our.bowl who.fed)
    ~&  [%wrong-private-key-ship who.fed]
    this
  =|  caz=(list card)
  %-  emil
  |-
  ?~  kyz.fed  (flop caz)
  %=  $
    kyz.fed  t.kyz.fed
    caz      [[%pass / %arvo %j %rekey i.kyz.fed] caz]
  ==
::
++  ames-secret
  ^-  @t
  =;  pax  (crip +:<.^(@p %j pax)>)
  /(scot %p our.bowl)/code/(scot %da now.bowl)/(scot %p our.bowl)
::
++  poke-sec-atom
  |=  [hot=host:eyre dat=@]
  ?>  ?=(%& -.hot)
  =.  p.hot  (scag 2 p.hot)      :: ignore subdomain
  =.  dat  (scot %uw (en:crub:crypto ames-secret dat))
  =-  abet:(emit %pass /write %arvo %c %info -)
  =/  byk=path  (en-beam byk.bowl(r da+now.bowl) ~)
  =+  .^(=tube:clay cc+(welp byk /mime/atom))
  =/  =cage  atom+(tube !>([/ (as-octs:mimes:html dat)]))
  (foal:space:userlib :(welp byk sec+p.hot /atom) cage)
::
++  poke-moon                                        ::  rotate moon keys
  |=  sed=(unit [=ship =udiff:point:jael])
  =<  abet
  ?~  sed
    this
  (emit %pass / %arvo %j %moon u.sed)
::
++  poke-moon-breach
  |=  =ship
  ?>  ?|  =(our src):bowl
          =(src.bowl ship)
      ==
  abet:(emit %pass /helm/moon-breach/(scot %p ship) %arvo %b %wait now.bowl)
::
++  take-wake-moon-breach
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog %take-wake-moon-breach-fail u.error)
    abet
  ?>  ?=([@ ~] way)
  =/  =ship  (slav %p i.way)
  ?>  =(%earl (clan:title ship))
  ?>  =(our.bowl (sein:title our.bowl now.bowl ship))
  =/  =rift
    +(.^(rift j+/(scot %p our.bowl)/rift/(scot %da now.bowl)/(scot %p ship)))
  abet:(emit %pass / %arvo %j %moon ship *id:block:jael %rift rift %.n)
::
++  poke-code
  |=  act=?(~ %reset)
  =<  abet
  ?~  act
    this
  (emit %pass / %arvo %j %step ~)
::
++  poke-mass
  |=  ~  =<  abet
  (emit %pass /heft %arvo %d %flog %heft ~)
::
++  poke-automass
  |=  recur=@dr
  =.  mass-timer.sat
    [/helm/automass (add now.bowl recur) recur]
  abet:(emit %pass way.mass-timer.sat %arvo %b %wait nex.mass-timer.sat)
::
++  poke-cancel-automass
  |=  ~
  abet:(emit %pass way.mass-timer.sat %arvo %b %rest nex.mass-timer.sat)
::
++  poke-meld
  |=  ~  =<  abet
  (emit %pass /pack %arvo %d %flog %meld ~)
::
++  poke-pack
  |=  ~  =<  abet
  (emit %pass /pack %arvo %d %flog %pack ~)
::
++  poke-pans
  |=  pans=(list note-arvo)
  ?~  pans  abet
  =.  this  (emit %pass /helm/pans %arvo i.pans)
  $(pans t.pans)
::
++  poke-pass
  |=  =note-arvo  =<  abet
  (emit %pass /helm/pass %arvo note-arvo)
::
++  take-wake-automass
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog u.error)
    ~&  %helm-wake-automass-fail
    abet
  =.  nex.mass-timer.sat  (add now.bowl tim.mass-timer.sat)
  =<  abet
  %-  emil
  :~  [%pass /heft %arvo %d %flog %crud %hax-heft ~]
      [%pass way.mass-timer.sat %arvo %b %wait nex.mass-timer.sat]
  ==
::
++  poke-send-hi
  |=  [her=ship mes=(unit tape)]  =<  abet
  %-  emit
  :*  %pass  /helm/hi/(scot %p her)
      %agent  [her %hood]  %poke
      %helm-hi  !>(?~(mes '' (crip u.mes)))
  ==
::
::
++  poke-hi
  |=  mes=@t
  ~|  %poke-hi-fail
  ?:  =(%fail mes)
    ~&  %poke-hi-fail
    !!
  abet:(flog %text "< {<src.bowl>}: {(trip mes)}")
::
++  poke-ames-prod
  |=  ships=(list ship)
  abet:(emit %pass /helm/prod %arvo %a %prod ships)
::
++  poke-ames-snub
  |=  snub=[form=?(%allow %deny) ships=(list ship)]
  abet:(emit %pass /helm/snub %arvo %a %snub snub)
::
++  poke-atom
  |=  ato=@
  =+  len=(scow %ud (met 3 ato))
  =+  gum=(scow %p (mug ato))
  =<  abet
  (flog %text "< {<src.bowl>}: atom: {len} bytes, mug {gum}")
::
++  coup-hi
  |=  [pax=path cop=(unit tang)]  =<  abet
  ?>  ?=([@t ~] pax)
  (flog %text "hi {(trip i.pax)} {?~(cop "" "un")}successful")
::
++  poke-trim
  |=  pri=@ud  =<  abet
  (emit %pass /pack %arvo %d %flog %crop pri)
::
++  poke-verb                                         ::  toggle verbose
  |=  ~  =<  abet
  (flog %verb ~)
::
++  poke-ames-sift
  |=  ships=(list ship)  =<  abet
  (emit %pass /helm %arvo %a %sift ships)
::
++  poke-ames-verb
  |=  veb=(list verb:ames)  =<  abet
  (emit %pass /helm %arvo %a %spew veb)
::
++  poke-gall-sift
  |=  dudes=(list dude:gall)  =<  abet
  (emit %pass /helm %arvo %g %sift dudes)
::
++  poke-gall-verb
  |=  veb=(list verb:gall)  =<  abet
  (emit %pass /helm %arvo %g %spew veb)
::
++  poke-ames-wake
  |=  ~  =<  abet
  (emit %pass /helm %arvo %a %stir '')
::
++  poke-ames-kroc
  |=  [dry=? bones=(list [ship bone])]  =<  abet
  ?:  dry  this
  (emit %pass /helm %arvo %a %kroc bones)
::
++  poke-ames-cong
  |=  cong=[msg=@ud mem=@ud]  =<  abet
  (emit %pass /helm %arvo %a %cong cong)
::
++  poke-serve
  |=  [=binding:eyre =generator:eyre]  =<  abet
  (emit %pass /helm/serv %arvo %e %serve binding generator)
::
++  poke-cors-approve
  |=  =origin:eyre
  =<  abet
  (emit %pass /helm/cors/approve %arvo %e %approve-origin origin)
::
++  poke-cors-reject
  |=  =origin:eyre
  =<  abet
  (emit %pass /helm/cors/reject %arvo %e %reject-origin origin)
::
++  poke-doff
  |=  [dude=(unit dude:gall) ship=(unit ship)]
  =<  abet
  (emit %pass /helm/doff %arvo %g %doff dude ship)
::
++  poke
  |=  [=mark =vase]
  ?>  ?|  ?=(%helm-hi mark)
          ?=(%helm-moon-breach mark)
          =(our src):bowl
      ==
  ?+  mark  ~|([%poke-helm-bad-mark mark] !!)
    %helm-ames-prod        =;(f (f !<(_+<.f vase)) poke-ames-prod)
    %helm-ames-snub        =;(f (f !<(_+<.f vase)) poke-ames-snub)
    %helm-ames-sift        =;(f (f !<(_+<.f vase)) poke-ames-sift)
    %helm-ames-verb        =;(f (f !<(_+<.f vase)) poke-ames-verb)
    %helm-ames-wake        =;(f (f !<(_+<.f vase)) poke-ames-wake)
    %helm-ames-kroc        =;(f (f !<(_+<.f vase)) poke-ames-kroc)
    %helm-ames-cong        =;(f (f !<(_+<.f vase)) poke-ames-cong)
    %helm-atom             =;(f (f !<(_+<.f vase)) poke-atom)
    %helm-automass         =;(f (f !<(_+<.f vase)) poke-automass)
    %helm-cancel-automass  =;(f (f !<(_+<.f vase)) poke-cancel-automass)
    %helm-code             =;(f (f !<(_+<.f vase)) poke-code)
    %helm-cors-approve     =;(f (f !<(_+<.f vase)) poke-cors-approve)
    %helm-cors-reject      =;(f (f !<(_+<.f vase)) poke-cors-reject)
    %helm-doff             =;(f (f !<(_+<.f vase)) poke-doff)
    %helm-gall-sift        =;(f (f !<(_+<.f vase)) poke-gall-sift)
    %helm-gall-verb        =;(f (f !<(_+<.f vase)) poke-gall-verb)
    %helm-hi               =;(f (f !<(_+<.f vase)) poke-hi)
    %helm-pans             =;(f (f !<(_+<.f vase)) poke-pans)
    %helm-mass             =;(f (f !<(_+<.f vase)) poke-mass)
    %helm-meld             =;(f (f !<(_+<.f vase)) poke-meld)
    %helm-moon             =;(f (f !<(_+<.f vase)) poke-moon)
    %helm-moon-breach      =;(f (f !<(_+<.f vase)) poke-moon-breach)
    %helm-pack             =;(f (f !<(_+<.f vase)) poke-pack)
    %helm-pass             =;(f (f !<(_+<.f vase)) poke-pass)
    %helm-rekey            =;(f (f !<(_+<.f vase)) poke-rekey)
    %helm-send-hi          =;(f (f !<(_+<.f vase)) poke-send-hi)
    %helm-serve            =;(f (f !<(_+<.f vase)) poke-serve)
    %helm-trim             =;(f (f !<(_+<.f vase)) poke-trim)
    %helm-verb             =;(f (f !<(_+<.f vase)) poke-verb)
    %helm-write-sec-atom   =;(f (f !<(_+<.f vase)) poke-sec-atom)
  ==
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+  wire  ~|([%helm-bad-take-agent wire -.sign] !!)
    [%hi *]  ?>  ?=(%poke-ack -.sign)
                   (coup-hi t.wire p.sign)
  ==
::
++  take-bound
  |=  [wir=wire success=? binding=binding:eyre]  =<  abet
  (flog %text "bound: {<success>}")
::
++  take-arvo
  |=  [=wire =sign-arvo]
  ?+  wire  ~|([%helm-bad-take-wire wire +<.sign-arvo] !!)
    [%automass *]     %+  take-wake-automass  t.wire
                      ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
    [%serv *]         %+  take-bound  t.wire
                      ?>(?=(%bound +<.sign-arvo) +>.sign-arvo)
    [%moon-breach *]  %+  take-wake-moon-breach  t.wire
                      ?>(?=(%wake +<.sign-arvo) +>.sign-arvo)
    [%pass *]         abet
  ==
--
