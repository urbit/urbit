::  %fine, remote scries
!:
::
::TODO  jael does ames-specific behavior for %public-keys gifts(?).
::      should it do something similar for fine?
::
=,  fine
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $%  $:  %b
          $%  $>(%wait task:behn)
              $>(%rest task:behn)
          ==  ==
          $:  %j
          $%  $>(%private-keys task:jael)
              $>(%public-keys task:jael)
      ==  ==  ==
    +$  sign
      $%  $:  %behn
          $%  $>(%wake gift:behn)
          ==  ==
          $:  %jael
          $%  $>(%private-keys gift:jael)
              $>(%public-keys gift:jael)
      ==  ==  ==
    ::
    ::TODO  get pubkeys from jael in the same way ames does
    ::
    +$  fine-state
      $:  %0
          urth=duct                                     ::  unix duct
          hear=(jug path duct)                          ::  awaiting existence
          want=(jug path duct)                          ::  awaiting response
          part=(map path partial)                       ::  partial responses
          ::TODO  re-send request timers?
      ==
    ::
    +$  bits  [w=@ud d=@]
    ++  protocol-version  %0
    ::
    +$  partial
      $:  num-fragments=@ud
          num-received=@ud
          fragments=(map @ud byts)
      ==
    ::
    +$  twit  ::  signed request
      $:  signature=@
          peep
      ==
    ::
    +$  peep  ::  request data
      $:  =path
          num=@ud
      ==
    ::
    +$  rawr  ::  response packet
      $:  sig=@
          siz=@ud
          byts
      ==
    ::
    +$  roar  ::  response message
      $:  sig=@
          dat=(cask)
      ==
    --
::
~%  %fine  ..part  ~
::
=|  fine-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  fine-gate  .
=>  |%
    ++  encode-packet  (encode-packet:ames | protocol-version)
    ++  decode-packet  (decode-packet:ames | protocol-version)
    ::
    ++  spit
      |=  =path
      ^-  [pat=@t wid=@ud]
      =+  pat=(spat path)
      =+  wid=(met 3 pat)
      ?>  (lte wid 384)  ::TODO  check when we handle %keen, instead of here?
      [pat wid]
    ::
    ++  request-body
      |=  [=path num=@ud]
      ^-  byts
      ?>  (lth num (bex 32))
      =.  path  
        ?>  ?=([@ @ *] path)
        [i t.t]:path
      =+  (spit path)
      :-  :(add 4 2 wid)
      %+  can  3
      :~  4^num       ::  fragment number
          2^wid       ::  path size
          wid^`@`pat  ::  namespace path
      ==
    ::
    ++  encode-request
      |=  [=path num=@ud]
      ^-  hoot  ^-  @
      =+  bod=(request-body path num)
      =+  syn=(can 3 64^(sign:keys dat.bod) bod ~)
      %+  con  0b100  ::NOTE  request bit
      (encode-packet [our ~zod] (mod life:keys 16) 0b0 ~ syn)
    ::
    ++  encode-response
      |=  [=path data=(unit (cask))]
      ^-  song
      ::  prepend request descriptions to each response packet
      ::
      =;  pacs=(list @ux)
        %-  head
        %^  spin  pacs  1
        |=  [pac=@ux num=@ud]
        ^-  [purr _num]
        :_  +(num)
        ^-  @ux
        ::NOTE  we stub out the receiver & origin details,
        ::      runtime should replace them as appropriate.
        (encode-packet [our ~zod] (mod life:keys 16) 0b0 ~ pac)
      ::  prepend a signature and split the data into 1024-byte fragments
      ::
      =/  frag=(list @)
        =/  sig=@  ::(full:keys path (fall data ~))
          (fil 5 16 0xdead.beef)
        ?~  data  [sig]~
        %+  rip  13  ::NOTE  1024 bytes
        (cat 3 sig (jam u.data))  ::TODO  should include life
      ::  sign & packetize the fragments
      ::
      %-  head
      =-  ~&(-> -)
      %^  spin  frag  1
      |=  [dat=@ num=@ud]
      :_  +(num)
      ^-  @ux
      =/  req=byts  (request-body path num)
      =/  bod=byts
        =/  wid=@ud  (met 3 dat)
        :-  :(add 4 2 wid)
        %+  can  3
        :~  4^(lent frag)  ::  number of fragments
            2^wid          ::  response data fragment size in bytes
            wid^dat        ::  response data fragment
        ==
      =/  sig=byts
        64^(sign:keys (can 3 req bod ~))
      (can 3 req sig bod ~)
    ::
    ++  keys
      |%
      ++  full
        |=  [=path mess=*]
        (sign (shax (jam [our life path mess])))
      ::
      ++  life  ~+  (jael ^life %life /(scot %p our))
      ++  ring  ~+  (jael ^ring %vein /(scot %ud life))
      ++  sign      sign:as:(nol:nu:crub:crypto ring)
      ::
      ++  jael
        |*  [=mold =desk =path]
        !<  mold
        %-  tail  %-  need  %-  need
        (rof `[our ~ ~] [%jael %$] [our desk da+now] path)
      ::
      ++  lyfe
        |=  who=ship
        (jael (unit ^life) %lyfe /(scot %p our))
      ::
      ::TODO  don't scry! subscribe & get from state instead
      ++  pass
        |=  [who=ship lyf=^life]
        ::TODO  but might fail need checks?
        :: (jael * %deed )
        !!
      --
    ::
    ++  get-lane
      |=  =ship
      ^-  lane:ames
      ::=;  lanes
        ::TODO  should we send to all lanes?
        ::  ?^  lanes  i.lanes
        ~&(%fine-lane-stub &+~zod)  ::TODO
      ::.^  (list lane:ames)
        ::%ax  (scot %p our)  %$  (scot %da now)
        ::/peers/(scot %p ship)/forward-lane
      ::==
    ::
    ++  decode-request
      |=  [sndr=ship =hoot]
      ^-  twit
      :-  sig=(cut 3 [0 64] hoot)
      -:(decode-request-info sndr (rsh 3^64 hoot))
    ::
    ++  decode-request-info
      |=  [sndr=ship =hoot]
      ^-  [=peep =purr]
      =+  num=(cut 3 [0 4] hoot)
      =+  len=(cut 3 [4 2] hoot)
      =+  pat=(cut 3 [6 len] hoot)
      =/  path  (stab pat)
      =.  path
        ?>  ?=([@ @ *] path)
        [i.path (scot %p sndr) t.path]
      :-  [path num]
      ::  if there is data remaining, it's the response
      (rsh [3 (add 6 len)] hoot)
    ::
    ++  decode-response-packet
      |=  =purr
      =;  =rawr
        ~?  !=(wid.rawr (met 3 dat.rawr))  [%fine %unexpected-dat-size]
        rawr
      :*  sig=(cut 3 [0 64] purr)
          siz=(cut 3 [64 4] purr)
          wid=(cut 3 [68 2] purr)
          dat=(rsh 3^70 purr)
      ==
    ::
    ++  verify-response-packet
      |=  rawr
      !!
    ::
    ++  decode-response-msg
      |=  partial  ::TODO  maybe take @ instead
      ^-  roar
      ~&  ~(key by fragments)
      =/  mess=@ux
        %+  rep  13
        %+  turn  (gulf 1 num-fragments)
        |=  frag-num=@ud
        +:(~(got by fragments) frag-num)
      ~&  mess/mess
      ~&  jamdat/`@ux`(rsh 3^64 mess)
      =+  sig=`@ux`(end 3^64 mess)
      :-  sig
      ~&  sig/sig
      ~|  [%fine %response-not-cask]
      ~&  dat/(cue (rsh 3^64 mess))
      ;;((cask) (cue (rsh 3^64 mess)))
    ::
    ++  process-response
      |=  [=path data=(unit (cask))]
      ^-  (quip move _state)
      :-  %+  turn  ~(tap in (~(get ju want) path))
          (late [%give %tune path data])
      =.  want  (~(del by want) path)
      =.  part  (~(del by part) path)
      state
    ::
    ++  handle-request
      |=  [=duct =twit]
      ^-  (list move)
      =/  =song
        %+  encode-response  path.twit
        (get-scry-result *gang path.twit)
      ::TODO  different task, pick the right packet
      [duct %give %howl path.twit song]~
    ::
    ++  handle-response
      |=  [[from=ship =lane:ames] =peep =rawr]
      ^-  (quip move _state)
      ?:  =(0 siz.rawr)
        ?>  =(0 dat.rawr)
        (process-response path.peep ~)
      ?.  (~(has by part) path.peep)
        ::  we did not initiate this request, or it's been cancelled
        ::
        !!
      =/  partial  (~(got by part) path.peep)
      ~&  siz.rawr 
      ~&  partial
      =.  partial
        ?:  (~(has by fragments.partial) num.peep)
          ~&  [%fine %duplicate-response peep]  ::TODO  disable
          ::TODO  what if non-equal?
          partial
        =,  partial
        :+  ~|  [%fine %response-size-changed have=num-fragments new=siz.rawr]
            ?>  |(=(0 num-fragments) =(num-fragments siz.rawr))
            siz.rawr
          +(num-received)
        (~(put by fragments) num.peep [wid dat]:rawr)
      ~&  partial
      ::
      ?:  =(num-fragments num-received):partial
        ::  we have all the parts now, construct the full response
        ::
        =/  =roar  (decode-response-msg partial)
        ::TODO  check signature
        (process-response path.peep `dat.roar)
      ::  otherwise, store the part, and send out the next request
      ::
      =.  part  (~(put by part) path.peep partial)
      =/  next-num=@ud
        =/  next=@ud  +(num.peep)
        ::  we should receive responses in order, but in case we don't...
        ::
        |-
        ?.  (~(has by fragments.partial) next)  next
        $(next +((mod next num-fragments.partial)))
      ::
      =/  =hoot  (encode-request path.peep next-num)
      ::TODO  ask amsden, should we shotgun? we can tweak this
      ::      for now (mvp) though, stay 1-to-1
      ::TODO  update lane in ames state
      ::TODO  is reusing the lane fine?
      [[urth %give %hoot lane hoot]~ state]
    ::
    ++  get-scry-result
      |=  [=gang =path]
      ^-  (unit (cask))
      ?~  nom=(de-omen path)  ~
      ?>  =(our p.bem.u.nom)
      ::  we only support scrying into clay,
      ::  and only if the data is fully public.
      ::
      ?.  =(%c (end 3 (snag 0 path)))  ~
      =+  res=(rof gang u.nom)
      ~!  res
      ~&  gang
      ~&  u.nom
      ?-  res
        ~        !!  ::REVIEW  crashing in the blocking case is fine.. right?
        [~ ~]    ~
        [~ ~ *]  `[p q.q]:u.u.res
      ==
    --
^?
|%
::  +call: handle a +task:fine request
::
++  call
  ~%  %fine-call  ..part  ~
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _fine-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ::
  ?^  dud
    ~|(%fine-call-dud (mean -.task tang.u.dud))
  ::
  =^  moves  state
    ?-  -.task
        %keen
      ^-  (quip move _state)
      =/  omen
        ~|  [%fine %invalid-namespace-path path.task]
        (need (de-omen path.task))
      =.  want  (~(put ju want) path.task hen)
      ?:  (~(has by part) path.task)
        ::  request is already ongoing
        ::
        [~ state]
      ::  kick off the request
      ::
      =.  part  (~(put by part) path.task *partial)
      =/  =lane:ames  (get-lane p.bem.omen)
      =/  =hoot  (encode-request path.task 1)
      [[urth %give %hoot lane hoot]~ state]
    ::
        %yawn
      [~ state(want (~(del ju want) path.task hen))]
    ::
    ::
        %purr
      ^-  (quip move _state)
      =/  header  `@ub`(end 5 `@ux`purr.task)
      =/  =packet:ames  (decode-packet `@ux`purr.task)
      =/  resp=?        =(& (cut 0 [2 1] purr.task))
      ?.  resp
        ::TODO  crash instead, scry/peek should be used for this
        =/  =twit  (decode-request sndr.packet `@ux`content.packet)
        ::TODO  verify request signature
        [(handle-request hen twit) state]
      =/  [=peep =purr]  (decode-request-info sndr.packet `@ux`content.packet)
      =/  =rawr          (decode-response-packet purr)
      ::TODO  validate response signature
      (handle-response [from lane]:task peep rawr)
    ::
        %bide
      [~ state(hear (~(put ju hear) path.task hen))]
    ::
        %born
      ^-  (quip move _state)
      ::REVIEW  assuming hear is for runtime use only?
      [~ state(hear ~, urth hen)]
    ::
        %trim
      [~ state]  ::TODO  maybe clear part?
    ::
        %vega
      [~ state]
    ==
  [moves fine-gate]
::  +load: migrate an old state to a new fine version
::
++  load
  |=  old=fine-state
  ^+  fine-gate
  fine-gate(state old)
::  +scry: get packets
::
::    /fx/message/[full-scry-path]    song    all packets for some scry path
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ::TODO  don't special-case whey scry
  ::
  ?:  &(=(car %$) =(s.bem /whey))
    =/  maz=(list mass)
      :~  state+&+state
      ==
    ``mass+!>(maz)
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%da now] r.bem)
      ==
    ~
  ::
  ?.  ?=(%x car)  ~
  ?.  ?=([%message @ *] s.bem)  ~
  ::  t.s.bem is expected to be a scry path of the shape /vc/desk/rev/etc,
  ::  so we need to give it the right shape
  ::
  =/  pax=path
    [i.t.s.bem (scot %p our) t.t.s.bem]
  ~&  pax
  ``noun+!>((encode-response pax (get-scry-result lyc pax)))
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _fine-gate]
  ?^  dud
    ~|(%fine-take-dud (mean tang.u.dud))
  ::
  ::TODO
  [~ fine-gate]
--
