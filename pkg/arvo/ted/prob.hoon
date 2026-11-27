::  ahoy/prob: peer kelvin hash scanner
::
::  scan via remote scry successive cases until
::  it receives the expected hash or timeout.
::
::
/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
::
=>  |%
    ++  peek-for-kelvin
      |=  [=bowl:spider =keen=wire =timer=wire timeout-time=@da =spar:ames]
      :: XX
      ::   ^-  form:m-race
      ::   %+  (map-err ,~)  |=(* [%offline *tang])
      ::   %+  (set-timeout ,~)  timeout-time
      =/  m-race  (strand ,[timeout=? result=vase])
      ^-  form:m-race
      |=  tin=strand-input:strand
      ^-  output:m-race
      ?+    in.tin  `[%skip ~]
          ~  `[%wait ~]
      ::
      ::  sage response: give kids hash
      ::
          [~ %sign * %ames %sage *]
        ?.  =(keen-wire wire.u.in.tin)
          `[%skip ~]
        =/  =sage:mess:ames  sage.sign-arvo.u.in.tin
        :^    [%pass timer-wire %arvo %b %rest timeout-time]~
            %done
          %.n
        ::
        !>
        ?~  q.sage  ~
        =+  .^  =dais:clay  %cb
              /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.q.sage]
            ==
        =/  res  (mule |.((vale.dais q.q.sage)))
        ?:  ?=(%| -.res)
          ~
        ::  try to extract kelvin data
        ::
        =/  kelvin-data=(list weft)
          =+  !<(=waft:clay p.res)
          ?.  ?=([[%1 ~] *] waft)
            ^-  (list weft)  [waft]~
          ~(tap in p.waft)
        ?:  =(~ kelvin-data)
          ~
        =/  arvo-kelvin=(unit @ud)
          |-  ^-  (unit @ud)
          ?~  kelvin-data  ~
          ?:  =(lal.i.kelvin-data %zuse)
            `num.i.kelvin-data
          ~!  kelvin-data
          $(kelvin-data t.kelvin-data)
        ?~  arvo-kelvin
          ~
        `u.arvo-kelvin
      ::
      ::  timer: peer timed out
      ::
          [~ %sign * %behn %wake *]
        ?.  =(timer-wire wire.u.in.tin)
          `[%skip ~]
        ?^  error.sign-arvo.u.in.tin
          `[%fail %timer-error u.error.sign-arvo.u.in.tin]
        ~&  >>  yawn/keen-wire^spar
        :-  [%pass keen-wire %arvo %a %yawn spar]~
        [%done [%.y !>(~)]]
      ==
    ::
    ++  peek-for-hash
      |=  [=bowl:spider =keen=wire =timer=wire timeout-time=@da =spar:ames]
      =/  m-race  (strand ,[timeout=? result=vase])
      :: XX
      ::   ^-  form:m-race
      ::   %+  (map-err ,~)  |=(* [%offline *tang])
      ::   %+  (set-timeout ,~)  timeout-time
      ~&  peek-for-hash/keen-wire^timer-wire
      ^-  form:m-race
      |=  tin=strand-input:strand
      ^-  output:m-race
      ?+    in.tin  `[%skip ~]
          ~  `[%wait ~]
      ::
      ::  sage response: give kids hash
      ::
          [~ %sign * %ames %sage *]
        ?.  =(keen-wire wire.u.in.tin)
          `[%skip ~]
        =/  =sage:mess:ames  sage.sign-arvo.u.in.tin
        :^    [%pass timer-wire %arvo %b %rest timeout-time]~
            %done
          %.n
        ::
        ?:  ?|  ?=(~ q.sage)
                !=(%uvi p.q.sage)
            ==
          !>(*(unit @uvi))
        !>([~ ;;(hash=@uvi q.q.sage)])
      ::
      ::  timer: peer timed out
      ::
          [~ %sign * %behn %wake *]
        ?.  =(timer-wire wire.u.in.tin)
          `[%skip ~]
        ?^  error.sign-arvo.u.in.tin
          `[%fail %timer-error u.error.sign-arvo.u.in.tin]
        ~&  >>>  yawn/keen-wire^ship^path
        :-  [%pass keen-wire %arvo %a %yawn spar]~
        [%done [%.y !>(*(unit @uvi))]]
      ==
    ::
    --
::
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
::
=+  !<  $:  ~
            timeout=@dr
            [case=@ud has=@uvi wen=@da]
            who=ship
            kel=@ud
            veb=?
        ==
    arg
::
;<  =bowl:spider  bind:m  get-bowl:strandio
::
::  if peer not in ames, abort
::
=+  .^  peers=(map ship ?(%alien %known))  %ax
    /(scot %p our.bowl)//(scot %da now.bowl)/peers
  ==
?.  (~(has by peers) who)
  !!  :: XX crash thread?
::
=/  start=@da  now.bowl
~?  >  veb  "ahoy-prob: start {<who>} {<now.bowl>}"
=|  no-response=?
=|  remote-kel=@ud
::
|-
;<  =bowl:spider  bind:m  get-bowl:strandio  ::  refresh bowl
=/  scry-path=path  /c/x/(scot %ud case)/kids/sys/kelvin
=/  =spar:ames      [who scry-path]
=/  wire-keen       /keen
=/  timeout-time    (add now.bowl timeout)
=/  wire-timer      /wait/(scot %da timeout-time)
::
::  send %keen and set timer
::
;<  ~  bind:m  (keen:strandio wire-keen spar sec=~)
;<  ~  bind:m
  (send-raw-card:strandio %pass wire-timer %arvo %b %wait timeout-time)
::
::  race sage vs timer
::
;<  [did-timeout=? result=vase]  bind:m
  (peek-for-kelvin bowl wire-keen wire-timer timeout-time spar)
::
::  process result
::
?:  did-timeout
  ::  timed out; flag as no-responsive if no previous attempt worked
  ::
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ~?  >  veb
    "ahoy-prob: {<who>} timed out (took {<`@dr`(sub now.bowl start)>})"
  %-  pure:m   !>
  ::  if no previous attempts worked return when to track it in
  ::  the no-response map
  ::
  =?  case  !=(0 case)
    ::  if we time out, and the case is not 0,
    ::  last succesful hash is the previous case
    ::
    (dec case)
  :_  no-response
  [case [remote-kel ~] ?:(no-response now.bowl wen)]
::
::  sage responded; check kelvin
::
=.  no-response  %.n
=+  !<(kelvin=(unit @ud) result)
?~  kelvin
  ::  sys/kelvin desk doesn't exist?; try next case
  ::
  $(case +(case))
::
=.  remote-kel  u.kelvin
~?  >>  &(veb !=(kel u.kelvin))
  "ahoy-prob: {<who>} kelvin is {<u.kelvin>}"
::  we have sys/kelvin; scry for kids desk hash
::
::  send %keen and set timer
::
=/  wire-keen     /kids
=/  =spar:ames    [who /c/z/(scot %ud case)/kids]
;<  =bowl:spider  bind:m  get-bowl:strandio
~&  >  `@s`now.bowl
=/  timeout-time  (add now.bowl timeout)
~&  timeout-time/`@da`timeout-time
=/  wire-timer    /wait/(scot %da timeout-time)
;<  ~  bind:m  (keen:strandio wire-keen spar sec=~)
;<  ~  bind:m
  (send-raw-card:strandio %pass wire-timer %arvo %b %wait timeout-time)
;<  [did-timeout=? result=vase]  bind:m
  (peek-for-hash bowl wire-keen wire-timer timeout-time spar)
::
::  XX ignore possible timeout?
::
~|  result
=+  !<(kids-hash=(unit @uvi) result)
~&  >>>  kids-hash
=.  wen  now.bowl
?~  kids-hash  $(case +(case))
=.  has  u.kids-hash  :: if sys/kelvin exist it should have a hash
::
?.  =(kel u.kelvin)
  ::  not the kelvin we want; try next case
  ::
  $(case +(case))
::  found wait-hash; done with .who
::
~?  >  veb  "ahoy-prob: done with {<who>}"
;<  =bowl:spider  bind:m  get-bowl:strandio
~?  >  veb  end/`@dr`(sub now.bowl start)
(pure:m !>([[case [remote-kel `has] wen] no-response]))
