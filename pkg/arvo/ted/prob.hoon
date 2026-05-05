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
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
::
=+  !<  $:  ~
            timeout=@dr
            [case=@ud has=@uvi wen=@da]
            who=ship
            wait-hash=@uvi
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
::
|-
;<  =bowl:spider  bind:m  get-bowl:strandio  ::  refresh bowl
=/  scry-path=path  /c/z/(scot %ud case)/kids
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
=/  m-race  (strand ,[timeout=? result=vase])
;<  [timeout=? result=vase]  bind:m
:: XX
::   ^-  form:m-race
::   %+  (map-err ,~)  |=(* [%offline *tang])
::   %+  (set-timeout ,~)  timeout-time
  ^-  form:m-race
  |=  tin=strand-input:strand
  ^-  output:m-race
  ?+    in.tin  `[%skip ~]
      ~  `[%wait ~]
  ::
  ::  sage response: give kids hash
  ::
      [~ %sign * %ames %sage *]
    ?.  =(wire-keen wire.u.in.tin)
      `[%skip ~]
    =/  =sage:mess:ames  sage.sign-arvo.u.in.tin
    :^    [%pass wire-timer %arvo %b %rest timeout-time]~
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
    ?.  =(wire-timer wire.u.in.tin)
      `[%skip ~]
    ?^  error.sign-arvo.u.in.tin
      `[%fail %timer-error u.error.sign-arvo.u.in.tin]
    :-  [%pass wire-keen %arvo %a %yawn spar]~
    [%done [%.y !>(*(unit @uvi))]]
  ==
::
::  process result
::
?:  timeout
  ;<  =bowl:spider  bind:m  get-bowl:strandio
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
  [case has ?:(no-response now.bowl wen)]
::
::  sage responded; check kelvin
::
=.  no-response  %.n
=+  !<(kids-hash=(unit @uvi) result)
?~  kids-hash
  ::  %kids desk doesn't exist?; try next case
  ::
  $(case +(case))
::
~?  >>  &(veb !=(wait-hash u.kids-hash))
  "ahoy-prob: {<who>} kids hash is {<u.kids-hash>}"
=:  has  u.kids-hash
    wen  now.bowl
  ==
::
?.  =(wait-hash has)
  ::  not last-hash; try next case
  ::
  $(case +(case))
::  found wait-hash; done with .who
::
~?  >  veb  "ahoy-prob: done with {<who>}"
;<  =bowl:spider  bind:m  get-bowl:strandio
~?  >  veb  end/`@dr`(sub now.bowl start)
(pure:m !>([[case has wen] no-response]))
