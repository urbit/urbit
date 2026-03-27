::  ahoy/comb: sequential peer kelvin scanner
::
::  scans peers one at a time via remote scry
::  for each peer, tries successive cases until it receives the hash or timeout.
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
            old-hashes=(map ship [num=@ud has=@uvi when=@da])
            peers=(list ship)
            last-hash=@uvi
            veb=?
        ==
    arg
;<  =bowl:spider  bind:m  get-bowl:strandio
::
::  resolve peer list
::
=/  peer-list=(list ship)
  ?^  peers  peers
  =/  all=(map ship ?(%alien %known))
    .^  (map ship ?(%alien %known))  %ax
      /(scot %p our.bowl)//(scot %da now.bowl)/peers
    ==
  %+  murn  ~(tap by all)
  |=  [who=ship sta=?(%alien %known)]
  ?:  =(our.bowl who)  ~
  ?.  ?=(%known sta)   ~
  ?:  ?=(%pawn (clan:title who))  ~
  `who
::
=/  start=@da  now.bowl
::
=|  hashes=(map ship [num=@ud has=@uvi when=@da])
=|  no-response=(map ship @da)
=|  cas=(unit @ud)
::
|-
?~  peer-list
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ~?  >  veb  end/`@dr`(sub now.bowl start)
  (pure:m !>([hashes no-response]))
::
=/  who=@p  i.peer-list
=/  case=@ud
  ?^  cas  u.cas
  ?~  c=(~(get by old-hashes) who)  1
  num.u.c  ::  check num again, since we now that worked
::  refresh bowl
::
;<  =bowl:spider  bind:m  get-bowl:strandio
::
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
=/  m-race  (strand ,[which=? result=vase])
;<  [which=? result=vase]  bind:m
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
    =/  cancel=(list card:agent:gall)
      [%pass wire-timer %arvo %b %rest timeout-time]~
    ?~  q.sage
      [cancel %done [%.y !>(*(unit @uvi))]]
    ?.  =(%uvi p.q.sage)
      [cancel %done [%.y !>(*(unit @uvi))]]
    [cancel %done [%.y !>([~ ;;(hash=@uvi q.q.sage)])]]
  ::
  ::  timer: peer timed out
  ::
      [~ %sign * %behn %wake *]
    ?.  =(wire-timer wire.u.in.tin)
      `[%skip ~]
    ?^  error.sign-arvo.u.in.tin
      `[%fail %timer-error u.error.sign-arvo.u.in.tin]
    =/  yawn=card:agent:gall  [%pass wire-keen %arvo %a %yawn spar]
    [[yawn ~] %done [%.n !>(*(unit @uvi))]]
  ==
::
::  process result
::
?.  which
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ::  timed out; mark no-response if no previous attempt worked and try next peer
  ::
  ~?  >  veb  "ahoy-comb: {<who>} timed out"
  =?  no-response  !(~(has by hashes) who)
    (~(put by no-response) who now.bowl)
  $(peer-list t.peer-list, cas ~)
::
::  sage responded; check kelvin
::
:: =.  no-response  (~(del in no-response) who)
=+  !<(kids-hash=(unit @uvi) result)
?~  kids-hash
  ::  %kids desk doesn't exist?; try next case
  ::
  $(cas `+(case))
::
~?  >>  &(veb !=(last-hash u.kids-hash))
  "ahoy-comb: {<who>} kids hash is {<u.kids-hash>}"
=.  hashes    (~(put by hashes) who [case u.kids-hash now.bowl])
::
?.  =(last-hash u.kids-hash)
  ::  not last-hash; try next case
  ::
  $(cas `+(case))
::  found last-hash; done with this peer
::
$(peer-list t.peer-list, cas ~)
