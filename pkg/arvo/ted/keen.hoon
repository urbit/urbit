/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
=>  |%  +$  out  $%  [%sage =sage:mess:ames]
                     [%tune (pair spar:ames (unit roar:ames))]
                     [%page (pair spar:ames (unit (unit page)))]
                 ==
    --
::
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =spar:ames sec=$@(~ ?([%chum ~] [%shut idx=@ key=@]))] arg)
;<    ~   bind:m
  ?@  sec  (keen:strandio /peek spar ~)
  ?-  -.sec
    %chum  (chum:strandio /peek spar)
    %shut  (keen:strandio /peek spar `+.sec)
  ==

;<  =out  bind:m  (take-message:strandio /peek)
::=/  =sage:mess:ames
::  ?-  -.out
::    %sage  sage.out
::    %tune  :-  p.+.out
::           ?~  q.+.out  ~
::           ?~  q.dat.u.q.+.out  ~
::           u.q.dat.u.q.+.out
::    %page  :-  p.+.out
::           ?~  q.+.out    ~
::           ?~  u.q.+.out  ~
::           u.u.q.+.out
::  ==
::?~  q.sage
::  (pure:m !>([leaf+"... empty response ..." ~]))
::::
::;<  =bowl:spider  bind:m  get-bowl:strandio
::=+  .^  =dais:clay  %cb
::        /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.q.sage]
::    ==
::=/  res  (mule |.((vale.dais q.q.sage)))
::?.  ?=(%| -.res)
::  (pure:m p.res)
::~|(%keen-mark-fail (mean leaf+"-peek: ames vale fail {<mark>}" p.res))
(pure:m !>(out))
