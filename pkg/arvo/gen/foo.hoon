::  construct an arvo shaped noun from a pill
::
/+  pill
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] [pil=pill:pill ~] ~]
:-  %noun
=?  pil  ?=(%cash -.pil)
  ^-  $>(%pill pill:pill)
  [%pill +<.pil]
?>  ?=(%pill -.pil)
::
=.  kernel-ova.pil                            ::  filter to /sys
  %+  turn  kernel-ova.pil
  |=  =unix-event:pill
  ^-  unix-event:pill
  ?.  ?=([%what *] q.unix-event)  unix-event
  =/  files=(list (pair path (cask)))
    %+  skim  p.q.unix-event
    |=  [=path *]
    ?=([%sys *] path)
  unix-event(q [%what files])
::
=/  =wynn        ::  XX this is wrong, if we care, double bootstrap it?
  :~  zuse+zuse
      lull+lull
      arvo+arvo
      hoon+hoon-version
      nock+4
  ==
::
=/  res=toon
  %-  mock
  :_  ~
  :_  [%2 [%0 3] %0 2]
  ;:  weld
    boot-ova.pil
    ^-  (list)
    :~  [*@da //arvo %wack *@uvJ]
        [*@da //arvo %whom *@p]
        [*@da //arvo %wyrd [~.nonce /] wynn]
    ==
    `(list)`(turn kernel-ova.pil (lead *@da))
    `(list)`[*@da [/d/term/1 %boot & %fake *@p]]^~
  ==
::
?-  -.res
  %0  +7.p.res                       ::  success
::
  %1  ~&  [%vere-blocked p.res]  !!
  %2  ~&  %vere-fail  (mean p.res)
==
