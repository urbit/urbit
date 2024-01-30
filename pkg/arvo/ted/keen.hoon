/-  *spider
/+  strandio
=,  strand
^-  thread
=>  |%
    +$  rate    (unit [frag=_0 num=_1])
    +$  u-roar  (unit roar:ames)
    ++  take-rate
      |=  [=wire until=(unit @da)]
      =/  m  (strand ,vase)
      ^-  form:m
      |=  tin=strand-input
      ?+  in.tin  `[%skip ~]
          ~
        `[%wait ~]
        ::
          [~ %sign [%wait @ ~] %behn %wake *]
        ?.  |(?=(~ until) =(`u.until (slaw %da i.t.wire.u.in.tin)))
          `[%skip ~]
        ?~  error.sign-arvo.u.in.tin
          `[%done !>(~)]
        `[%fail %timer-error u.error.sign-arvo.u.in.tin]
        ::
          [~ %sign * sign-arvo=[%ames %tune spar=^ roar=*]]
        ?.  =(wire wire.u.in.tin)
          `[%skip ~]
        `[%done !>(+>+.sign-arvo.u.in.tin)]
      ==
    --
::
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ freq=@dr =spar:ames] arg)
;<  =bowl  bind:m  get-bowl:strandio
;<  ~      bind:m  (keen:strandio /keen spar)
::  short timeout to account for small files
::
;<  ~      bind:m  (send-wait:strandio (add now.bowl (div ~s1 4)))
;<  =vase  bind:m  (take-rate /keen `(add now.bowl (div ~s1 4)))
::
=+  !<(=u-roar vase)
=+  rate=[frag=0 num=1]
|-  ^-  form:m
?:  &((lth [frag num]:rate) ?=(~ u-roar))
  ;<  =^bowl   bind:m  get-bowl:strandio
  ;<  ~        bind:m  (send-wait:strandio (add now.bowl freq))
  ;<  =^vase   bind:m  (take-rate /keen `(add now.bowl freq))
  ;<  r=^rate  bind:m  %+  scry:strandio  ^rate
                       (weld /ax//fine/rate [(scot %p ship.spar) path.spar])
  ~?  >>  ?=(^ r)
    rate/(mul:rs (div:rs (sun:rs frag.u.r) (sun:rs num.u.r)) (sun:rs 100))
  =+  !<(maybe=^u-roar vase)
  $(rate ?~(r [1 0] u.r), u-roar maybe)
::
?~  u-roar               ~&  >>>  %no-roar  (pure:m !>(~))
?~  data=q.dat.u.u-roar  ~&  >>>  %no-data  (pure:m !>(~))
~&  >  rate/.100
::
;<  =^bowl  bind:m  get-bowl:strandio
=+  .^  =dais:clay  %cb
        /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.u.data]
    ==
=/  res  (mule |.((vale.dais q.u.data)))
?.  ?=(%| -.res)
  (pure:m p.res)
~|(%keen-mark-fail (mean leaf+"-keen: ames vale fail {<mark>}" p.res))
