/@  accel
/@  accel-diff
::
=>
|%
++  make-cells
  |=  [=bowl:neo colstart=@ud colend=@ud rowstart=@ud rowend=@ud]
  ^-  (list card:neo)
  %-  zing
  %+  turn  (gulf colstart colend)
  |=  col=@ud
  %+  turn  (gulf rowstart rowend)
  |=  row=@ud
  [#/[p/our.bowl]/home/accel/[ud/col]/[ud/row] %make %accel-cell `!>(['~' ~ ~ ~ ~]) ~]
--
::
^-  firm:neo
|%
++  state  %accel
++  poke  (sy %accel-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%ud |/%ud |]
      [%accel-cell %sig]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  old=(unit vase)
    =/  width  2
    =/  height  2
    :_  !>([width height])
    (make-cells bowl 1 width 1 height)
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%accel-diff stud)
    =/  state  !<(accel sta)
    =/  poke  !<(accel-diff vax)
    ?>  =(our ship.src):bowl
    ?-    -.poke
        %inc-width
      =/  new  (add 1 width.state)
      :_  !>([new height.state])
      (make-cells bowl new new 1 height.state)
    ::
        %inc-height
      =/  new  (add 1 height.state)
      :_  !>([width.state new])
      (make-cells bowl 1 width.state new new)
    ::
        %new
      :_  sta
      =;  conf
        :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
            :*  %make 
                %accel-cell 
                `!>([text.poke ~ refa.poke refb.poke target.poke]) 
                conf
            ==
        ==
      :: there has to be a more elegant way to do this
      ?~  refa.poke
        ?~  refb.poke
          ~
        (malt ~[[%b u.refb.poke]])
      ?~  refb.poke
        (malt ~[[%a u.refa.poke]])
      (malt `(list [term pith])`~[[%a u.refa.poke] [%b u.refb.poke]])
    ==
  --
--
