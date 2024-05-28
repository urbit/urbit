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
  [(welp here.bowl #/[ud/col]/[ud/row]) %make %accel-cell `accel-cell/!>(['~' ~ ~ ~ ~]) ~]
--
::
^-  kook:neo
|%
++  state  pro/%accel
++  poke  (sy %accel-diff ~)
++  kids
  :-  ~
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |/%ud |]
      [pro/%accel-cell (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  ::|_  [=bowl:neo =ever:neo sta=vase *]
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  width  10
    =/  height  10
    :_  accel/!>([width height])
    (make-cells bowl 1 width 1 height)
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%accel-diff stud)
    =/  state  !<(accel q.pail)
    =/  poke  !<(accel-diff vax)
    ?>  =(our ship.src):bowl
    ?-    -.poke
        %inc-width
      =/  new  (add 1 width.state)
      :_  accel/!>([new height.state])
      (make-cells bowl new new 1 height.state)
    ::
        %inc-height
      =/  new  (add 1 height.state)
      :_  accel/!>([width.state new])
      (make-cells bowl 1 width.state new new)
    ::
        %new
      :_  accel/q.pail
      =;  conf
        :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
            :*  %make
                %accel-cell
                `accel-cell/!>([text.poke ~ refa.poke refb.poke target.poke])
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
