/@  accel-diff
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
    :_  (need old)
    :~  [#/[p/our.bowl]/home/accel/1/1 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/1/2 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/1/3 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/1/4 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/2/1 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/2/2 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/2/3 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/2/4 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/3/1 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/3/2 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/3/3 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/3/4 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/4/1 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/4/2 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/4/3 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
        [#/[p/our.bowl]/home/accel/4/4 %make %accel-cell `!>(['~' ~ ~ ~]) ~]
    ==
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%accel-diff stud)
    =/  poke  !<(accel-diff vax)
    ?>  =(our ship.src):bowl
    :_  sta
    =;  conf  
      :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
          [%make %accel-cell `!>([text.poke ~ refa.poke refb.poke]) conf]
      ==
    :: there has to be a more elegant way to do this
    ?~  refa.poke
      ?~  refb.poke
        ~
      (malt ~[[%b u.refb.poke]])
    ?~  refb.poke
      (malt ~[[%a u.refa.poke]])
    (malt `(list [term pith])`~[[%a u.refa.poke] [%b u.refb.poke]])
  --
--
