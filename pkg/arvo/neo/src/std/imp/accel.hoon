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
    %-  zing
    %+  turn  (gulf 1 10)
    |=  col=@ud
    %+  turn  (gulf 1 10)
    |=  row=@ud
    [#/[p/our.bowl]/home/accel/[ud/col]/[ud/row] %make %accel-cell `!>(['~' ~ ~ ~]) ~]
  ::
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
