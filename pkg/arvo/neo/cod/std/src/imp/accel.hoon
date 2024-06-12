/@  accel
/@  accel-diff
/@  accel-conf
::
=>
|%
+$  kind  ?(%in %out %both)
++  make-cells
  |=  [=bowl:neo colstart=@ud colend=@ud rowstart=@ud rowend=@ud =kind]
  ^-  (list card:neo)
  %-  zing
  %+  turn  (gulf colstart colend)
  |=  col=@ud
  ^-  (list card:neo)
  %-  zing
  %+  turn  (gulf rowstart rowend)
  |=  row=@ud
  ^-  (list card:neo)
  (make-cell bowl row col *accel-conf kind)
::
++  make-cell
  |=  [=bowl:neo row=@ud col=@ud conf=accel-conf =kind]
  ^-  (list card:neo)
  =/  =pith:neo  (welp here.bowl #/[ud/col]/[ud/row])
  =/  =stud:neo  
    `@tas`(cat 3 'accel-' (scot %t (spat (pout pith))))
  =;  caz=(list card:neo)
    ?-  kind
      %in   (snag 0 caz)^~
      %out  (snag 1 caz)^~
      %both  caz
    ==
  :~  [`pith:neo`(snoc pith `@tas`%in) %make %accel-conf `accel-conf/!>(conf) ~]
      [`pith:neo`(snoc pith `@tas`%out) %make stud ~ crew.conf]
  ==

--
::
^-  kook:neo
|%
++  state  pro/%accel
++  poke  (sy %accel-diff %gift ~)
++  kids
  :-  ~
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |/%ud &/%in |]
      [pro/%accel-conf ~]
    ::
      :-  [|/%ud |/%ud &/%out |]
      [any/~ ~]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  width  3
    =/  height  3
    :_  accel/!>([width height])
    (make-cells bowl 1 width 1 height %in)
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  state  !<(accel q.pail)
    |^  ^-  (quip card:neo pail:neo)
    =^  cards=(list card:neo)  state
      ?+  stud   !!
        %accel-diff  (on-diff !<(accel-diff vax))
        %gift        (on-gift !<(gift:neo vax))
      ==
    [cards accel/!>(state)]
    ++  on-diff
      |=  poke=accel-diff
      ?>  =(our ship.src):bowl
      ?+    -.poke  !!
          %inc-width
        =/  new  (add 1 width.state)
        =.  width.state  +(width.state)
        :_  state
        =,  state
        (make-cells bowl width width 1 height %in)
      ::
          %inc-height
        =/  new  (add 1 height.state)
        =.  height.state  +(height.state)
        :_  state
        =,  state
        (make-cells bowl 1 width new new %in)
      ==
    ++  on-gift
      |=  =gift:neo
      ^-  (quip card:neo _state)
      :_  state
      =/  changes  ~(tap by ~(tar of:neo gift))
      %-  zing
      %+  turn  changes
      |=  [=road:neo =loot:neo]
      ^-  (list card:neo)
      ?.  ?=([[%ud col=@] [%ud row=@ud] %in ~] road)
        ~
      ?.  (~(has of:neo kids.bowl) road)
        ~
      =+  !<(conf=accel-conf q.pail:(~(got of:neo kids.bowl) road))
      ?.  ready.conf
        ~
      (make-cell bowl row.road col.road conf %out)
    --
  --
--
