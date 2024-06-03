/@  accel-conf
=> 
|%
++  card  card:neo
++  build
  |=  [=bowl:neo conf=accel-conf]
  ^-  (quip card pail:neo)
  :_  accel-conf/!>(conf)
  =/  =term
    `@tas`(cat 3 'accel-' (scot %t (spat (pout (snip here.bowl)))))
  =/  =pith:neo
    #/[p/our.bowl]/cod/std/out/imp/[term]
  =;  =vase
    [pith %make %ford-riff `vase/vase ~]^~
  =-  !>(-)
  =>  [..zuse neo=neo og=bowl conf=conf]
  =>
    |%
    ++  lift-lore
      |=  =lore:neo
      ^-  vase
      q.pail:(~(got of:neo lore) ~)
    ::
    ++  get-prelude
      |=  =bowl:neo
      ^-  vase
      %+  with-faces:ford:neo  (slop !>(neo) !>(..zuse))
      :-  bowl/!>(bowl)
      %+  turn  ~(tap by deps.bowl)
      |=  [=term =pith:neo =lore:neo]
      ^-  [^term vase]
      [term (lift-lore lore)]
    ::
    ++  produce
      |=  =bowl:neo
      ^-  pail:neo
      =/  res=(each vase tang)
        (mule |.((slap (get-prelude bowl) hoon.conf)))
      ?:  ?=(%& -.res)
        vase/p.res
      tang/!>(p.res)
    --
  ^-  kook:neo
  |%
  ++  state  any/~
  ++  poke  (sy %rely ~)
  ++  kids  *kids:neo
  ++  deps  
    %-  ~(gas by *band:neo)
    %+  turn  ~(tap in ~(key by crew.conf))
    |=  =term
    ^-  [_term fief:neo]
    [term req=& [any/~ ~] ~]
  ++  form
    ^-  form:neo
    |_  [=bowl:neo =aeon:neo =pail:neo]
    ++  poke
      |=  [=stud:neo vax=vase]
      ^-  (quip card:neo pail:neo)
      `(produce bowl)
    ++  init
      |=  pal=(unit pail:neo)
      `(produce bowl)
    --
  --
--
^-  kook:neo
|%
++  state  pro/%accel-conf
++  poke  (sy %hoon %accel-dep %accel-conf %ack ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  +*  sta  !<(conf=accel-conf q.pail)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =+  !<(conf=accel-conf q.pail)
    =.  conf
      ?+  stud  !!
        %hoon        conf(ready |, hoon !<(hoon vax))
        %accel-dep   conf(ready |, crew (~(put by crew.conf) !<([term pith] vax)))
        %accel-conf  !<(accel-conf vax)
        %ack         ((slog leaf/"ack" (sell vax) ~) conf(ready &))
      ==
    ?:  ready.conf
      `accel-conf/!>(conf)
    (build bowl conf)
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =+  !<(conf=accel-conf q:(need old))
    (build bowl conf)
  --
--
