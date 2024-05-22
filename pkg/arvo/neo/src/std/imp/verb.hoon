=> 
|%
++  card  card:neo
++  wrap-firm
  |=  vax=vase
  ^-  vase
  =+  !<(inner=kook:neo vax)
  !>  ^-  kook:neo
  |%
  ++  poke  poke:inner
  ++  state  state:inner
  ++  kids  kids:inner
  ++  deps  deps:inner
  ++  form
    ^-  form:neo
    |_  [=bowl:neo =saga:neo]
    +*  sta  !<([cache=(unit vase) ~] state-vase)
        og   ~(. form:inner [bowl saga])
    ++  poke
      |=  [=stud:neo vax=vase]
      ^-  (quip card:neo pail:neo)
      %-  (slog leaf/"poke {(en-tape:name:neo src.bowl)} -> {<stud>}" ~)
      (poke:og stud vax)
    ++  init
      |=  vax=(unit vase)
      ^-  (quip card:neo pail:neo)
      %-  (slog leaf/"init {(en-tape:name:neo src.bowl)} " ~)
      (init:og vax)
    --
  --
--
^-  kook:neo
|%
++  poke    (sy %ford-in ~)
++  state   pro/%vase
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  src/dep:ford:neo
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<([cache=(unit vase) ~] state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(?(%ford-in %rely) stud)
    `vase/(need (bind (get-output:ford:neo bowl %src) wrap-firm))
    :: ~&  ford-same/[were.bowl !=(~ cache.sta)]
    `!>(sta)
  ++  init
    |=  vax=(unit vase)
    ^-  (quip card:neo vase)
    `vase/(need (bind (get-output:ford:neo bowl %src) wrap-firm))
  --
--
