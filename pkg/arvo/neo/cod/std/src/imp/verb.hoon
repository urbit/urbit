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
    +*  og   ~(. form:inner [bowl saga])
    ++  poke
      |=  [=stud:neo vax=vase]
      ^-  (quip card:neo pail:neo)
      %-  (slog leaf/"poke {(en-tape:name:neo src.bowl)} -> {<stud>}" ~)
      (poke:og stud vax)
    ++  init
      |=  pal=(unit pail:neo)
      ^-  (quip card:neo pail:neo)
      %-  (slog leaf/"init {(en-tape:name:neo src.bowl)} " ~)
      (init:og pal)
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
  |_  [=bowl:neo =aeon:neo sted=stud:neo state-vase=vase]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(?(%ford-in %rely) stud)
    `vase/(need (bind (get-output:ford:neo bowl %src) wrap-firm))
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `vase/(need (bind (get-output:ford:neo bowl %src) wrap-firm))
  --
--
