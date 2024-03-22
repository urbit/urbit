=> 
|%
++  card  card:neo
+$  state  [cache=(unit vase) ~]
++  wrap-firm
  |=  vax=vase
  ^-  vase
  =+  !<(inner=firm:neo vax)
  !>  ^-  firm:neo
  |%
  ++  poke  poke:inner
  ++  state  state:inner
  ++  kids  kids:inner
  ++  deps  deps:inner
  ++  form
    ^-  form:neo
    |_  [=bowl:neo =icon:neo]
    +*  sta  !<([cache=(unit vase) ~] state-vase)
        og   ~(. form:inner [bowl icon])
    ++  poke
      |=  [=stud:neo vax=vase]
      ^-  (quip card:neo vase)
      %-  (slog leaf/"poke {(en-tape:name:neo src.bowl)} -> {<stud>}" ~)
      (poke:og stud vax)
    ++  init
      |=  vax=(unit vase)
      ^-  (quip card:neo vase)
      %-  (slog leaf/"init {(en-tape:name:neo src.bowl)} " ~)
      (init:og vax)
    --
  --
--
^-  firm:neo
|%
++  poke    (sy %ford-in ~)
++  state   %ford-out
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
    =/  sta  sta
    =.  cache.sta  (bind (get-output:ford:neo bowl %src) wrap-firm)
    :: ~&  ford-same/[were.bowl !=(~ cache.sta)]
    `!>(sta)
  ++  init
    |=  vax=(unit vase)
    ^-  (quip card:neo vase)
    =|  sta=[cache=(unit vase) ~]
    =.  cache.sta  (bind (get-output:ford:neo bowl %src) wrap-firm)
    %-  ?~  cache.sta  (slog leaf/"no link" ~)
        (slog leaf/"link" (sell u.cache.sta) ~)
    `!>(sta)
  --
--
