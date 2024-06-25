/@  sail
/-  sl=sail
^-  kook:neo
|%
++  state  pro/%sail
++  poke   (sy %sail ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  =pail:neo
    ^-  (quip card:neo pail:neo)
    ?>  =(p.pail %sail)
    `pail
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ?~  pal  sail/!>(*sail)
    ::  non-empty init unit always re-renders the code
    =/  sal  !<(sail q.u.pal)
    :-  %sail
    !>
    sal(result `(render-udon:sl code.sal))
  --
--
