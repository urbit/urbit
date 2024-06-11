/@  sail
^-  kook:neo
=<
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
    sal(result `(render-udon code.sal))
  --
--
|%
++  render-udon
  |=  code=@t
  ^-  (each manx tang)
  =/  newline  (trip 10)
  =/  udon
    :: format as udon document
    %-  crip
    ;:  welp
      ";>"  newline  newline
      (trip code)  newline
    ==
  =/  mul
    %-  mule
    |.
    !<  manx
    %+  slap
      ;:  slop
        !>(..zuse)
      ==
    (ream udon)
  ?-  -.mul
    %.y  [%.y (manx p.mul)]
    %.n  [%.n (tang p.mul)]
  ==
--
