/@  sky
|%
++  state  %sky
++  poke   (sy %sky ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~
    :-  [&/%settings |]
    [%sky-settings %sky-settings]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%sky stud)
    =/  new  (sky !<(sky vax))
    [~ !>(new)]
  ++  init
    |=  vas=(unit vase)
    ^-  (quip card:neo vase)
    :-
      :~  [(welp here.bowl /settings) %make %sky-settings ~ ~]
      ==
    !>
    [~[~[%home]] 1]
  --
--
