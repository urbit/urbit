::  blit: runtime blit structure
::
~%  %mar-blit  ..is  ~
|_  =blit:dill
++  grad  %noun
::  +grab: convert from
::
++  grab
  |%
  ++  noun  blit:dill
  --
::  +grow: convert to
::
++  grow
  ~%  %blit-grow  ..grow  ~
  |%
  ++  noun  blit
  ++  json
    ^-  ^json
    =,  enjs:format
    %+  frond  -.blit
    ?-  -.blit
      %bel  b+&
      %clr  b+&
      %hop  (numb p.blit)
      %lin  s+(crip (tufa p.blit))
      %mor  b+&
      %sag  (pairs 'path'^(path p.blit) 'file'^s+(jam q.blit) ~)
      %sav  (pairs 'path'^(path p.blit) 'file'^s+q.blit ~)
      %url  s+p.blit
    ::
        %klr
      :-  %a
      %+  turn  p.blit
      |=  [=stye text=(list @c)]
      %-  pairs
      :~  'text'^s+(crip (tufa text))
        ::
          :-  'stye'
          %-  pairs
          :~  'back'^[?~(. ~ s+.)]:p.q.stye
              'fore'^[?~(. ~ s+.)]:q.q.stye
              'deco'^a+(turn ~(tap in p.stye) |=(d=deco ?~(d ~ s+d)))
          ==
      ==
    ==
  --
--
