::  blit: runtime blit structure
::
/+  base64
::
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
      %url  s+p.blit
    ::
        %sag
      %-  pairs
      :~  'path'^(path p.blit)
          'file'^s+(en:base64 (as-octs:mimes:html (jam q.blit)))
      ==
    ::
        %sav
      %-  pairs
      :~  'path'^(path p.blit)
          'file'^s+(en:base64 (as-octs:mimes:html q.blit))
      ==
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
