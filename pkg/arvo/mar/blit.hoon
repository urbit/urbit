::  blit: runtime blit structure
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
      %lin  a+(turn p.blit |=(c=@c s+(tuft c)))
      %mor  b+&
      %url  s+p.blit
    ::
        %sag
      %-  pairs
      :~  'path'^(path p.blit)
          'file'^s+(en:base64:mimes:html (as-octs:mimes:html (jam q.blit)))
      ==
    ::
        %sav
      %-  pairs
      :~  'path'^(path p.blit)
          'file'^s+(en:base64:mimes:html (as-octs:mimes:html q.blit))
      ==
    ::
        %klr
      :-  %a
      %+  turn  p.blit
      |=  [=stye text=(list @c)]
      %-  pairs
      :~  'text'^a+(turn text |=(c=@c s+(tuft c)))
        ::
          :-  'stye'
          %-  pairs
          |^  :~  'back'^(color p.q.stye)
                  'fore'^(color q.q.stye)
                  'deco'^a+(turn ~(tap in p.stye) |=(d=deco ?~(d ~ s+d)))
              ==
          ++  color
            |=  =tint
            ?@  tint  ?~(tint ~ s+tint)
            s+(crip ((x-co:co 6) (rep 3 ~[b g r]:tint)))
          --
      ==
    ==
  --
--
