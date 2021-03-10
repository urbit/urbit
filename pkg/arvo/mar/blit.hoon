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
      %hop  ?@  p.blit  (numb p.blit)
            (pairs 'r'^(numb r.p.blit) 'c'^(numb c.p.blit) ~)
      %put  a+(turn p.blit |=(c=@c s+(tuft c)))
      %nel  b+&
      %url  s+p.blit
      %wyp  b+&
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
            =,  tint
            (pairs r+(numb r) g+(numb g) b+(numb b) ~)
          --
      ==
    ==
  --
--
