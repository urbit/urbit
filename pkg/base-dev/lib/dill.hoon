::  dill: utilities for dill's data structures
::
=,  dill
|%
++  enjs
  |%
  ++  blit
    |=  =blit:dill
    ^-  json
    =,  enjs:format
    %+  frond  -.blit
    ?-  -.blit
      %bel  b+&
      %clr  b+&
      %hop  ?@  p.blit  (numb p.blit)
            (pairs 'x'^(numb x.p.blit) 'y'^(numb y.p.blit) ~)
      %put  a+(turn p.blit |=(c=@c s+(tuft c)))
      %nel  b+&
      %url  s+p.blit
      %wyp  b+&
      %mor  a+(turn p.blit ^blit)
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
::
++  dejs
  |%
  ++  belt
    |=  jon=json
    ^-  belt:dill
    ?:  ?=([%s *] jon)
      (taft p.jon)
    =,  dejs:format
    %.  jon
    %-  of
    |^  :*  mod+(ot 'mod'^mod 'key'^bot ~)
            txt+(ar (cu taft so))
            bol
        ==
    ::
    ++  bol
      :~  aro+(su (perk %d %l %r %u ~))
          bac+ul
          del+ul
          hit+(ot 'x'^ni 'y'^ni ~)
          ret+ul
      ==
    ::
    ++  bot
      |=  j=json
      ^-  bolt:dill
      ?+  j  !!
        [%s *]  (taft p.j)
        [%o *]  ((of bol) j)
      ==
    ::
    ++  mod
      |=  j=json
      ((su (perk %ctl %met %hyp ~)) j)
    --
  --
--