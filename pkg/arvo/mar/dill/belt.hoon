::
::::  /hoon/belt/dill/mar
  ::
/?    310
/-    kyev, sole
::
::::
  ::
=,  sole
|_  dill-belt:dill
::
++  grab                                                ::  convert from
  |%
  ++  json
    =<  (cork . kyev)
    |=  jon/^json  ^-  ^kyev
    !!
::    %-  need
::    %.  jon  =>  jo  %-  ot
::    :~  mod+(cu silt (ar (su (perk ~[%ctrl %shift %alt %meta]))))
::        :-  %key
::        %+  cu  |*(a/$%({$str @t} {$act @}) ?+(-.a a $str +.a))
::        =-  (of [str+so act+(su (perk -)) ~])
::        :~  %ctrl  %shift  %alt   %meta   %entr  %esc  %caps  %uncap
::            %pgup  %pgdn   %home  %end    %baxp  %del  %ins
::            %up    %down   %left  %right
::    ==  ==
  ++  kyev
    |=  kev/^kyev  ^-  dill-belt:dill
    ~|  dill-belt-incomplete+kev
    ?:  ?=({$act ?($ctrl $shift $alt $meta)} q.kev)
      [%txt ~]                        ::  ignore
    =+  mod=(~(del in p.kev) %shift)
    ?^  mod
      ?^  q.kev  !!                   ::  only accept strings
      =.  q.kev
        ?.  (~(has in p.kev) %ctrl)
          q.kev
        (con 96 q.kev)                ::  ctrl key decoding
      =+  cha=(tuba (trip q.kev))
      ?>  ?=({@ ~} cha)               ::  of a single character
      ?+  mod  !!                     ::  modified by one buckykey
        {$ctrl ~ ~}  [%ctl i.cha]
        {$alt ~ ~}   [%met i.cha]
      ==
    ?@  q.kev
      [%txt (tuba (trip q.kev))]
    ?+  +.q.kev  !!
      $del   [%del ~]
      $baxp  [%bac ~]
      $entr  [%ret ~]
      $up     [%aro %u]
      $down   [%aro %d]
      $left   [%aro %l]
      $right  [%aro %r]
    ==  ::  %yow, %rez?
  ::
  ++  noun  dill-belt:dill            ::  clam from %noun
  --
--
