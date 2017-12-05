::  Womb: allocate urbits to the holder of an email
::
::::  /hoon/invite/hood/gen
  ::
/?    310
::
::::
  ::
/+  hood-womb
=*  invite     invite:hood-womb
=*  reference  reference:hood-womb
|%
++  plural
  |=  {a/@u b/tape}  ^+  b
  ?:  =(1 a)  "one {b}"
  =;  n/tape  "{n} {b}s"
  ~|  plural-stub+a  ::TODO expand
  %-  trip
  %+  snag  a  ^~
  %+  weld
    /no/''/two/three/four/five/six/seven/eight/nine/ten/elven/twelve
  /thirteen/fourteen/fifteen/sixteen/seventeen/eighteen/nineteen/twenty
::
++  type  $%({$planets planets/@u} {$stars stars/@u})
--
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        {{who/@t etc/$@($~ {typ/type $~})} ref/(unit (each ship mail:hood-womb))}
    ==
?~  etc  $(etc ~[planets+2])
:-  %womb-invite
^-  {cord reference invite}
=+  inv=(scot %uv (end 7 1 eny))
=;  d/[planets=@u stars=@u inf=tape]
  [inv ref [who planets.d stars.d "Your invite for {inf.d}: {(trip inv)}" ~]]
::
?:  =(0 +.typ.etc)  ~|(%empty-invite !!)
?-  -.typ.etc
  $stars    [planets=0 stars.typ.etc (plural stars.typ.etc "star")]
  $planets  [planets.typ.etc stars=0 (plural planets.typ.etc "planet")]
==
