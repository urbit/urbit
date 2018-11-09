::  Kiln: Merge local desk from (optionally-)foreign one
::
::::  /hoon/merge/hood/gen
  ::
/?    310
=,  clay
::
|%
++  beaky  {knot knot knot ~}
++  sorc  ?({bek/beaky ~} {her/@p sud/@tas ~})
--
::
::::
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bek/beak}
        {arg/{?(sorc {syd/$@(desk beaky) sorc})} cas/case gem/?(germ $auto)}
    ==
=*  our  p.bek
|^  :-  %kiln-merge
    ^-  {syd/desk her/ship sud/desk cas/case gem/?(germ $auto)}
    ?-  arg
      {@ @ ~}
        =+(arg [sud ?.(=(our her) her (sein:title p.bek now her)) sud (opt-case da+now) gem])
    ::
      {^ ~}
        =+  (pars bek.arg)
        [dez ?.(=(our who) who (sein:title p.bek now who)) dez (opt-case caz) gem]
    ::
      {* @ @ ~}
        :-  (pars-src syd.arg)
        =+(arg [her sud (opt-case da+now) gem])
    ::
      {* ^ ~}
        :-  (pars-src syd.arg)
        =+((pars bek.arg) [who dez (opt-case caz) gem])
    ==
++  opt-case  |=(a/case ?:(=(*case cas) a cas))  :: override
++  pars  |=(a/beaky `{{who/ship dez/desk caz/case} *}`(need (de-beam:format a)))
++  pars-src
  |=  syd/$@(desk beaky)
  ?@  syd  syd
  =+  (pars syd)
  ~|  [%into-foreign who `path`syd]
  ?>(=(our who) dez)
--
