::
::::  /hoon/merge/hood/gen
  ::
/?  314
!:
|%
++  beaky  ,[span span span ~]
++  sorc  ?([bek=beaky ~] [her=@p sud=@tas ~])
--
::
::::
  !:
:-  %say
|=  $:  [now=@da eny=@uvI bek=beak]
        [arg=[?(sorc [syd=$|(desk beaky) sorc])] cas=case gem=?(%auto germ)]
    ==
=.  cas  ?:(=(*case cas) da/now cas)
=*  our  p.bek
|^  :-  %kiln-merge
    ^-  [syd=desk her=ship sud=desk cas=case gem=?(%auto germ)]
    ?-  arg
      [@ @ ~]    =+(arg [sud ?.(=(our her) her (sein her)) sud cas gem])
      [^ ~]      =+  (pars bek.arg)
                 [dez ?.(=(our who) who (sein who)) dez cas gem]
      [* @ @ ~]  [(pars-src syd.arg) =+(arg [her sud cas gem])]
      [* ^ ~]    [(pars-src syd.arg) =+((pars bek.arg) [who dez cas gem])]
    ==
++  pars  |=(a=beaky =+((need (tome a)) `[who=ship dez=desk]`[p q]))
++  pars-src
  |=  syd=$|(desk beaky)
  ?@  syd  syd
  =+  (pars syd)
  ~|  [%into-foreign who `path`syd]
  ?>(=(our who) dez)
--
