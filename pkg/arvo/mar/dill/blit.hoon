::
::::  /hoon/blit/dill/mar
  ::
/?    310
/-    sole
=,  sole
=,  enjs:format
|_  dib/dill-blit:dill
::
++  grab                                                   ::  convert from
  |%
  ++  noun  dill-blit:dill                                 ::  clam from %noun
  --
++  grow
  |%
  ++  json
    ^-  ^json
    ?+  -.dib  ~|(unsupported-blit+-.dib !!)
      $mor  [%a (turn p.dib |=(a/dill-blit:dill json(dib a)))]
      $hop  (frond %hop (numb p.dib))
      ?($pro $out)  (frond -.dib (tape (tufa p.dib)))
      ?($bel $clr)  (frond %act %s -.dib)
    ==
  --
--
