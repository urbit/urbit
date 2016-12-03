::
::::  /hoon/quri/mar
  ::
/?    310
=,  ^eyre
=,  mimes:html
=,  html
|_  url/quri
::
++  grow  |%    ++  mime  [text+/x-uri (as-octt (apex:en-purl url))]
          --
++  grab                                                ::  convert from
  |%
  ++  noun  quri                                        ::  clam from %noun
  ++  mime  |=(mim/^mime (rash q.q.mim zest:de-purl))
  --
--
