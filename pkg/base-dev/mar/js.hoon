::
::::  /hoon/js/mar
  ::
/?    310
::
=,  eyre
|_  mud=@
++  grow
  |%
  ++  mime  [/application/javascript (as-octs:mimes:html (@t mud))]
  ++  hymn                                          ::  convert to %hymn
    |^  html
    ++  script  ;script
                    ;-  (trip (@t mud))
                ==
    ++  html    ;html:(head:"{script}" body)
    --
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=([p=mite q=octs] (@t q.q))
  ++  noun  cord                                        ::  clam from %noun
  --
++  grad  %mime
--
