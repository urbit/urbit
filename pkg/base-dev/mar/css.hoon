::
::::  /hoon/css/mar
  ::
/?    310
=,  eyre
=,  mimes:html
|_  mud=@t
++  grow                                                ::  convert to
  |%  ++  mime  [/text/css (as-octs mud)]               ::  convert to %mime
      ++  elem  ;style                                  ::  convert to %hymn
                  ;-  (trip mud)
                ==
      ++  hymn  ;html:(head:"{elem}" body)
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=([p=mite q=octs] (@t q.q))
  ++  noun  @t                                         ::  clam from %noun
  --
++  grad  %mime
--
