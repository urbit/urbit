::
::::  /hoon/css/mar
  ::
/?    310
=,  eyre
=,  mimes:html
|_  mud=@t
++  grow                                                ::  convert to
  |%  
  ++  mime  [/text/css (as-octs mud)]               ::  convert to %mime
  ++  hymn                                          ::  convert to %hymn
    |^  html
    ++  style  ;style                                  
                 ;-  (trip mud)
               ==
    ++  html   ;html:(head:"{style}" body)
    --
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=([p=mite q=octs] (@t q.q))
  ++  noun  @t                                         ::  clam from %noun
  --
++  grad  %mime
--
