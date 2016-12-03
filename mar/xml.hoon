::
::::  /hoon/xml/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  bytes:eyre
=,  html
|_  xml/@t
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime  [/application/xml (taco xml)]               ::  to %mime
  ++  hymn  (need (de-xml xml))                         ::  to %hymn
  --                                                    ::
++  grab  |%                                            ::  convert from
          ++  noun  @t                                  ::  clam from %noun
          ++  mime  |=({p/mite q/octs} q.q)             ::  retrieve form $mime
--        --
