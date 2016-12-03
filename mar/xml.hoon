::
::::  /hoon/xml/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  mimes:html
=,  html
|_  xml/@t
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime  [/application/xml (as-octs xml)]            ::  to %mime
  ++  hymn  (need (de-xml xml))                         ::  to %hymn
  --                                                    ::
++  grab  |%                                            ::  convert from
          ++  noun  @t                                  ::  clam from %noun
          ++  mime  |=({p/mite q/octs} q.q)             ::  retrieve form $mime
--        --
