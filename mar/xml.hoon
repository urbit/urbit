::
::::  /hoon/xml/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  bytes:eyre
=,  xml:eyre
|_  htm/@t
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime  [/application/xml (taco htm)]               ::  to %mime
  ++  hymn  (need (parse htm))                          ::  to %hymn
  --                                                    ::
++  grab  |%                                            ::  convert from
          ++  noun  @t                                  ::  clam from %noun
          ++  mime  |=({p/mite q/octs} q.q)             ::  retrieve form $mime
--        --
