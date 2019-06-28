::
::::  /hoon/html/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  html
|_  htm/@t
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime  [/text/html (met 3 htm) htm]                ::  to %mime
  ++  hymn  (need (de-xml htm))                         ::  to %hymn
  --                                                    ::
++  grab  |%                                            ::  convert from
          ++  noun  @t                                  ::  clam from %noun
          ++  mime  |=({p/mite q/octs} q.q)             ::  retrieve form $mime
          --
++  grad  %mime
--
