::
::::  /hoon/xml/mar
  ::
/?  314
  ::
::::  compute
  ::
|_  htm=@t
::
++  grow                                                ::  convert to
  |%                                                    ::
  ++  mime  [/application/xml (taco htm)]               ::  to %mime
  ++  hymn  (need (poxa htm))                           ::  to %hymn
  --                                                    ::
++  grab  |%                                            ::  convert from
          ++  noun  ,@t                                 ::  clam from %noun
          ++  mime  |=([p=mite q=octs] q.q)             ::  retrieve form %mime
--        --
