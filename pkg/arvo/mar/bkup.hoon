::
/?    310
::::  compute
=,  html
|_  bkup=*
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/x-urb-backup (as-octs:mimes (jam bkup))]   ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  mime  |=([p=mite q=octs] (vase (cue `@`q.q)))
  ++  noun  *
  --
++  grad  %mime
--
