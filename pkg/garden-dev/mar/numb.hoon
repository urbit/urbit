::::  /hoon/atom/numb
  ::
/?    310
::
::::  A minimal atom mark
  ::
=,  mimes:html
|_  ato=@
++  grab  |%
          ++  noun  @
          ++  mime  |=([* p=octs] q.p)
          ++  json  ni:dejs:format
          --
++  grow  |%
          ++  mime  [/application/x-urb-unknown (as-octs ato)]
          ++  json  (numb:enjs:format ato)
          --
++  grad  %mime
--
