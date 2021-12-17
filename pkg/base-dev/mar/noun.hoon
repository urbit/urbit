::
::::  /hoon/noun/mar
  ::
/?    310
!:
::::  A minimal noun mark
|_  non=*
++  grab  |%
          ++  noun  *
          ++  mime  |=([* =octs] (cue q.octs))
          --
++  grad
  |%
  ++  form  %noun
  ++  diff  |=(* +<)
  ++  pact  |=(* +<)
  ++  join  |=([* *] *(unit *))
  ++  mash  |=([[ship desk *] [ship desk *]] `*`~|(%noun-mash !!))
  --
++  grow
  |%
  ++  mime  [/application/x-urb-noun (as-octs:mimes:html (jam non))]
  --
--
