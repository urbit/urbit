::
::::  /hoon/profile/mar
  ::
/?    310
::/-    plan-acct, plan-diff, hall
/-    profile
::
=,  mimes:html
=,  format
|_  all=profile:profile
::
++  grow                                                ::  convert to
  =+  all
  |%
  ++  json
    |^
    ^-  ^json
    %-  pairs:enjs
    :~  [%nickname s+nickname.all]
        [%location s+location.all]
        [%streams a+(turn ~(tap in streams.all) json-circle)]
        [%collections a+(turn ~(tap in collections.all) json-circle)]
    ==
    ::
    ++  json-circle
      |=  a/circle:hall  
      ^-  ^json
      %-  pairs:enjs
      :~  hos+(ship:enjs hos.a)
          nom+s+nom.a
      ==
    --
  ++  mime  [/mime/urb-profile (as-octs (jam all))]
  ++  noun  all
  --
++  grab  
  |%                                            ::  convert from
  ++  noun  profile:profile                     ::  clam from %noun
  ++  mime  
    |=  [* a=octs]
    ^-  profile:profile
    ((hard profile:profile) (cue q.a))
    ++  json
      |^
      |=  jon=^json
      ^-  profile:profile
      =/  out  %.  jon
      %-  ot:dejs
      :~  nickname+so:dejs
          location+so:dejs
          streams+(ar:dejs parse-circle)
          collections+(ar:dejs parse-circle)
      ==
      %=  out
        +>-  (silt +>-.out)
        +>+  (silt +>+.out)
      ==
      ::
      ++  parse-circle
        |=  jon=^json
        ^-  circle:hall
        %.  jon
        %-  ot:dejs
        :~  hos+(su:dejs fed:ag)
            nom+so:dejs
        ==
      --
  --
++  grad  %mime
--
