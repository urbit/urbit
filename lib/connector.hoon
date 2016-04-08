|*  {move/mold sub-result/mold}
=>  |%
    ++  place
      $:  guard/mold
          read-x/$-(path move)
          read-y/$-(path move)
          sigh-x/$-(jon/json (unit sub-result))
          sigh-y/$-(jon/json (unit arch))
      ==
    --
|%
::  Generic helpers for place definitions
::
++  helpers
  |=  {ost/bone wir/wire}
  |%
  ++  read-null  |=(pax/path [ost %diff %null ~])
  ++  read-static
    |=  children/(list @t)
    |=  pax/path
    [ost %diff %arch ~ (malt (turn children |=(@t [+< ~])))]
  ::
  ++  read-get
    |=  endpoint/path
    |=  pax/path
    (get endpoint)
  ::
  ++  sigh-strange  |=(jon/json ~)
  ::
  ++  get
    |=  endpoint/path
    ^-  move
    :*  ost  %hiss  wir  `~  %httr  %hiss
        (endpoint-to-purl endpoint)  %get  ~  ~
    ==
  ::
  ++  endpoint-to-purl
    |=  endpoint/path
    (scan "https://api.github.com{<`path`endpoint>}" auri:epur)
  --
::
::  This handles one-time requests by mapping them to their
::  handling in ++places.
::
++  read
  |=  {ost/bone places/(list place) ren/care pax/path}
  ^-  move
  ?~  places
    ~&  [%strange-path pax]
    (move [ost %diff ?+(ren !! $x null+~, $y arch+*arch)])
  =+  match=((soft guard.i.places) pax)
  ?~  match
    $(places t.places)
  (?+(ren !! $x read-x.i.places, $y read-y.i.places) pax)
::
++  sigh
  |=  {places/(list place) ren/care pax/path res/httr}
  ^-  sub-result
  =<  ?+(ren ~|([%invalid-care ren] !!) $x sigh-x, $y sigh-y)
  |%
  ++  sigh-x
    ?~  r.res
      json+(jobe err+s+%empty-response code+(jone p.res) ~)
    =+  jon=(rush q.u.r.res apex:poja)
    ?~  jon
      json+(jobe err+s+%bad-json code+(jone p.res) body+s+q.u.r.res ~)
    ?.  =(2 (div p.res 100))
      json+(jobe err+s+%request-rejected code+(jone p.res) msg+u.jon ~)
    |-  ^-  sub-result
    ?~  places
      ~&([%sigh-strange-path pax] (sub-result null+~))
    =+  match=((soft guard.i.places) pax)
    ?~  match
      $(places t.places)
    =+  (sigh-x.i.places u.jon)
    ?~  -
      ~&  [err+s+%response-not-valid pax+pax code+(jone p.res) msg+u.jon]
      (sub-result null+~)
    u.-
  ::
  ++  sigh-y
    ?~  r.res
      ~&  [err+s+%empty-response code+(jone p.res)]
      arch+*arch
    =+  jon=(rush q.u.r.res apex:poja)
    ?~  jon
      ~&  [err+s+%bad-json code+(jone p.res) body+s+q.u.r.res]
      arch+*arch
    ?.  =(2 (div p.res 100))
      ~&  [err+s+%request-rejected code+(jone p.res) msg+u.jon]
      arch+*arch
    %-  sub-result
    |-  ^-  {$arch arch}
    ?~  places
      ~&([%sigh-strange-path pax] arch+*arch)
    =+  match=((soft guard.i.places) pax)
    ?~  match
      $(places t.places)
    =+  (sigh-y.i.places u.jon)
    ?~  -
      ~&  [err+s+%response-not-valid pax+pax code+(jone p.res) msg+u.jon]
      arch+*arch
    arch+u.-
  --

--
