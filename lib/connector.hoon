::  This is a library for writing API connectors.
::
::  The basic flow is as follows:
::  --  define a list of `++place`s, which specify the exported
::      interface.  
::  --  in `++peer-scry` in the connector app, call `++read` in
::      this library to match to the appropriate place and
::      produce a move (usually either an immediate response or
::      an http request to the api).
::  --  in `++sigh-httr` in the connector app, call `++sigh` in
::      this library to handle the response according to the
::      place.
|*  {move/mold sub-result/mold}
=>  |%
    ::  A place consists of:
    ::  --  `guard`, the type of the paths we should match.  For
    ::      example, to match `/issues/<user>/<repo>` use
    ::      `{$issues @t @t $~}`.
    ::  --  `read-x`, called when someone tries to read the
    ::      place with care `%x`.  Should produce a single move,
    ::      usually either a `%diff` response if we can
    ::      immediately answer or a `%hiss` http request if we
    ::      need to make a request to the api.  See the
    ::      `++read-*` functions in `++helpers` for some common
    ::      handlers.
    ::  --  `read-y`, same as `read-x` except with care `%y`.
    ::  --  `sigh-x`, called when an http response comes back on
    ::      this place.  You're given the json of the result, and
    ::      you should produce either a result or null.  Null
    ::      represents an error.  If you didn't create an http
    ::      request in `read-x`, then this should never be
    ::      called.  Use `++sigh-strange` from `++helpers` to
    ::      unconditionally signal an error.
    ::  --  `sigh-y`, same as `sigh-x` except with care `%y`.
    ::      Note that a `%y` request must produce an arch, unlike
    ::      a `%x` request, which may produce data of any mark.
    ::
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
  |=  {ost/bone wir/wire api-url/tape}
  |%
  ::  Produce null.  Used as `++read-x` in places which are pure
  ::  directories.  `++sigh-x` should be `++sigh-strange`.
  ::  
  ++  read-null  |=(pax/path [ost %diff %null ~])
  ::
  ::  Produce an arch with the given list of children.  Used as
  ::  `++read-y` in places which have a static list of (known)
  ::  children rather than having to ask the api.  `++sigh-y`
  ::  should be `++sigh-strange`.
  ::
  ++  read-static
    |=  children/(list @t)
    |=  pax/path
    [ost %diff %arch ~ (malt (turn children |=(@t [+< ~])))]
  ::
  ::  Produce an api request to the given path.  Use this if the
  ::  endpoint is static.  If the endpoint depends on parameters
  ::  in the path, use `++get`.  For example:
  ::  `|=(pax/path (get /users/[+<.pax]/repos))`.
  ::
  ++  read-get
    |=  endpoint/path
    |=  pax/path
    (get endpoint)
  ::
  ::  Make an api request to the specified endpoint.
  ::
  ++  get
    |=  endpoint/path
    ^-  move
    :*  ost  %hiss  wir  `~  %httr  %hiss
        (endpoint-to-purl endpoint)  %get  ~  ~
    ==
  ::
  ::  Convert an endpoint path to a purl.
  ::
  ++  endpoint-to-purl
    |=  endpoint/path
    (scan (weld api-url <`path`endpoint>) auri:urlp)
  ::
  ::  Return error.  Used when no http response is expected.
  ::
  ++  sigh-strange  |=(jon/json ~)
  --
::
::  Handles one-time requests by mapping them to their handling,
::  either `read-x` or `read-y`, in `places`.
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
::  Handles http responses sent in `++read` by mappig them to
::  their handling, either `sigh-x` or `sigh-y`, in `places`.
::
++  sigh
  =,  html
  |=  {places/(list place) ren/care pax/path res/httr}
  ^-  sub-result
  =<  ?+(ren ~|([%invalid-care ren] !!) $x sigh-x, $y sigh-y)
  |%
  ++  sigh-x
    ?~  r.res
      ~&  [err+%empty-response code+p.res]
      null+~
    =+  jon=(rush q.u.r.res apex:de-json)
    ?~  jon
      ~&  [err+%bad-json code+p.res body+q.u.r.res]
      null+~
    ?.  =(2 (div p.res 100))
      ~&  [err+%request-rejected code+p.res msg+u.jon]
      null+~
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
    =+  jon=(rush q.u.r.res apex:de-json)
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
