/+  *test
::
/=  app  /:  /===/app/acme
             /!noun/
::
|%
::  tests that acme preps without moves
::
++  test-prep
  =^  moves  app  (~(prep app *bowl:gall *acme:app) ~)
  %+  expect-eq
      !>  &
      !>  ?=([[bone %serve *] ~] moves)
::  tests that acme inits on first order
::
++  test-first-order
  =/  dom=(set turf)  (sy /org/urbit/zod ~)
  =^  moves  app  (~(poke-acme-order app *bowl:gall *acme:app) dom)
  ;:  weld
    %+  expect-eq
        !>  2
        !>  (lent moves)
  ::
    %+  expect-eq
        !>  [~ dom]
        !>  ?~(next-order.app ~ (some ~(key by dom.u.next-order.app)))
  ::
    %+  expect-eq
        !>  &
        !>  !=(*key:rsa:app key.act.app)
  ::
    %+  expect-eq
        !>  &
        !>  !=(*key:rsa:app cey.app)
  ==
::  tests that acme requests service directory on %wake
::
++  test-first-order-wake
  =^  moves  app  (~(wake app *bowl:gall *acme:app) /acme/try/1/directory ~)
  %+  expect-eq
      !>  :~  :*  ost.bow.app
                  %request
                  /acme/try/2/directory
                  [%'GET' (crip (en-purl:html directory-base:app)) ~ ~]
                  *outbound-config:iris
          ==  ==
      !>  moves
--
