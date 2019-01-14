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
      !>  *(list move:app)
      !>  moves
::  tests that acme inits on first order
::
++  test-first-order
  =/  dom=(set turf)  (sy /org/urbit/zod ~)
  =^  moves  app  (~(poke-acme-order app *bowl:gall *acme:app) dom)
  =/  msg  "requesting an https certificate for zod.urbit.org"
  ;:  weld
    %+  expect-eq
        !>  :~  [ost.bow.app %wait /acme/directory +(now.bow.app)]
                [ost.bow.app %flog / %text msg]
            ==
        !>  moves
  ::
    %+  expect-eq
        !>  [~ dom]
        !>  pen.app
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
  =^  moves  app  (~(wake app *bowl:gall *acme:app) /acme/directory ~)
  =/  url
    =-  (need (de-purl:html -))
    'https://acme-staging-v02.api.letsencrypt.org/directory'
  %+  expect-eq
      !>  ~[[ost.bow.app [%hiss /acme/directory/~zod ~ %httr %hiss url %get ~ ~]]]
      !>  moves
--
