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
  =/  msg  'requesting an https certificate for zod.urbit.org'
  ;:  weld
    %+  expect-eq
        !>  :~  =-  [ost.bow.app [%poke / -]]
                =-  [[~zod %hall] %hall-action %phrase (sy [~zod %inbox] ~) -]
                ~[[%app %$ [%lin & msg]]]
                [ost.bow.app %wait /acme/try/1/directory +(now.bow.app)]
            ==
        !>  moves
  ::
    %+  expect-eq
        !>  [~ dom]
        !>  (some ~(key by (fall next-order.app ~)))
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
      !>  ~[[ost.bow.app [%hiss /acme/try/2/directory ~ %httr %hiss directory-base:app %get ~ ~]]]
      !>  moves
--
