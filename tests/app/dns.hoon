/+  *test
::
/=  app  /:  /===/app/dns
             /!noun/
::
|%
::  tests that :dns preps without moves
::
++  test-prep
  ::  .our explicitly set to not-a-galaxy to avoid failing %jael scry
  ::  (can't control the scry product without virtualizing)
  ::
  =/  bow=bowl:gall  =>(*bowl:gall .(our ~marzod))
  =^  moves  app  (~(prep app bow *state:app) ~)
  %+  expect-eq
      !>  ^-  (list move:app)
          :~  [ost.bow %connect /dns/oauth [~ /dns/oauth] %dns]
          ==
      !>  moves
--
