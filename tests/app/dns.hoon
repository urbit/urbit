/+  *test
::
/=  app  /:  /===/app/dns
             /!noun/
::
|%
::  tests that :dns preps without moves
::
++  test-prep
  =^  moves  app  (~(prep app *bowl:gall *state:app) ~)
  %+  expect-eq
      !>  *(list move:app)
      !>  moves
--
