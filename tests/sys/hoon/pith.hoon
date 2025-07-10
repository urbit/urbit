::  tests for $pith, $iota, their literal and pattern syntaxes, and utils
::
/+  *test
|%
++  test-literal-syntax
  %+  expect-eq
    !>(`pith`~[%$ %$ %a ud+1 p+~zod t+'BAZ!'])
  !>(`pith`#/$//a/1/~zod/'BAZ!')
::
++  test-pattern-syntax
  =/  pith=(pole iota)  #/$//a/1/~zod/'BAZ!'
  ?>  ?=(#/$//a/x=@ud/y=@p/@t pith)
  %+  weld
    (expect-eq !>(1) !>(x.pith))
  (expect-eq !>(~zod) !>(y.pith))
::
++  test-stip
  %+  expect-eq
    !>(#/$//a/1/~zod/'BAZ!')
  !>((rash '/$//a/1/~zod/\'BAZ!\'' stip))
::
++  test-pout
  %+  expect-eq
    !>(/$//a/1/~zod/~~~42.~41.~5a.~21.)
  !>((pout #/$//a/1/~zod/'BAZ!'))
::
++  test-pave
  %+  expect-eq
    !>(#/$//a/1/~zod/'BAZ!')
  !>((pave /$//a/1/~zod/~~~42.~41.~5a.~21.))
--
