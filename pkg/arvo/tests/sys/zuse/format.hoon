/+  *test
=,  format
|%
:: split a cord on newlines
::
++  test-to-wain
  ;:  weld
    ::  basic usage
    ::
    %+  expect-eq
      !>  ~['hello' 'world']
      !>  (to-wain 'hello\0aworld')
    :: string with no newlines
    ::
    %+  expect-eq
      !>  ~['hey']
      !>  (to-wain 'hey')
    ::  empty string works fine
    ::
    %+  expect-eq
      !>  ~['']
      !>  (to-wain '')
    ::  leading/trailing/consecutive newlines all work fine
    ::
    %+  expect-eq
      !>  ~['' 'hi' '' '' 'there' '']
      !>  (to-wain '\0ahi\0a\0a\0athere\0a')
  ==
::  join a list of lines (cords) into a single cord
::
++  test-of-wain
  ;:  weld
    ::  basic usage
    ::
    %+  expect-eq
      !>  'hey\0athere\0aworld!'
      !>  (of-wain ~['hey' 'there' 'world!'])
    ::  empty list
    ::
    %+  expect-eq
      !>  ''
      !>  (of-wain ~)
    :: single list
    ::
    %+  expect-eq
      !>  'hey'
      !>  (of-wain ~['hey'])
    ::  list with empties
    ::
    %+  expect-eq
      !>  'hey\0a\0athere'
      !>  (of-wain ~['hey' '' 'there'])
  ==
::  join a list of lines (tapes) into a single cord.
::
::    Appends an extra newline - this matches unix conventions of a
::    trailing newline. Also see #1, #2
::
++  test-of-wall
  ;:  weld
    ::  basic usage
    ::
    %+  expect-eq
      !>  "hey\0athere\0aworld!\0a"
      !>  (of-wall ~["hey" "there" "world!"])
    ::  empty list
    ::
    %+  expect-eq
      !>  ""
      !>  (of-wall ~)
    :: single list
    ::
    %+  expect-eq
      !>  "hey\0a"
      !>  (of-wall ~["hey"])
    ::  list with empties
    ::
    %+  expect-eq
      !>  "hey\0a\0athere\0a"
      !>  (of-wall ~["hey" "" "there"])
  ==
::  encoding and decoding of beams <-> paths

::    (a beam is a fully-qualified file reference. ship, desk, version,
::    path)
::
++  test-beam
  =/  b=beam  [[p=~zod q=%home r=[%ud p=12]] s=/hoon/zuse/sys]
  =/  p=path  /~zod/home/12/sys/zuse/hoon
  ;:  weld
    ::  proper encode
    ::
    %+  expect-eq
      !>  p
      !>  (en-beam b)
    ::  proper decode
    ::
    %+  expect-eq
      !>  (some b)
      !>  (de-beam p)
    ::  proper round trip
    ::
    %+  expect-eq
      !>  (some b)
      !>  (de-beam (en-beam b))
    ::  path too short
    ::
    %+  expect-eq
      !>  ~
      !>  (de-beam /~zod/home)
    ::  invalid ship
    ::
    %+  expect-eq
      !>  ~
      !>  (de-beam /'~zodisok'/home/12/sys/zuse/hoon)
    ::  invalid desk
    ::
    %+  expect-eq
      !>  ~
      !>  (de-beam /~zod/12/12/sys/zuse/hoon)
    ::  invalid case
    ::
    %+  expect-eq
      !>  ~
      !>  (de-beam /~zod/home/~zod/sys/zuse/hoon)
  ==
::  example values used in test
::
++  ex
  |%
  ++  nul  `json`~
  ++  tru  `json`[%b &]
  ++  num  `json`[%n ~.12]
  ++  str  `json`[%s 'hey']
  ++  frond  `json`(frond:enjs 'foo' num)
  ++  obj  `json`(pairs:enjs ~[['foo' num] ['bar' str]])
  --
::  functions for creating `json` values
::
++  test-enjs
  =,  enjs
  ;:  weld
    ::  numbers
    ::
    %+  expect-eq
      !>  num:ex
      !>  (numb 12)
    %+  expect-eq
      !>  num:ex
      !>  (numb 0xc)
    %+  expect-eq
      !>  [%n '0']
      !>  (numb 0)
    :: strings
    ::
    %+  expect-eq
      !>  str:ex
      !>  (tape "hey")
    %+  expect-eq
      ::  uses of-wall, so adds the trailing newline
      ::
      !>  [%s 'hi\0athere\0a']
      !>  (wall ~["hi" "there"])
    ::  objects
    ::
    %+  expect-eq
      !>  [%o (molt ~[['foo' num:ex]])]
      !>  (frond 'foo' num:ex)
    =+  props=~[['foo' num:ex] ['bar' tru:ex]]
    %+  expect-eq
      !>  [%o (molt props)]
      !>  (pairs props)
    ::  time - stored as integer number of milliseconds since the unix epoch
    ::
    %+  expect-eq
      !>   [%n '1000']
      !>   (time ~1970.1.1..0.0.1)
    :: ship - store ship identity as a string
    ::
    %+  expect-eq
      !>  [%s 'zod']
      !>  (ship ~zod)
  ==
::  dejs - recursive processing of `json` values
::
::    This version crashes when used on improper input. Prefer using
::    dejs-soft (also tested below) which returns units instead.
::
::  decoding from null, booleans, numbers, strings
::
++  test-dejs-primitives
  =,  dejs
  ;:  weld
    ::  null
    ::
    %+  expect-eq
      !>  ~
      !>  (ul `json`~)
    ::  booleans
    ::
    ::  bo extracts as-is, bu negates it
    ::
    %+  expect-eq
      !>  &
      !>  (bo tru:ex)
    %+  expect-eq
      !>  |
      !>  (bu tru:ex)
    %-  expect-fail
      |.  (bo num:ex)
    %-  expect-fail
      |.  (bu num:ex)
    ::  integers
    ::
    ::  as @
    ::
    %+  expect-eq
      !>  12
      !>  (ni num:ex)
    %-  expect-fail
      |.  (ni tru:ex)
    ::  as cord
    ::
    %+  expect-eq
      !>  '12'
      !>  (no num:ex)
    %-  expect-fail
      |.  (no tru:ex)
    ::  timestamp - ms since the unix epoch
    ::
    %+  expect-eq
      !>  ~1970.1.1..00.00.01
      !>  (di [%n ~.1000])
    %-  expect-fail
      |.  (di tru:ex)
    :: strings
    ::
    :: string as tape
    ::
    %+  expect-eq
      !>  "hey"
      !>  (sa str:ex)
    %-  expect-fail
      |.  (sa tru:ex)
    :: string as cord
    ::
    %+  expect-eq
      !>  'hey'
      !>  (so str:ex)
    %-  expect-fail
      |.  (so tru:ex)
    :: string with custom parser
    ::
    %+  expect-eq
      !>  ' '
      !>  ((su (just ' ')) [%s ' '])
    %-  expect-fail
      |.  ((su (just ' ')) tru:ex)
  ==
::  decoding arrays
::
++  test-dejs-arrays
  =,  dejs
  ;:  weld
    ::  ar - as list
    ::
    %+  expect-eq
      !>  ~[1 2 3]
      !>  ((ar ni) [%a ~[[%n '1'] [%n '2'] [%n '3']]])
    %-  expect-fail
      |.  ((ar ni) str:ex)
    %-  expect-fail
      |.  ((ar ni) [%a ~[str:ex]])
    ::  at - as tuple
    ::
    ::  handlers must match exactly
    ::
    %+  expect-eq
      !>  [1 'hey']
      !>  ((at ~[ni so]) [%a ~[[%n '1'] [%s 'hey']]])
    ::  too few or many handlers crash
    ::
    %-  expect-fail
      |.  ((at ~[ni so]) [%a ~])
    %-  expect-fail
      |.  ((at ~[ni so]) [%a ~[[%n '1'] [%s 'hey'] [%b &]]])
    ::  a nested error will crash
    ::
    %-  expect-fail
      |.  ((at ~[ni]) [%a ~[[%s 'hey']]])
  ==
::  decoding objects
::
++  test-dejs-objects
  =,  dejs
  ;:  weld
    ::  of - single-property objects
    ::
    %+  expect-eq
      !>  ['foo' 12]
      !>  ((of ~[['foo' ni]]) frond:ex)
    %+  expect-eq
      !>  ['foo' 12]
      !>  ((of ~[['bar' so] ['foo' ni]]) frond:ex)
    %-  expect-fail
      ::  the handler needs to apply properly to the value
      ::
      |.  ((of ~[['foo' ni]]) num:ex)
    %-  expect-fail
      ::  the key of the frond needs to exist in the handler list
      ::
      |.  ((of ~[['bar' so]]) frond:ex)
    %-  expect-fail
      ::  an object with multiple properties is an error
      ::
      |.  ((of ~[['bar' so] ['foo' ni]]) obj:ex)
    ::  ot - exact-shape objects to tuple
    ::
    %+  expect-eq
      !>  [12 'hey']
      !>  ((ot ~[['foo' ni] ['bar' so]]) obj:ex)
    %-  expect-fail
      ::  it checks it's called on an actual object
      ::
      |.  ((ot ~[['foo' ni]]) num:ex)
    %-  expect-fail
      ::  missing property on the object
      ::
      |.  ((ot ~[['foo' ni] ['baz' so]]) obj:ex)
    ::  ou - object to tuple, with optional properties. value handlers
    ::
    ::  are passed (unit json)
    ::
    %+  expect-eq
      !>  [12 14]
      !>  ((ou ~[['foo' (uf 14 ni)] ['baz' (uf 14 ni)]]) obj:ex)
    ::  om - simple object as map
    ::
    %+  expect-eq
      !>  (molt ~[['foo' num:ex] ['bar' str:ex]])
      !>  ((om same) obj:ex)
    ::  op - object to map, but run a parsing function on the keys
    ::
    %+  expect-eq
      !>  (molt ~[[12 num:ex] [14 str:ex]])
      !>  ((op dem same) (pairs:enjs ~[['12' num:ex] ['14' str:ex]]))
  ==
::  decoder transformers
::
++  test-dejs-transformers
  =,  dejs
  ;:  weld
    ::  cu - decode, then transform
    ::
    %+  expect-eq
      !>  11
      !>  ((cu dec ni) [%n ~.12])
    ::  ci - decode, then assert a transformation succeeds
    ::
    %+  expect-eq
      !>  12
      !>  ((ci some ni) num:ex)
    %-  expect-fail
      |.  ((ci |=(* ~) ni) num:ex)
    ::  mu - decode if not null
    ::
    %+  expect-eq
      !>  ~
      !>  ((mu ni) nul:ex)
    %+  expect-eq
      !>  (some 12)
      !>  ((mu ni) num:ex)
    ::  pe - add prefix to decoded value
    ::
    %+  expect-eq
      !>  ['a' 12]
      !>  ((pe 'a' ni) num:ex)
    ::  uf - defaults for empty (unit json)
    ::
    %+  expect-eq
      !>  'nah'
      !>  ((uf 'nah' ni) ~)
    %+  expect-eq
      !>  12
      !>  ((uf 'nah' ni) (some num:ex))
    ::  un - dangerous ensure a (unit json)
    ::
    %+  expect-eq
      !>  12
      !>  ((un ni) (some num:ex))
    %-  expect-fail
      |.  ((un ni) ~)
  ==
::  various unit/collection helpers
::
++  test-dejs-helpers
  =,  dejs
  =+  all=`(list (unit @))`~[(some 1) (some 2) (some 3)]
  =+  nall=`(list (unit @))`~[(some 1) ~ (some 3)]
  ;:  weld
    ::  za - are all units in this list full?
    ::
    %+  expect-eq
      !>  &
      !>  (za ~)
    %+  expect-eq
      !>  &
      !>  (za all)
    %+  expect-eq
      !>  |
      !>  (za nall)
    ::  zl - collapse (list (unit)) -> (unit (list))
    ::
    %+  expect-eq
      !>  (some ~[1 2 3])
      !>  (zl all)
    %+  expect-eq
      !>  ~
      !>  (zl nall)
    %+  expect-eq
      !>  (some ~)
      !>  (zl ~)
    ::  zp - force unwrap a (list (unit)) as tuple
    ::
    %+  expect-eq
      !>  [1 2 3]
      !>  (zp all)
    %-  expect-fail
      |.  (zp nall)
    %-  expect-fail
      |.  (zp ~)
    ::  zm - collapse a (map @tas (unit *)) -> (unit (map @tas *))
    ::
    %+  expect-eq
      !>  (some (molt ~[['a' 1] ['b' 2]]))
      !>  (zm (molt ~[['a' (some 1)] ['b' (some 2)]]))
    %+  expect-eq
      !>  ~
      !>  (zm (molt ~[['a' `(unit @)`(some 1)] ['b' ~]]))
    %+  expect-eq
      !>  (some ~)
      !>  (zm ~)
  ==
::
::  dejs-soft recursive processing of `json` values
::
::    These functions return units, which will be nil if the input
::    doesn't match the defined structure.
::
++  test-dejs-soft-primitives
  =,  dejs-soft
  ;:  weld
    ::  null
    ::
    %+  expect-eq
      !>  `~
      !>  (ul `json`~)
    ::  booleans
    ::
    ::  bo extracts as-is, bu negates it
    ::
    %+  expect-eq
      !>  `&
      !>  (bo tru:ex)
    %+  expect-eq
      !>  `|
      !>  (bu tru:ex)
    %+  expect-eq
      !>  ~
      !>  (bo num:ex)
    %+  expect-eq
      !>  ~
      !>  (bu num:ex)
    ::  integers
    ::  as @
    ::
    %+  expect-eq
      !>  `12
      !>  (ni num:ex)
    %+  expect-eq
      !>  ~
      !>  (ni tru:ex)
    ::  as cord
    ::
    %+  expect-eq
      !>  `'12'
      !>  (no num:ex)
    %+  expect-eq
      !>  ~
      !>  (no tru:ex)
    ::  timestamp - ms since the unix epoch
    ::
    %+  expect-eq
      !>  `~1970.1.1..00.00.01
      !>  (di [%n ~.1000])
    %+  expect-eq
      !>  ~
      !>  (di tru:ex)
    :: string as tape
    ::
    %+  expect-eq
      !>  `"hey"
      !>  (sa str:ex)
    %+  expect-eq
      !>  ~
      !>  (sa tru:ex)
    :: string as cord
    ::
    %+  expect-eq
      !>  `'hey'
      !>  (so str:ex)
    %+  expect-eq
      !>  ~
      !>  (so tru:ex)
    :: string with custom parser
    ::
    %+  expect-eq
      !>  `' '
      !>  ((su (just ' ')) [%s ' '])
    %+  expect-eq
      !>  ~
      !>  ((su (just ' ')) tru:ex)
  ==
::  decoding arrays
::
++  test-dejs-soft-arrays
  =,  dejs-soft
  ;:  weld
    ::  ar - as list
    ::
    %+  expect-eq
      !>  `~[1 2 3]
      !>  ((ar ni) [%a ~[[%n '1'] [%n '2'] [%n '3']]])
    %+  expect-eq
      !>  ~
      !>  ((ar ni) str:ex)
    %+  expect-eq
      !>  ~
      !>  ((ar ni) [%a ~[str:ex]])
    ::  at - as tuple
    ::
    ::  handlers must match exactly
    ::
    %+  expect-eq
      !>  `[1 'hey']
      !>  ((at ~[ni so]) [%a ~[[%n '1'] [%s 'hey']]])
    ::  too few or many handlers won't match
    ::
    %+  expect-eq
      !>  ~
      !>  ((at ~[ni so]) [%a ~])
    %+  expect-eq
      !>  ~
      !>  ((at ~[ni so]) [%a ~[[%n '1'] [%s 'hey'] [%b &]]])
    ::  a nested failure to match will propagate upwards
    ::
    %+  expect-eq
      !>  ~
      !>  ((at ~[ni]) [%a ~[[%s 'hey']]])
  ==
::  decoding objects
::
++  test-dejs-soft-objects
  =,  dejs-soft
  ;:  weld
    ::  of - single-property objects
    ::
    %+  expect-eq
      !>  `['foo' 12]
      !>  ((of ~[['foo' ni]]) frond:ex)
    %+  expect-eq
      !>  `['foo' 12]
      !>  ((of ~[['bar' so] ['foo' ni]]) frond:ex)
    %+  expect-eq
      !>  ~
      ::  the handler needs to apply properly to the value
      ::
      !>  ((of ~[['foo' ni]]) num:ex)
    %+  expect-eq
      !>  ~
      ::  the key of the frond needs to exist in the handler list
      ::
      !>  ((of ~[['bar' so]]) frond:ex)
    %+  expect-eq
      !>  ~
      ::  an object with multiple properties is an error
      ::
      !>  ((of ~[['bar' so] ['foo' ni]]) obj:ex)
    ::  ot - exact-shape objects to tuple
    ::
    %+  expect-eq
      !>  `[12 'hey']
      !>  ((ot ~[['foo' ni] ['bar' so]]) obj:ex)
    %+  expect-eq
      !>  ~
      ::  missing property on the object
      ::
      !>  ((ot ~[['foo' ni] ['baz' so]]) obj:ex)
    ::  om - simple object as map
    ::
    %+  expect-eq
      !>  `(molt ~[['foo' num:ex] ['bar' str:ex]])
      !>  ((om some) obj:ex)
    ::  op - object to map, but run a parsing function on the keys
    ::
    %+  expect-eq
      !>  `(molt ~[[12 num:ex] [14 str:ex]])
      !>  ((op dem some) (pairs:enjs ~[['12' num:ex] ['14' str:ex]]))
  ==
::  decoder transformers
::
++  test-dejs-soft-transformers
  =,  dejs-soft
  ;:  weld
    ::  cu - decode, then transform
    ::
    %+  expect-eq
      !>  `11
      !>  ((cu dec ni) [%n ~.12])
    ::  ci - decode, then transform, adapting the transformer to return a
    ::  unit
    ::
    %+  expect-eq
      !>  `12
      !>  ((ci some ni) num:ex)
    %+  expect-eq
      !>  ~
      !>  ((ci |=(* ~) ni) num:ex)
    ::  mu - decode if not null
    ::
    %+  expect-eq
      !>  `~
      !>  ((mu ni) nul:ex)
    %+  expect-eq
      !>  `(some 12)
      !>  ((mu ni) num:ex)
    ::  pe - add prefix to decoded value
    ::
    %+  expect-eq
      !>  `['a' 12]
      !>  ((pe 'a' ni) num:ex)
  ==
::  various unit/collection helpers
::
++  test-dejs-soft-helpers
  =,  dejs-soft
  =+  all=`(list (unit @))`~[(some 1) (some 2) (some 3)]
  =+  nall=`(list (unit @))`~[(some 1) ~ (some 3)]
  ;:  weld
    ::  za - are all units in this list full?
    ::
    %+  expect-eq
      !>  &
      !>  (za ~)
    %+  expect-eq
      !>  &
      !>  (za all)
    %+  expect-eq
      !>  |
      !>  (za nall)
    ::  zl - collapse (list (unit)) -> (unit (list))
    ::
    %+  expect-eq
      !>  (some ~[1 2 3])
      !>  (zl all)
    %+  expect-eq
      !>  ~
      !>  (zl nall)
    %+  expect-eq
      !>  (some ~)
      !>  (zl ~)
    ::  zp - force unwrap a (list (unit)) as tuple
    ::
    %+  expect-eq
      !>  [1 2 3]
      !>  (zp all)
    %-  expect-fail
      |.  (zp nall)
    %-  expect-fail
      |.  (zp ~)
    ::  zm - collapse a (map @tas (unit *)) -> (unit (map @tas *))
    ::
    %+  expect-eq
      !>  (some (molt ~[['a' 1] ['b' 2]]))
      !>  (zm (molt ~[['a' (some 1)] ['b' (some 2)]]))
    %+  expect-eq
      !>  ~
      !>  (zm (molt ~[['a' `(unit @)`(some 1)] ['b' ~]]))
    %+  expect-eq
      !>  (some ~)
      !>  (zm ~)
  ==
--
