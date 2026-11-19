/+  *test,
    *text,
    *utf
|%
++  test-text-from-da
  ;:  weld
  %+  expect-eq
    !>  '~2000.1.1'
    !>  (from-da *@da)
  %+  expect-eq
    !>  '~292277024401-.1.1'
    !>  (from-da `@da`0)
  ==
::
++  test-text-from-dr
  ;:  weld
  %+  expect-eq
    !>  '~s1'
    !>  (from-dr ~s1)
  %+  expect-eq
    !>  '~d213503982334601.h7.s16'
    !>  (from-dr (bex 128))
  ==
::
++  test-text-from-p
  ;:  weld
  %+  expect-eq
    !>  '~zod'
    !>  (from-p *@p)
  %+  expect-eq
    !>  '~marzod'
    !>  (from-p ~marzod)
  ==
::
++  test-text-from-rd
  ;:  weld
  %+  expect-eq
    !>  '.~5'
    !>  (from-rd .~5.0)
  %+  expect-eq
    !>  '.~nan'
    !>  (from-rd (dec (bex 64)))
  %+  expect-eq
    !>  '.~inf'
    !>  (from-rd .~inf)
  ==
::
++  test-text-from-rh
  ;:  weld
  %+  expect-eq
    !>  '.~~5'
    !>  (from-rh .~~5.0)
  %+  expect-eq
    !>  '.~~nan'
    !>  (from-rh (dec (bex 16)))
  %+  expect-eq
    !>  '.~~inf'
    !>  (from-rh .~~inf)
  ==
::
++  test-text-from-rq
  ;:  weld
  %+  expect-eq
    !>  '.~~~5'
    !>  (from-rq .~~~5.0)
  %+  expect-eq
    !>  '.~~~nan'
    !>  (from-rq (dec (bex 128)))
    %+  expect-eq
    !>  '.~~~inf'
    !>  (from-rq .~~~inf)
  ==
::
++  test-text-from-rs
  ;:  weld
  %+  expect-eq
    !>  '.5'
    !>  (from-rs .5.0)
  %+  expect-eq
    !>  '.nan'
    !>  (from-rs (dec (bex 32)))
  %+  expect-eq
    !>  '.inf'
    !>  (from-rs .inf)
  ==
::
++  test-text-from-ub
  ;:  weld
  %+  expect-eq
    !>  '0b0'
    !>  (from-ub *@ub)
  %+  expect-eq
    !>  '0b1111.1111.0000.0000'
    !>  (from-ub `@ub`0xff00)
  ==
::
++  test-text-from-ud
  ;:  weld
  %+  expect-eq
    !>  '0'
    !>  (from-ud *@ud)
  %+  expect-eq
    !>  '256'
    !>  (from-ud 256)
  %+  expect-eq
    !>  '1.000'
    !>  (from-ud 1.000)
  ==
::
++  test-text-from-ux
  ;:  weld
  %+  expect-eq
    !>  '0x0'
    !>  (from-ux *@ux)
  %+  expect-eq
    !>  '0xff'
    !>  (from-ux 0xff)
  ==
::
++  test-text-from-path
  ;:  weld
  %+  expect-eq
    !>  '/a'
    !>  (from-path /a)
  %+  expect-eq
    !>  '/a/b/c'
    !>  (from-path /a/b/c)
  %+  expect-eq
    !>  '/'
    !>  (from-path *path)
  ==
::
++  test-text-from-number
  ;:  weld
  %+  expect-eq
    !>  '0'
    !>  (from-number .~0)
  %+  expect-eq
    !>  '2'
    !>  (from-number .~2.0)
  %+  expect-eq
    !>  '2.5'
    !>  (from-number .~2.5)
  %+  expect-eq
    !>  '1e3'
    !>  (from-number .~1000)
  ==
::
++  test-text-from-int
  ;:  weld
  %+  expect-eq
    !>  '0'
    !>  (from-int 0)
  %+  expect-eq
    !>  '2'
    !>  (from-int 2)
  %+  expect-eq
    !>  '1000'
    !>  (from-int 1.000)
  ==
::
++  test-text-to-da
  ;:  weld
  %+  expect-eq
    !>  *@da
    !>  (to-da '~2000.1.1')
  %+  expect-eq
    !>  `@da`0
    !>  (to-da '~292277024401-.1.1')
  %+  expect-eq
    !>  ~2000.2.1
    !>  (to-da '~2000.1.32')
  %-  expect-fail
    |.  (to-da 'January 1, 2000')
  ==
::
++  test-text-to-dr
  ;:  weld
  %+  expect-eq
    !>  ~s1
    !>  (to-dr '~s1')
  %+  expect-eq
    !>  ~d366
    !>  (to-dr '~d365.h24')
  %+  expect-eq
    !>  `@dr`*@da
    !>  (to-dr '~d106751991814902')
  %-  expect-fail
    |.  (to-dr '~y1')
  ==
::
++  test-text-to-p
  ;:  weld
  %+  expect-eq
    !>  *@p
    !>  (to-p '~zod')
  %+  expect-eq
    !>  ~marzod
    !>  (to-p '~marzod')
  %-  expect-fail
    |.  (to-p 'zod')
  ==
::
++  test-text-to-rd
  ;:  weld
  %+  expect-eq
    !>  .~5
    !>  (to-rd '.~5')
  %+  expect-eq
    !>  .~5
    !>  (to-rd '.~5.0')
  %+  expect-eq
    !>  .~inf
    !>  (to-rd '.~inf')
  %+  expect-eq
    !>  .~nan
    !>  (to-rd '.~nan')
  %-  expect-fail
    |.  (to-rd '.5')
  ==
::
++  test-text-to-rh
  ;:  weld
  %+  expect-eq
    !>  .~~5
    !>  (to-rh '.~~5')
  %+  expect-eq
    !>  .~~5
    !>  (to-rh '.~~5.0')
  %+  expect-eq
    !>  .~~inf
    !>  (to-rh '.~~inf')
  %+  expect-eq
    !>  .~~nan
    !>  (to-rh '.~~nan')
  %-  expect-fail
    |.  (to-rh '.5')
  ==
::
++  test-text-to-rq
  ;:  weld
  %+  expect-eq
    !>  .~~~5
    !>  (to-rq '.~~~5')
  %+  expect-eq
    !>  .~~~5
    !>  (to-rq '.~~~5.0')
  %+  expect-eq
    !>  .~~~inf
    !>  (to-rq '.~~~inf')
  %+  expect-eq
    !>  .~~~nan
    !>  (to-rq '.~~~nan')
  %-  expect-fail
    |.  (to-rq '.5')
  ==
::
++  test-text-to-rs
  ;:  weld
  %+  expect-eq
    !>  .5
    !>  (to-rs '.5')
  %+  expect-eq
    !>  .5
    !>  (to-rs '.5.0')
  %+  expect-eq
    !>  .inf
    !>  (to-rs '.inf')
  %+  expect-eq
    !>  .nan
    !>  (to-rs '.nan')
  %-  expect-fail
    |.  (to-rs '.~5')
  ==
::
++  test-text-to-ub
  ;:  weld
  %+  expect-eq
    !>  *@ub
    !>  (to-ub '0b0')
  %+  expect-eq
    !>  `@ub`0xff00
    !>  (to-ub '0b1111.1111.0000.0000')
  %-  expect-fail
    |.  (to-ub '0x1.0000')
  ==
::
++  test-text-to-ud
  ;:  weld
  %+  expect-eq
    !>  *@ud
    !>  (to-ud '0')
  %+  expect-eq
    !>  256
    !>  (to-ud '256')
  %+  expect-eq
    !>  1.000
    !>  (to-ud '1.000')
  %-  expect-fail
    |.  (to-ud '0x1.0000')
  ==
::
++  test-text-to-ux
  ;:  weld
  %+  expect-eq
    !>  *@ux
    !>  (to-ux '0x0')
  %+  expect-eq
    !>  0xff
    !>  (to-ux '0xff')
  %-  expect-fail
    |.  (to-ux '0b1111.0000')
  ==
::
++  test-text-to-path
  ;:  weld
  %+  expect-eq
    !>  /a
    !>  (to-path '/a')
  %+  expect-eq
    !>  /a/b/c
    !>  (to-path '/a/b/c')
  %+  expect-eq
    !>  *path
    !>  (to-path '/')
  ==
::
++  test-text-to-number
  ;:  weld
  %+  expect-eq
    !>  .~0
    !>  (to-number '0')
  %+  expect-eq
    !>  .~2
    !>  (to-number '2')
  %+  expect-eq
    !>  .~2.5
    !>  (to-number '2.5')
  %+  expect-eq
    !>  .~1000
    !>  (to-number '1e3')
  %-  expect-fail
    |.  (to-number 'one')
  ==
::
++  test-text-to-int
  ;:  weld
  %+  expect-eq
    !>  0
    !>  (to-int '0')
  %+  expect-eq
    !>  2
    !>  (to-int '2')
  %+  expect-eq
    !>  1.000
    !>  (to-int '1000')
  %-  expect-fail
    |.  (to-int '2.5')
  ==
::
++  test-utf8-lasso
  ;:  weld
  %+  expect-eq
    !>  *calf:utf8
    !>  (lasso:utf8 '')
  %+  expect-eq
    !>  "A"
    !>  (lasso:utf8 'A')
  %+  expect-eq
    !>  ~['Å']
    !>  (lasso:utf8 'Å')
  %+  expect-eq
    !>  `tape``(list @)`~[0xc3 0x85]
    !>  "Å"
  %+  expect-eq
    !>  ~['𐐞' '𐐰' '𐑌' '𐐲' '𐐼' '𐐭']
    !>  (lasso:utf8 '𐐞𐐰𐑌𐐲𐐼𐐭')
  ==
++  test-utf8-brand
  ;:  weld
  %+  expect-eq
    !>  ''
    !>  (brand:utf8 *calf:utf8)
  %+  expect-eq
    !>  'A'
    !>  (brand:utf8 ~['A'])
  %+  expect-eq
    !>  'Å'
    !>  (brand:utf8 ~['Å'])
  %+  expect-eq
    !>  '𐐞𐐰𐑌𐐲𐐼𐐭'
    !>  (brand:utf8 ~['𐐞' '𐐰' '𐑌' '𐐲' '𐐼' '𐐭'])
  %+  expect-eq
    !>  'Å'
    !>  (brand:utf8 `tape``(list @)`~[0xc3 0x85])
  %+  expect-eq
    !>  'Xanadu'
    !>  (brand:utf8 "Xanadu")
  ==
::
++  test-utf8-upper
  ;:  weld
  %+  expect-eq
    !>  ''
    !>  (upper:utf8 *@t)
  %+  expect-eq
    !>  'A'
    !>  (upper:utf8 'a')
  %+  expect-eq
    !>  'A'
    !>  (upper:utf8 'A')
  %+  expect-eq
    !>  'Å'
    !>  (upper:utf8 'å')
  %+  expect-eq
    !>  'Å'
    !>  (upper:utf8 'Å')
  %+  expect-eq
    !>  '𐐞𐐈𐐤𐐊𐐔𐐅'
    !>  (upper:utf8 '𐐞𐐰𐑌𐐲𐐼𐐭')
  %+  expect-eq
    !>  'Å'
    !>  (upper:utf8 (crip ;;(tape ~[0xc3 0xa5])))
  %+  expect-eq
    !>  'XANADU'
    !>  (upper:utf8 'Xanadu')
  ==
::
++  test-utf8-lower
  ;:  weld
  %+  expect-eq
    !>  ''
    !>  (lower:utf8 *@t)
  %+  expect-eq
    !>  'a'
    !>  (lower:utf8 'a')
  %+  expect-eq
    !>  'a'
    !>  (lower:utf8 'A')
  %+  expect-eq
    !>  'å'
    !>  (lower:utf8 'å')
  %+  expect-eq
    !>  'å'
    !>  (lower:utf8 'Å')
  %+  expect-eq
    !>  '𐑆𐐰𐑌𐐲𐐼𐐭'
    !>  (lower:utf8 '𐐞𐐰𐑌𐐲𐐼𐐭')
  %+  expect-eq
    !>  'å'
    !>  (lower:utf8 (crip ;;(tape ~[0xc3 0xa5])))
  ==
--
