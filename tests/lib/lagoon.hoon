::  /tests/lib/lagoon: regression anchors for lagoon semantics
::
::  Each arm encodes a hand-computed (or ship-verified) oracle for a
::  behavior that has broken before, either in this library or in its
::  vere jets (urbit/vere#1057, urbit/urbit#7388).  They pass identically
::  interpreted and jetted; a jet that disagrees with any of these is
::  wrong.  All rays are %i754; bare +la rounds %z (the bunt of
::  rounding-mode), +lake picks others.
::
/-  *lagoon
/+  *test
/+  *lagoon
^|
|_  ~
::  A 2x3 with distinct entries, its transpose, a 3x3 diagonal matrix,
::  and a rank-1 ray with extrema away from index 0.
++  ray-a    (en-ray:la [~[2 3] 5 %i754 ~] ~[~[.1 .2 .3] ~[.4 .5 .6]])
++  ray-at   (en-ray:la [~[3 2] 5 %i754 ~] ~[~[.1 .4] ~[.2 .5] ~[.3 .6]])
++  ray-b    (en-ray:la [~[3 3] 5 %i754 ~] ~[~[.1 .0 .0] ~[.0 .2 .0] ~[.0 .0 .3]])
++  ray-c    (en-ray:la [~[4] 5 %i754 ~] ~[.3 .1 .4 .2])
::
::  +transpose of a non-square matrix: swapped shape, row-major data.
::  (The jet was row/column-swapped and did not swap the result shape.)
++  test-transpose-nonsquare  ^-  tang
  (expect-eq !>(ray-at) !>((transpose:la ray-a)))
::
::  +diag returns the main diagonal in natural order, as an n x 1 ray.
::  (The Hoon flopped it; the jet crashed unconditionally.)
++  test-diag-natural-order  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[3 1] 5 %i754 ~] ~[~[.1] ~[.2] ~[.3]]))
  !>((diag:la ray-b))
::
::  +trace is the SUM of the diagonal, not the sum of its squares.
::  (The jet computed (dot d d): identity matrices mask this, 1+2+3=6
::  and 1+4+9=14 do not.)
++  test-trace-sums-not-squares  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1 1] 5 %i754 ~] ~[~[.6]]))
  !>((trace:la ray-b))
::
::  +abs preserves element order.  (+el-wise-op flopped the ravel.)
++  test-abs-natural-order  ^-  tang
  %+  expect-eq
    !>(ray-a)
  !>((abs:la (en-ray:la [~[2 3] 5 %i754 ~] ~[~[.-1 .2 .-3] ~[.4 .-5 .6]])))
::
::  Reductions box their result at the INPUT's rank (+scalar-to-ray):
::  rank-1 in, shape ~[1] out.  (The jets hard-coded ~[1 1] or worse.)
++  test-cumsum-rank1  ^-  tang
  (expect-eq !>((en-ray:la [~[1] 5 %i754 ~] ~[.10])) !>((cumsum:la ray-c)))
++  test-min-rank1  ^-  tang
  (expect-eq !>((en-ray:la [~[1] 5 %i754 ~] ~[.1])) !>((min:la ray-c)))
++  test-max-rank1  ^-  tang
  (expect-eq !>((en-ray:la [~[1] 5 %i754 ~] ~[.4])) !>((max:la ray-c)))
++  test-dot-rank1  ^-  tang
  (expect-eq !>((en-ray:la [~[1] 5 %i754 ~] ~[.30])) !>((dot:la ray-c ray-c)))
::
::  +argmin/+argmax return the ravel index of the extremum.
::  (The jet returned len-i-1.)
++  test-argmin-argmax-position  ^-  tang
  ;:  weld
    (expect-eq !>(`@ud`1) !>((argmin:la ray-c)))
    (expect-eq !>(`@ud`2) !>((argmax:la ray-c)))
  ==
::
::  NaN handling in the reductions follows the fold: a NaN in the HEAD
::  position is sticky (all comparisons against it are false); a NaN
::  in the interior is skipped.  (SoftBLAS's gt/ge macros are !le/!lt,
::  which are TRUE against NaN; the jets used them.)
++  test-argmax-nan-head  ^-  tang
  %+  expect-eq  !>(`@ud`0)
  !>((argmax:la (en-ray:la [~[4] 5 %i754 ~] ~[.nan .1 .3 .-5])))
++  test-argmax-nan-interior  ^-  tang
  %+  expect-eq  !>(`@ud`2)
  !>((argmax:la (en-ray:la [~[4] 5 %i754 ~] ~[.1 .nan .3 .-5])))
++  test-max-nan-head-sticky  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1] 5 %i754 ~] ~[.nan]))
  !>((max:la (en-ray:la [~[4] 5 %i754 ~] ~[.nan .1 .3 .-5])))
++  test-min-nan-interior-skipped  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1] 5 %i754 ~] ~[.-1]))
  !>((min:la (en-ray:la [~[4] 5 %i754 ~] ~[.3 .nan .-1 .2])))
::
::  Comparisons against NaN are FALSE in both directions.
++  test-gth-gte-nan-false  ^-  tang
  =/  sev  (en-ray:la [~[2] 5 %i754 ~] ~[.7 .7])
  =/  nan  (en-ray:la [~[2] 5 %i754 ~] ~[.nan .nan])
  =/  zer  (en-ray:la [~[2] 5 %i754 ~] ~[.0 .0])
  ;:  weld
    (expect-eq !>(zer) !>((gth:la sev nan)))
    (expect-eq !>(zer) !>((gte:la sev nan)))
    (expect-eq !>(zer) !>((lth:la sev nan)))
    (expect-eq !>(zer) !>((lte:la sev nan)))
  ==
::
::  +mmul of non-square operands.  a(2x3) x aT(3x2) = [[14 32] [32 77]].
++  test-mmul-nonsquare  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[2 2] 5 %i754 ~] ~[~[.14 .32] ~[.32 .77]]))
  !>((mmul:la ray-a ray-at))
::
::  %mod rounds the quotient to an integer in the DOOR mode (+toi):
::  7 mod 2 has quotient 3.5, so %n and %u give 7-2*4 = -1 while
::  %z and %d give 7-2*3 = 1.  (The jet hardcoded truncation.)
++  test-mod-honors-door-mode  ^-  tang
  =/  sev  (en-ray:la [~[1] 5 %i754 ~] ~[.7])
  =/  two  (en-ray:la [~[1] 5 %i754 ~] ~[.2])
  =/  one  (en-ray:la [~[1] 5 %i754 ~] ~[.1])
  =/  neg  (en-ray:la [~[1] 5 %i754 ~] ~[.-1])
  ;:  weld
    (expect-eq !>(one) !>((mod:la sev two)))
    (expect-eq !>(one) !>((mod:(lake %d) sev two)))
    (expect-eq !>(neg) !>((mod:(lake %n) sev two)))
    (expect-eq !>(neg) !>((mod:(lake %u) sev two)))
  ==
::
::  %mod with a zero divisor crashes: the quotient is non-finite and
::  (need (toi ...)) fails.  (The jet silently returned a value.)
++  test-mod-zero-divisor-crashes  ^-  tang
  %-  expect-fail
  |.  %+  mod:la
        (en-ray:la [~[1] 5 %i754 ~] ~[.7])
      (en-ray:la [~[1] 5 %i754 ~] ~[.0])
::
::  Scalar mod/div divide DIRECTLY; multiplying by a rounded 1/n is
::  wrong even on exact quotients.  (The jet gave 21 mod 7 = 7 and
::  21/7 = 2.9999998.)
++  test-mod-scalar-exact  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1] 5 %i754 ~] ~[.0]))
  !>((mod-scalar:la (en-ray:la [~[1] 5 %i754 ~] ~[.21]) .7))
++  test-div-scalar-exact  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1] 5 %i754 ~] ~[.3]))
  !>((div-scalar:la (en-ray:la [~[1] 5 %i754 ~] ~[.21]) .7))
::
::  +range iterates x+d in the door mode, stopping (exclusive) when the
::  next sum passes b; the count and the values come from the ACTUAL
::  accumulation, not from ceil((b-a)/d).  Under bare-la %z with d=.0.1
::  that is 11 elements ending just under 1.  (The jet's one-shot count
::  said 10 and its values were a+i*d.)  Ship-verified oracle.
++  test-range-accumulates  ^-  tang
  =/  res  (range:la [~[1] 5 %i754 ~] [.0 .1] .0.1)
  ;:  weld
    (expect-eq !>(`(list @)`~[11]) !>(shape.meta.res))
    %+  expect-eq
      !>(0x1.3f7f.fffd.3f66.6664.3f4c.cccb.3f33.3332.3f19.9999.3eff.ffff.3ecc.cccc.3e99.9999.3e4c.cccd.3dcc.cccd.0000.0000)
    !>(data.res)
  ==
::
::  +range at bloq 4 sets the result shape like its 5/6/7 siblings.
::  (It returned the caller's meta shape unchanged.)
++  test-range-bloq4-shape  ^-  tang
  =/  res  (range:la [~[1] 4 %i754 ~] [.~~0 .~~1] .~~0.25)
  ;:  weld
    (expect-eq !>(`(list @)`~[4]) !>(shape.meta.res))
    (expect-eq !>(0x1.3a00.3800.3400.0000) !>(data.res))
  ==
::
::  +linspace with n=1 is the left bound alone.
++  test-linspace-n1  ^-  tang
  %+  expect-eq
    !>((en-ray:la [~[1] 5 %i754 ~] ~[.5]))
  !>((linspace:la [~[1] 5 %i754 ~] [.5 .9] 1))
--
