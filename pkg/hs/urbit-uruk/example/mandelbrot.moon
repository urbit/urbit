:: Complete Mandlebrot demo

:: This is a port of enough of hoon.hoon's standard library and the hoon code in
:: mandelbrot.hoon into one file for demo.
::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

::
:: Natural number operations and low level stuff that don't exist as jets in
:: Uruk, but should.
::
:: Note, at this level, we can use ?: instead of +iff and ?- instead of +cas,
:: but in the dash file, we'll need to rebuild these in the lower level syntax
:: when these operations get jetted.
::

::  concatenate natural numbers assuming word size
::
=/  cat
  ~/  3  cat
  |=  (a b c)
  :: think this through
  (add (lsh a c) b)

::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::

::  List operations

::  Left tagged cons cell
=/  cons
  ~/  2  cons
  |=  (head tail)
  (lef [head tail])

::  Right tagged null terminator
=/  null
  (rit uni)

::  Takes a list and a function
=/  turn-
  ~/  2  turn
  ..  $
  |=  (data fun)
  ?-  data
    p  (cons (fun (car p)) ($ (cdr p) fun))
    y  null
  ==

=/  gulf
  ~/  2  gulf
  ..  $
  |=  (a b)
  ?:  (eql a (inc b))
    null
  (cons a ($ (inc a) b))

::  Assemble a list of natural numbers, aligned to the nearest :a bit blocks
=/  rapp
  ~/  2  rapp
  ..  $
  |=  (a b)
  ?-  b
    l  (cat a (car l) ($ a (cdr l)))
    r  0
  ==

:: Tape to cord
=/  crip
  ~/  1  crip
  (rap 8)

::  Takes two lists and returns the two concatendated
=/  weld
  ~/  2  weld
  ..  $
  |=  (first second)
  %^  cas  first
    |=  p
    (cons (car p) ($ (cdr p) second))
  ::
    |=  nil
    second

::  Take a list of lists and welds each one together
=/  zing-
  ~/  1  zing
  ..  $
  |=  rest
  %^  cas  rest
    |=  p
    (weld (car p) ($ (cdr p)))
  ::
    |=  nil
    null

::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::

:: Signed Integer math
::
:: data Signed = Negative Nat  -- lef
::             | Positive Nat  -- rit
::
:: Positive is all natural numbers including 0, (Negative 0) is invalid, so we
:: don't have negative and positive zero.

::  New signed natural (+new:si)
=/  snew
  ~/  2  snew
  |=  (sign val)
  ?:  sign
    (rit val)
  ?:  (eql val 0)
    ::  no such thing as negative zero
    (rit val)
  (lef val)

::  Natural to Signed (+sun:si)
=/  snat
  ~/  1  snat
  |=  n
  (snew %.y n)

::  Absolute value (+abs:si)
=/  sabs
  ~/  1  sabs
  |=  s
  ?-  s
    neg  neg
    pos  pos
  ==

::  Sign bit as boolean (+syn:si)
=/  ssign
  ~/  1  ssign
  |=  s
  ?-  s
    neg  %.n
    pos  %.y
  ==

=/  ssum-aneg
  ~/  2  ssum-aneg
  |=  (aneg b)
  ?-  b
    bneg
      (snew %.n (add aneg bneg))
    bpos
      ?:  (lth aneg bpos)
        (snew %.n (fub bpos aneg))
      (snew %.y (fub aneg bpos))
  ==

=/  ssum-apos
  ~/  2  ssum-apos
  |=  (apos b)
  ?-  b
    bneg
      ?:  (lth apos bneg)
        (snew %.n (fub bneg apos))
      (snew %.y (fub apos bneg))
    bpos
      (snew %.y (add apos bpos))
  ==

::  Addition (+sum:si)
=/  ssum
  ~/  2  ssum
  |=  (a b)
  ?-    a
    aneg  (ssum-aneg aneg b)
    apos  (ssum-apos apos b)
  ==

:: Subtraction (+dif:si)
=/  ssub
  ~/  2  ssub
  |=  (a b)
  %+  ssum  a
  (snew (not (ssign b)) (sabs b))

:: Multiplication (+pro:si)
=/  smul
  ~/  2  smul
  |=  (a b)
  (snew (not (xor (ssign a) (ssign b))) (mul (sabs a) (sabs b)))

:: Division (+fra:si)
=/  sdiv
  ~/  2  sdiv
  |=  (a b)
  (snew (not (xor (ssign a) (ssign b))) (div (sabs a) (sabs b)))

:: signed less than, a < b
=/  slth
  ~/  2  slth
  |=  (a b)
  ?:  (ssign a)  :: a positive
    ?:  (ssign b)  :: b positive
      (lth (sabs a) (sabs b))
    %.n
  :: a is negative
  ?:  (ssign b)
    %.y
  :: b is negative
  ?:  (eql (sabs a) (sabs b))
    %.n
  (not (lth (sabs a) (sabs b)))

::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::

::
:: A port of the mandelbrot generator code itself.
::
:: We emulate floating point math with by using signed integers with a fixed
:: point, so that 10.000 is floating 1.0.
::

=/  left-edge    (snew %.n 21.000)
=/  right-edge   (snew %.y 15.000)
=/  top-edge     (snew %.y 15.000)
=/  bottom-edge  (snew %.n 15.000)

=/  max-iterations  254

::  Normal natural number to signed fixed-point representation.
=/  to-fp
  ~/  1  to-fp
  |=  x
  (smul (snat x) (snat 10.000))

::  Add fixed-pint numbers.
=/  add-fp
  ssum

::  Subtract fixed-point numbers.
=/  sub-fp
  ssub

::  Multiply fixed-point numbers.
=/  mul-fp
  ~/  2  mul-fp
  |=  (a b)
  (sdiv (smul (sdiv a (snat 10)) b) (snat 1.000))

::  Testing crud.
::
:: :*  (to-fp 2)
::     (to-fp 5)
::     :: (sdiv (to-fp 2) (snat 10))
::     (sdiv (smul (sdiv (to-fp 2) (snat 10)) (to-fp 5)) (snat 1.000))
:: ==


::  returns a fraction where when over=0, fp-fraction=0 and when over=under,
::  fp-fraction=--10.000
=/  fraction-fp
  ~/  2  fraction-fp
  |=  (over under)
  (sdiv (smul over (snat 10.000)) under)

::  output color triples
=/  plasma-colors
  :*  [12 7 134]  [16 7 135]  [19 6 137]  [21 6 138]  [24 6 139]
      [27 6 140]  [29 6 141]  [31 5 142]  [33 5 143]  [35 5 144]
      [37 5 145]  [39 5 146]  [41 5 147]  [43 5 148]  [45 4 148]
      [47 4 149]  [49 4 150]  [51 4 151]  [52 4 152]  [54 4 152]
      [56 4 153]  [58 4 154]  [59 3 154]  [61 3 155]  [63 3 156]
      [64 3 156]  [66 3 157]  [68 3 158]  [69 3 158]  [71 2 159]
      [73 2 159]  [74 2 160]  [76 2 161]  [78 2 161]  [79 2 162]
      [81 1 162]  [82 1 163]  [84 1 163]  [86 1 163]  [87 1 164]
      [89 1 164]  [90 0 165]  [92 0 165]  [94 0 165]  [95 0 166]
      [97 0 166]  [98 0 166]  [100 0 167]  [101 0 167]  [103 0 167]
      [104 0 167]  [106 0 167]  [108 0 168]  [109 0 168]  [111 0 168]
      [112 0 168]  [114 0 168]  [115 0 168]  [117 0 168]  [118 1 168]
      [120 1 168]  [121 1 168]  [123 2 168]  [124 2 167]  [126 3 167]
      [127 3 167]  [129 4 167]  [130 4 167]  [132 5 166]  [133 6 166]
      [134 7 166]  [136 7 165]  [137 8 165]  [139 9 164]  [140 10 164]
      [142 12 164]  [143 13 163]  [144 14 163]  [146 15 162]  [147 16 161]
      [149 17 161]  [150 18 160]  [151 19 160]  [153 20 159]  [154 21 158]
      [155 23 158]  [157 24 157]  [158 25 156]  [159 26 155]  [160 27 155]
      [162 28 154]  [163 29 153]  [164 30 152]  [165 31 151]  [167 33 151]
      [168 34 150]  [169 35 149]  [170 36 148]  [172 37 147]  [173 38 146]
      [174 39 145]  [175 40 144]  [176 42 143]  [177 43 143]  [178 44 142]
      [180 45 141]  [181 46 140]  [182 47 139]  [183 48 138]  [184 50 137]
      [185 51 136]  [186 52 135]  [187 53 134]  [188 54 133]  [189 55 132]
      [190 56 131]  [191 57 130]  [192 59 129]  [193 60 128]  [194 61 128]
      [195 62 127]  [196 63 126]  [197 64 125]  [198 65 124]  [199 66 123]
      [200 68 122]  [201 69 121]  [202 70 120]  [203 71 119]  [204 72 118]
      [205 73 117]  [206 74 117]  [207 75 116]  [208 77 115]  [209 78 114]
      [209 79 113]  [210 80 112]  [211 81 111]  [212 82 110]  [213 83 109]
      [214 85 109]  [215 86 108]  [215 87 107]  [216 88 106]  [217 89 105]
      [218 90 104]  [219 91 103]  [220 93 102]  [220 94 102]  [221 95 101]
      [222 96 100]  [223 97 99]  [223 98 98]  [224 100 97]  [225 101 96]
      [226 102 96]  [227 103 95]  [227 104 94]  [228 106 93]  [229 107 92]
      [229 108 91]  [230 109 90]  [231 110 90]  [232 112 89]  [232 113 88]
      [233 114 87]  [234 115 86]  [234 116 85]  [235 118 84]  [236 119 84]
      [236 120 83]  [237 121 82]  [237 123 81]  [238 124 80]  [239 125 79]
      [239 126 78]  [240 128 77]  [240 129 77]  [241 130 76]  [242 132 75]
      [242 133 74]  [243 134 73]  [243 135 72]  [244 137 71]  [244 138 71]
      [245 139 70]  [245 141 69]  [246 142 68]  [246 143 67]  [246 145 66]
      [247 146 65]  [247 147 65]  [248 149 64]  [248 150 63]  [248 152 62]
      [249 153 61]  [249 154 60]  [250 156 59]  [250 157 58]  [250 159 58]
      [250 160 57]  [251 162 56]  [251 163 55]  [251 164 54]  [252 166 53]
      [252 167 53]  [252 169 52]  [252 170 51]  [252 172 50]  [252 173 49]
      [253 175 49]  [253 176 48]  [253 178 47]  [253 179 46]  [253 181 45]
      [253 182 45]  [253 184 44]  [253 185 43]  [253 187 43]  [253 188 42]
      [253 190 41]  [253 192 41]  [253 193 40]  [253 195 40]  [253 196 39]
      [253 198 38]  [252 199 38]  [252 201 38]  [252 203 37]  [252 204 37]
      [252 206 37]  [251 208 36]  [251 209 36]  [251 211 36]  [250 213 36]
      [250 214 36]  [250 216 36]  [249 217 36]  [249 219 36]  [248 221 36]
      [248 223 36]  [247 224 36]  [247 226 37]  [246 228 37]  [246 229 37]
      [245 231 38]  [245 233 38]  [244 234 38]  [243 236 38]  [243 238 38]
      [242 240 38]  [242 241 38]  [241 243 38]  [240 245 37]  [240 246 35]
      [239 248 33]
  ==

::  look through the con list above for index idx.
=/  ur-snag
  ~/  2  ur-snag
  ..  $
  |=  (idx l)
  ?:  (eql idx 0)
    (car l)
  ($ (fec idx) (cdr l))

=/  calculate-color-for
  ~/  1  calculate-color-for
  |=  o
  ?:  (eql o 255)
    [0 0 0]
  (ur-snag o plasma-colors)

::  Compiler should do this automatically (lambda lifting)
=/  calc-pixel-loop
  ~/  5  calc-pixel-loop
  ..  $
  |=  (cr ci i x0 y0)
  ?:  (not (lth i max-iterations))
    (calculate-color-for 255)
  ::
  =/  xx  (mul-fp x0 x0)
  =/  yy  (mul-fp y0 y0)
  ::
  ?:  (not (slth (add-fp xx yy) (snew %.y 40.000)))
    (calculate-color-for i)
  ::
  =/  nu-x0  (add-fp (sub-fp xx yy) cr)
  =/  nu-y0  (add-fp (smul (snat 2) (mul-fp x0 y0)) ci)
  ::
  ($ cr ci (inc i) nu-x0 nu-y0)

=/  calc-pixel
  ~/  4  calc-pixel
  |=  (scr-x scr-y width height)
  ::
  =/  cr   (sub-fp (mul-fp (fraction-fp scr-x width) (snat 25.000)) (snat 20.000))
  =/  ci   (sub-fp (mul-fp (fraction-fp scr-y height) (snat 25.000)) (snat 12.500))
  ::
  (calc-pixel-loop cr ci 0 (snat 0) (snat 0))

=/  mandelbrot
  ~/  2  mandelbrot
  |=  (width height)
  =/  fp-width   (to-fp width)
  =/  fp-height  (to-fp height)
  ::
  =/  width-pixels  (gulf 0 (fub width 1))
  ::
  %+  turn  (gulf 0 (fub height 1))
  |=  y
  %+  trace  ['line' y]
  |=  ignore
  %+  turn  width-pixels
  |=  x
  (calc-pixel (to-fp x) (to-fp y) fp-width fp-height)

::(mandelbrot 5 5)

=/  ntot-loop
  ~/  2  ntot-loop
  ..  $
  |=  (num list)
  ?:  (zer num)  list
  ($ (div num 10) (cons (add (mod num 10) '0') list))

::  renders a natural as a tape, in the spirit of itoa
=/  ntot
  ~/  1  ntot
  |=  num
  ?:  (zer num)
    (cons '0' null)
  (ntot-loop num null)

::  a list containing just the space character
=/  space    (cons ' ' null)
=/  newline  (cons 10 null)

::  renders three integers as a space deliminated tape.
=/  nums
  ~/  1  nums
  |=  triple
  =/  a  (car triple)
  =/  b  (car (cdr triple))
  =/  c  (cdr (cdr triple))
  %-  zing
  %+  cons  (ntot a)
  %+  cons  space
  (cons (ntot b) (cons space (cons (ntot c) (cons space null))))
::
=/  build-ppm-line
  ~/  1  build-ppm-line
  |=  y
  %+  weld
    %-  zing
    %+  turn  y  nums
  newline
::
=/  build-ppm
  ~/  2  build-ppm
  |=  (w h)
  %-  crip
  %+  weld  (cons 'P' (cons '3' (cons 10 null)))
  %+  weld
    (zing (cons (ntot w) (cons space (cons (ntot h) (cons newline null)))))
  %+  weld  (cons '2' (cons '5' (cons '5' (cons 10 null))))
  %-  zing
  %+  turn  (mandelbrot w h)
  build-ppm-line

::  Takes a list and a function
=/  list-id
  ~/  1  list-id
  ..  $
  |=  data
  ?-  data
    p  (cons (I (car p)) ($ (cdr p)))
    y  null
  ==

::  ~/  2  asdfasdf
::  ..  $
::  |=  (x y)
::  ?-  x
  ::  p  (y p)
  ::  p  (y p)
::  ==

::  (build-ppm 10 10)

::(turn (cons 3 (cons 4 null)) <x (div x 2)>)

:: =/  foo
  :: ~/  1  foo
  :: |=  x
  :: ?-  (lef x)
    :: l
      :: ?-  l
        :: ll  [l ll]
        :: lr  [l lr]
      :: ==
    :: r
      :: ?-  r
        :: rl  [r rl]
        :: rr  [r rr]
      :: ==
  :: ==

:: (foo (lef (lef 3)))

:: Doing a 20x20 render takes 4m40s.
(build-ppm 300 300)

:: TODO: The following should run in a reasonable amount of time
::(build-ppm 1000 1000)

::
::  | CAS
::  |   0
::  |   (REF 0)
::  |   (CALN
::  |      (VAL J2_cons_65331)
::  |      (fromListN
::  |         2
::  |         [ CALN (REF 0) (fromListN 1 [ CAR (REG 0) ])
::  |         , REC2 (CDR (REG 0)) (REF 0)
::  |         ]))
::  |   (VAL RU)
