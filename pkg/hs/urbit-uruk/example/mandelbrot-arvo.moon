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

:: Tape to cord
=/  crip
  ~/  1  crip
  (rap 8)

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

::::  New signed natural (+new:si)
::=/  snew
::  ~/  2  snew
::  |=  (sign val)
::  ?:  sign
::    (rit val)
::  ?:  (zer val)
::    ::  no such thing as negative zero
::    (rit val)
::  (lef val)
::
::::  Natural to Signed (+sun:si)
::=/  snat
::  ~/  1  snat
::  |=  n
::  (snew %.y n)
::
::::  Absolute value (+abs:si)
::=/  sabs
::  ~/  1  sabs
::  |=  s
::  ?-  s
::    neg  neg
::    pos  pos
::  ==
::
::::  Sign bit as boolean (+syn:si)
::=/  ssign
::  ~/  1  ssign
::  |=  s
::  ?-  s
::    neg  %.n
::    pos  %.y
::  ==
::
::::  Addition (+sum:si)
::=/  ssum
::  ~/  2  ssum
::  |=  (a b)
::  ?-  a
::    aneg
::      ?-  b
::        bneg
::          (snew %.n (add aneg bneg))
::        bpos
::          ?:  (lth aneg bpos)
::            (snew %.n (fub bpos aneg))
::          (snew %.y (fub aneg bpos))
::      ==
::    apos
::      ?-  b
::        bneg
::          ?:  (lth apos bneg)
::            (snew %.n (fub bneg apos))
::          (snew %.y (fub apos bneg))
::        bpos
::          (snew %.y (add apos bpos))
::      ==
::  ==
::
:::: Subtraction (+dif:si)
::=/  ssub
::  ~/  2  ssub
::  |=  (a b)
::  %+  ssum  a
::  (snew (not (ssign b)) (sabs b))
::
:::: Multiplication (+pro:si)
::=/  smul
::  ~/  2  smul
::  |=  (a b)
::  (snew (not (xor (ssign a) (ssign b))) (mul (sabs a) (sabs b)))
::
:::: Division (+fra:si)
::=/  sdiv
::  ~/  2  sdiv
::  |=  (a b)
::  (snew (not (xor (ssign a) (ssign b))) (div (sabs a) (sabs b)))
::
:::: signed less than, a < b
::=/  slth
::  ~/  2  slth
::  |=  (a b)
::  ?:  (ssign a)  :: a positive
::    ?:  (ssign b)  :: b positive
::      (lth (sabs a) (sabs b))
::    %.n
::  :: a is negative
::  ?:  (ssign b)
::    %.y
::  :: b is negative
::  ?:  (eql (sabs a) (sabs b))
::    %.n
::  (not (lth (sabs a) (sabs b)))

::
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::

::
:: A port of the mandelbrot generator code itself.
::
:: We emulate floating point math with by using signed integers with a fixed
:: point, so that 10.000 is floating 1.0.
::

=/  left-edge    (int-negative 21.000)
=/  right-edge   (int-positive 15.000)
=/  top-edge     (int-positive 15.000)
=/  bottom-edge  (int-negative 15.000)

=/  max-iterations  254

=/  i0      (int-positive 0)
=/  i2      (int-positive 2)
=/  i10     (int-positive 10)
=/  i1000   (int-positive 1000)
=/  i10000  (int-positive 10.000)
=/  i20000  (int-positive 20.000)
=/  i25000  (int-positive 25.000)
=/  i12500  (int-positive 12.500)
=/  i40000  (int-positive 40.000)

::  Normal natural number to signed fixed-point representation.
=/  to-fp
  ~/  1  to-fp
  |=  x
  (int-mul (int-positive x) i10000)

::  Add fixed-pint numbers.
=/  add-fp
  int-add

::  Subtract fixed-point numbers.
=/  sub-fp
  int-sub

::  Multiply fixed-point numbers.
=/  mul-fp
  ~/  2  mul-fp
  |=  (a b)
  (int-div (int-mul (int-div a i10) b) i1000)

::  Testing crud.
::
:: :*  (to-fp 2)
::     (to-fp 5)
::     :: (int-div (to-fp 2) i10)
::     (int-div (int-mul (int-div (to-fp 2) i10) (to-fp 5)) i1000)
:: ==


::  returns a fraction where when over=0, fp-fraction=0 and when over=under,
::  fp-fraction=--10.000
=/  fraction-fp
  ~/  2  fraction-fp
  |=  (over under)
  (int-div (int-mul over (int-positive 10.000)) under)

::  output color triples
:: =/  base-plasma-colors
::   :*  [12 7 134]  [16 7 135]  [19 6 137]  [21 6 138]  [24 6 139]
::       [27 6 140]  [29 6 141]  [31 5 142]  [33 5 143]  [35 5 144]
::       [37 5 145]  [39 5 146]  [41 5 147]  [43 5 148]  [45 4 148]
::       [47 4 149]  [49 4 150]  [51 4 151]  [52 4 152]  [54 4 152]
::       [56 4 153]  [58 4 154]  [59 3 154]  [61 3 155]  [63 3 156]
::       [64 3 156]  [66 3 157]  [68 3 158]  [69 3 158]  [71 2 159]
::       [73 2 159]  [74 2 160]  [76 2 161]  [78 2 161]  [79 2 162]
::       [81 1 162]  [82 1 163]  [84 1 163]  [86 1 163]  [87 1 164]
::       [89 1 164]  [90 0 165]  [92 0 165]  [94 0 165]  [95 0 166]
::       [97 0 166]  [98 0 166]  [100 0 167]  [101 0 167]  [103 0 167]
::       [104 0 167]  [106 0 167]  [108 0 168]  [109 0 168]  [111 0 168]
::       [112 0 168]  [114 0 168]  [115 0 168]  [117 0 168]  [118 1 168]
::       [120 1 168]  [121 1 168]  [123 2 168]  [124 2 167]  [126 3 167]
::       [127 3 167]  [129 4 167]  [130 4 167]  [132 5 166]  [133 6 166]
::       [134 7 166]  [136 7 165]  [137 8 165]  [139 9 164]  [140 10 164]
::       [142 12 164]  [143 13 163]  [144 14 163]  [146 15 162]  [147 16 161]
::       [149 17 161]  [150 18 160]  [151 19 160]  [153 20 159]  [154 21 158]
::       [155 23 158]  [157 24 157]  [158 25 156]  [159 26 155]  [160 27 155]
::       [162 28 154]  [163 29 153]  [164 30 152]  [165 31 151]  [167 33 151]
::       [168 34 150]  [169 35 149]  [170 36 148]  [172 37 147]  [173 38 146]
::       [174 39 145]  [175 40 144]  [176 42 143]  [177 43 143]  [178 44 142]
::       [180 45 141]  [181 46 140]  [182 47 139]  [183 48 138]  [184 50 137]
::       [185 51 136]  [186 52 135]  [187 53 134]  [188 54 133]  [189 55 132]
::       [190 56 131]  [191 57 130]  [192 59 129]  [193 60 128]  [194 61 128]
::       [195 62 127]  [196 63 126]  [197 64 125]  [198 65 124]  [199 66 123]
::       [200 68 122]  [201 69 121]  [202 70 120]  [203 71 119]  [204 72 118]
::       [205 73 117]  [206 74 117]  [207 75 116]  [208 77 115]  [209 78 114]
::       [209 79 113]  [210 80 112]  [211 81 111]  [212 82 110]  [213 83 109]
::       [214 85 109]  [215 86 108]  [215 87 107]  [216 88 106]  [217 89 105]
::       [218 90 104]  [219 91 103]  [220 93 102]  [220 94 102]  [221 95 101]
::       [222 96 100]  [223 97 99]  [223 98 98]  [224 100 97]  [225 101 96]
::       [226 102 96]  [227 103 95]  [227 104 94]  [228 106 93]  [229 107 92]
::       [229 108 91]  [230 109 90]  [231 110 90]  [232 112 89]  [232 113 88]
::       [233 114 87]  [234 115 86]  [234 116 85]  [235 118 84]  [236 119 84]
::       [236 120 83]  [237 121 82]  [237 123 81]  [238 124 80]  [239 125 79]
::       [239 126 78]  [240 128 77]  [240 129 77]  [241 130 76]  [242 132 75]
::       [242 133 74]  [243 134 73]  [243 135 72]  [244 137 71]  [244 138 71]
::       [245 139 70]  [245 141 69]  [246 142 68]  [246 143 67]  [246 145 66]
::       [247 146 65]  [247 147 65]  [248 149 64]  [248 150 63]  [248 152 62]
::       [249 153 61]  [249 154 60]  [250 156 59]  [250 157 58]  [250 159 58]
::       [250 160 57]  [251 162 56]  [251 163 55]  [251 164 54]  [252 166 53]
::       [252 167 53]  [252 169 52]  [252 170 51]  [252 172 50]  [252 173 49]
::       [253 175 49]  [253 176 48]  [253 178 47]  [253 179 46]  [253 181 45]
::       [253 182 45]  [253 184 44]  [253 185 43]  [253 187 43]  [253 188 42]
::       [253 190 41]  [253 192 41]  [253 193 40]  [253 195 40]  [253 196 39]
::       [253 198 38]  [252 199 38]  [252 201 38]  [252 203 37]  [252 204 37]
::       [252 206 37]  [251 208 36]  [251 209 36]  [251 211 36]  [250 213 36]
::       [250 214 36]  [250 216 36]  [249 217 36]  [249 219 36]  [248 221 36]
::       [248 223 36]  [247 224 36]  [247 226 37]  [246 228 37]  [246 229 37]
::       [245 231 38]  [245 233 38]  [244 234 38]  [243 236 38]  [243 238 38]
::       [242 240 38]  [242 241 38]  [241 243 38]  [240 245 37]  [240 246 35]
::       [239 248 33]
::   ==

::  output color triples (list)
=/  plasma-colors
  %+  lcon  [12 7 134]
  %+  lcon  [16 7 135]
  %+  lcon  [19 6 137]
  %+  lcon  [21 6 138]
  %+  lcon  [24 6 139]
  %+  lcon  [27 6 140]
  %+  lcon  [29 6 141]
  %+  lcon  [31 5 142]
  %+  lcon  [33 5 143]
  %+  lcon  [35 5 144]
  %+  lcon  [37 5 145]
  %+  lcon  [39 5 146]
  %+  lcon  [41 5 147]
  %+  lcon  [43 5 148]
  %+  lcon  [45 4 148]
  %+  lcon  [47 4 149]
  %+  lcon  [49 4 150]
  %+  lcon  [51 4 151]
  %+  lcon  [52 4 152]
  %+  lcon  [54 4 152]
      %+  lcon  [56 4 153]
  %+  lcon  [58 4 154]
  %+  lcon  [59 3 154]
  %+  lcon  [61 3 155]
  %+  lcon  [63 3 156]
      %+  lcon  [64 3 156]
  %+  lcon  [66 3 157]
  %+  lcon  [68 3 158]
  %+  lcon  [69 3 158]
  %+  lcon  [71 2 159]
      %+  lcon  [73 2 159]
  %+  lcon  [74 2 160]
  %+  lcon  [76 2 161]
  %+  lcon  [78 2 161]
  %+  lcon  [79 2 162]
      %+  lcon  [81 1 162]
  %+  lcon  [82 1 163]
  %+  lcon  [84 1 163]
  %+  lcon  [86 1 163]
  %+  lcon  [87 1 164]
      %+  lcon  [89 1 164]
  %+  lcon  [90 0 165]
  %+  lcon  [92 0 165]
  %+  lcon  [94 0 165]
  %+  lcon  [95 0 166]
      %+  lcon  [97 0 166]
  %+  lcon  [98 0 166]
  %+  lcon  [100 0 167]
  %+  lcon  [101 0 167]
  %+  lcon  [103 0 167]
      %+  lcon  [104 0 167]
  %+  lcon  [106 0 167]
  %+  lcon  [108 0 168]
  %+  lcon  [109 0 168]
  %+  lcon  [111 0 168]
      %+  lcon  [112 0 168]
  %+  lcon  [114 0 168]
  %+  lcon  [115 0 168]
  %+  lcon  [117 0 168]
  %+  lcon  [118 1 168]
      %+  lcon  [120 1 168]
  %+  lcon  [121 1 168]
  %+  lcon  [123 2 168]
  %+  lcon  [124 2 167]
  %+  lcon  [126 3 167]
      %+  lcon  [127 3 167]
  %+  lcon  [129 4 167]
  %+  lcon  [130 4 167]
  %+  lcon  [132 5 166]
  %+  lcon  [133 6 166]
  %+  lcon  [134 7 166]
  %+  lcon  [136 7 165]
  %+  lcon  [137 8 165]
  %+  lcon  [139 9 164]
  %+  lcon  [140 10 164]
  %+  lcon  [142 12 164]
  %+  lcon  [143 13 163]
  %+  lcon  [144 14 163]
  %+  lcon  [146 15 162]
  %+  lcon  [147 16 161]
  %+  lcon  [149 17 161]
  %+  lcon  [150 18 160]
  %+  lcon  [151 19 160]
  %+  lcon  [153 20 159]
  %+  lcon  [154 21 158]
  %+  lcon  [155 23 158]
  %+  lcon  [157 24 157]
  %+  lcon  [158 25 156]
  %+  lcon  [159 26 155]
  %+  lcon  [160 27 155]
  %+  lcon  [162 28 154]
  %+  lcon  [163 29 153]
  %+  lcon  [164 30 152]
  %+  lcon  [165 31 151]
  %+  lcon  [167 33 151]
  %+  lcon  [168 34 150]
  %+  lcon  [169 35 149]
  %+  lcon  [170 36 148]
  %+  lcon  [172 37 147]
  %+  lcon  [173 38 146]
  %+  lcon  [174 39 145]
  %+  lcon  [175 40 144]
  %+  lcon  [176 42 143]
  %+  lcon  [177 43 143]
  %+  lcon  [178 44 142]
  %+  lcon  [180 45 141]
  %+  lcon  [181 46 140]
  %+  lcon  [182 47 139]
  %+  lcon  [183 48 138]
  %+  lcon  [184 50 137]
  %+  lcon  [185 51 136]
  %+  lcon  [186 52 135]
  %+  lcon  [187 53 134]
  %+  lcon  [188 54 133]
  %+  lcon  [189 55 132]
  %+  lcon  [190 56 131]
  %+  lcon  [191 57 130]
  %+  lcon  [192 59 129]
  %+  lcon  [193 60 128]
  %+  lcon  [194 61 128]
  %+  lcon  [195 62 127]
  %+  lcon  [196 63 126]
  %+  lcon  [197 64 125]
  %+  lcon  [198 65 124]
  %+  lcon  [199 66 123]
  %+  lcon  [200 68 122]
  %+  lcon  [201 69 121]
  %+  lcon  [202 70 120]
  %+  lcon  [203 71 119]
  %+  lcon  [204 72 118]
  %+  lcon  [205 73 117]
  %+  lcon  [206 74 117]
  %+  lcon  [207 75 116]
  %+  lcon  [208 77 115]
  %+  lcon  [209 78 114]
  %+  lcon  [209 79 113]
  %+  lcon  [210 80 112]
  %+  lcon  [211 81 111]
  %+  lcon  [212 82 110]
  %+  lcon  [213 83 109]
  %+  lcon  [214 85 109]
  %+  lcon  [215 86 108]
  %+  lcon  [215 87 107]
  %+  lcon  [216 88 106]
  %+  lcon  [217 89 105]
  %+  lcon  [218 90 104]
  %+  lcon  [219 91 103]
  %+  lcon  [220 93 102]
  %+  lcon  [220 94 102]
  %+  lcon  [221 95 101]
  %+  lcon  [222 96 100]
  %+  lcon  [223 97 99]
  %+  lcon  [223 98 98]
  %+  lcon  [224 100 97]
  %+  lcon  [225 101 96]
  %+  lcon  [226 102 96]
  %+  lcon  [227 103 95]
  %+  lcon  [227 104 94]
  %+  lcon  [228 106 93]
  %+  lcon  [229 107 92]
  %+  lcon  [229 108 91]
  %+  lcon  [230 109 90]
  %+  lcon  [231 110 90]
  %+  lcon  [232 112 89]
  %+  lcon  [232 113 88]
  %+  lcon  [233 114 87]
  %+  lcon  [234 115 86]
  %+  lcon  [234 116 85]
  %+  lcon  [235 118 84]
  %+  lcon  [236 119 84]
  %+  lcon  [236 120 83]
  %+  lcon  [237 121 82]
  %+  lcon  [237 123 81]
  %+  lcon  [238 124 80]
  %+  lcon  [239 125 79]
  %+  lcon  [239 126 78]
  %+  lcon  [240 128 77]
  %+  lcon  [240 129 77]
  %+  lcon  [241 130 76]
  %+  lcon  [242 132 75]
  %+  lcon  [242 133 74]
  %+  lcon  [243 134 73]
  %+  lcon  [243 135 72]
  %+  lcon  [244 137 71]
  %+  lcon  [244 138 71]
  %+  lcon  [245 139 70]
  %+  lcon  [245 141 69]
  %+  lcon  [246 142 68]
  %+  lcon  [246 143 67]
  %+  lcon  [246 145 66]
  %+  lcon  [247 146 65]
  %+  lcon  [247 147 65]
  %+  lcon  [248 149 64]
  %+  lcon  [248 150 63]
  %+  lcon  [248 152 62]
  %+  lcon  [249 153 61]
  %+  lcon  [249 154 60]
  %+  lcon  [250 156 59]
  %+  lcon  [250 157 58]
  %+  lcon  [250 159 58]
  %+  lcon  [250 160 57]
  %+  lcon  [251 162 56]
  %+  lcon  [251 163 55]
  %+  lcon  [251 164 54]
  %+  lcon  [252 166 53]
  %+  lcon  [252 167 53]
  %+  lcon  [252 169 52]
  %+  lcon  [252 170 51]
  %+  lcon  [252 172 50]
  %+  lcon  [252 173 49]
  %+  lcon  [253 175 49]
  %+  lcon  [253 176 48]
  %+  lcon  [253 178 47]
  %+  lcon  [253 179 46]
  %+  lcon  [253 181 45]
  %+  lcon  [253 182 45]
  %+  lcon  [253 184 44]
  %+  lcon  [253 185 43]
  %+  lcon  [253 187 43]
  %+  lcon  [253 188 42]
  %+  lcon  [253 190 41]
  %+  lcon  [253 192 41]
  %+  lcon  [253 193 40]
  %+  lcon  [253 195 40]
  %+  lcon  [253 196 39]
  %+  lcon  [253 198 38]
  %+  lcon  [252 199 38]
  %+  lcon  [252 201 38]
  %+  lcon  [252 203 37]
  %+  lcon  [252 204 37]
  %+  lcon  [252 206 37]
  %+  lcon  [251 208 36]
  %+  lcon  [251 209 36]
  %+  lcon  [251 211 36]
  %+  lcon  [250 213 36]
  %+  lcon  [250 214 36]
  %+  lcon  [250 216 36]
  %+  lcon  [249 217 36]
  %+  lcon  [249 219 36]
  %+  lcon  [248 221 36]
  %+  lcon  [248 223 36]
  %+  lcon  [247 224 36]
  %+  lcon  [247 226 37]
  %+  lcon  [246 228 37]
  %+  lcon  [246 229 37]
  %+  lcon  [245 231 38]
  %+  lcon  [245 233 38]
  %+  lcon  [244 234 38]
  %+  lcon  [243 236 38]
  %+  lcon  [243 238 38]
  %+  lcon  [242 240 38]
  %+  lcon  [242 241 38]
  %+  lcon  [241 243 38]
  %+  lcon  [240 245 37]
  %+  lcon  [240 246 35]
  %+  lcon  [239 248 33]
  lnil

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
  (snag o plasma-colors)

::
=/  calc-pixel-loop-inlined
  ~/  5  calc-pixel-loop-inlined
  ..  $
  |=  (cr ci i x0 y0)
  ?:  (not (lth i max-iterations))
    [0 0 0]
  ::
  =/  xx  (int-div (int-mul (int-div x0 i10) x0) i1000)
  =/  yy  (int-div (int-mul (int-div y0 i10) y0) i1000)
  ::
  ?:  (not (int-lth (int-add xx yy) i40000))
    ?:  (eql i 255)
      [0 0 0]
    (snag i plasma-colors)
  ::
  %^    ($ cr ci)
      (inc i)
    (add-fp (sub-fp xx yy) cr)
  (add-fp (int-mul i2 (int-div (int-mul (int-div x0 i10) y0) i1000)) ci)

::  calc-color-for
::  mul-fp
::  int-lth
::  smul
::  int-add

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
  ?:  (not (int-lth (add-fp xx yy) i40000))
    (calculate-color-for i)
  ::
  %^    ($ cr ci)
      (inc i)
    (add-fp (sub-fp xx yy) cr)
  (add-fp (int-mul i2 (mul-fp x0 y0)) ci)

=/  calc-pixel
  ~/  4  calc-pixel
  |=  (scr-x scr-y width height)
  ::
  %*  calc-pixel-loop-inlined
    (sub-fp (mul-fp (fraction-fp scr-x width) i25000) i20000)   :: cr
    (sub-fp (mul-fp (fraction-fp scr-y height) i25000) i12500)  :: ci
    0
    i0
    i0
  ==

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

=/  ntot-loop
  ~/  2  ntot-loop
  ..  $
  |=  (num list)
  ?:  (zer num)  list
  ($ (div num 10) (lcon (add (mod num 10) '0') list))

::  renders a natural as a tape, in the spirit of itoa
=/  ntot
  ~/  1  ntot
  |=  num
  ?:  (zer num)
    (lcon '0' lnil)
  (ntot-loop num lnil)

::  a list containing just the space character
=/  space    (lcon ' ' lnil)
=/  newline  (lcon 10 lnil)

::  renders three integers as a space deliminated tape.
=/  nums
  ~/  1  nums
  |=  triple
  =/  a  (car triple)
  =/  b  (car (cdr triple))
  =/  c  (cdr (cdr triple))
  %-  zing
  %+  lcon  (ntot a)
  %+  lcon  space
  (lcon (ntot b) (lcon space (lcon (ntot c) (lcon space lnil))))
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
  %+  weld  (lcon 'P' (lcon '3' (lcon 10 lnil)))
  %+  weld
    (zing (lcon (ntot w) (lcon space (lcon (ntot h) (lcon newline lnil)))))
  %+  weld  (lcon '2' (lcon '5' (lcon '5' (lcon 10 lnil))))
  %-  zing
  %+  turn  (mandelbrot w h)
  build-ppm-line

:: Doing a 20x20 render takes 4m40s.
::(build-ppm 400 400)

:: TODO: The following should run in a reasonable amount of time
::(build-ppm 1000 1000)

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: =/  find-assoc
::   ..  $
::   |=  (eq l k)
::   %+  (cas l)
::     |=  link
::     =/  entry  (car link)
::     =/  e-key  (car entry)
::     ?:  (eql e-key k)
::       (lef (cdr entry))
::     ($ eq (cdr link) k)
::   <u (rit uni)>

::  always returns a list, replacing the key if equivalent
:: =/  add-assoc
::   ~/  5  add-assoc
::   ..  $
::   |=  (lt eq l k v)
::   %+  (cas l)
::     |=  link
::     =/  entry  (car link)
::     =/  e-key  (car entry)
::     ?:  (eq e-key k)
::       (lcon (con k v) (cdr link))
::     ?:  (lt e-key k)
::       (lcon entry ($ lt eq (cdr link) k v))
::     (lcon (con k v) (lcon entry (cdr link)))
::   ::
::   |=  u
::   :: if we made it to the end, this is the sorted position
::   (lcon (con k v) lnil)

=/  is-cons-nat-eq
  ~/  2  is-cons-nat-eq
  |=  (a b)
  ?:  (eql (car a) (car b))
    ?:  (eql (cdr a) (cdr b))
      %.y
    %.n
  %.n

=/  is-cons-nat-lt
  ~/  2  is-cons-nat-lt
  |=  (a b)
  ?:  (lth (car a) (car b))
    %.y
  ?:  (eql (car a) (car b))
    (lth (cdr a) (cdr b))
  %.n

=/  arvo
  ..  $
  |=  (cache w h)
  ?-    (find-assoc is-cons-nat-eq cache (con w h))
      image
    %+  trace  'cached'  |=  ignore
    [image ($ cache)]
  ::
      nothing
    %+  trace  'uncached'  |=  ignore
    =/  image  (build-ppm w h)
    [image ($ (add-assoc is-cons-nat-lt is-cons-nat-eq cache (con w h) image))]
  ==

:: Initial function state: empty cache
(arvo lnil)


