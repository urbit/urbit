~%  %gzip  ..part  ~
|%
++  gzip
  ~/  %gunzip
  |=  file=octs
  ^-  octs
  (unzip file)
::
+$  input  [size=@ud bitstream=@ub]
+$  output  octs
+$  blocks-output  (list output)
+$  values  (map @ud @ud)
+$  codes  (map [size=@ud code=@ub] literal=@ud)
++  swp-bits
  |=  bits=[size=@ud data=@ub]
  ^-  [size=@ud data=@ub]
  =/  original-data-length  (met 0 data.bits)
  =/  original-leading-zeros  (sub size.bits original-data-length)
  =/  data  (swp 0 data.bits)
  =/  new-data-length  (met 0 data)
  =.  data  (lsh [0 original-leading-zeros] data)
  [size.bits data]
::
++  cut-bytes
  |=  [index=@ud length=@ud bytes=octs]
  ^-  octs
  [length (cut 3 [index length] q.bytes)]
::
++  cat-bytes
  |=  [target=octs literals=octs]
  ^-  octs
  ?:  =(q.literals 0)
    [(add p.target p.literals) q.target]
  =/  target-lz  (sub p.target (met 3 q.target))
  =?  q.literals  (gth target-lz 0)
    (lsh [3 target-lz] q.literals)
  [(add p.target p.literals) (cat 3 q.target q.literals)]
::
++  get-fixed-table
  |=  $:  litlen-codes=codes
          literal-start=@ud
          range-size=@ud
          code-start=@ub
          code-size=@ud
      ==
  ^-  codes
  =/  i  0
  |-
  ?:  =(i range-size)
    litlen-codes
  =/  code  (swp-bits [code-size `@ub`(add code-start i)])
  =/  literal  (add literal-start i)
  $(litlen-codes (~(put by litlen-codes) code literal), i +(i))
::
++  get-code
  |=  [=input =codes i=@ud]
  ^-  [code=@ud i=@ud]
  =/  code-size  1
  |-
  ?:  =(code-size 50)
    ~&  'too many length or distance symbols'
    !!
  =/  bits  (cut 0 [i code-size] bitstream.input)
  =/  code  (~(get by codes) [code-size bits])
  ?~  code
    $(code-size +(code-size))
  [u.code (add i code-size)]
::
++  get-cl-table-components
  |=  [hclen=@ud bitstream=@ub i=@ud]
  ^-  [cl-lengths=values cl-lengths-count=values i=@ud]
  =/  code-lengths  `(list @ud)`[16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15 ~]
  =/  cl-lengths  `values`~
  =/  cl-lengths-count  `values`~
  =/  j  0
  |-
  ?:  =(j hclen)
    [cl-lengths cl-lengths-count i]
  ?:  =((cut 0 [i 3] bitstream) 0)
    $(i (add i 3), j +(j))
  =/  length  (cut 0 [i 3] bitstream)
  =/  count  (~(get by cl-lengths-count) length)
  =.  cl-lengths-count  ?~  count
    (~(put by cl-lengths-count) length 1)
  (~(put by cl-lengths-count) length +(u.count))
  %=  $
    cl-lengths  (~(put by cl-lengths) (snag j code-lengths) length)
    i  (add i 3)
    j  +(j)
  ==
::
++  get-table-components
  |=  $:  =input
          =codes
          codes-amount=@ud
          i=@ud
      ==
  ^-  [code-lengths=values cl-count=values i=@ud]
  =/  code-lengths  `values`~
  =/  cl-count  `values`~
  =/  j  0
  |-
  ?:  =(j codes-amount)
    [code-lengths cl-count i]
  ?<  (gth j 285)
  =/  code  (get-code input codes i)
  =.  i  i.code
  ?:  =(code.code 0)
    $(j +(j))
  ?:  =(code.code 16)
    =/  repeat  (add (cut 0 [i 2] bitstream.input) 3)
    =/  previous  (~(got by code-lengths) (sub j 1))
    =/  count  (~(got by cl-count) previous)
    =/  l  0
    =.  code-lengths  |-
      ?:  =(l repeat)
        code-lengths
      %=  $
        code-lengths  (~(put by code-lengths) j previous)
        j  +(j)
        l  +(l)
      ==
    %=  $
      cl-count  (~(put by cl-count) previous (add count repeat))
      i  (add i 2)
      j  (add j repeat)
    ==
  ?:  =(code.code 17)
    =/  repeat  (add (cut 0 [i 3] bitstream.input) 3)
    $(i (add i 3), j (add j repeat))
  ?:  =(code.code 18)
    =/  repeat  (add (cut 0 [i 7] bitstream.input) 11)
    $(i (add i 7), j (add j repeat))
  =.  cl-count
    =/  count  (~(get by cl-count) code.code)
    ?~  count
      (~(put by cl-count) code.code 1)
    (~(put by cl-count) code.code +(u.count))
  %=  $
    code-lengths  (~(put by code-lengths) j code.code)
    j  +(j)
  ==
::
++  get-longest-cl
  |=  cl-count=values
  ^-  @ud
  =/  j  1
  =/  result  0
  |-
  ?:  =(j 16)
    result
  ?~  (~(get by cl-count) j)
    $(j +(j))
  $(result j, j +(j))
::
++  get-smallest-codes
  |=  [cl-count=values longest-cl=@ud]
  ^-  values
  =/  result  `values`~
  =/  j  1
  |-
  ?:  (gth j longest-cl)
    result
  ?~  (~(get by cl-count) j)
    $(j +(j))
  ?~  =(result ~)
    $(result (~(put by result) j 0), j +(j))
  =/  k  1
  =/  previous  |-
    =/  temp  (~(get by result) (sub j k))
    ?~  temp
      $(k +(k))
    [u.temp k]
  =.  k  +.previous
  =/  previous-count  (~(get by cl-count) (sub j k))
  ?~  previous-count
    !!
  =/  next  (lsh [0 k] (add -.previous u.previous-count))
  %=  $
    result  (~(put by result) j next)
    j  +(j)
  ==
::
++  make-code-table
  |=  $:  code-lengths=values
          max-cl=@ud
          smallest-codes=values
          codes-amount=@ud
      ==
  ^-  codes
  =/  cl-count
    ^-  values
    =/  result  `values`~
    =/  j  1
    |-
    ?:  =(j max-cl)
      result
    $(result (~(put by result) j 1), j +(j))
  =/  result  `codes`~
  =/  j  0
  |-
  ?:  =(j codes-amount)
    result
  =/  length  (~(get by code-lengths) j)
  ?~  length
    $(j +(j))
  =/  length-count  (~(got by cl-count) u.length)
  =/  code  `@ub`(~(got by smallest-codes) u.length)
  =?  code  !=(length-count 1)
    (sub (add code length-count) 1)
  =/  code  (swp-bits [u.length code])
  %=  $
    result  (~(put by result) code j)
    cl-count  (~(put by cl-count) u.length +(length-count))
    j  +(j)
  ==
::
++  get-length
  |=  [code=@ud i=@ud bitstream=@ub]
  ^-  [code=@ud i=@ud]
  ?:  (lth code 265)
    [(sub code 254) i]
  ?:  (lth code 269)
    =/  extra-bits  `@ud`(cut 0 [i 1] bitstream)
    =/  multiplier  (sub code 265)
    [(add (add 11 (mul multiplier 2)) extra-bits) +(i)]
  ?:  (lth code 273)
    =/  extra-bits  `@ud`(cut 0 [i 2] bitstream)
    =/  multiplier  (sub code 269)
    [(add (add 19 (mul multiplier 4)) extra-bits) (add i 2)]
  ?:  (lth code 277)
    =/  extra-bits  `@ud`(cut 0 [i 3] bitstream)
    =/  multiplier  (sub code 273)
    [(add (add 35 (mul multiplier 8)) extra-bits) (add i 3)]
  ?:  (lth code 281)
    =/  extra-bits  `@ud`(cut 0 [i 4] bitstream)
    =/  multiplier  (sub code 277)
    [(add (add 67 (mul multiplier 16)) extra-bits) (add i 4)]
  ?:  (lth code 285)
    =/  extra-bits  `@ud`(cut 0 [i 5] bitstream)
    =/  multiplier  (sub code 281)
    [(add (add 131 (mul multiplier 32)) extra-bits) (add i 5)]
  ?:  =(code 285)
    [258 i]
  !!
::
++  get-distance
  |=  [code=@ud bitstream=@ub i=@ud]
  ^-  [code=@ud i=@ud]
  ?:  (lth code 4)
    [+(code) i]
  =/  divided  (dvr code 2)
  =/  extra-bits-amount  (sub p.divided 1)
  =/  j  1
  =/  distance  ?:  =(code 4)
    5
  =/  result  5
  |-
  ^-  @ud
  ?:  =(j extra-bits-amount)
    result
  %=  $
    result  (add result (mul (pow 2 j) 2))
    j  +(j)
  ==
  =/  extra-bits  `@ud`(cut 0 [i extra-bits-amount] bitstream)
  =.  i  (add i extra-bits-amount)
  ?:  =(q.divided 0)
    [(add distance extra-bits) i]
  [(add (add distance (pow 2 extra-bits-amount)) extra-bits) i]
::
++  cut-backreference
  |=  $:  block-output=output
          =blocks-output
          length=@ud
          distance=@ud
      ==
  ^-  octs
  ::  CHECK IF DISTANCE REACHES OUTPUT FROM PREVIOUS BLOCKS
  =?  block-output  (gth distance p.block-output)
    =.  blocks-output  [block-output blocks-output]
    =/  canned-blocks  `@ux`(can 3 (flop blocks-output))
    =/  leading-zeros  (sub p.block-output (met 3 q.block-output))
    [(add leading-zeros (met 3 canned-blocks)) canned-blocks]
  ?:  (gth distance p.block-output)
    ~&  'invalid distance too far back'
    !!
  =/  index  (sub p.block-output distance)
  ::  REGULAR OR SELF-REFERRING BACKREFERENCE
  ?:  (gth distance length)
    (cut-bytes index length block-output)
  =/  temp-length  length
  =/  temp-index  index
  |-
  ?:  (lte temp-length distance)
    =/  backreference  (cut-bytes temp-index temp-length block-output)
    =.  block-output  (cat-bytes block-output backreference)
    (cut-bytes index length block-output)
  =/  backreference  (cut-bytes temp-index distance block-output)
  %=  $
    block-output  (cat-bytes block-output backreference)
    temp-length  (sub temp-length distance)
    temp-index  (add temp-index distance)
  ==
::
++  parse-block
  |=  $:  =input
          i=@ud
          =blocks-output
          litlen-codes=codes
          distance-codes=codes
      ==
  ^-  [=output i=@ud]
  =|  block-output=output
  |-
  =/  litlen-code  (get-code input litlen-codes i)
  =.  i  i.litlen-code
  ?:  (gth i size.input)
    ~&  'invalid code -- missing end-of-block'
    !!
  ::  END OF BLOCK
  ?:  =(code.litlen-code 256)
    [block-output i]
  ::  LITERAL OR BACKREFERENCE
  ?:  (lth code.litlen-code 256)
    $(block-output (cat-bytes block-output [1 `@ux`code.litlen-code]))
  =/  length  (get-length code.litlen-code i bitstream.input)
  =.  i  i.length
  ::  BLOCK TYPE 1 OR 2 DISTANCE CODE
  =/  distance-code  ?~  distance-codes
    `[code=@ud i=@ud]`[(swp 0 (cut 0 [i 5] bitstream.input)) (add i 5)]
  (get-code input distance-codes i)
  =.  i  i.distance-code
  =/  distance
    %-  get-distance
    :+  code.distance-code
      bitstream.input
    i
  =.  i  i.distance
  =/  backreference
    %-  cut-backreference
    :^    block-output
        blocks-output
      code.length
    code.distance
  $(block-output (cat-bytes block-output backreference))
::
++  decompress-block-type-0
  |=  [=input i=@ud]
  ^-  [=output i=@ud]
  =/  product  (dvr i 8)
  =/  bit-padding  ?:  =((sub 8 q.product) 8)
    0
  (sub 8 q.product)
  =.  i  (add i bit-padding)
  =/  len  `@ud`(cut 0 [i 16] bitstream.input)
  ?:  =(len 0)
    ~&  'invalid stored block lengths'
    !!
  =.  i  (add i 32)
  =/  cut-data  `@ux`(cut 0 [i (mul len 8)] bitstream.input)
  =.  i  (add i (mul len 8))
  [[len cut-data] i]
::
++  decompress-block-type-1
  |=  [=input =blocks-output i=@ud]
  ^-  [=output i=@ud]
  =/  litlen-codes  `codes`~
  =.  litlen-codes  (get-fixed-table litlen-codes 0 144 0b11.0000 8)
  =.  litlen-codes  (get-fixed-table litlen-codes 144 112 0b1.1001.0000 9)
  =.  litlen-codes  (get-fixed-table litlen-codes 256 24 0b0 7)
  =.  litlen-codes  (get-fixed-table litlen-codes 280 8 0b1100.0000 8)
  (parse-block input i blocks-output litlen-codes ~)
::
++  decompress-block-type-2
  |=  [=input =blocks-output i=@ud]
  ^-  [=output i=@ud]
  ::  HEADER
  =/  hlit  (cut 0 [i 5] bitstream.input)
  =.  i  (add i 5)
  =/  hdist  (add 1 (cut 0 [i 5] bitstream.input))
  =.  i  (add i 5)
  =/  hclen  (add 4 (cut 0 [i 4] bitstream.input))
  =.  i  (add i 4)
  ::  GET CODE LENGTH CODES
  =/  cl-components
    %-  get-cl-table-components
    :+  hclen
      bitstream.input
    i
  =/  longest-cl-cl  (get-longest-cl cl-lengths-count.cl-components)
  =/  smallest-codes
    %-  get-smallest-codes
    :-  cl-lengths-count.cl-components
      longest-cl-cl
  =/  cl-codes
    %-  make-code-table
    :^    cl-lengths.cl-components
        8
      smallest-codes
    19
  =.  i  i.cl-components
  ::  GET LITERAL/LENGTH CODES
  =/  litlen-components
    %-  get-table-components
    :^    input
        cl-codes
      (add 257 hlit)
    i
  =/  longest-litlen-cl  (get-longest-cl cl-count.litlen-components)
  =/  litlen-smallest-codes
    %-  get-smallest-codes
    :-  cl-count.litlen-components
      longest-litlen-cl
  =/  litlen-codes
    %-  make-code-table
    :^    code-lengths.litlen-components
        16
      litlen-smallest-codes
    (add 257 hlit)
  =.  i  i.litlen-components
  ::  GET DISTANCE CODES
  =/  distance-components
    %-  get-table-components
    :^    input
        cl-codes
      hdist
    i
  =/  longest-distance-cl  (get-longest-cl cl-count.distance-components)
  =/  distance-smallest-codes
    %-  get-smallest-codes
    :-  cl-count.distance-components
      longest-distance-cl
  =/  distance-codes
    %-  make-code-table
    :^    code-lengths.distance-components
        16
      distance-smallest-codes
    hdist
  =.  i  i.distance-components
  ::  PARSE COMPRESSED DATA
  (parse-block input i blocks-output litlen-codes distance-codes)
::
++  unzip
  |=  file=octs
  ^-  octs


  ::  ----------------------------
  ::  PARSE HEADER
  ::  ----------------------------
  =/  i  0

  ::  ID1
  =/  id1  (cut 3 [i 1] q.file)
  ?>  =(31 id1)
  =.  i  +(i)

  ::  ID2
  =/  id2  (cut 3 [i 1] q.file)
  ?>  =(139 id2)
  =.  i  +(i)

  ::  CM
  =/  cm  (cut 3 [i 1] q.file)
  ?>  =(8 cm)
  =.  i  +(i)

  ::  FLG
  =/  flg  (cut 3 [i 1] q.file)
  =.  i  +(i)

  ::  MTIME
  =/  mtime  (cut 3 [i 4] q.file)
  =.  i  (add i 4)

  ::  XFL
  =/  xfl  (cut 3 [i 1] q.file)
  =.  i  +(i)

  ::  OS
  =/  os  (cut 3 [i 1] q.file)
  =.  i  +(i)

  ::  FLG FLAGS
  =/  fhcrc  (cut 0 [1 1] flg)
  =/  fextra  (cut 0 [2 1] flg)
  =/  fname  (cut 0 [3 1] flg)
  =/  fcomment  (cut 0 [4 1] flg)

  ::  IF FEXTRA SET
  =/  xlen=(unit @ud)
    ?:  =(fextra 0)
      ~
    [~ (cut 3 [i 2] q.file)]
  =?  i  ?=(^ xlen)  (add i (add 2 u.xlen))

  ::  IF FNAME SET
  =/  file-name=(unit @t)
    ?:  =(fname 0)
      ~
    =/  j  0
    :-  ~
    |-
    ?:  =((cut 3 [(add i j) 1] q.file) 0)
      (cut 3 [i +(j)] q.file)
    $(j +(j))
  =?  i  ?=(^ file-name)  (add i (add (met 3 u.file-name) 1))

  ::  IF FCOMMENT SET
  =/  comment=(unit @t)
    ?:  =(fcomment 0)
      ~
    =/  j  0
    :-  ~
    |-
    ?:  =((cut 3 [(add i j) 1] q.file) 0)
      (cut 3 [i +(j)] q.file)
    $(j +(j))
  =?  i  ?=(^ comment)  (add i (add (met 3 u.comment) 1))

  ::  IF FHCRC SET
  =/  crc16=(unit @ud)
    ?:  =(fhcrc 0)
      ~
    [~ (cut 3 [i 2] q.file)]
  ?:  ?&  ?=(^ crc16)
          !=(u.crc16 (cut 3 [0 2] (crc32:crc (cut-bytes 0 i file))))
      ==
    ~&  'header crc mismatch'
    !!
  =?  i  ?=(^ crc16)  (add i 2)


  ::  ----------------------------
  :: DECOMPRESS COMPRESSED BLOCKS
  ::  ----------------------------

  =.  i  (mul i 8)
  =/  input=[size=@ud bitstream=@ub]  [(mul p.file 8) q.file]

  =|  =blocks-output
  =/  decompressed-data  |-
    ^-  [i=@ud result=octs]
    =/  is-last-block  =((cut 0 [i 1] bitstream.input) 1)
    =.  i  +(i)
    =/  block-type  (cut 0 [i 2] bitstream.input)
    =.  i  (add i 2)
    =/  decompressed-block
      ?:  =(block-type 0)
        (decompress-block-type-0 input i)
      ?:  =(block-type 1)
        (decompress-block-type-1 input blocks-output i)
      ?:  =(block-type 2)
        (decompress-block-type-2 input blocks-output i)
      ~&  'invalid block type'
      !!
    ?:  is-last-block
      =.  blocks-output  [output.decompressed-block blocks-output]
      =/  result  `@`(can 3 (flop blocks-output))
      ?:  =(result 0)
        [i.decompressed-block output.decompressed-block]
      [i.decompressed-block [(met 3 result) result]]
    %=  $
      blocks-output  [output.decompressed-block blocks-output]
      i  i.decompressed-block
    ==


  ::  ----------------------------
  ::  PARSE FOOTER
  ::  ----------------------------

  =.  i  (div i.decompressed-data 8)
  =/  remainder  (dvr i.decompressed-data 8)
  =?  i  (gth q.remainder 0)  +(i)

  =/  footer-length  (sub p.file i)
  ?:  (lth footer-length 8)
    ~&  'incorrect data check'
    !!

  ::  CRC32
  =/  crc32  (cut-bytes i 4 file)
  =.  i  (add i 4)

  ::  ISIZE
  =/  isize  (cut-bytes i 4 file)

  ?:  =(q.crc32 (crc32:crc result.decompressed-data))
    result.decompressed-data
  ~&  'incorrect data check'
  !!
--
