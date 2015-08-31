section 2eJ, formatting (basic text)
====================================

### `++cass`

To lowercase

    ++  cass                                                ::  lowercase
      |=  vib=tape
      %+  rap  3
      (turn vib |=(a=@ ?.(&((gte a 'A') (lte a 'Z')) a (add 32 a))))
    ::

Produce the case insensitive (all lowercase) cord of a tape.

`vib` is a [tape]().

    ~zod/try=> (cass "john doe")
    7.309.170.810.699.673.450
    ~zod/try=> `cord`(cass "john doe")
    'john doe'
    ~zod/try=> (cass "abc, 123, !@#")
    2.792.832.775.110.938.439.066.079.945.313
    ~zod/try=> `cord`(cass "abc, 123, !@#")
    'abc, 123, !@#' 

------------------------------------------------------------------------

### `++cuss`

To uppercase

    ++  cuss                                                ::  uppercase
      |=  vib=tape
      ^-  @t
      %+  rap  3
      (turn vib |=(a=@ ?.(&((gte a 'a') (lte a 'z')) a (sub a 32))))
    ::

Turn all occurances of lowercase letters in any tape into uppercase
letters, as a cord.

`vib` is a [tape]().

    ~zod/try=> (cuss "john doe")
    'JOHN DOE'
    ~zod/try=> (cuss "abc ABC 123 !@#")
    'ABC ABC 123 !@#'
    ~zod/try=> `@ud`(cuss "abc")
    4.407.873
    ~zod/try=> (cuss "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsQqRrVvWwXxYyZz")
    'AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSQQRRVVWWXXYYZZ'

------------------------------------------------------------------------

### `++crip`

Tape to cord

    ++  crip  |=(a=tape `@t`(rap 3 a))                      ::  tape to cord

Produce cord from a tape.

`a` is a [tape]().

    ~zod/try=> (crip "john doe")
    'john doe'
    ~zod/try=> (crip "abc 123 !@#")
    'abc 123 !@#'
    ~zod/try=> `@ud`(crip "abc")
    6.513.249

------------------------------------------------------------------------

### `++mesc`

Escape special chars

    ++  mesc                                                ::  ctrl code escape
      |=  vib=tape
      ^-  tape
      ?~  vib
        ~
      ?:  =('\\' i.vib)
        ['\\' '\\' $(vib t.vib)]
      ?:  ?|((gth i.vib 126) (lth i.vib 32) =(39 i.vib))
        ['\\' (welp ~(rux at i.vib) '/' $(vib t.vib))]
      [i.vib $(vib t.vib)]
    ::

Escape special characters, used in [`++show`](/doc/hoon/library/2ez#++show)

`vib` is a [tape]().

    /~zod/try=> (mesc "ham lus")
    "ham lus"
    /~zod/try=> (mesc "bas\\hur")
    "bas\\\\hur"
    /~zod/try=> (mesc "as'saß")
    "as\0x27/sa\0xc3/\0x9f/"

------------------------------------------------------------------------

### `++runt`

Prepend `n` times

    ++  runt                                                ::  prepend repeatedly
      |=  [[a=@ b=@] c=tape]
      ^-  tape
      ?:  =(0 a)
        c
      [b $(a (dec a))]
    ::

Add `a` repetitions of character `b` to the head of `c`

`a` and `b` are [atom]()s.

`c` is a [tape]().

    /~zod/try=> (runt [2 '/'] "ham")
    "//ham"
    /~zod/try=> (runt [10 'a'] "")
    "aaaaaaaaaa"

------------------------------------------------------------------------

### `++sand`

Soft-cast by odor

    ++  sand                                                ::  atom sanity
      |=  a=@ta
      |=  b=@  ^-  (unit ,@)
      ?.(((sane a) b) ~ [~ b])
    ::

Soft-cast validity by odor.

`a` is a [`@ta`]().

`b` is an [atom]().

    /~zod/try=> `(unit ,@ta)`((sand %ta) 'sym-som')
    [~ ~.sym-som]
    /~zod/try=> `(unit ,@ta)`((sand %ta) 'err!')
    ~

------------------------------------------------------------------------

### `++sane`

Check odor validity

    ++  sane                                                ::  atom sanity
      |=  a=@ta
      |=  b=@  ^-  ?
      ?.  =(%t (end 3 1 a))
        ~|(%sane-stub !!)
      =+  [inx=0 len=(met 3 b)]
      ?:  =(%tas a)
        |-  ^-  ?
        ?:  =(inx len)  &
        =+  cur=(cut 3 [inx 1] b)
        ?&  ?|  &((gte cur 'a') (lte cur 'z'))
                &(=('-' cur) !=(0 inx) !=(len inx))
                &(&((gte cur '0') (lte cur '9')) !=(0 inx))
            ==
            $(inx +(inx))
        ==
      ?:  =(%ta a)
        |-  ^-  ?
        ?:  =(inx len)  &
        =+  cur=(cut 3 [inx 1] b)
        ?&  ?|  &((gte cur 'a') (lte cur 'z'))
                &((gte cur '0') (lte cur '9'))
                |(=('-' cur) =('~' cur) =('_' cur) =('.' cur))
            ==
            $(inx +(inx))
        ==
      |-  ^-  ?
      ?:  =(0 b)  &
      =+  cur=(end 3 1 b)
      ?:  &((lth cur 32) !=(10 cur))  |
      =+  len=(teff cur)
      ?&  |(=(1 len) =+(i=1 |-(|(=(i len) &((gte (cut 3 [i 1] b) 128) $(i +(i)))))))
          $(b (rsh 3 len b))
      ==
    ::

Check validity by odor. Produces a gate.

`a` is a [`@ta`]().

`b` is an [atom]().

    /~zod/try=> ((sane %tas) %mol)
    %.y
    /~zod/try=> ((sane %tas) 'lam')
    %.y
    /~zod/try=> ((sane %tas) 'more ace')
    %.n

------------------------------------------------------------------------

### `++trim`

Tape split

    ++  trim                                                ::  tape split
      |=  [a=@ b=tape]
      ^-  [p=tape q=tape]
      ?~  b
        [~ ~]
      ?:  =(0 a)
        [~ b]
      =+  c=$(a (dec a), b t.b)
      [[i.b p.c] q.c]
    ::

Split first `a` characters off tape.

`a` is an [atom]().

`b` is a [tape]().

    /~zod/try=> (trim 5 "lasok termun")
    [p="lasok" q=" termun"]
    /~zod/try=> (trim 5 "zam")
    [p="zam" q=""]

------------------------------------------------------------------------

### `++trip`

Cord to tape

    ++  trip                                                ::  cord to tape
      ~/  %trip
      |=  a=@  ^-  tape
      ?:  =(0 (met 3 a))
        ~
      [^-(@ta (end 3 1 a)) $(a (rsh 3 1 a))]
    ::

Produce tape from cord.

`a` is an [atom]().

    /~zod/try=> (trip 'john doe')
    "john doe"
    /~zod/try=> (trip 'abc 123 !@#')
    "abc 123 !@#"
    /~zod/try=> (trip 'abc')
    "abc"

------------------------------------------------------------------------

### `++teff`

UTF8 Length

    ++  teff                                                ::  length utf8
      |=  a=@t  ^-  @
      =+  b=(end 3 1 a)
      ?:  =(0 b)
        ?>(=(0 a) 0)
      ?>  |((gte b 32) =(10 b))
      ?:((lte b 127) 1 ?:((lte b 223) 2 ?:((lte b 239) 3 4)))
    ::

Number of utf8 bytes.

`a` is a [`@t`]().

    /~zod/try=> (teff 'a')
    1
    /~zod/try=> (teff 'ß')
    2

------------------------------------------------------------------------

### `++turf`

UTF8 to UTF32 cord

    ++  turf                                                ::  utf8 to utf32
      |=  a=@t
      ^-  @c
      %+  rap  5
      |-  ^-  (list ,@c)
      =+  b=(teff a)
      ?:  =(0 b)  ~
      :-  %+  can  0
          %+  turn
            ^-  (list ,[p=@ q=@])
            ?+  b  !!
              1  [[0 7] ~]
              2  [[8 6] [0 5] ~]
              3  [[16 6] [8 6] [0 4] ~]
              4  [[24 6] [16 6] [8 6] [0 3] ~]
            ==
          |=([p=@ q=@] [q (cut 0 [p q] a)])
      $(a (rsh 3 b a))
    ::

Convert utf8 ([cord]()) to utf32 codepoints.

`a` is a [`@t`]().

    /~zod/try=> (turf 'my ßam')
    ~-my.~df.am
    /~zod/try=> 'я тут'
    'я тут'
    /~zod/try=> (turf 'я тут')
    ~-~44f..~442.~443.~442.
    /~zod/try=> `@ux`'я тут'
    0x82.d183.d182.d120.8fd1
    /~zod/try=> `@ux`(turf 'я тут')
    0x442.0000.0443.0000.0442.0000.0020.0000.044f

------------------------------------------------------------------------

### `++tuba`

UTF8 to UTF32 tape

    ++  tuba                                                ::  utf8 to utf32 tape
      |=  a=tape
      ^-  (list ,@c)
      (rip 5 (turf (rap 3 a)))                              ::  XX horrible
    ::

Convert tape to list of codepoints.

`a` is a [tape]()

    /~zod/try=> (tuba "я тут")
    ~[~-~44f. ~-. ~-~442. ~-~443. ~-~442.]
    /~zod/try=> (tuba "chars")
    ~[~-c ~-h ~-a ~-r ~-s]

------------------------------------------------------------------------

### `++tufa`

UTF32 to UTF8 tape

    ++  tufa                                                ::  utf32 to utf8 tape
      |=  a=(list ,@c)
      ^-  tape
      ?~  a  ""
      (weld (rip 3 (tuft i.a)) $(a t.a))
    ::

Wrap list of utf32 codepoints to utf8 [tape]().

`a` is a [list]() of [`@c`]().

    /~zod/try=> (tufa ~[~-~44f. ~-. ~-~442. ~-~443. ~-~442.])
    "я тут"
    /~zod/try=> (tufa ((list ,@c) ~[%a %b 0xb1 %c]))
    "ab±c"

------------------------------------------------------------------------

### `++tuft`

UTF32 to UTF8 text

    ++  tuft                                                ::  utf32 to utf8 text
      |=  a=@c
      ^-  @t
      %+  rap  3
      |-  ^-  (list ,@)
      ?:  =(0 a)
        ~
      =+  b=(end 5 1 a)
      =+  c=$(a (rsh 5 1 a))
      ?:  (lth b 0x7f)
        [b c]
      ?:  (lth b 0x7ff)
        :*  (mix 0b1100.0000 (cut 0 [6 5] b))
            (mix 0b1000.0000 (end 0 6 b))
            c
        ==
      ?:  (lth b 0xffff)
        :*  (mix 0b1110.0000 (cut 0 [12 4] b))
            (mix 0b1000.0000 (cut 0 [6 6] b))
            (mix 0b1000.0000 (end 0 6 b))
            c
        ==
      :*  (mix 0b1111.0000 (cut 0 [18 3] b))
          (mix 0b1000.0000 (cut 0 [12 6] b))
          (mix 0b1000.0000 (cut 0 [6 6] b))
          (mix 0b1000.0000 (end 0 6 b))
          c
      ==
    ::

Convert utf32 glyph to
[LSB](http://en.wikipedia.org/wiki/Least_significant_bit) utf8 cord.

`a` is a [`@c`]().

    /~zod/try=> (tuft `@c`%a)
    'a'
    /~zod/try=> (tuft `@c`0xb6)
    '¶'

------------------------------------------------------------------------

### `++wack`

Coin format encode

    ++  wack                                                ::  coin format
      |=  a=@ta
      ^-  @ta
      =+  b=(rip 3 a)
      %+  rap  3
      |-  ^-  tape
      ?~  b
        ~
      ?:  =('~' i.b)  ['~' '~' $(b t.b)]
      ?:  =('_' i.b)  ['~' '-' $(b t.b)]
      [i.b $(b t.b)]
    ::

Escape span `~` as `~~` and `_` as `~-`. Used for printing.

`a` is a [`@ta`]().

    /~zod/try=> (wack '~20_sam~')
    ~.~~20~-sam~~
    /~zod/try=> `@t`(wack '~20_sam~')
    '~~20~-sam~~'
    ~zod/try=> ~(rend co %many ~[`ud/5 `ta/'~20_sam'])
    "._5_~~.~~20~-sam__"
    ~zod/try=> ._5_~~.~~20~-sam__
    [5 ~.~20_sam]

------------------------------------------------------------------------

### `++wick`

Coin format decode

    ++  wick                                                ::  coin format
      |=  a=@
      ^-  @ta
      =+  b=(rip 3 a)
      %+  rap  3
      |-  ^-  tape
      ?~  b
        ~
      ?:  =('~' i.b)
        ?~  t.b  !!
        [?:(=('~' i.t.b) '~' ?>(=('-' i.t.b) '_')) $(b t.t.b)]
      [i.b $(b t.b)]
    ::

Unescape span `~~` as `~` and `~-` as `_`.

`a` is a an [atom]().

    /~zod/try=> `@t`(wick '~-ams~~lop')
    '_ams~lop'
    /~zod/try=> `@t`(wick (wack '~20_sam~'))
    '~20_sam~'

------------------------------------------------------------------------

### `++woad`

Unescape cord

    ++  woad                                                ::  cord format
      |=  a=@ta
      ^-  @t
      %+  rap  3
      |-  ^-  (list ,@)
      ?:  =(0 a)
        ~
      =+  b=(end 3 1 a)
      =+  c=(rsh 3 1 a)
      ?:  =('.' b)
        [' ' $(a c)]
      ?.  =('~' b)
        [b $(a c)]
      =>  .(b (end 3 1 c), c (rsh 3 1 c))
      ?+  b  =-  (weld (rip 3 (tuft p.d)) $(a q.d))
             ^=  d
             =+  d=0
             |-  ^-  [p=@ q=@]
             ?:  =('.' b)
               [d c]
             ?<  =(0 c)
             %=    $
                b  (end 3 1 c)
                c  (rsh 3 1 c)
                d  %+  add  (mul 16 d)
                   %+  sub  b
                   ?:  &((gte b '0') (lte b '9'))  48
                   ?>(&((gte b 'a') (lte b 'z')) 87)
             ==
        %'.'  ['.' $(a c)]
        %'~'  ['~' $(a c)]
      ==
    ::

Unescape cord codepoints.

`a` is a [`@ta`]().

    /~zod/try=> (woad ~.~b6.20.as)
    '¶20 as'

------------------------------------------------------------------------

### `++wood`

Escape cord

    ++  wood                                                ::  cord format
      |=  a=@t
      ^-  @ta
      %+  rap  3
      |-  ^-  (list ,@)
      ?:  =(0 a)
        ~
      =+  b=(teff a)
      =+  c=(turf (end 3 b a))
      =+  d=$(a (rsh 3 b a))
      ?:  ?|  &((gte c 'a') (lte c 'z'))
              &((gte c '0') (lte c '9'))
              =('-' c)
          ==
        [c d]
      ?+  c
        :-  '~'
        =+  e=(met 2 c)
        |-  ^-  tape
        ?:  =(0 c)
          ['.' d]
        =.  e  (dec e)
        =+  f=(rsh 2 e c)
        [(add ?:((lte f 9) 48 87) f) $(c (end 2 e c))]
      ::
        %' '  ['.' d]
        %'.'  ['~' '.' d]
        %'~'  ['~' '~' d]
      ==

Escape cord codepoints.

`a` is a [`@ta`]().

    /~zod/try=> (wood 'my ßam')
    ~.my.~df.am
