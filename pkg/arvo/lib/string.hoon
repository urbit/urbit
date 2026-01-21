  ::  /lib/string
::::
::  Implements a set of userspace functions for string manipulation adopting
::  similar names to the Python string standard library.  All inputs are tapes.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
|%
::
::  ++alphabet
::
::  26-letter Roman alphabet, upper case and lower case. 
::
::  Source: 
++  alphabet        ^~  `tape`(weld (gulf 65 90) (gulf 97 122))
::
::  ++alpha-lower
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  alpha-lower     ^~  `tape`(slag 26 alphabet)
::
::  ++alpha-upper
::
::  26-letter Roman alphabet, upper case. 
::
::  Source: 
++  alpha-upper     ^~  `tape`(scag 26 alphabet)
::
::  ++digits
::
::  Ten decimal digits. 
::
::  Source: 
++  digits          ^~  `tape`(gulf 48 57)
::
::  ++alpha-digits
::
::  26-letter Roman alphabet, upper case and lower case, plus ten decimal digits. 
::
::  Source: 
++  alpha-digits    ^~  `tape`(weld alphabet digits)
::
::  ++hexdigits
::
::  Sixteen hexadecimal digits, upper case and lower case. 
::
::  Source: 
++  hexdigits       ^~  `tape`:(weld digits (gulf 65 70) (gulf 97 102))
::
::  ++octdigits
::
::  Eight octal digits. 
::
::  Source: 
++  octdigits       ^~  `tape`(gulf 48 55)
::
::  ++punctuation
::
::  All ASCII punctuation characters. 
::
::  Source: 
++  punctuation     ^~  `tape`:(weld (gulf 32 47) (gulf 58 64) (gulf 91 96) (gulf 123 126))
::
::  ++whitespace
::
::  All ASCII whitespace characters. 
::
::  Source: 
++  whitespace      ^~  `tape`~[' ' '\0a' '\09']
::
::  ++ascii
::
::  All printable ASCII characters (not in ASCII order). 
::
::  Source: 
++  ascii           ^~  `tape`:(weld alphabet digits punctuation whitespace)
::
::    Utility sets
::
::  ++set-alphabet
::
::  26-letter Roman alphabet, upper case and lower case. 
::
::  Source: 
++  set-alphabet      ^~  (~(gas in *(set @tD)) alphabet)
::
::  ++set-alpha-lower
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  set-alpha-lower   ^~  (~(gas in *(set @tD)) alpha-lower)
::
::  ++set-alpha-upper
::
::  26-letter Roman alphabet, upper case. 
::
::  Source: 
++  set-alpha-upper   ^~  (~(gas in *(set @tD)) alpha-upper)
::
::  ++set-digits
::
::  Ten decimal digits. 
::
::  Source: 
++  set-digits        ^~  (~(gas in *(set @tD)) digits)
::
::  ++set-alpha-digits
::
::  26-letter Roman alphabet, upper case and lower case, plus ten decimal digits. 
::
::  Source: 
++  set-alpha-digits  ^~  (~(gas in *(set @tD)) alpha-digits)
::
::  ++set-hexdigits
::
::
::  Sixteen hexadecimal digits, upper case and lower case. 
::
::  Source: 
++  set-hexdigits     ^~  (~(gas in *(set @tD)) hexdigits)
::
::  ++set-octdigits
::
::  Eight octal digits. 
::
::  Source: 
++  set-octdigits     ^~  (~(gas in *(set @tD)) octdigits)
::
::  ++set-punctuation
::
::  Standard ASCII punctuation characters. 
::
::  Source: 
++  set-punctuation   ^~  (~(gas in *(set @tD)) punctuation)
::
::  ++set-whitespace
::
::  Standard ASCII whitespace characters (space, newline, horizontal tab). 
::
::  Source: 
++  set-whitespace    ^~  (~(gas in *(set @tD)) whitespace)
::
::  ++set-ascii
::
::  All printable ASCII characters. 
::
::  Source: 
++  set-ascii         ^~  (~(gas in *(set @tD)) ascii)
::
::  ++tape-in-set
::
::  Helper function to compare characters in tape against a set.
::
::  Source:
++  tape-in-set
  |=  chars=(set @tD)
  |=  =tape
  ^-  ?
  =(~ (~(dif in (~(gas in *(set @tD)) tape)) chars)) 
::
::  ++is-alpha
::
::  Tests whether all characters in a tape are from the ASCII alphabet.
::
::  Source:
++  is-alpha    (tape-in-set set-alphabet)
::
::  ++is-lower
::
::  Tests whether all characters in a tape are from the lower case ASCII alphabet.
::
::  Source:
++  is-lower    (tape-in-set set-alpha-lower)
::
::  ++is-upper
::
::  Tests whether all characters in a tape are from the upper case ASCII alphabet.
::
::  Source: 
++  is-upper    (tape-in-set set-alpha-upper)
::
::  ++is-digit.
::
::  Source: 
++  is-digit    (tape-in-set set-digits)
::
::  ++is-alnum
::
::  Tests whether all characters in a tape are either ASCII alphabetic characters or decimal digits.
::
::  Source: 
++  is-alnum    (tape-in-set set-alpha-digits)
::
::  ++is-hex
::
::  Tests whether all characters in a tape are ASCII hexadecimal digits (upper case and lower case).
::
::  Source: 
++  is-hex      (tape-in-set set-hexdigits)
::
::  ++is-octal
::
::  Tests whether all characters in a tape are ASCII octal digits.
::
::  Source: 
++  is-octal    (tape-in-set set-octdigits)
::
::  ++is-ascii
::
::  Tests whether all characters in a tape are from the printable ASCII character set.
::
::  Source: 
++  is-ascii    (tape-in-set set-ascii)
::
::  ++is-decimal
::
::  Tests whether all characters in a tape are ASCII decimal digits.
::
::  Alias for ++is-digit.
::
::  Source: 
++  is-decimal  is-digit
::
::  ++is-numeric
::
::  Tests whether all characters in a tape are ASCII decimal digits.
::
::  Alias for ++is-digit.
::
::  Source: 
++  is-numeric  is-digit
::
::  ++is-space
::
::  Tests whether all characters in a tape are ASCII whitespace characters.
::
::  Source: 
++  is-space    (tape-in-set set-whitespace)
::
::  ++is-title
::
::  Tests whether a tape is title case.
::
::  Source: 
++  is-title    |=(=tape =((title tape) tape))
::
::  ++is-knot
::
::  Tests whether a tape is a valid @ta label.
::
::  Source: 
++  is-knot     |=(=tape ((sane %ta) (crip tape)))
::
::  ++is-tas
::
::  Tests whether a tape is a valid @tas label.
::
::  Alias for ++is-term.
::
::  Source: 
++  is-tas      is-term
::
::  ++is-ta
::
::  Tests whether a tape is a valid @ta label.
::
::  Source: 
++  is-ta       is-knot
::
::  ++is-term.
::
::  Source: 
++  is-term     |=(=tape ((sane %tas) (crip tape)))
::
::  ++is-uc
::
::  Tests whether a tape is a valid @uc value.
::
::  Source: 
++  is-uc
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%uc -.u.res)
::
::  ++is-ud
::
::  Tests whether a tape is a valid @ud value.
::
::  Source: 
++  is-ud
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%ud -.u.res)
::
::  ++is-ui
::
::  Tests whether a tape is a valid @ui value.
::
::  Source: 
++  is-ui
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%ui -.u.res)
::
::  ++is-uv
::
::  Tests whether a tape is a valid @uv value.
::
::  Source: 
++  is-uv
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%uv -.u.res)
::
::  ++is-uw
::
::  Tests whether a tape is a valid @uw value.
::
::  Source: 
++  is-uw
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%uw -.u.res)
::
::  ++is-ux
::
::  Tests whether a tape is a valid @ux value.
::
::  Source: 
++  is-ux
  |=  =tape
  ^-  ?
  ?~  res=(rust tape bisk:so)
    |
  =(%ux -.u.res)
::
::  ++upper
::
::  Convert the string to all upper case.  Synonymous with ++cuss.
::
::  Alias for ++cuss.
::
::  Source: 
++  upper  cuss
::
::  ++lower
::
::  Convert the string to all lower case.  Synonymous with ++cass.
::
::  Alias for ++cass.
::
::  Source: 
++  lower  cass
::
::  ++capitalize
::
::  Converts the first character of the string to upper case.
::
::  Source: 
++  capitalize  |=(=tape (weld (upper (scag 1 tape)) (slag 1 tape)))
::
::  ++center
::
::  Center the string among whitespace.
::
::  Source: 
++  center
  |=  [=tape wid=@ud]
  ^-  ^tape
  ?.  (gth wid (lent tape))  tape
  =/  lof  (div (sub wid (lent tape)) 2)
  =/  rof  (sub wid (add lof (lent tape)))
  :(weld (reap lof ' ') tape (reap rof ' '))
::
::  ++count
::
::  Count the number of times a value occurs in a given string.
::
::  Source: 
++  count  |=([nedl=tape hstk=tape] (lent (fand nedl hstk)))
::
::  ++find-all
::
::  Produce the index of every match of a given search string in a string.
::
::  Alias for ++fand.
::
::  Source: 
++  find-all  fand
::
::  ++starts-with
::
::  Tests whether a string starts with a given substring.
::
::  Source: 
++  starts-with
  |=  [=tape subs=tape]
  ^-  ?
  ?~  subs  &
  ?~  tape  |
  ?.  =(i.tape i.subs)  |
  $(tape t.tape, subs t.subs)
::
::  ++ends-with
::
::  Tests whether a string ends with a given substring.
::
::  Source: 
++  ends-with    |=([=tape subs=tape] ^-(? =(subs (slag (lent subs) tape))))
::
::  ++link
::
::  Joins a list of tapes by inserting a separator between each element.
::  Tape-based version of ++join.
::
::  Source: 
++  link
  |=  [sep=tape tapes=(list tape)]
  ^-  tape
  ?~  tapes  ~
  =/  res=(list tape)  [i.tapes ~]
  |-
  ?~  t.tapes
    (zing (flop res))
  $(t.tapes t.t.tapes, res [i.t.tapes sep res])
::
::  ++echo
::
::  Repeat a tape as a tape (rather than as a (list tape)).
::
::  Source: 
++  echo  |=([=tape n=@ud] ^-(^tape (zing (reap n tape))))
::
::  ++ljust
::
::  Left-justifies a string within whitespace.
::
::  Source: 
++  ljust
  |=  [=tape wid=@ud]
  ^-  ^tape
  ?.  (gth wid (lent tape))  tape
  =/  rof  (sub wid (lent tape))
  (weld tape (reap rof ' '))
::
::  ++rjust
::
::  Right-justifies a string within whitespace.
::
::  Source: 
++  rjust
  |=  [=tape wid=@ud]
  ^-  ^tape
  ?.  (gth wid (lent tape))  tape
  =/  lof  (sub wid (lent tape))
  (weld (reap lof ' ') tape)
::
::  ++lstrip
::
::  Strips whitespace from the left-hand side of a tape.
::
::  Source: 
++  lstrip
  |=  =tape
  ^-  ^tape
  |-
  ?~  tape  tape
  ?.  (~(has in set-whitespace) i.tape)
    tape
  $(tape t.tape)
::
::  ++rstrip
::
::  Strips whitespace from the right-hand side of a tape.
::
::  Source: 
++  rstrip
  |=  =tape
  ^-  ^tape
  (flop (lstrip (flop tape)))
::
::  ++strip
::
::  Strips whitespace from both ends of a tape.
::
::  Source: 
++  strip  |=(=tape (rstrip (lstrip tape)))
::
::  ++partition
::
::  Separates a tape into a leading element, the matched element, and a trailing element.
::
::  Source: 
++  partition
  |=  [nedl=tape hstk=tape]
  ^-  [l=tape n=tape r=tape]
  =+  [len=(lent nedl) ned=nedl l=*tape]
  |-
  ?~  ned
    =|  i=@
    :+  |-
        ?:  =(i len)
          (flop l)
        ?>  ?=(^ l)
        $(l t.l, i +(i))
      nedl
    hstk
  ?>  ?=(^ hstk)
  ?:  =(i.ned i.hstk)
    $(l [i.hstk l], ned t.ned, hstk t.hstk)
  $(l [i.hstk l], ned nedl, hstk t.hstk)
::
::  ++replace
::
::  Replaces each instance of a given substring in a tape.
::
::  Source: 
++  replace
  |=  [bit=tape bot=tape =tape]
  ^-  ^tape
  ?:  =(~ bit)  tape
  =+  [len=(lent bit) bat=bit but=bot out=*^tape]
  |-
  ?~  bat
    =|  i=@
    |-
    ?:  =(i len)
      |-
      ?~  but
        ^^$(bat bit, but bot)
      $(out [i.but out], but t.but)
    ?>  ?=(^ out)
    $(out t.out, i +(i))
  ?~  tape
    (flop out)
  ?:  =(i.bat i.tape)
    $(out [i.tape out], bat t.bat, tape t.tape)
  $(out [i.tape out], bat bit, tape t.tape)
::
::  ++split
::
::  Splits a tape into tokens at a given separator.  (The separator is not returned as a token in the list.)
::
::  Source: 
++  split
  |=  [sep=tape =tape]
  ^-  (list ^tape)
  =+  rul=(jest (crip sep))
  (scan tape (more rul (star ;~(less rul next))))
::
::  ++rfind
::
::  Locates a value within a string, starting from the right-hand side and searching to the left.
::
::  Source: 
++  rfind
  |=  [seq=tape =tape]
  ^-  (unit @ud)
  ?~  found=(find (flop seq) (flop tape))
    ~
  `(sub (sub (lent tape) u.found) (lent seq))
::
::  ++title
::
::  Converts a string to title case (first letter of each space-separated word is capitalized).
::
::  Source: 
++  title
  |=  =tape
  ^-  ^tape
  =|  last=?
  |-
  ?~  tape
    ~
  :-  ?.  last
        i.tape
      ?.  &((gte i.tape 'a') (lte i.tape 'z'))
        i.tape
      (sub i.tape 32)
  $(tape t.tape, last (~(has in set-whitespace) i.tape))
::
::  ++zfill
::
::  Prefix a string with zeros to make it the given length.
::
::  Source:
++  zfill
  |=  [=tape wid=@ud]
  ^-  ^tape
  ?.  (gth wid (lent tape))  tape
  =/  lof  (sub wid (lent tape))
  (weld (reap lof '0') tape)
::
::  ++grab
::
::  Returns a character at the given index.
::
::  Differs from ++snag in that it returns a tape, not a cord.
::
::  Source: 
++  grab  |=([n=@ud =tape] ^-(^tape ~[(snag n tape)]))
--
