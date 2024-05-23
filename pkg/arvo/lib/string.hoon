  ::  /lib/string
::::
::  Implements a set of userspace functions for string manipulation adopting
::  similar names to the Python string standard library.  All inputs are tapes.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
|%
::  ++alphabet
::
::  26-letter Roman alphabet, upper case and lower case. 
::
::  Source: 
++  alphabet        ^~  `tape`(weld (gulf 65 90) (gulf 97 122))
::  ++alpha-lower
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  alpha-lower     ^~  `tape`(slag 26 alphabet)
::  ++alpha-upper
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  alpha-upper     ^~  `tape`(scag 26 alphabet)
::  ++digits
::
::
::  Ten decimal digits. 
::
::  Source: 
++  digits          ^~  `tape`(gulf 48 57)
::  ++alpha-digits
::
::  26-letter Roman alphabet, upper case and lower case, plus ten decimal digits. 
::
::  Source: 
++  alpha-digits    ^~  `tape`(weld alphabet digits)
::  ++hexdigits
::
::
::  Sixteen hexadecimal digits, upper case and lower case. 
::
::  Source: 
++  hexdigits       ^~  `tape`:(weld digits (gulf 65 70) (gulf 97 102))
::  ++octdigits
::
::  Eight octal digits. 
::
::  Source: 
++  octdigits       ^~  `tape`(gulf 48 55)
::  ++punctuation     tape:(weld (gulf 32 47) (gulf 58 64) (gulf 91 96) (gulf 123 126))
::
::  All ASCII punctuation characters. 
::
::  Source: 
++  punctuation     ^~  `tape`:(weld (gulf 32 47) (gulf 58 64) (gulf 91 96) (gulf 123 126))
::  ++whitespace      tape:(weld " " ~['a'] ~['9'])
::
::  All ASCII whitespace characters. 
::
::  Source: 
++  whitespace      ^~  `tape`:(weld " " ~['a'] ~['9'])
::  ++ascii
::
::  All printable ASCII characters (not in ASCII order). 
::
::  Source: 
++  ascii           ^~  `tape`:(weld alphabet digits punctuation whitespace)
::
::    Utility sets
::  ++set-alphabet
::
::  26-letter Roman alphabet, upper case and lower case. 
::
::  Source: 
++  set-alphabet      ^~  (~(gas in *(set @tD)) alphabet)
::  ++set-alpha-lower
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  set-alpha-lower   ^~  (~(gas in *(set @tD)) alpha-lower)
::  ++set-alpha-upper
::
::  26-letter Roman alphabet, lower case. 
::
::  Source: 
++  set-alpha-upper   ^~  (~(gas in *(set @tD)) alpha-upper)
::  ++set-digits
::
::  Ten decimal digits. 
::
::  Source: 
++  set-digits        ^~  (~(gas in *(set @tD)) digits)
::  ++set-alpha-digits
::
::  26-letter Roman alphabet, upper case and lower case, plus ten decimal digits. 
::
::  Source: 
++  set-alpha-digits  ^~  (~(gas in *(set @tD)) alpha-digits)
::  ++set-hexdigits
::
::
::  Sixteen hexadecimal digits, upper case and lower case. 
::
::  Source: 
++  set-hexdigits     ^~  (~(gas in *(set @tD)) hexdigits)
::  ++set-octdigits
::
::  Eight octal digits. 
::
::  Source: 
++  set-octdigits     ^~  (~(gas in *(set @tD)) octdigits)
::  ++set-punctuation
::
::  Standard ASCII punctuation characters. 
::
::  Source: 
++  set-punctuation   ^~  (~(gas in *(set @tD)) punctuation)
::  ++set-whitespace
::
::  Standard ASCII whitespace characters (space, newline, horizontal tab). 
::
::  Source: 
++  set-whitespace    ^~  (~(gas in *(set @tD)) whitespace)
::  ++set-ascii
::
::  All printable ASCII characters. 
::
::  Source: 
++  set-ascii         ^~  (~(gas in *(set @tD)) ascii)
::
::  ++is-alpha
::
::  Tests whether all characters in a tape are from the ASCII alphabet.
::
::  Source: 
++  is-alpha    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-alphabet)))
::  ++is-lower
::
::  Tests whether all characters in a tape are from the lower case ASCII alphabet.
::
::  Source: 
++  is-lower    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-alpha-lower)))
::  ++is-upper
::
::  Tests whether all characters in a tape are from the upper case ASCII alphabet.
::
::  Source: 
++  is-upper    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-alpha-upper)))
::  ++is-digit.
::
::  Source: 
++  is-digit    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-digits)))
::  ++is-alnum
::
::  Tests whether all characters in a tape are either ASCII alphabetic characters or decimal digits.
::
::  Source: 
++  is-alnum    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-alpha-digits)))
::  ++is-hex
::
::  Tests whether all characters in a tape are ASCII hexadecimal digits (upper case and lower case).
::
::  Source: 
++  is-hex      |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-hexdigits)))
::  ++is-octal
::
::  Tests whether all characters in a tape are ASCII octal digits.
::
::  Source: 
++  is-octal    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-octdigits)))
::  ++is-ascii
::
::  Tests whether all characters in a tape are from the printable ASCII character set.
::
::  Source: 
++  is-ascii    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-ascii)))
::  ++is-decimal
::
::  Tests whether all characters in a tape are ASCII decimal digits.
::
::  Alias for ++is-digit.
::
::  Source: 
++  is-decimal  is-digit
::  ++is-numeric
::
::  Tests whether all characters in a tape are ASCII decimal digits.
::
::  Alias for ++is-digit.
::
::  Source: 
++  is-numeric  is-digit
::  ++is-space
::
::  Tests whether all characters in a tape are ASCII whitespace characters.
::
::  Source: 
++  is-space    |=(=tape =(~ (~(dif in (~(gas in *(set @tD)) tape)) set-whitespace)))
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
::  ++is-tas
::
::  Tests whether a tape is a valid @tas label.
::
::  Alias for ++is-term.
::
::  Source: 
++  is-tas      is-term
::  ++is-ta
::
::  Tests whether a tape is a valid @ta label.
::
::  Source: 
++  is-ta       is-knot
::  ++is-term.
::
::  Source: 
++  is-term     |=(=tape ((sane %tas) (crip tape)))
::  ++is-uc
::
::  Tests whether a tape is a valid @uc value.
::
::  Source: 
++  is-uc
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%uc +>-<:p))
::  ++is-ud
::
::  Tests whether a tape is a valid @ud value.
::
::  Source: 
++  is-ud
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%ud +>-<:p))
::  ++is-ui
::
::  Tests whether a tape is a valid @ui value.
::
::  Source: 
++  is-ui
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%ui +>-<:p))
::  ++is-uv
::
::  Tests whether a tape is a valid @uv value.
::
::  Source: 
++  is-uv
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%uv +>-<:p))
::  ++is-uw
::
::  Tests whether a tape is a valid @uw value.
::
::  Source: 
++  is-uw
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%uw +>-<:p))
::  ++is-ux
::
::  Tests whether a tape is a valid @ux value.
::
::  Source: 
++  is-ux
  |=  =tape
  ^-  ?
  =/  p  (bisk:so [[1 1] tape])
  &(=(+((lent tape)) +>+<+:p) =(%ux +>-<:p))
::
::  Convert the string to all upper case.  Synonymous with
::   
++  cuss.
::  ++upper
::
::  Converts all characters in a tape to the upper case ASCII alphabet.
::
::  Alias for ++cuss.
::
::  Source: 
++  upper  cuss
::  Convert the string to all lower case.  Synonymous with
::   
++  cass.
::  ++lower
::
::  Converts all characters in a tape to the lower case ASCII alphabet.
::
::  Alias for ++cass.
::
::  Source: 
++  lower  cass
::  Convert the first character to upper case.
::  ++capitalize
::
::  Converts the first character of the string to upper case.
::
::  Source: 
++  capitalize  |=(=tape (weld (upper (scag 1 tape)) (slag 1 tape)))
::  Center the string in spaces.
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
  :(weld `^tape`(zing (reap lof " ")) tape `^tape`(zing (reap rof " ")))
::  Count the number of times a value occurs in the string.
::  ++count
::
::  Count the number of times a value occurs in a given string.
::
::  Source: 
++  count  |=([nedl=tape hstk=tape] (lent (fand nedl hstk)))
::  Produce the index of every match of nedl in hstk as a list of atoms.
::  ++find-all
::
::  Produce the index of every match of a given search string in a string.
::
::  Alias for ++fand.
::
::  Source: 
++  find-all  fand
::  Does the string start with the given substring?
::  ++starts-with
::
::  Tests whether a string starts with a given substring.
::
::  Source: 
++  starts-with  |=([=tape subs=tape] ^-(? =(subs (scag (lent subs) tape))))
::  Does the string end with the given substring?
::  ++ends-with
::
::  Tests whether a string ends with a given substring.
::
::  Source: 
++  ends-with    |=([=tape subs=tape] ^-(? =(subs (slag (lent subs) tape))))
::  Tape-based version of
::   
++  join.
::  ++link
::
::  Joins a list of tapes by inserting a separator between each element.
::
::  Source: 
++  link
  |=  [sep=tape =(list tape)]
  ^-  tape
  =/  res   (snag 0 list)
  =/  list  (slag 1 list)
  |-
  ?~  list  res
  %=  $
    list  t.list
    res   :(weld res sep i.list)
  ==
::  Repeat a tape as a tape (rather than as (list tape)).
::  ++echo
::
::  Repeat a tape as a tape (rather than as a (list tape)).
::
::  Source: 
++  echo  |=([=tape n=@ud] ^-(^tape (zing (reap n tape))))
::  Left-justify text with whitespace
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
  (weld tape `^tape`(zing (reap rof " ")))
::  Right-justify text with whitespace
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
  (weld `^tape`(zing (reap lof " ")) tape)
::  Strip whitespace from the left-hand side.
::  ++lstrip
::
::  Strips whitespace from the left-hand side of a tape.
::
::  Source: 
++  lstrip
  |=  =tape
  ^-  ^tape
  |-
  ?.  (is-space ~[(snag 0 tape)])
    tape
  $(tape (slag 1 tape))
::  Strip whitespace from the right-hand side.
::  ++rstrip
::
::  Strips whitespace from the right-hand side of a tape.
::
::  Source: 
++  rstrip
  |=  =tape
  ^-  ^tape
  |-
  ?.  (is-space ~[(snag 0 tape)])
    tape
  $(tape (slag 1 tape))
::  Strip whitespace on both ends.
::  ++strip
::
::  Strips whitespace from both ends of a tape.
::
::  Source: 
++  strip  |=(=tape (lstrip (rstrip tape)))
::
::  ++partition
::
::  Separates a tape into a leading element, the matched element, and a trailing element.
::
::  Source: 
++  partition
  |=  [nedl=tape hstk=tape]
  ^-  [l=tape n=tape r=tape]
  =/  l   (scag (need (find nedl hstk)) hstk)
  =/  nr  (slag (need (find nedl hstk)) hstk)
  =/  n   (scag (lent nedl) nr)
  =/  r   (slag (lent nedl) nr)
  [l=l n=n r=r]
::
::  ++replace
::
::  Replaces each instance of a given substring in a tape.
::
::  Source: 
++  replace
  |=  [bit=tape bot=tape =tape]
  ^-  ^tape
  |-
  =/  off  (find bit tape)
  ?~  off  tape
  =/  clr  (oust [(need off) (lent bit)] tape)
  $(tape :(weld (scag (need off) clr) bot (slag (need off) clr)))
::
::  ++split
::
::  Splits a tape into tokens at a given separator.  (The separator is not returned as a token in the list.)
::
::  Source: 
++  split
  |=  [sep=tape =tape]
  ^-  (list ^tape)
  =|  res=(list ^tape)
  |-
  ?~  tape  (flop res)
  =/  off  (find sep tape)
  ?~  off  (flop [`^tape`tape `(list ^tape)`res])
  %=  $
    res   [(scag `@ud`(need off) `^tape`tape) res]
    tape  (slag +(`@ud`(need off)) `^tape`tape)
  ==
::
::  ++rfind
::
::  Locates a value within a string, starting from the right-hand side and searching to the left.
::
::  Source: 
++  rfind  |=([seq=tape =tape] ?~((find seq (flop tape)) ~ `(dec (sub (lent tape) (need (find seq (flop tape)))))))
::
::  ++title
::
::  Converts a string to title case (first letter of each space-separated word is capitalized).
::
::  Source: 
++  title  |=(=tape (link " " (turn (split " " (zing (turn tape (cork trip lower)))) capitalize)))
::
::   
++  zfill
  |=  [=tape wid=@ud]
  ^-  ^tape
  ?.  (gth wid (lent tape))  tape
  =/  lof  (sub wid (lent tape))
  (weld `^tape`(zing (reap lof "0")) tape)
::  ++grab
::
::  Returns a character at the given index.
::
::  Differs from ++snag in that it returns a tape, not a cord.
::
::  Source: 
++  grab  |=([n=@ud =tape] ^-(^tape ~[(snag n tape)]))
--
