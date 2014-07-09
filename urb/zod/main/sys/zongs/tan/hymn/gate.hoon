=>  |%
    ++  mess                                            ::  message
      $%  [%do p=@t]                                    ::  act
          [%exp p=@t q=tank]                            ::  code
          [%say p=@t]                                   ::  speak
      ==
    ++  zong
      $%  [%mess p=@da q=ship r=mess]
      ==
    --
|=  zos=(list zong)
^-  manx
;html
  ;head
    ;title: Zongs!
  ==
  ;body
    ;table
      ;*  %+  turn  zos
          |=  zog=zong
          ;tr
            ;td:"{<p.zog>}"
            ;td:"{<q.zog>}"
            ;td:"{<r.zog>}"
          ==
    ==
  ==
==

