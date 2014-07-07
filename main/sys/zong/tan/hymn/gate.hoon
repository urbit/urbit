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
|=  zog=zong
^-  manx
;html
  ;head
    ;title: Zong!
  ==
  ;body
    ;p: {<zog>}
  ==
==

