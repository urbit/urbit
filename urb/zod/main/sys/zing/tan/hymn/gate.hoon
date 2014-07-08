=>  |%
    ++  mess                                            ::  message
      $%  [%do p=@t]                                    ::  act
          [%exp p=@t q=tank]                            ::  code
          [%say p=@t]                                   ::  speak
      ==
    ++  zing
      $%  [%backlog p=path q=?(%da %dr %ud) r=@]
          [%hola p=path]
          [%mess p=path q=mess]
      ==
    --
|=  zig=zing
^-  manx
;html
  ;head
    ;title: Zing!
  ==
  ;body
    ;p: {<zig>}
  ==
==

