::
::              Vane for kernel <= 203.  This file is in the public domain.
::
=>  |%
    ++  shoe                              ::  trace stack layer
      $%  [%yelp p=*]                     ::  raw noun
          [%bean p=*]                     ::  raw noun
          [%lean p=tank]                  ::  direct prettyprint
          [%lose p=term]                  ::  error message
          [%mean p=_|.(*tank)]            ::  prettyprint trigger
          [%pray p=path]                  ::  prayer attempt
          [%spot p=spot]                  ::  source position
      ==
    --
|%
++  swan
  |=  yos=shoe  ^-  tank
  ?-  -.yos
    %yelp  (show %q p.yos)
    %bean  (show %q p.yos)
    %lean  p.yos
    %lose  (show %t p.yos)
    %mean  $:p.yos
    %pray  (show %h p.yos)
    %spot  (show %o p.yos)
  == 
::
++  wash
  |=  [[tab=@ edg=@] tac=tank]  ^-  wall
  (~(win re tac) tab edg) 
--
