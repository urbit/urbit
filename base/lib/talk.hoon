::
::::  /hook/core/talk/lib
  ::  
  ::  This file is in the public domain.
  ::
/?    314
/-    *talk
::
::::
  ::
|%
++  main                                                ::  main story
  |=  our=ship  ^-  cord
  =+  can=(clan our)
  ?+  can  %porch
    %czar  %court
    %king  %floor
  == 
::
++  said                                                ::  app message
  |=  [our=@p dap=term now=@da eny=@uvI mes=(list tank)]
  :-  %talk-command
  ^-  command
  :-  %publish
  %-  flop
  =<  acc
  %+  roll  mes
  =<  .(eny ^eny)
  |=  [tan=tank acc=(list thought) eny=@uvI]
  ^-  [acc=(list thought) eny=@uvI]
  =+  (sham eny mes)
  :_  -
  :_  acc
  ^-  thought
  :+  -
    [[[%& our (main our)] [*envelope %pending]] ~ ~]
  [now *bouquet [%app dap (crip ~(ram re tan))]]
--
