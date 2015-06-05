::
::::  /hoon/talk/lib
  ::  
  ::  This file is in the public domain.
  ::
/?    314 
/-    *talk
!:
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
  |-  ^-  (list thought)
  ?~  mes  ~
  :_  $(mes t.mes, eny (sham eny mes))
  ^-  thought
  :+  (shaf %thot eny)
    [[[%& our (main our)] [[& ~] %pending]] ~ ~]
  [now ~ [%app dap (crip ~(ram re i.mes))]]
--
