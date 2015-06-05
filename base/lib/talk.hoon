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
  :: ~>  %slog.`(sell (slap !>(.) tsgl/[cnzy/'envelope' cnzz//[`1]/thought])) :: XX fails
  :-  %talk-command
  ^-  command
  :-  %publish
  |-  ^-  (list thought)
  ?~  mes  ~
  ((slog 0 mes) ~)
::   :_  $(mes t.mes, eny (sham eny mes))
::   ^-  thought
::   :+  (shaf %thot eny)
::     [[[%& our (main our)] [*envelope %pending]] ~ ~]
::   [now *bouquet [%app dap (crip ~(ram re i.mes))]]
--
