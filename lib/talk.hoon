::
::::  /hoon/talk/lib
  ::  
  ::  This file is in the public domain.
  ::
/?    314 
/-    talk
!:
::::
  ::
[talk .]
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
    [[[%& our (main our)] [*envelope %pending]] ~ ~]
  [now *bouquet [%app dap (crip ~(ram re i.mes))]]
++  rend-work-duty
  =+  work-stuff
  |^  |=  due=duty  ^-  tape
      ?-    -.due
          %archive  =+(due " -{(tr-id id)} {(trip -.due)}.")
          %create   =+(due " -{(tr-id id.tax)} {(trip -.due)}: {<title.tax>}")
          %change   =+(due " -{(tr-id id)} {(trip -.due)}: {(tr-meat meat)}")
          %update   
        =+  due
        " -{(tr-id id)} {(trip -.due)} {<version>} by {<her>}: {(tr-meat meat.due)}"
      ==
  ++  tr-id  |=(a=@uv (scow %uv (rsh 2 25 a)))
  ++  tr-term  |=(a=term (rash a (star ;~(pose (cold ' ' hep) next))))
  ++  tr-meat
    |=  feh=flesh  ^-  tape
    ?-  -.feh
      %set-done         =+(feh ?:(don (tr-term -.feh) (tr-term %set-undone)))
      %set-doer         =+(feh "{(tr-term -.feh)} {?~(her "none" <u.her>)}")
      %set-date-due     =+(feh "{(tr-term -.feh)} {?~(wen "none" <u.wen>)}")
      %set-tags         =+(feh "{(tr-term -.feh)} {<tag>}")
      %set-title        =+(feh "{(tr-term -.feh)} {<til>}")
      %set-description  =+(feh "{(tr-term -.feh)} {<des>}")
      %add-comment      =+(feh "{(tr-term -.feh)} {<com>}")
    ==
  --
--
