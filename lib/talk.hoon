::
::::  /hoon/talk/lib
  ::  
  ::  This file is in the public domain.
  ::
/?    310 
/-    talk
!:
::::
  ::
[. ^talk]
|_  bol/bowl
++  main                                                ::  main story
  |=  our/ship  ^-  cord
  =+  can=(clan our)
  ?+  can  %porch
    $czar  %court
    $king  %floor
  == 
::
++  said-url                                            ::  app url
  |=  url/purl
  :^  ost.bol  %poke  /said-url
  :+  [our.bol %talk]  %talk-action
  ^-  action
  :+  %phrase
    [[%& our.bol (main our.bol)] ~ ~]
  [%app dap.bol (crip (earn url))]~   :: XX
::
++  said                                                ::  app message
  |=  {our/@p dap/term now/@da eny/@uvJ mes/(list tank)}
  :-  %talk-action
  ^-  action
  :-  %convey
  |-  ^-  (list thought)
  ?~  mes  ~
  :_  $(mes t.mes, eny (sham eny mes))
  ^-  thought
  :+  (shaf %thot eny)
    [[[%& our (main our)] [*envelope %pending]] ~ ~]
  [now *bouquet [%app dap (crip ~(ram re i.mes))]]
::
++  uniq
  |=  eny/@uvJ
  ^-  {@uvH _eny}
  [(shaf %serial eny) (shax eny)]
--
