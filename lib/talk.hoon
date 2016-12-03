::
::::  /hoon/talk/lib
  ::  
  ::  This file is in the public domain.
  ::
/?    310 
/-    talk
::
::::
  ::
[. ^talk]
|_  bol/bowl:^gall
++  main                                                ::  main story
  |=  our/ship  ^-  cord
  =+  can=(clan:title our)
  ?+  can  %porch
    $czar  %court
    $king  %floor
  == 
::
++  said-url                                            ::  app url
  |=  url/purl:^eyre
  :^  ost.bol  %poke  /said-url
  :+  [our.bol %talk]  %talk-command
  ^-  command
  :-  %publish
  :_  ~
  ^-  thought
  :+  (shaf %thot eny.bol)
    [[[%& our.bol (main our.bol)] [*envelope %pending]] ~ ~]
  [now.bol *bouquet [%app dap.bol (crip (en-purl:html url))]]   :: XX
::
++  said                                                ::  app message
  |=  {our/@p dap/term now/@da eny/@uvJ mes/(list tank)}
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
--
