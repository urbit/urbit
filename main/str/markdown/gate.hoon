|%
++  down  (list bloc)                                 ::  markdown model
++  bloc                                              ::  block elements
  $%  [%head p=@ud q=(list spel) r=(unit tape)]
      [%para p=(list spel)]
      [%lise p=down] 
      [%list p=? q=down] 
      [%quot p=down]
      [%horz ~]
      [%code p=wall]
      [%html p=manx]
  ==
::
++  spel                                               ::  span elements
  $%  [%text p=tape] 
      [%emph p=term q=(list spel)]
      [%stri p=(list spel)]
      [%brek ~]
      [%link p=(list spel) q=tape r=(unit tape)]
      [%cods p=tape]
  ==
--
