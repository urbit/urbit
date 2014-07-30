|%
++  down  (list bloc)                                 ::  markdown model
++  bloc                                              ::  block elements
  $%  [%head p=@ud q=(list span) r=(unit tape)]
      [%para p=(list span)]
      [%lise p=down] 
      [%list p=? q=down] 
      [%quot p=down]
      [%horz ~]
      [%code p=wall]
      [%html p=manx]
  ==
::
++  span                                               ::  span elements
  $%  [%text p=tape] 
      [%emph p=term q=(list span)]
      [%stri p=(list span)]
      [%brek ~]
      [%link p=(list span) q=tape r=(unit tape)]
      [%cods p=tape]
  ==
--
