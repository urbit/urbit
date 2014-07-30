|%
++  down  (list bloc)                                   ::  markdown structure
++  bloc                                                ::  block elements
  $%  [%head p=@ud q=(list span) r=(unit tape)]         ::  depth, contents, id
      [%para p=(list span)]                             ::  paragraph
      [%lise p=down]                                    ::  list element
      [%list p=? q=down]                                ::
      [%quot p=down]
      [%horz ~]
      [%code p=wall]                                    ::  <pre>
      [%html p=manx]
  ==
::
++  span                                                ::  span elements
  $%  [%text p=tape] 
      [%emph p=term q=(list span)]
      [%stri p=(list span)]
      [%brek ~]
      [%link p=(list span) q=tape r=(unit tape)]
      [%cods p=tape]                                    ::  <code>
  ==
--
