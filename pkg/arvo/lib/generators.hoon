/-  sole
=,  sole
|%
++  produce                                             ::  construct result
  |*  pro=*                                             ::
  [p=*(list tank) q=[%& p=[~ u=pro]]]                   ::
::                                                      ::
++  print                                               ::  add output tank
  |*  [tan=tank res=(sole-result)]                      ::
  ?@  res  res                                          ::
  [p=[i=tan t=p.res] q=q.res]                           ::
::                                                      ::
++  prompt                                              ::  construct prompt
  |*  [pom=sole-prompt mor=(sole-dialog)]               ::
  [p=*(list tank) q=[%| p=pom q=mor]]                   ::
::                                                      ::
++  curl                                                ::  fetch url
  =|  usr=knot                                          ::
  |*  [pul=_purl:eyre fun=$-(httr:eyre *)]              ::
  :-  p=*(list tank)                                    ::
  q=[%| p=`usr q=[pul %get ~ ~] r=fun]                  ::
::                                                      ::
++  no-product                                          ::  empty result
  [p=*(list tank) q=[%& ~]]                             ::
::                                                      ::
++  parse                                               ::  parse by rule
  |*  [sef=rule fun=$-(* *)]                            ::
  |=  txt=sole-input                                    ::
  =+  vex=(sef [0 0] txt)                               ::
  ?:  |(!=((lent txt) q.p.vex) ?=(~ q.vex))             ::
    q.p.vex                                             ::
  (fun p.u.q.vex)                                       ::
--
