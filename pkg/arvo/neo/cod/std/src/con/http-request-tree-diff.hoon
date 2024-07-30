/@  http-request
/@  tree-diff
/-  serv=sky-server
:-  [%http-request %$ %tree-diff]
|=  =http-request
^-  tree-diff
=/  pam  (~(uni by pam:(parse-url:serv http-request)) (parse-form-body:serv http-request))
=/  bod  ~(. by pam)
=<
=/  head  (@tas (got:bod 'head'))
?+  head  ~|  [%unknown-head head]  !!
  ::
    %send-make
  =/  =stud:neo  (to-stud find-stud)
  =/  pail-head=stud:neo  (to-stud (got:bod 'head-pail'))
  [head *pith:neo stud [(some [pail-head *vase]) *conf:neo]]
  ::
    %send-poke
  =/  =pith:neo  bod-to-pith
  =/  =stud:neo  (to-stud find-stud)
  [head pith stud *vase]
  ::
    %send-cull
  =/  =pith:neo  bod-to-pith
  ~&  >  send-cull-to/pith
  [head pith ~]
==  
|%
++  bod-to-pith
  ^-  pith:neo
  %-  pave:neo  
    %-  stab  find-pith
::
++  find-pith
  %-  got:bod  'pith'
::
++  find-stud  
  %-  got:bod  'stud'
::
++  to-stud
  |=  =cord
  ^-  stud:neo
  !<  @tas 
    %+  slap  !>(~) 
      %-  ream  cord
--