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
  =/  mule-vax=(each vase tang)  
    %-  mule 
      |.
      (to-hoon (got:bod 'vase'))
  ?:  ?=(%| -.mule-vax)  [%req-parsing-err p.mule-vax]
  =/  vax=vase  p.mule-vax
  =/  mule-conf=(each conf:neo tang)  
    %-  mule  
      |.  
      !<  conf:neo  (to-hoon (got:bod 'conf'))
  ?:  ?=(%| -.mule-conf)  [%req-parsing-err p.mule-conf]
  =/  =conf:neo  p.mule-conf
  [head *pith:neo stud [(some [pail-head vax]) conf]]
  ::
    %send-poke
  =/  =pith:neo  bod-to-pith
  =/  =stud:neo  (to-stud find-stud)
  =/  mule-vax=(each vase tang)  
    %-  mule 
      |.
      (to-hoon (got:bod 'vase'))
  ?:  ?=(%| -.mule-vax)  [%req-parsing-err p.mule-vax]
  =/  vax=vase  p.mule-vax
  ~&  >  send-poke-to/pith
  [head pith stud vax]
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
::
++  to-hoon
  |=  hoon=cord 
    ^-  vase
    %+  slap  (slop !>(..zuse) !>(neo))
      %-  ream  hoon
--