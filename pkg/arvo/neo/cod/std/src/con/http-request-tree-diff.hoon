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
  =/  =path
    %+  scan 
      %+  welp  
        %-  trip  (got:bod 'here')
        %-  trip  find-pith
    stap
  =/  =stud:neo  (to-stud find-stud)
  =/  mule-vax=(each vase tang)  
    %-  mule 
      |.
      (to-hoon (got:bod 'vase'))
  ~&  >  mule-vase/mule-vax
  ?:  ?=(%| -.mule-vax)
    [%req-parsing-err p.mule-vax]
  =/  vax=vase  p.mule-vax
  =/  pail-head=stud:neo  (to-stud (got:bod 'head-pail'))
  =/  mule-conf=(each conf:neo tang)  
    %-  mule  
      |.  
      !<  conf:neo  (to-hoon (got:bod 'conf'))
  ~&  >  [%got mule-conf]
  ?:  ?=(%| -.mule-conf)
    [%req-parsing-err p.mule-conf]
  =/  =conf:neo  p.mule-conf
  [head (pave:neo path) stud [(some [pail-head vax]) conf]]
  ::
    %send-poke
  =/  =pith:neo  bod-to-pith
  =/  =stud:neo  (to-stud find-stud)
  =/  mule-vax=(each vase tang)  
    %-  mule 
      |.
      (to-hoon (got:bod 'vase'))
  ~&  >  mule-vase/mule-vax
  ?:  ?=(%| -.mule-vax)
    [%req-parsing-err p.mule-vax]
  =/  vax=vase  p.mule-vax
  [head pith stud vax]
  ::
    %send-tomb
  ~&  >>>  pith/(got:bod 'pith')
  =/  =pith:neo  bod-to-pith
  [head pith]
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