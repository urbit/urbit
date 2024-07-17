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
  =/  =stud:neo  bod-to-stud
  ~&  >  got-bad-vase/(got:bod 'vase')
  ?~  (rush (got:bod 'vase') vest) 
    [head (pave:neo path) %vase-error [~ ~]]
  =/  vax=vase  bod-to-vase
  ~&  vase/-.vax 
  ?.  |(=(-:!>([*(unit pail:neo) *conf:neo]) -.vax) =(-:!>([~ ~]) -.vax))
    [head (pave:neo path) %vase-not-made [~ ~]]
  =/  tail-made  !<([(unit pail:neo) conf:neo] vax)
  [head (pave:neo path) stud tail-made]
  ::
    %send-poke
  =/  =pith:neo  bod-to-pith
  =/  =stud:neo  bod-to-stud
  ~&  >  got-bad-vase/(got:bod 'vase')
  ?~  (rush (got:bod 'vase') vest) 
    [head pith %vase-error !>(~)]
  =/  vax=vase  bod-to-vase
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
++  bod-to-vase
  ^-  vase
  %+  slap  !>(.)
    %-  ream   (got:bod 'vase')
::
++  bod-to-stud
  ^-  stud:neo
  !<  @tas 
    %+  slap  !>(~) 
      %-  ream  (got:bod 'stud')
::
--