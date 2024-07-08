/@  http-request
/@  tree-diff
/-  serv=sky-server
:-  [%http-request %$ %tree-diff]
|=  =http-request
^-  tree-diff
=/  pam  (~(uni by pam:(parse-url:serv http-request)) (parse-form-body:serv http-request))
=/  bod  ~(. by pam)
=/  head  (@tas (got:bod 'head'))
?+  head  ~|  [%unknown-head head]  !!
    %send-tomb
  ~&  >>>  pith/(got:bod 'pith')
  =/  path  (stab (got:bod 'pith'))
  [head (pave:neo path)]
    %send-poke
  [head *pith:neo *@tas *]
==