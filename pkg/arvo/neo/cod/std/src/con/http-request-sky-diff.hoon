/@  sky-diff
/@  http-request
/-  serv=sky-server
:-  [%http-request %$ %sky-diff]
|=  =http-request
=/  pam  (~(uni by pam:(parse-url:serv http-request)) (parse-form-body:serv http-request))
=/  bod  ~(. by pam)
=/  head  (@tas (got:bod 'head'))
^-  sky-diff
?+    head  ~|(bad-head/head !!)
  %new-hawk
    [head (slav %da (got:bod 'now'))]
  %close
    [head (slav %ud (got:bod 'slot'))]
  %maximize
    [head (slav %ud (got:bod 'slot'))]
  %slide-up
    [head (slav %ud (got:bod 'slot'))]
  %slide-down
    [head (slav %ud (got:bod 'slot'))]
  %minimize
    [head (slav %ud (got:bod 'slot'))]
==
