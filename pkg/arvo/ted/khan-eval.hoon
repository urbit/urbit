/-  spider, eval=ted-eval
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  raw=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<(arg=(unit inpt:eval) raw)
?~  arg
  (strand-fail:strand %no-input ~)
=/  com
  ?@  u.arg
    u.arg
  p.u.arg
?~  com
  (strand-fail:strand %no-command ~)
;<  =beak  bind:m  get-beak:strandio
=/  paz=(list path)
  ?@  u.arg
    ~
  q.u.arg
=/  bez=(list beam)
  :~
    [beak /sur/spider/hoon]
    [beak /lib/strandio/hoon]
  ==
=/  =shed:khan
  |-
  ?~  paz
    ;<    vax=vase
        bind:m
      (eval-hoon:strandio (ream com) bez)
    !<(shed:khan vax)
  =/  bem
    %+  fall
      (de-beam i.paz)
    [beak i.paz]
  ;<  has=?  bind:m  (check-for-file:strandio bem)
  ?.  has
    (strand-fail:strand %no-file >bem< ~)
  $(paz t.paz, bez [bem bez])
=/  wir  /test/wire
:: TODO: if we're building the thread against user-provided dependencies, can
:: TODO:    we always use the default beak here?
;<  ~  bind:m  (send-thread:strandio beak shed wir)
;<  [wer=wire sig=sign-arvo]  bind:m  take-sign-arvo:strandio
?>  =(wir wer)
?>  ?=(%khan -.sig)
?>  ?=(%arow +<.sig)
=/  vow  ,.+>.sig
?-  -.vow
  %&  (pure:m q.p.vow)
  %|  (strand-fail:strand %child-failed +.vow)
==
::  tests:
::    success
::      -khan-eval '=/  m  (strand ,vase)  ;<  ~  bind:m  (poke [~zod %hood] %helm-hi !>(\'\'))  (pure:m !>(\'success\'))'
::    failure
::      -khan-eval '=/  m  (strand ,vase)  ;<  vax=vase  bind:m  (eval-hoon [%zpzp ~] ~)  (pure:m !>(\'success\'))'
