/-  spider
/+  *threadio
=,  thread=thread:spider
=;  core
  ^-  imp:spider
  |=  =bowl:mall
  =/  m  (thread ,~)
  ^-  form:m
  ~&  >  'Entering pH loop'
  %-  (main-loop ,~)
  :~  handle-run:core
      handle-run-all:core
  ==
::
|%
++  handle-run
  |=  ~
  =/  m  (thread ,~)
  ^-  form:m
  ;<  =vase      bind:m  ((handle ,vase) (take-poke %ph-run))
  =/  ph-name    !<(term vase)
  =/  poke-vase  !>([%ph-active (cat 3 %ph- ph-name)])
  ;<  ~          bind:m  (poke-our %spider %spider-start poke-vase)
  ::  ;<  ~          bind:m  (subscribe-our /active %spider /imp/active-ph)
  ::  ;<  =cage      bind:m  (take-subscription-update /active)
  (pure:m ~)
::
++  handle-run-all
  |=  ~
  =/  m  (thread ,~)
  ^-  form:m
  (pure:m ~)
--
