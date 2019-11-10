/-  spider
/+  *threadio
=,  thread=thread:spider
=<  ^-  imp:spider
    |=  [=bowl:spider vase]
    =/  m  (thread ,vase)
    ^-  form:m
    ~&  >  'Entering pH loop'
    ;<  ~  bind:m
      %-  (main-loop ,~)
      :~  handle-run
          handle-stop
          handle-run-all
      ==
    (pure:m *vase)
::
|%
++  handle-run
  |=  ~
  =/  m  (thread ,~)
  ^-  form:m
  ;<  =vase      bind:m  ((handle ,vase) (take-poke %ph-run))
  =/  ph-name    !<(term vase)
  =/  poke-vase  !>([%ph-active (cat 3 %ph- ph-name) *^vase])
  ;<  ~          bind:m  (poke-our %spider %spider-start poke-vase)
  ;<  ~          bind:m  (watch-our /active %spider /imp-result/ph-active)
  ;<  =cage      bind:m  (take-fact /active)
  ~&  >  got-fact=-.cage
  (pure:m ~)
::
++  handle-stop
  |=  ~
  =/  m  (thread ,~)
  ^-  form:m
  ;<  =vase  bind:m  ((handle ,vase) (take-poke %ph-stop))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%ph-active &]))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%aqua-ames &]))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%aqua-behn &]))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%aqua-dill &]))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%aqua-eyre &]))
  ;<  ~      bind:m  (poke-our %spider %spider-stop !>([%aqua-eyre-azimuth &]))
  (pure:m ~)
::
++  handle-run-all
  |=  ~
  =/  m  (thread ,~)
  ^-  form:m
  (pure:m ~)
--
