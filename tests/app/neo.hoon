/-  neo
/+  *test-agent
/=  neo-agent  /app/neo
|%
++  scry-handler
  |=  =(pole knot)
  ^-  (unit vase)
  ~
+$  card  card:agent:gall
++  make-grow
  |=  [=pith:neo =pail:neo]
  ^-  card:dirt:neo
  [pith %grow pail *oath:neo]
++  make-cull
  |=  =pith:neo
  ^-  card:dirt:neo
  [pith %cull ~]
::
++  test-dirt-card
  %-  eval-mare
  =/  m  (mare ,~)
  ^-  form:m
  =/  =pith:neo  #/foo
  ;<  caz=(list card)  bind:m
    (do-init %neo neo-agent)
  ;<  ~  bind:m
    (ex-cards caz ~)
  ;<  ~  bind:m
    (set-scry-gate scry-handler)
  ;<  =bowl  bind:m  get-bowl
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow pith atom+!>(1))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar atom+!>(1))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar/baz atom+!>(1))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow pith atom+!>(2))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar atom+!>(2))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar/baz atom+!>(2))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow pith atom+!>(3))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar atom+!>(3))))
  ;<  caz=(list card)  bind:m
    (do-poke %neo-dirt-card !>((make-grow #/foo/bar/baz atom+!>(3))))
  ;<  vax=vase  bind:m  get-save
  %-  (slog (sell vax) ~)
  (pure:m ~)
--
