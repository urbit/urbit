/@  htmx
/-  serv=server
=>
  |%
  ++  main
    ^-  curb:neo
    [%or rol/[%ui-main pro/%htmx] pro/%htmx ~]
    :: rol/[%ui-main pro/%htmx]
  ++  kids-curb
    ^-  curb:neo
    any/~
  :: rol/[%ui-list pro/%htmx]
  ++  manx-to-octs
    |=  man=manx
    (as-octt:mimes:html (en-xml:html man))
  ::
  ++  render
    |=  [main=manx kids=marl]
    ;div
      ;+  main
      ;div
        ;*  kids
      ==
    ==
  --
^-  kook:neo
|%
++  state  pro/%eyre-task
++  poke   *(set stud:neo)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  ~
++  deps  
  %-  ~(gas by *band:neo)
  :~  :-  %src
      ^-  fief:neo
      :-  req=|
      ^-  quay:neo
      :-  [main ~]
      ^-  (unit port:neo)
      :+  ~  %y
      %-  ~(gas by *lads:neo)
      :~  :-  &
          `lash:neo`[kids-curb ~]
      ==
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `pail
  ++  init
    |=  pal=(unit pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    :_  [stud vase]
    =/  =pith:neo  #/[p/our.bowl]/$/eyre
    =;  =manx
      =/  head=sign:eyre:neo  [eyre-id %head [200 [['content-type' 'text/html'] ~]]]
      =/  data=sign:eyre:neo  [eyre-id %data `(manx-to-octs manx)]
      =/  done=sign:eyre:neo  [eyre-id %done ~]
      :~  [pith %poke eyre-sign/!>(head)]
          [pith %poke eyre-sign/!>(data)]
          [pith %poke eyre-sign/!>(done)]
          [here.bowl %cull ~]
      ==
    ?~  src=(~(get by deps.bowl) %src)
      ;div: 404
    =/  root=idea:neo  (~(got of:neo q.u.src) /)
    ?>  =(%htmx p.pail.root)
    =/  bol  *bowl:neo
    =.  here.bol  p.u.src
    (!<(htmx q.pail.root) bol)
  --
--

