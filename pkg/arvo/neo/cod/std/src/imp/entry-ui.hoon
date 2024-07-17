/@  renderer
/-  feather-icons
^-  kook:neo
=<
|%
++  state  pro/%renderer
++  poke   ~
++  kids  *kids:neo
++  deps
  %-  ~(gas by *band:neo)
  :~  :-  %src
      :-  req=&
      :-  [pro/%txt ~]
      ~
  ==
::
++  form
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    !!
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =/  [=stud:neo =vase]  (need pal)
    =/  sesh  session:!<(renderer vase)
    :-  ~
    :-  %renderer
    !>
    :-  sesh
    :-  ~
    ^-  manx
    =/  pax
      p:(~(got by deps.bowl) %src)
    =/  =idea:neo
      %.  / 
      %~  got
        of:neo
      q:(~(got by deps.bowl) %src)
    =/  text  !<(@t q.pail.idea)
    =/  tape  (trip text)
    =/  subject-end  (fall (find [10]~ tape) 56)
    =/  subject  (scag subject-end tape)
    ;div.fr.g2
      ;a.p2.br1.grow.b1.hover.loader
        ;div.loaded.fc.g1.js.as.g2
          ;span.f3: {(pretty-date `@da`->:pax)}
          ;span.bold: {subject}
        ==
        ;span.loading
          ;+  loading.feather-icons
        ==
      ==
    ==
  --
--
::
|%
++  pretty-date
  |=  date=@da
  ^-  tape
  =/  d  (yore date)
  "{(y-co:co y:d)}-{(y-co:co m:d)}-{(y-co:co d:t:d)}"
--