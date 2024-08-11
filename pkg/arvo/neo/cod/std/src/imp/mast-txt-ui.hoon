/@  ui-event
/@  txt
/@  diary-diff
^-  kook:neo
=<
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids  *kids:neo
++  deps
  ^-  deps:neo
  %-  my
  :~  :^  %src  &  [only/%txt ~]  ~
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    manx/!>((render (get-render-data bowl)))
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    ?+  sud  ~|(bad-stud/sud !!)
      ::
        %ui-event
      =/  eve  !<(ui-event vaz)
      ?+  path.eve  ~|(missing-event-handler-for/path.eve !!)
        ::
          [%input %text ~]
        =/  dat             (~(got by data.eve) '/target/value')
        =/  dst=pith:neo    p:(~(got by deps.bowl) %src)
        =/  key=@da         =/(r (rear dst) ?>(&(?=(^ r) ?=(%da -.r)) +.r))
        ~&  >  [key dat]
        =/  dif=diary-diff  [%put-entry key dat]
        :_  pail
        :~  [(snip dst) %poke diary-diff/!>(dif)]
        ==
        ::
      ==
      ::
        %rely
      `manx/!>((render (get-render-data bowl)))
      ::
    ==
  ::
  --
--
::
|%
::
++  render
  |_  =txt
  ::
  ++  $
    ^-  manx
    ;html
      ;head
        ;meta(charset "utf-8");
      ==
      ;+  body
    ==
  ::
  ++  body
    ^-  manx
    ;body
      ;textarea
        =event     "/input/text"
        =return    "/target/value"
        =debounce  "0.7"
        ;+  ;/  (trip txt)
      ==
    ==
  ::
  --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::
++  get-render-data
  |=  =bowl:neo
  ^-  txt
  =/  dep  (~(got by deps.bowl) %src)
  ?~  fil.q.dep  ''
  !<(txt q.pail.u.fil.q.dep)
::
--
