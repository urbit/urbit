/@  ui-event
/@  txt
/@  diary-diff
=>  |%
    ::
    ++  render-sail
      |=  =bowl:neo
      ^-  manx
      =;  text=tape
      ::
        ;div
          ;p: {text}
          ;textarea
            =event     "/input/text"
            =return    "/target/value"
            =debounce  "0.7"
            ;+  ;/  text
          ==
        ==
      ::
      =/  dep  (~(got by deps.bowl) %src)
      ?~  fil.q.dep  ""
      %-  trip
      !<(txt q.pail.u.fil.q.dep)
    ::
    --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::
^-  kook:neo
|%
++  state  pro/%manx
++  poke   (sy %ui-event %rely %gift ~)
++  kids   *kids:neo
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
    [~ manx/!>((render-sail bowl))]
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    ?+  sud  !!
      ::
        %rely
      [~ manx/!>((render-sail bowl))]
      ::
        %ui-event
      =/  eve  !<(ui-event vaz)
      ?+  path.eve  ~|(missing-event-handler-for/path.eve !!)
        ::
          [%input %text ~]
        =/  dat             (~(got by data.eve) '/target/value')
        =/  dst=pith:neo    p:(~(got by deps.bowl) %src)
        =/  key=@da         =/(r (rear dst) ?>(&(?=(^ r) ?=(%da -.r)) +.r))
        =/  dif=diary-diff  [%put-entry key dat]
        :_  pail
        :~  [(snip dst) %poke diary-diff/!>(dif)]
        ==
        ::
      ==
      ::
    ==
  ::
  --
--
