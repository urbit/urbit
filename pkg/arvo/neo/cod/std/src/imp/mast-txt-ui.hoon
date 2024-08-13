/@  ui-event
/@  txt
/@  diary-diff
=>  |%
    ::
    ++  sail
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
        =/  tex=tape  (trip txt)
        ;body
          ;p: {tex}
          ;textarea
            =event     "/input/text"
            =return    "/target/value"
            =debounce  "0.7"
            ;+  ;/  tex
          ==
        ==
      ::
      --
    ::
    ++  get-deps-data
      |=  =bowl:neo
      ^-  txt
      =/  dep  (~(got by deps.bowl) %src)
      ?~  fil.q.dep  ''
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
    [~ manx/!>((sail (get-deps-data bowl)))]
  ::
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    ?+  sud  !!
      ::
        %rely
      [~ manx/!>((sail (get-deps-data bowl)))]
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
