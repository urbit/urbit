::
::::  /hoon/txt/mar
  ::
/?    310
::
=,  clay
=,  differ
=,  format
=,  mimes:html
|_  txt=wain
::
++  grab                                                ::  convert from
  |%
  ++  mime  |=((pair mite octs) (to-wain q.q))
  ++  noun  wain                                        ::  clam from %noun
  --
++  grow
  =>  v=.
  |%
  ++  mime  =>  v  [/text/plain (as-octs (of-wain txt))]
  ++  elem  =>  v  ;pre: {(trip (of-wain txt))}
  --
++  grad
  |%
  ++  form  %txt-diff
  ++  diff
    |=  tyt=wain
    ^-  (urge cord)
    (lusk txt tyt (loss txt tyt))
  ::
  ++  pact
    |=  dif=(urge cord)
    ~|  [%pacting dif]
    ^-  wain
    (lurk txt dif)
  ::
  ++  join
    |=  [ali=(urge cord) bob=(urge cord)]
    ^-  (unit (urge cord))
    |^
    =.  ali  (clean ali)
    =.  bob  (clean bob)
    |-  ^-  (unit (urge cord))
    ?~  ali  `bob
    ?~  bob  `ali
    ?-    -.i.ali
        %&
      ?-    -.i.bob
          %&
        ?:  =(p.i.ali p.i.bob)
          %+  bind  $(ali t.ali, bob t.bob)
          |=(cud=(urge cord) [i.ali cud])
        ?:  (gth p.i.ali p.i.bob)
          %+  bind  $(p.i.ali (sub p.i.ali p.i.bob), bob t.bob)
          |=(cud=(urge cord) [i.bob cud])
        %+  bind  $(ali t.ali, p.i.bob (sub p.i.bob p.i.ali))
        |=(cud=(urge cord) [i.ali cud])
      ::
          %|
        ?:  =(p.i.ali (lent p.i.bob))
          %+  bind  $(ali t.ali, bob t.bob)
          |=(cud=(urge cord) [i.bob cud])
        ?:  (gth p.i.ali (lent p.i.bob))
          %+  bind  $(p.i.ali (sub p.i.ali (lent p.i.bob)), bob t.bob)
          |=(cud=(urge cord) [i.bob cud])
        ~
      ==
    ::
        %|
      ?-  -.i.bob
          %|
        ?.  =(i.ali i.bob)
          ~
        %+  bind  $(ali t.ali, bob t.bob)
        |=(cud=(urge cord) [i.ali cud])
      ::
          %&
        ?:  =(p.i.bob (lent p.i.ali))
          %+  bind  $(ali t.ali, bob t.bob)
          |=(cud=(urge cord) [i.ali cud])
        ?:  (gth p.i.bob (lent p.i.ali))
          %+  bind  $(ali t.ali, p.i.bob (sub p.i.bob (lent p.i.ali)))
          |=(cud=(urge cord) [i.ali cud])
        ~
      ==
    ==
    ++  clean                                          ::  clean
      |=  wig=(urge cord)
      ^-  (urge cord)
      ?~  wig  ~
      ?~  t.wig  wig
      ?:  ?=(%& -.i.wig)
        ?:  ?=(%& -.i.t.wig)
          $(wig [[%& (add p.i.wig p.i.t.wig)] t.t.wig])
        [i.wig $(wig t.wig)]
      ?:  ?=(%| -.i.t.wig)
        $(wig [[%| (welp p.i.wig p.i.t.wig) (welp q.i.wig q.i.t.wig)] t.t.wig])
      [i.wig $(wig t.wig)]
    --
  ::
  ++  mash
    |=  $:  [als=ship ald=desk ali=(urge cord)]
            [bos=ship bod=desk bob=(urge cord)]
        ==
    ^-  (urge cord)
    |^
    =.  ali  (clean ali)
    =.  bob  (clean bob)
    |-  ^-  (urge cord)
    ?~  ali  bob
    ?~  bob  ali
    ?-    -.i.ali
        %&
      ?-    -.i.bob
          %&
        ?:  =(p.i.ali p.i.bob)
          [i.ali $(ali t.ali, bob t.bob)]
        ?:  (gth p.i.ali p.i.bob)
          [i.bob $(p.i.ali (sub p.i.ali p.i.bob), bob t.bob)]
        [i.ali $(ali t.ali, p.i.bob (sub p.i.bob p.i.ali))]
      ::
          %|
        ?:  =(p.i.ali (lent p.i.bob))
          [i.bob $(ali t.ali, bob t.bob)]
        ?:  (gth p.i.ali (lent p.i.bob))
          [i.bob $(p.i.ali (sub p.i.ali (lent p.i.bob)), bob t.bob)]
        =/  [fic=(unce cord) ali=(urge cord) bob=(urge cord)]
            (resolve ali bob)
        [fic $(ali ali, bob bob)]
        ::  ~   ::  here, alice is good for a while, but not for the whole
      ==    ::  length of bob's changes
    ::
        %|
      ?-  -.i.bob
          %|
        =/  [fic=(unce cord) ali=(urge cord) bob=(urge cord)]
            (resolve ali bob)
        [fic $(ali ali, bob bob)]
      ::
          %&
        ?:  =(p.i.bob (lent p.i.ali))
          [i.ali $(ali t.ali, bob t.bob)]
        ?:  (gth p.i.bob (lent p.i.ali))
          [i.ali $(ali t.ali, p.i.bob (sub p.i.bob (lent p.i.ali)))]
        =/  [fic=(unce cord) ali=(urge cord) bob=(urge cord)]
            (resolve ali bob)
        [fic $(ali ali, bob bob)]
      ==
    ==
    ::
    ++  annotate                                        ::  annotate conflict
      |=  $:  ali=(list @t)
              bob=(list @t)
              bas=(list @t)
          ==
      ^-  (list @t)
      %-  zing
      ^-  (list (list @t))
      %-  flop
      ^-  (list (list @t))
      :-  :_  ~
          %^  cat  3  '<<<<<<<<<<<<'
          %^  cat  3  ' '
          %^  cat  3  `@t`(scot %p bos)
          %^  cat  3  '/'
          bod

      :-  bob
      :-  ~['------------']
      :-  bas
      :-  ~['++++++++++++']
      :-  ali
      :-  :_  ~
          %^  cat  3  '>>>>>>>>>>>>'
          %^  cat  3  ' '
          %^  cat  3  `@t`(scot %p als)
          %^  cat  3  '/'
          ald
      ~
    ::
    ++  clean                                          ::  clean
      |=  wig=(urge cord)
      ^-  (urge cord)
      ?~  wig  ~
      ?~  t.wig  wig
      ?:  ?=(%& -.i.wig)
        ?:  ?=(%& -.i.t.wig)
          $(wig [[%& (add p.i.wig p.i.t.wig)] t.t.wig])
        [i.wig $(wig t.wig)]
      ?:  ?=(%| -.i.t.wig)
        $(wig [[%| (welp p.i.wig p.i.t.wig) (welp q.i.wig q.i.t.wig)] t.t.wig])
      [i.wig $(wig t.wig)]
    ::
    ++  resolve
      |=  [ali=(urge cord) bob=(urge cord)]
      ^-  [fic=[%| p=(list cord) q=(list cord)] ali=(urge cord) bob=(urge cord)]
      =-  [[%| bac (annotate alc boc bac)] ali bob]
      |-  ^-  $:  $:  bac=(list cord)
                      alc=(list cord)
                      boc=(list cord)
                  ==
                  ali=(urge cord)
                  bob=(urge cord)
              ==
      ?~  ali  [[~ ~ ~] ali bob]
      ?~  bob  [[~ ~ ~] ali bob]
      ?-    -.i.ali
          %&
        ?-    -.i.bob
            %&  [[~ ~ ~] ali bob]                       ::  no conflict
            %|
          =+  lob=(lent p.i.bob)
          ?:  =(lob p.i.ali)
            [[p.i.bob p.i.bob q.i.bob] t.ali t.bob]
          ?:  (lth lob p.i.ali)
            [[p.i.bob p.i.bob q.i.bob] [[%& (sub p.i.ali lob)] t.ali] t.bob]
          =+  wat=(scag (sub lob p.i.ali) p.i.bob)
          =+  ^=  res
              %=  $
                ali  t.ali
                bob  [[%| (scag (sub lob p.i.ali) p.i.bob) ~] t.bob]
              ==
          :*  :*  (welp bac.res wat)
                  (welp alc.res wat)
                  (welp boc.res q.i.bob)
              ==
              ali.res
              bob.res
          ==
        ==
      ::
          %|
        ?-    -.i.bob
            %&
          =+  loa=(lent p.i.ali)
          ?:  =(loa p.i.bob)
            [[p.i.ali q.i.ali p.i.ali] t.ali t.bob]
          ?:  (lth loa p.i.bob)
            [[p.i.ali q.i.ali p.i.ali] t.ali [[%& (sub p.i.bob loa)] t.bob]]
          =+  wat=(slag (sub loa p.i.bob) p.i.ali)
          =+  ^=  res
              %=  $
                ali  [[%| (scag (sub loa p.i.bob) p.i.ali) ~] t.ali]
                bob  t.bob
              ==
          :*  :*  (welp bac.res wat)
                  (welp alc.res q.i.ali)
                  (welp boc.res wat)
              ==
              ali.res
              bob.res
          ==
        ::
            %|
          =+  loa=(lent p.i.ali)
          =+  lob=(lent p.i.bob)
          ?:  =(loa lob)
            [[p.i.ali q.i.ali q.i.bob] t.ali t.bob]
          =+  ^=  res
              ?:  (gth loa lob)
                $(ali [[%| (scag (sub loa lob) p.i.ali) ~] t.ali], bob t.bob)
              ~&  [%scagging loa=loa pibob=p.i.bob slag=(scag loa p.i.bob)]
              $(ali t.ali, bob [[%| (scag (sub lob loa) p.i.bob) ~] t.bob])
          :*  :*  (welp bac.res ?:((gth loa lob) p.i.bob p.i.ali))
                  (welp alc.res q.i.ali)
                  (welp boc.res q.i.bob)
              ==
              ali.res
              bob.res
          ==
        ==
      ==
    --
  --
--
