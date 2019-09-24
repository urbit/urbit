::
::::  /mar/lyre/action/hoon
  ::
/?  309
/-  *lyre
=,  format
::
|_  act=action
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action
  ++  json
    |^
    |=  jon=^json
    ;;  action
    %.  jon
    %-  of:dejs
    :~  new-view+(ot:dejs:format nom+so:dejs:format dep+deps ~)
        change-deps+(ot:dejs:format nom+so:dejs:format dep+deps ~)
        switch-view+(of:dejs:format nom+so:dejs:format ~)
        delete-view+(of:dejs:format nom+so:dejs:format ~)
    ==
    ::
    ++  deps
      |=  jon=^json
      %.  jon
      %-  ot:dejs
      :~  clay+(ar:dejs:format clay)
          gall+(ar:dejs:format gall)
          raw+ul:dejs
          ren+so:dejs
      ==
    ::
    ++  clay
      %-  ot:dejs:format
      :~  :-  %bem
          %-  su:dejs
          %+  cook
            |=(p=path (need (de-beam:format p)))
          ;~(pfix net (more net urs:ab))
      ::
          :-  %car
          %-  su:dejs
          ;~  pose
            (just 'd')  (just 'p')  (just 'g')  (just 'u')  (just 'v')
            (just 'w')  (just 'x')  (just 'y')  (just 'z')
          ==
      ==
    ::
    ++  gall
      %-  ot:dejs:format
      :~  app+so:dejs
          pax+(su:dejs ;~(pfix net (more net urs:ab)))
      ==
    --
  --
--
