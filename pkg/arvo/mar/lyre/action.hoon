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
    |=  jon=^json
    %.  jon
    %-  of:dejs
    :~  new-session+(mu:dejs (su:dejs ;~(pfix net (more net urs:ab))))
        delete-session+ni:dejs
        switch-session+ni:dejs
        set-path+(su:dejs ;~(pfix net (more net urs:ab)))
    ==
  --
--
