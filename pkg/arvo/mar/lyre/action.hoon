::
::::  /hoon/lyre/publish/mar
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
::    |=  jon=^json
::    =<  (action jon)
::    |%
::    ++  action
::      %-  of-dejs
::      :~  new-session+new-session
::          delete-session+delete-session
::          switch-session+switch-session
::          set-path+set-path
::      ==
::    --
  --
--
