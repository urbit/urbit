::
::::  /hoon/noun/mar
  ::
/?  310
/+  noun-diff
!:
|_  non=*
++  grab  |%
          ++  noun  *
          --
++  grow  |%
          ++  mime  [/application/x-urb-jam (as-octs:mimes:html (jam non))]
          --
++  grad
  |%
  ++  form  %noun
  ++  diff
    |=  new=*
    ^-  (list diff:noun-diff)
    ~[(diff:noun-diff non new)]
  ++  pact
    |=  patches=(list patch:noun-diff)
    (roll patches |=([=patch:noun-diff n=_non] (apply:noun-diff patch n)))
  ::  A poor mans' version of merging patches. Just apply them in sequence.
  ++  join
    |=  [old=(list patch:noun-diff) new=(list patch:noun-diff)]
    `(weld new old)
  ++  mash
    |=  [[ship desk old=(list patch:noun-diff)] [ship desk new=(list patch:noun-diff)]] 
    (weld new old)
  --
--
