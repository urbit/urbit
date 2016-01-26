/-  gh
/+  gh-parse
|_  issue-comment=issue-comment:gh
++  grab
  |%
  ++  json
    |=  jon=^json
    ^-  issue-comment:gh
    =+  top=(need ((om:jo some) jon))
    :*  (need (repository:gh-parse (~(got by top) %repository)))
        (need (user:gh-parse (~(got by top) %sender)))
        (need (so:jo (~(got by top) %action)))
        (need (issue:gh-parse (~(got by top) %issue)))
        (need (comment:gh-parse (~(got by top) %comment)))
    ==
  --
--
