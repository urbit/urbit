::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse
|_  issues=issues:gh
++  grab
  |%
  ++  json
    |=  jon=^json
    ^-  issues:gh
    =+  top=(need ((om:jo some) jon))
    :*  (need (repository:gh-parse (~(got by top) %repository)))
        (need (user:gh-parse (~(got by top) %sender)))
        =+  action=(need (so:jo (~(got by top) %action)))
        ?+  action  ~|([%bad-action action] !!)
          %assigned    [action (need (user:gh-parse (~(got by top) %assignee)))]
          %unassigned  [action (need (user:gh-parse (~(got by top) %assignee)))]
          %labeled     [action (need (label:gh-parse (~(got by top) %label)))]
          %unlabeled   [action (need (label:gh-parse (~(got by top) %label)))]
          %opened      [action ~]
          %closed      [action ~]
          %reopened    [action ~]
        ==
        (need (issue:gh-parse (~(got by top) %issue)))
    ==
  --
--
