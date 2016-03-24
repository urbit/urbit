::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse, talk
|_  issues/issues:gh
++  grow
  |%
  ++  talk-speeches
    ^-  (list speech:talk)
    :_  ~
    :*  %api  %github
        login.sender.issues
        =-  [- - (need (epur url.issue.issues))]
        ?-    -.action.issues
            $assigned
          ;:  (cury cat 3)
              'assigned issue #'
              (scot %ud number.issue.issues)
              ' to '
              login.assignee.action.issues
              ' ('
              title.issue.issues
              ')'
          ==
        ::
            $unassigned
          ;:  (cury cat 3)
              'unassigned issue #'
              (scot %ud number.issue.issues)
              ' from '
              login.assignee.action.issues
              ' ('
              title.issue.issues
              ')'
          ==
        ::
            $labeled
          ;:  (cury cat 3)
              'labeled issue #'
              (scot %ud number.issue.issues)
              ' as '
              name.label.action.issues
              ' ('
              title.issue.issues
              ')'
          ==
        ::
            $unlabeled
          ;:  (cury cat 3)
              'unlabeled issue #'
              (scot %ud number.issue.issues)
              ' as '
              name.label.action.issues
              ' ('
              title.issue.issues
              ')'
          ==
        ::
            $opened
          ;:  (cury cat 3)
              'opened issue #'
              (scot %ud number.issue.issues)
              ': '
              title.issue.issues
          ==
        ::
            $closed
          ;:  (cury cat 3)
              'closed issue #'
              (scot %ud number.issue.issues)
              ': '
              title.issue.issues
          ==
        ::
            $reopened
          ;:  (cury cat 3)
              'reopened issue #'
              (scot %ud number.issue.issues)
              ': '
              title.issue.issues
          ==
        ==
    ==
  --
++  grab
  |%
  ++  json
    |=  jon/^json
    ^-  issues:gh
    =+  top=(need ((om:jo some) jon))
    :*  (need (repository:gh-parse (~(got by top) %repository)))
        (need (user:gh-parse (~(got by top) %sender)))
        =+  action=(need (so:jo (~(got by top) %action)))
        ?+  action  ~|([%bad-action action] !!)
          $assigned    [action (need (user:gh-parse (~(got by top) %assignee)))]
          $unassigned  [action (need (user:gh-parse (~(got by top) %assignee)))]
          $labeled     [action (need (label:gh-parse (~(got by top) %label)))]
          $unlabeled   [action (need (label:gh-parse (~(got by top) %label)))]
          $opened      [action ~]
          $closed      [action ~]
          $reopened    [action ~]
        ==
        (need (issue:gh-parse (~(got by top) %issue)))
    ==
  --
--
