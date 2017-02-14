::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse, talk, old-zuse
=,  old-zuse
|_  issues/issues:gh
++  grow
  |%
  ++  talk-speeches
    ^-  (list speech:talk)
    :_  ~
    =+  ^=  txt
        ?-    -.action.issues
            $assigned
          ;:  (cury cat 3)
              'assigned issue #'
              (rsh 3 2 (scot %ui number.issue.issues))
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
              (rsh 3 2 (scot %ui number.issue.issues))
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
              (rsh 3 2 (scot %ui number.issue.issues))
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
              (rsh 3 2 (scot %ui number.issue.issues))
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
              (rsh 3 2 (scot %ui number.issue.issues))
              ': '
              title.issue.issues
          ==
        ::
            $closed
          ;:  (cury cat 3)
              'closed issue #'
              (rsh 3 2 (scot %ui number.issue.issues))
              ': '
              title.issue.issues
          ==
        ::
            $reopened
          ;:  (cury cat 3)
              'reopened issue #'
              (rsh 3 2 (scot %ui number.issue.issues))
              ': '
              title.issue.issues
          ==
        ==
    ^-  speech:talk
    :*  %api  %github
        login.sender.issues
        (rash html-url.sender.issues aurf:urlp)
        txt  txt
        (rash html-url.issue.issues aurf:urlp)
        %-  jobe
        %+  welp
          :~  repository+s+name.repository.issues
              number+(jone number.issue.issues)
              title+s+title.issue.issues
              action+s+-.action.issues
          ==
        ?-    -.action.issues
            $assigned
          :~  assignee+s+login.assignee.action.issues
              assignee-url+s+url.assignee.action.issues
          ==
        ::
            $unassigned
          :~  assignee+s+login.assignee.action.issues
              assignee-url+s+url.assignee.action.issues
          ==
        ::
            $labeled
          :~  label+s+name.label.action.issues
          ==
        ::
            $unlabeled
          :~  label+s+name.label.action.issues
          ==
        ::
            $opened    ~
            $closed    ~
            $reopened  ~
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
