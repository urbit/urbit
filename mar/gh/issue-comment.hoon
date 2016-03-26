::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse, talk
|_  issue-comment/issue-comment:gh
++  grow
  |%
  ++  talk-speeches
    ^-  (list speech:talk)
    :_  ~
    =+  ^=  txt
        ;:  (cury cat 3)
            'on issue #'
            `@t`(rsh 3 2 (scot %ui number.issue.issue-comment))
            ': '
            body.comment.issue-comment
        ==
    :*  %api  %github
        login.sender.issue-comment
        (need (epur url.sender.issue-comment))
        txt
        txt
        (need (epur url.comment.issue-comment))
        %-  jobe  :~
          repository+s+name.repository.issue-comment
          number+(jone number.issue.issue-comment)
          title+s+title.issue.issue-comment
        ==
    ==
  --
++  grab
  |%
  ++  json
    |=  jon/^json
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
