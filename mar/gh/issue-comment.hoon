::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse, talk, old-zuse
=,  old-zuse
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
        (rash html-url.sender.issue-comment aurf:urlp)
        txt
        txt
        (rash html-url.comment.issue-comment aurf:urlp)
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
    =;  jop  |=(jon/^json `issue-comment:gh`(need (jop jon)))
    %-  ot:jo
    :~  repository+repository:gh-parse
        sender+user:gh-parse
        action+so:jo
        issue+issue:gh-parse
        comment+comment:gh-parse
    ==
  --
--
