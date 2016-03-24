::  Converts the result of an 'issues' event into a issues:gh.
/-  gh
/+  gh-parse, talk
|_  issue-comment/issue-comment:gh
++  grow
  |%
  ++  talk-speeches
    ^-  (list speech:talk)
    :_  ~
    :*  %api  %github
        login.sender.issue-comment
        (end 3 64 body.comment.issue-comment)
        body.comment.issue-comment
        (need (epur url.comment.issue-comment))
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
