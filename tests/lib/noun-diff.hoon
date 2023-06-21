/+  quiz, *test, noun-diff
|%
++  giv  givers.quiz
++  check  ~(check quiz `@uv`1 200)
++  test-diff
  =/  fate
    !>
    |=  [old=* new=*]
    =/  patch  (diff:noun-diff old new)
    =(new (apply:noun-diff patch old))
  (expect !>((check fate ~ ~)))
++  test-id
  =/  fate
    !>
    |=  non=*
    =(non (apply:noun-diff id.noun-diff non))
  (expect !>((check fate ~ ~)))
--


