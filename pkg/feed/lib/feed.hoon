/-  sur=feed
/+  pos=post
^?
=<  [. sur]
=,  sur
|%
++  enjs
  =,  enjs:format
  |%
  ++  diff
    |=  d=^diff
    %+  frond  -.d
    ?+  -.d   ~
    ::
        %post  
      %-  pairs
      :~  id+s+(scot %da id.d)
          update+(update:enjs:pos update.d)
      ==
    ==
  ::
  ++  update
    |=  u=^update
    %-  pairs
    :~  time+s+(scot %da p.u)
        diff+(diff q.u)
    ==
  ::
  ++  action
    |=  a=^action
    %-  pairs
    :~  ship+s+(scot %p p.a)
        diff+(diff q.a)
    ==
  --
--
