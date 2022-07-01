/+  *deq
/+  *test
=/  big-num
  100
=/  de  (deq ,@)
=/  big-list
  (gulf 1 big-num)
=/  big
  (apl:de *(pha @) big-list)
=/  foo-list  (gulf 1 8)
|%
++  foo  
  (apl:de *(pha @) 1 2 3 4 5 6 7 8 ~)
++  bar  
  `(pha @)`(apl:de *(pha @) 8 9 10 11 12 13 14 15 ~)
::
++  test-tap
  =/  ls  
    ~>  %bout.[1 %tap]
    (tap:de big)
  (expect-eq !>(ls) !>(big-list))
::
++  test-left
  ^-  tang
  =/  bar
    ~>  %bout.[1 %cons]
    (cons:de bar 7)
  =.  bar
    ~>  %bout.[1 %apl]
    (apl:de bar 1 2 3 4 5 6 ~)
  %-  zing
  :- 
    ~>  %bout.[1 %eq-1]
     (expect-eq !>((tap:de bar)) !>((gulf 1 15)))
  =^  val=(unit @)  bar
    ~>  %bout.[1 %pop-left]
    (pop-left:de bar)
  ~>  %bout.[1 %eq-2]
  :~  (expect-eq !>(1) !>((need val)))
      (expect-eq !>((gulf 2 15)) !>((tap:de bar)))
  ==
::
++  test-cons-tree
  =/  foo
    (cons:de foo 1)
  ~
::
++  test-cons-list
  =/  big-list
    [1 big-list]
  ~
::
++  test-rear-tree
  =/  big  big
  =/  res  (peek-right:de big)
  ~
::
++  test-rear-list
  =/  last  (rear big-list)
  ~
::
++  test-right
  ^-  tang
  =/  foo
    ~>  %bout.[1 %snoc]
    (snoc:de foo 9)
  =.  foo
    (apr:de foo 10 11 12 13 14 15 ~)
  %-  zing
  :-  (expect-eq !>((tap:de foo)) !>((gulf 1 15)))
  =^  val=(unit @)  foo
    (pop-right:de foo)
  :~  (expect-eq !>((need val)) !>(15))
      (expect-eq !>((gulf 1 14)) !>((tap:de foo)))
  ==
++  test-queue
  ^-  tang
  =/  foo   foo
  =.  foo
    (apr:de foo 9 10 11 12 13 14 15 ~)
  =/  expected  (gulf 1 15)
  %-  zing
  |-  ^-  (list tang)
  =^  val=(unit @)  foo
    (pop-left:de foo)
  ?~  val
    (expect-eq !>(~) !>(expected))^~
  ~&  got/u.val
  ?~  expected
    ~[leaf/"queue mismatch"]
  :-  (expect-eq !>(i.expected) !>(u.val))
  $(expected t.expected)
--
