::
::::  /hoon/comments/tree/mar
  ::
/?    310
/+    elem-to-react-json, time-to-id
!:
::::
  ::
|_  all/(list (pair time {ship marl}))
::
++  grow                                                ::  convert to
  |%
  ++  json
    :-  %a 
    %+  turn
      (sort all |=({a/* b/*} (lor b a)))
    |=  {a/time b/ship c/marl}  ^-  ^json
    =+  bod=[[%div id+(time-to-id a) ~] c]
    (jobe time+(jode a) user+(jape +:<b>) body+(elem-to-react-json bod) ~)
  --
++  grab  |%                                            ::  convert from
          ++  noun  (list {time manx})                  ::  clam from %noun
          ::++  elem  |=(a=manx `_all`[[/ ((getall %h1) a)] ~ ~])
--        --
