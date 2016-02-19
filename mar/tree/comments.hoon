::
::::  /hoon/comments/tree/mar
  ::
/?    310
/+    react
!:
::::
  ::
|_  all/(list (pair time manx))
::
++  grow                                                ::  convert to
  |%
  ++  json
    :-  %a 
    %+  turn
      (sort all |=({a/* b/*} (lor b a)))
    |=  {a/time b/manx}  ^-  ^json
    (jobe time+(jode a) body+(react-to-json:react b) ~)
  --
++  grab  |%                                            ::  convert from
          ++  noun  (list {time manx})                  ::  clam from %noun
          ::++  elem  |=(a=manx `_all`[[/ ((getall %h1) a)] ~ ~])
--        --
