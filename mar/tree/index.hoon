::
::::  /hoon/index/tree/mar
  ::
/?    310
/+    tree,map-to-json,elem-to-react-json
[. tree]
::
::::
  ::
|_  all/(map path marl)
::
++  grow                                                ::  convert to
  |%
  ++  json
    %.  all
    %+  map-to-json
      |=(a/path (crip (spud a)))
    |=(a/marl [%a (turn a elem-to-react-json)])
  --
++  grab  |%                                            ::  convert from
          ++  noun  (map path marl)                     ::  clam from %noun
          ::++  elem  |=(a=manx `_all`[[/ ((getall %h1) a)] ~ ~])
--        --
