::
::::  /hoon+core+elem+mar
  ::
/?    314
/+    tree,react
[. tree react]
!:
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
    |=(a/marl [%a (turn a react-to-json)])
  --
++  grab  |%                                            ::  convert from
          ++  noun  (map path marl)                     ::  clam from %noun
          ::++  elem  |=(a=manx `_all`[[/ ((getall %h1) a)] ~ ~])
--        --
