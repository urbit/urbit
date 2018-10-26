/-  gh
/+  gh-parse
=,  mimes:html
|_  issues/(list issue:gh)
++  grab
  |%
  ++  noun  (list issue:gh)
  --
++  grow
  |%
  ++  json  [%a (turn issues |=(issue:gh raw))]
  ++  mime  [/txt/plain (as-octs (crip <issues>))]
  ++  txt   =-  ?~  -  -  ->
            %+  roll  (turn issues print-issue:gh-parse)
            |=  {a/wain b/wain}
            :(welp b ~['----------------------------------------'] a)
  --
--
