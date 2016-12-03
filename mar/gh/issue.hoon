/-  gh
/+  gh-parse
=,  mimes:html
|_  issue/issue:gh
++  grab
  |%
  ++  noun  issue:gh
  --
++  grow
  |%
  ++  json  raw.issue
  ++  mime  [/txt/plain (as-octs (crip <issue>))]
  ++  txt   (print-issue:gh-parse issue)
  --
--
