/-  gh
/+  gh-parse, old-zuse
=,  mimes:html
=,  old-zuse
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
