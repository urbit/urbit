/-  gh
/+  gh-parse, httr-to-json, old-zuse
=,  mimes:html
=,  old-zuse
|_  issue/issue:gh
++  grab
  |%
  ++  noun  issue:gh
  ++  httr  (cork httr-to-json json)
  ++  json  issue:gh-parse
  --
++  grow
  |%
  ++  json  raw.issue
  ++  mime  [/txt/plain (as-octs (crip <issue>))]
  ++  txt   (print-issue:gh-parse issue)
  --
--
