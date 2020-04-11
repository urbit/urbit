/-  *language-server
/+  lsp-json=language-server-json
|_  not=all:notification
++  grab
  |%
  ++  noun  not
  ++  json
    |=  jon=^json
    (notification:dejs:lsp-json jon)
  --
++  grow
  |%
  ++  json
    (notification:enjs:lsp-json not)
  --
--
