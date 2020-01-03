/-  *language-server
/+  lsp-json=language-server-json
|_  req=request-message
++  grab
  |%
  ++  noun  req
  ++  json
    |=  jon=^json
    (parse-request:lsp-json jon)
  --
--
