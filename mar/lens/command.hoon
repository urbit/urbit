/-  lens
!:
|_  com/command:lens
++  grab
  |%
  ++  noun  command:lens
  ++  json
    |=  jon/^json
    ^-  command:lens
    %-  need
    %.  jon
    =>  [. jo]
    =<  %-  ot  :~
          flags+(ot merge-input+bo fan-output+bo ~)
          sources+(ar source)
          transformers+(ar transformer)
          sinks+(ar sink)
        ==
    |%
    ++  source
      ^-  $-(^json (unit source:lens))
      %-  of  :~
        data+so:jo
        dojo+so:jo
        clay+(su (easy /sentinel/path))  ::  XX  parse-to-pah
        url+(su auri:urlp)
        api+(su ;~(plug sym ;~(pfix col prn)))
        get-api+(su ;~(plug sym ;~(pfix col auri:urlp)))
        listen-api+(su ;~(plug sym ;~(pfix col sym)))
      ==
    ++  transformer  (of as+so hoon+so ~):jo
    ++  sink
      ^-  $-(^json (unit sink:lens))
      %-  of  :~
        stdout+|=(^json (some ~))
        output-file+so:jo
        output-clay+(su (easy /sentinel/path))
        url+(su auri:urlp)
        to-api+(su ;~(plug sym ;~(pfix col prn)))
        send-api+(su ;~(plug sym ;~(pfix col auri:urlp)))
        command+so:jo
        app+(su sym)
      ==
    --
  --
--
