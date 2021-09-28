/-  lens
|_  com=command:lens
++  grad  %noun
++  grow
  |%
  ++  noun  com
  --
++  grab
  |%
  ++  noun  command:lens
  ++  json
    |=  jon=^json
    ^-  command:lens
    ~|  jon=jon
    %-  need
    %.  jon
    =<  %-  ot  :~
          source+source
          sink+sink
        ==
    =,  dejs-soft:format
    |%
    ++  source
      ^-  $-(^^json (unit source:lens))
      |=  jon=^^json
      =+  tuple=%.(jon (ar source))
      ?^  tuple
        `[%tuple u.tuple]
      %.  jon
      %-  of  :~
        data+so
        dojo+so
        clay+so
        url+(su auri:de-purl:html)
        api+(su ;~(plug sym ;~(pfix col prn)))
        :-  %get-api
        %-  su
        ;~  plug
            sym
            ;~(pfix col (more fas (cook crip (star ;~(less fas prn)))))
        ==
        listen-api+(su ;~(plug sym ;~(pfix col sym)))
        export+so
        import+(ot app+so base64-jam+so ~)
        export-all+none
        import-all+(ot base64-jam+so ~)
        as+(ot mark+(su sym) next+source ~)
        hoon+(ot code+so next+source ~)
        cancel+none
      ==
    ++  none  |=(^^json (some ~))
    ++  sink
      ^-  $-(^^json (unit sink:lens))
      %-  of  :~
        stdout+none
        output-file+so
        output-pill+so
        output-clay+(su (easy /sentinel/path))
        url+(su auri:de-purl:html)
        to-api+(su ;~(plug sym ;~(pfix col prn)))
        :-  %send-api
        %-  su
        ;~  plug
            sym
            ;~(pfix col (more fas (cook crip (star ;~(less fas prn)))))
        ==
        command+so
        app+(su sym)
      ==
    --
  --
--
