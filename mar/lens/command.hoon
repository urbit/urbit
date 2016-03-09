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
          source+source
          sink+sink
        ==
    |%
    ++  source
      ^-  $-(^json (unit source:lens))
      |=  jon/^json
      %.  jon
      %-  of  :~
        data+so:jo
        dojo+so:jo
        clay+so:jo
        url+(su auri:urlp)
        api+(su ;~(plug sym ;~(pfix col prn)))
        :-  %get-api
        %-  su
        ;~  plug
            sym
            ;~(pfix col (more fas (cook crip (star ;~(less fas prn)))))
        ==
        listen-api+(su ;~(plug sym ;~(pfix col sym)))
        as+(ot mark+(su sym) next+source ~)
        hoon+(ot code+so:jo next+source ~)
        tuple+(ar source)
      ==
    ++  sink
      ^-  $-(^json (unit sink:lens))
      %-  of  :~
        stdout+|=(^json (some ~))
        output-file+so:jo
        output-clay+(su (easy /sentinel/path))
        url+(su auri:urlp)
        to-api+(su ;~(plug sym ;~(pfix col prn)))
        :-  %send-api
        %-  su
        ;~  plug
            sym
            ;~(pfix col (more fas (cook crip (star ;~(less fas prn)))))
        ==
        command+so:jo
        app+(su sym)
      ==
    --
  --
--
