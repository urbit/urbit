/+  collections
/=  item
  /^  item:collections
  /;  |=  $:  raw=?(~ raw-item:collections)
              col=?(~ collection:collections)
              ~
          ==
      ?~  raw
        ?~  col
          !!
        [%collection col]
      ?~  col
        [%raw raw]
      [%both col raw]
  ::
  /.
    ::
      /|  /;  |=  [a=(map knot cord) b=@t ~]
              [%umd a b]
              /.  /front/
                  /umd/
              ==
          ::
          /~  ~
      ==
    ::
      /|  /collections/
          /~  ~
      ==
    ::
  ==
::
item
