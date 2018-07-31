/+  collections
/=  gas  /$  fuel:html
/=  raw
  /^  $?(raw-item:collections ~)
  /|  /;  |=  [a=(map knot cord) b=@t ~]
          ^-  raw-item:collections
          [%umd a b]
          /.  /front/
              /umd/
          ==
      ::
      /~  ~
  ==
::
/=  col
  /^  $?  [config:collections (map knot item:collections) ~]
          [%no-config (map knot item:collections) ~]
          ~
      ==
  /|  /.  /collection-config/
          /_  /collection-item/
      ==
      ::
      /.  /~  %no-config
          /_  /collection-item/
      ==
      ::
      /~  ~
  ==
::
::
^-  item:collections
?~  col
  ?~  raw
    !!
  [%raw raw]
::
?:  ?=(%no-config -.col)
  ?:  =(s.bem.gas /collections/web)
    ?~  raw
      [%collection *config:collections +<.col]
    [%both [*config:collections +<.col] raw]
  ?~  raw
    !!
  [%raw raw]
?~  raw
  [%collection -.col +<.col]
[%both [-.col +<.col] raw]
