|%
::
+$  version
  [major=@ud minor=@ud patch=@ud]
::
+$  glob  (map path mime)
::
+$  url   cord
::  $glob-location: How to retrieve a glob
::
+$  glob-location
  $%  [%http =url]
      [%ship =ship]
  ==
::  $href: Where a tile links to
::
+$  href
  $%  [%glob base=term =glob-location]
      [%site =path]
  ==
::  $chad: State of a docket
::
+$  chad
  $%  :: Done
      [%glob =glob]
      [%site ~]
      :: Waiting 
      [%install ~]
      [%suspend ~]
      :: Error
      [%hung err=cord]
  ==
::
::  $charge: A realized $docket
::
+$  charge
  $:  =docket
      =chad
  ==
::
::  $clause: A key and value, as part of a docket
::
::    Only used to parse $docket
::
+$  clause
  $%  [%title title=@t]
      [%info info=@t]
      [%color color=@ux]
      [%glob-http url=cord]
      [%glob-ames =ship]
      [%image =url]
      [%site =path]
      [%base base=term]
      [%version =version]
      [%website website=url]
      [%license license=cord]
  ==
::
::  $docket: A description of JS bundles for a desk
::
+$  docket
  $:  %1
      title=@t
      info=@t
      color=@ux 
      =href
      image=(unit url)
      =version
      website=url
      license=cord
  ==
::
+$  charge-update
  $%  [%initial initial=(map desk charge)]
      [%add-charge =desk =charge]
      [%del-charge =desk]
  ==
--
