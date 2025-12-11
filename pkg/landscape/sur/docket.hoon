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
+$  glob-reference
  [hash=@uvH location=glob-location]
::
+$  glob-location
  $%  [%http =url]
      [%ames =ship]
  ==
::  $href: Where a tile links to
::
+$  href
  $%  [%glob base=term =glob-reference]
      [%site =path]
  ==
::  $chad: State of a docket
::
+$  chad
  $~  [%install ~]
  $%  :: Done
      [%glob =glob]
      [%site ~]
      :: Waiting
      [%install ~]
      [%suspend glob=(unit glob)]
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
      [%glob-http url=cord hash=@uvH]
      [%glob-ames =ship hash=@uvH]
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
