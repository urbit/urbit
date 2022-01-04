|%
::
++  docket-0
  =<  docket
  |%
  +$  docket
    $:  %1
        title=@t
        info=@t
        color=@ux
        =href-0
        image=(unit url)
        =version
        website=url
        license=cord
    ==
  --
::
+$  version
  [major=@ud minor=@ud patch=@ud]
::
+$  glob  (map path mime)
::
+$  url   cord
::  $glob-location: where to retrieve a glob
::
+$  glob-reference
  [hash=@uvH location=glob-location]
::
+$  glob-location
  $%  [%http =url]
      [%ames =ship]
  ==
::  $spot: full glob reference including the base desk
::
+$  spot  (unit [base=term =glob-reference])
::  $href: app links
::
+$  href
  $:  =spot
  $%  [%glob ~]
      [%site =path]
  ==  ==
::  $href-0: where tile links to
::
+$  href-0
  $%  [%glob base=term =glob-reference]
      [%site =path]
  ==
::  $chad: State of a docket
::
+$  chad
  $~  [%install ~]
  $%  :: Done
      [%glob =glob]
      [%site glob=(unit glob)]
      :: Waiting
      [%install ~]
      [%suspend glob=(unit glob)]
      :: Error
      [%hung err=cord]
  ==
::
::  $charge: A realized $docket
::
+$  charge-0
  $:  =docket-0
      =chad
  ==
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
