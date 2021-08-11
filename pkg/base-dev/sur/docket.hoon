|%
::
+$  version
  [major=@ud minor=@ud patch=@ud]
::
+$  glob  (map path mime)
::
+$  url   cord
::
::  $charge: A realized $docket
::
+$  charge
  [=glob =docket]
::
::  $clause: A key and value, as part of a docket
::
::    Only used to parse $docket
::
+$  clause
  $%  [%title title=@t]
      [%info info=@t]
      [%color color=@ux]
      [%glob url=cord]
      [%base base=term]
      [%version =version]
      [%website website=url]
      [%license license=cord]
  ==
::
::  $docket: A infoion of JS bundles for a desk
::
+$  docket
  $:  %1
      title=@t
      info=@t
      color=@ux 
      glob=url 
      base=term
      =version
      website=url
      license=cord
  ==
::
::  $treaty: A foreign docket
+$  treaty
  [=ship =desk =case hash=@uv =docket]
::
::
+$  update
  $%  [%initial initial=(map desk docket)]
      [%add-dock =desk =docket]
      [%del-dock =desk]
  ==
--
