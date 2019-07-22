::
::::  /hoon/urb-split/lib
  ::
/?    310
|=  {dep/@uvH urb/manx}  ^-  {hed/{@uvh marl} bod/{@uvH marl}}
~|  [%malformed-urb urb]  :: XX types
?>  ?=({{$html ~} {{$head ~} *} {{$body ~} *} ~} urb)
=+  `{{$html ~} {{$head ~} hed/marl} {{$body ~} bod/marl} ~}`urb
:-  [dep hed]                         :: Assume all dependencies are hard
[0v0 bod]
