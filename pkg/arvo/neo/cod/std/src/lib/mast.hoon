|%
::
+$  crow                ::  ::  ::  mast event poke data
  [=path data=(map @t @t)]
::
+$  prow                ::  ::  ::  mast init data
  $:  urth=?            ::  > are you serving to earth?
      url=path          ::  > the base url that mast will bind with eyre
      =made:neo         ::  > the +$made mast uses to spawn your ui shrubs
  ==
::
+$  rig                 ::  ::  ::  mast state
  $:  urth=?
      =made:neo
      base-sub=path
      open-http=(map @p @ta)  :: eyre ids of http requests pending session creation
  ==
::
--
