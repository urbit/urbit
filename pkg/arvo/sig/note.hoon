/-  shrub
=<  sing
|%
+$  note
  $:  book=term :: XX: makes assumption about namespace that escapes the publication context
      title=cord :: /notes/[id]/title ->
      desc=cord  ::  /notes/[id]/desc
      content=cord
      src=ship
      =time
  ==
+$  diff
  $%  [%edit content=cord]  :: /notes/[id]/edit
      [%rename title=cord]
      ::[%comments *] :: can we add this automatically?
      :: /notes/[id]/comments is automatically rerouted because of the #
      :: rune
  ==
++  sing
  |_  [=bowl:shrub =note kid=*]
  ++  this  .
  ++  pull
    |=  =bush:shrub
    ^-  (quip bush:shrub _this)
    `this
  ++  poke
    |=  =diff
    ^-  (quip bush:shrub _this)
    ?-  -.diff
      %edit    `this(content.note content.diff)
      %rename  `this(title.note title.diff)
    ==
  ++  peek
    |=  p=path
    ^-  cage
    !!
  --
--
