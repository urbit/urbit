/+  *test
=,  ford
|%
::  +expect-schematic: assert a +schematic:ford is what we expect
::
::    Since Ford requests contain types, we can't do simple
::    equality checking. This function handles all the different
::    kinds of +schematic:ford, dealing with types as necessary.
::
++  expect-schematic
  |=  [expected=schematic actual=vase]
  ^-  tang
  ::
  ?^    -.expected
    ::  specialize :actual's type to be an autocons
    ::
    =.  actual
      %+  slap  actual
      ^-  hoon
      :+  %wtbn
        [%wtts [%base %cell] [%& 2]~]
      [%wing [%& 1]~]
    ::
    %+  weld
      $(expected -.expected, actual (slap actual [%limb %head]))
    $(expected +.expected, actual (slap actual [%limb %tail]))
  ::
  =.  actual  (specialize-by-tag -.expected actual)
  ::
  ?-    -.expected
      %$
    %+  weld
      ^-  tang
      %+  expect-eq
        `vase`[`type`[%atom %tas ~] `term`p.literal.expected]
      `vase`(slap actual `hoon`[%wing ~[%p %literal]])
    ::
    %+  expect-eq
      q.literal.expected
    ((hard vase) q:(slap actual [%wing ~[%q %literal]]))
  ::
      %pin
    ::
    %+  weld
      %+  expect-eq
        [[%atom %da ~] date.expected]
      (slap actual [%limb %date])
    ::
    $(expected schematic.expected, actual (slap actual [%limb %schematic]))
  ::
      %alts
    =.  actual  (slap actual [%limb %choices])
    ::
    =|  res=tang
    |-  ^+  res
    ?~  choices.expected
      ::  make sure there aren't any extra :choices in :actual
      ::
      =.  actual  (assert-null actual)
      ::  no more choices; produce any errors we got
      ::
      res
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    =.  actual  (assert-not-null actual)
    ::  recurse on the first sub-schematic
    ::
    =.  res
      %+  weld  res
      ^$(expected i.choices.expected, actual (slap actual [%limb %i]))
    ::  recurse on the rest of the sub-schematics
    ::
    $(choices.expected t.choices.expected, actual (slap actual [%limb %t]))
  ::
      %bake
    ::
    %+  weld
      %+  expect-eq
        [[%atom %tas ~] renderer.expected]
      (slap actual [%limb %renderer])
    ::
    %+  weld
      %+  expect-eq
        [-:!>(*coin) query-string.expected]
      (slap actual [%limb %query-string])
    ::
    %+  expect-eq
      [-:!>(*rail) path-to-render.expected]
    (slap actual [%limb %path-to-render])
  ::
      %bunt
    ::
    %-  expect-eq
    :_  %+  slap  actual
        ^-  hoon
        [%clhp [%limb %disc] [%limb %mark]]
    ^-  vase
    :_  [disc mark]:expected
    ^-  type
    :+  %cell
      [%face %disc -:!>(*disc)]
    [%face %mark -:!>(*term)]
  ::
      %call
    %+  weld
      $(expected gate.expected, actual (slap actual [%limb %gate]))
    $(expected sample.expected, actual (slap actual [%limb %sample]))
  ::
      %cast
    ;:  weld
      %+  expect-eq
        [-:!>(*disc) disc.expected]
      (slap actual [%limb %disc])
    ::
      %+  expect-eq
        [-:!>(*term) mark.expected]
      (slap actual [%limb %mark])
    ::
      $(expected input.expected, actual (slap actual [%limb %input]))
    ==
  ::
      %core
    %+  expect-eq
      [-:!>(*rail) source-path.expected]
    (slap actual [%limb %source-path])
  ::
      %diff
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      $(expected start.expected, actual (slap actual [%limb %start]))
    $(expected end.expected, actual (slap actual [%limb %end]))
  ::
      %dude
    =/  error-result=tank  (error.expected)
    %+  expect-eq
      [-:!>(*tank) error-result]
    (slap actual `hoon`[%kttr [%like ~[%error] ~]])
  ::
      %hood
    %+  expect-eq
      [-:!>(*rail) source-path.expected]
    (slap actual [%limb %source-path])
  ::
      %join
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      %+  expect-eq
        [[%face %mark [%atom %tas ~]] mark.expected]
      (slap actual [%limb %mark])
    ::
    %+  weld
      $(expected first.expected, actual (slap actual [%limb %first]))
    $(expected second.expected, actual (slap actual [%limb %second]))
  ::
      %list
    =.  actual  (slap actual [%limb %schematics])
    ::
    =/  schematics  schematics.expected
    ::
    =|  res=tang
    |-  ^+  res
    ?~  schematics
      ::  make sure there aren't any extra :choices in :actual
      ::
      =.  actual  (assert-null actual)
      ::  no more choices; produce any errors we got
      ::
      res
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    =.  actual  (assert-not-null actual)
    ::  recurse on the first sub-schematic
    ::
    =.  res
      %+  weld  res
      ^$(expected i.schematics, actual (slap actual [%limb %i]))
    ::  recurse on the rest of the sub-schematics
    ::
    $(schematics t.schematics, actual (slap actual [%limb %t]))
  ::
      %mash
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      %+  expect-eq
        [[%face %mark [%atom %tas ~]] mark.expected]
      (slap actual [%limb %mark])
    ::
    %+  weld
      %+  expect-eq
        [[%face %disc-first -:!>(*disc)] disc.expected]
      (slap actual [%wing ~[%disc %first]])
    ::
    %+  weld
      %+  expect-eq
        [[%face %mark-first -:!>(*disc)] disc.expected]
      (slap actual [%wing ~[%mark %first]])
    ::
    %+  weld
      %_  $
        expected  schematic.first.expected
        actual    (slap actual [%wing ~[%schematic %first]])
      ==
    ::
    %+  weld
      %+  expect-eq
        [[%face %disc-second -:!>(*disc)] disc.expected]
      (slap actual [%wing ~[%disc %second]])
    ::
    %+  weld
      %+  expect-eq
        [[%face %mark-second -:!>(*disc)] disc.expected]
      (slap actual [%wing ~[%mark %second]])
    ::
    %_  $
      expected  schematic.second.expected
      actual    (slap actual [%wing ~[%schematic %second]])
    ==
  ::
      %mute
    %+  weld
      $(expected subject.expected, actual (slap actual [%limb %subject]))
    ::
    =/  mutations  mutations.expected
    =.  actual     (slap actual [%limb %mutations])
    ::
    =|  index=@ud
    =|  res=tang
    |-  ^+  res
    ?~  mutations
      ::  make sure there aren't any extra :choices in :actual
      ::
      =.  actual  (assert-null actual)
      ::  no more choices; produce any errors we got
      ::
      res
    ::  :expected isn't empty yet; make sure :actual isn't either
    ::
    =.  actual  (assert-not-null actual)
    ::  recurse on the first sub-schematic
    ::
    =.  res
      %+  weld
        res
      %+  weld
        %+  expect-eq
          [[%face (cat 3 'wing-' (scot %tas index)) -:!>(*wing)] p.i.mutations]
        (slap actual [%wing ~[%p %mutations]])
      ::
      ^$(expected q.i.mutations, actual (slap actual [%wing ~[%q %mutations]]))
    ::  recurse on the rest of the sub-schematics
    ::
    $(mutations t.mutations, actual (slap actual [%limb %t]))
  ::
      %pact
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      $(expected start.expected, actual (slap actual [%limb %start]))
    $(expected diff.expected, actual (slap actual [%limb %diff]))
  ::
      %path
    %-  expect-eq
    ::
    :_  %+  slap  actual
        [%cnls [%limb %disc] [%limb %prefix] [%limb %raw-path]]
    ::
    ^-  vase
    :_  [disc prefix raw-path]:expected
    ^-  type
    :+  %cell
      [%face %disc -:!>(*disc)]
    :+  %cell
      [%face %prefix [%atom %tas ~]]
    [%face %raw-path [%atom %tas ~]]
  ::
      %plan
    %+  weld
      %+  expect-eq
        [[%face %path-to-render -:!>(*rail)] path-to-render.expected]
      (slap actual [%limb %path-to-render])
    ::
    %+  weld
      %+  expect-eq
        [[%face %query-string -:!>(*coin)] query-string.expected]
      (slap actual [%limb %query-string])
    ::
    %+  expect-eq
      [[%face %scaffold -:!>(*scaffold)] scaffold.expected]
    (slap actual [%limb %scaffold])
  ::
      %reef
    %+  expect-eq
      [[%face %disc -:!>(*disc)] disc.expected]
    (slap actual [%limb %disc])
  ::
      %ride
    %+  weld
      %+  expect-eq
        [[%face %formula -:!>(*hoon)] formula.expected]
      (slap actual [%limb %formula])
    ::
    $(expected subject.expected, actual (slap actual [%limb %subject]))
  ::
      %same
    $(expected schematic.expected, actual (slap actual [%limb %schematic]))
  ::
      %scry
    %+  expect-eq
      [[%face %resource -:!>(*resource)] resource.expected]
    (slap actual [%limb %resource])
  ::
      %slim
    %+  weld
      %+  expect-eq
        !>(`?`%.y)
      ^-  vase
      :-  -:!>(*?)
      ^-  ?
      =<  -
      %+  ~(nets wa *worm)
        subject-type.expected
      q:(slap actual [%limb %subject-type])
    ::
    %+  expect-eq
      [[%face %formula -:!>(*hoon)] formula.expected]
    (slap actual [%limb %formula])
  ::
      %slit
    %+  weld
      %+  expect-eq
        gate.expected
      ((hard vase) q:(slap actual [%limb %gate]))
    ::
    %+  expect-eq
      sample.expected
    ((hard vase) q:(slap actual [%limb %sample]))
  ::
      ?(%vale %volt)
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      %+  expect-eq
        [[%face %mark -:!>(*term)] mark.expected]
      (slap actual [%limb %mark])
    ::
    %+  expect-eq
      [[%face %input -:!>(**)] input.expected]
    (slap actual [%limb %input])
  ::
      %walk
    %+  weld
      %+  expect-eq
        [[%face %disc -:!>(*disc)] disc.expected]
      (slap actual [%limb %disc])
    ::
    %+  weld
      %+  expect-eq
        [[%face %source -:!>(*term)] source.expected]
      (slap actual [%limb %source])
    ::
    %+  expect-eq
      [[%face %source -:!>(*term)] target.expected]
    (slap actual [%limb %target])
  ==
::  +specialize-by-tag: specialize a $% vase's type to a particular :tag
::
::    Vase equivalent of:
::
::    ```
::    ?>  ?=(<tag> -.value)  value
::    ```
::
++  specialize-by-tag
  |=  [tag=@tas =vase]
  ^+  vase
  ::
  %+  slap  vase
  ^-  hoon
  :+  %wtbn
    [%wtts [%leaf %tas tag] [%& 2]~]
  [%wing [%& 1]~]
::  +assert-null: specialize a vase to have a null type, crashing if invalid
::
::    Vase equivalent of:
::
::    ```
::    ?>  ?=(~ value)  value
::    ```
::
++  assert-null
  |=  =vase
  ^+  vase
  ::
  %+  slap  vase
  ^-  hoon
  :+  %wtbn
    [%wtts [%base %null] [%& 1]~]
  [%wing [%& 1]~]
::  +assert-not-null: specialize a vase to non-null type, crashing if invalid
::
::    Vase equivalent of:
::
::    ```
::    ?<  ?=(~ value)  value
::    ```
::
++  assert-not-null
  |=  =vase
  ^+  vase
  ::
  %+  slap  vase
  ^-  hoon
  :+  %wtld
    [%wtts [%base %null] [%& 1]~]
  [%wing [%& 1]~]
--
