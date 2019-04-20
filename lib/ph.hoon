::
::::  /hoon/ph/lib
  ::
/-  aquarium
=,  aquarium
|%
+$  ph-input
  [now=@da who=ship uf=unix-effect]
::
++  ph
  |*  a=mold
  |%
  ++  ph-output  (ph-output-raw a)
  ++  ph-output-raw
    |*  a=mold
    $~  [& ~ %done *a]
    $:  thru=?
        events=(list ph-event)
        $=  next
        $%  [%wait ~]
            [%cont self=(data-raw a)]
            [%fail ~]
            [%done value=a]
        ==
    ==
  ::
  ++  data  (data-raw a)
  ++  data-raw
    |*  a=mold
    $-(ph-input (ph-output-raw a))
  ::
  ++  return
    |=  arg=a
    ^-  data
    |=  ph-input
    [& ~ %done arg]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(data-raw b) fun=$-(b data)]
    ^-  data
    |=  input=ph-input
    =/  b-res=(ph-output-raw b)
      (m-b input)
    ^-  ph-output
    :+  thru.b-res  events.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail ~]
      %done  [%cont (fun value.next.b-res)]
    ==
  --
--
