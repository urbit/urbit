/+  *test, pinto
::
=/  ford  ((pinto) ~nul %home ~1234.5.6 *sley)
=/  ca  (by-clock:contain hoon-cache-key:ford vase)
=/  hoon-cache  (clock hoon-cache-key:ford vase)  ::  TODO: broken import?
|%
++  test-make-ride  ^-  tang
  =|  =hoon-cache
  =.  hoon-cache
    %+  ~(put ca hoon-cache)  [%slim -:!>([%foo 17]) (ream '-')]
    !>((~(mint ut -:!>([%foo 17])) %noun (ream '-')))
  =.  hoon-cache
    (~(put ca hoon-cache) [%ride !>([%foo 17]) (ream '-')] !>(%foo))
  ::
  =/  m  (fume:ford ,cage)
  =/  =output:m
    ((make:ford %ride $+noun+!>([%foo 17]) (ream '-')) ~ *^hoon-cache)
  ::
  ;:  welp
    %+  expect-eq
      !>  %foo
      ?>(?=(%done -.next.output) q.value.next.output)
  ::
    %+  expect-eq
      !>  `(list @tas)`(turn ~(tap in ~(key by lookup.s.output)) head)
      !>  `(list @tas)`~[%ride %slim]
  ==
--
