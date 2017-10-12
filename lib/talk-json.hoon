::
::::  /lib/talk-json/hoon
  ::
/-    talk
::
::>  rules for sur-json conversion: (proposal, adhered to below)
::>  1. to save the devs from having to learn multiple interfaces, try to match
::>     the hoon structure as closely as possible, including attribute names.
::>  2. if these names are p/q/r, slightly more semantic naming is preferred.
::>  2. when dealing with $%, the tag is the variable name. ie `{$x ...}` -> `x`
::>     for tagged unions with one piece of data, `x` holds that value.
::>     for tagged unions with multiple pieces of data, `x` is an object.
::>  3. ...
::
=,  talk
|%
++  en-json                                             ::>  sur to json
  |%
  ++  spec                                              ::>  speech
  --
::
++  de-json                                             ::>  json to sur
  =,  dejs-soft:format
  |%
  ::TODO  these first few should maybe make their way
  ::      into the stdlib...
  ++  as                                                ::>  array as set
    |*  a/fist
    (cu ~(gas in *(set _(need *a))) (ar a))
  ::
  ++  dank                                              ::>  tank
    ^-  $-(json (unit tank))
    %-  of  :~
      leaf+sa
      palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
      rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
    ==
  ::
  ::
  ++  dist
    ^-  $-(json (unit diff-status))
    %-  of  :~
      full+(ot pec+(su pres) man+huma ~)
      presence+(su pres)
      human+dihu
      remove+ul
    ==
  ::
  ++  dihu
    ^-  $-(json (unit diff-human))
    %-  of  :~
      full+huma
      handle+(mu so)
      true+
    ==
  ::
  ::>  ||
  ::>  ||  %circles
  ::>  ||
  ::>    messaging targets and their metadata.
  ::+|
  ::
  ++  circ                                              ::>  circle
    ^-  $-(nail (like circle))
    ;~((glue fas) ;~(pfix sig fed:ag) urt:ab)
  ::
  ++  pres                                              ::>  presence
    ^-  $-(nail (like presence))
    (perk %gone %idle %hear %talk ~)
  ::
  ++  huma                                              ::>  human
    ^-  $-(json (unit human))
    %-  ot  :~
      han+(mu so)
      tru+(mu true)
    ==
  ::
  ++  true                                              ::>  truename
    ^-  $-(json (unit truename))
    (ot fir+so mid+(mu so) las+so ~)
  ::
  ::>  ||
  ::>  ||  %message-data
  ::>  ||
  ::>    structures for containing main message data.
  ::+|
  ::
  ++  thot                                              ::>  thought
    ^-  $-(json (unit thought))
    %-  ot  :~
      uid+seri
      aud+audi
      wen+di
      sep+spec
    ==
  ::
  ++  spec                                              ::>  speech
    ^-  $-(json (unit speech))
    %-  of  :~
      lin+(ot pat+bo txt+so ~)
      url+(su aurf:de-purl:html)
      exp+(ot exp+so res+(ar dank) ~)
      ire+(ot top+seri sep+spec ~)
      fat+(ot tac+atta sep+spec ~)
      inv+(ot inv+bo cir+(su circ) ~)
    ==
  ::
  ++  atta                                              ::>  attache
    ^-  $-(json (unit attache))
    %-  of  :~
      name+(ot nom+so tac+atta ~)
      text+(ar so)
      tank+(ar dank)
    ==
  ::
  ::>  ||
  ::>  ||  %message-metadata
  ::>  ||
  ::     structures for containing message metadata.
  ::+|
  ::
  ++  seri                                              ::>  serial
    ^-  $-(json (unit serial))
    (ci (slat %uv) so)
  ::
  ++  audi                                              ::>  audience
    ^-  $-(json (unit audience))
    (as (su circ))
  --
--
