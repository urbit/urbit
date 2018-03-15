:: An index of all own collections
/-  collections
/+  old-zuse
/=  cols  /:  /===/web/collections
             /^  (map knot config:collections)  /_  /collections-config/
^-  manx
=,  old-zuse
;div(class "container")
  ;a(href "/~~/pages/nutalk/stream/create")
    ;button(class "btn btn-secondary", type "button")
      Create Stream →
    ==
  ==
  ;a(href "/~~/pages/nutalk/collection/create")
    ;button(class "btn btn-tetiary", type "button")
      Create Collection →
    ==
  ==
  ;*  %+  turn
        %+  sort
          ~(tap by cols)
        |=  [a=(pair knot config:collections) b=(pair knot config:collections)]
        (gth (unt (slav %da p.a)) (unt (slav %da p.b)))
      |=  [t=knot con=config:collections]
      ;div(class "row")
        ;div(class "da row col-md-12")
          ;a(href "/~~/collections/{(trip t)}"): {(trip t)}
        ==
        ;div(class "collection-title row col-md-12")
          ;h1: {(trip desc.con)}
        ==
      ==
==

