/-  collections
/+  colls
/=  gas  /$  fuel:html
/=  all-configs  /:  /===/web/collections
            /^  (map knot config:collections)  /_  /collections-config/
=/  sorted-configs  %+  sort
                    ~(tap by all-configs)
                  |=  [a=(pair knot *) b=(pair knot *)]
                  (dor:colls p.a p.b)
=/  config  -.sorted-configs
^-  manx
;div.collection-index
  ;h1: {(trip desc.q.i.config)}
  ;*  ?:  (authed:colls gas)
    ;=
      ;div.row
        ;a(href "/~~/pages/nutalk/collection/post?coll=latest")
          ;button.btn.btn-secondary
            ; Write →
          ==
        ==
        ;a.ml-4.mt-2.text-600(href "")
            ; Settings →
        ==
      ==
    ==
  ;=
    ;div(data-component "Subscribe", data-circle "latest");
  ==
==
