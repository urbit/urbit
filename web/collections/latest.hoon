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
;div.container
  ;input(type "hidden", name "urb-header", value "collection-index", title "{(trip desc.q.i.config)}", id "{(trip p.i.config)}", ship "{(scow %p p.bem.gas)}");
==
