/-  collections
/+  colls
/=  gas  /$  fuel:html
/=  configs  /:  /===/web/collections
            /^  (map knot config:collections)  /_  /collections-config/
=/  id  %+  fall
          %-  %~  get
                  by
                  qix.gas
              ==
          %coll
        ''
:: get either the indicated config or the latest
=/  config-pair/(pair knot config:collections)
?:  =(id 'latest')
  =/  sorted-configs  %+  sort
                        ~(tap by configs)
                      |=  [a=(pair knot *) b=(pair knot *)]
                      (dor:colls p.a p.b)
  :: ew
  i:-.sorted-configs
:-  id
%-  need
%-  %~  get
      by
      configs
  ==
id
~&  [%config-pair desc.q.config-pair]
^-  manx
;div
  ;div.topic-info.mb-4
    ;div.row.coll-title
      {(trip desc.q.config-pair)} /
    ==
    ;div.row.mod.text-mono
      ; @now
    ==
  ==
  ;div(urb-component "TopicCreatePage", urb-coll "{(trip p.config-pair)}");
==
