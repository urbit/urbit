/-  collections
/=  gas  /$  fuel:html
/=  configs  /:  /===/web/collections
            /^  (map knot config:collections)  /_  /collections-config/
=/  config  %-  %~  get 
                    by 
                    configs
                ==
            %+  fall
              %-  %~  get 
                      by 
                      qix.gas 
                  ==
              %coll
            ''
^-  manx
;div
  ;div.topic-info.mb-4
    ;div.row.coll-title
      {(trip desc:(need config))} /
    ==
    ;div.row.mod.text-mono
      ; @now
    ==
  ==
  ;div(data-component "TopicCreatePage");
==
