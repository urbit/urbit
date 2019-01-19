::
::::
  ::
/+  collections
/=  gas  /$  fuel:html
=/  sho=@t  (fall (~(get by qix.gas) %show) %default)
|=  [raw=(unit raw-item:collections) pax=path]
=/  body  ?~(raw '' data.u.raw)
=/  front  ?~(raw ~ meta.u.raw)
=/  lastmod  (fall (~(get by front) %last-modified) ~.missing-date)
^-  manx
;div
  ;div
    =urb-component  "TopicCreatePage"
    =urb-ship   "{(scow %p p.bem.gas)}"
    =urb-claypath  "{<pax>}"
    =urb-content    "{(trip body)}"
    =urb-show       "{(trip sho)}"
    =urb-lastedit   "{(trip lastmod)}";
  ;input(type "hidden", name "urb-header", value "collection-write", station "query");
==
