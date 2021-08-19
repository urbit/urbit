/-  ms=metadata-store
/+  crunch
:-  %say
|=  [[now=@da * bec=beak] [csv-path=path from=@da ~] [to=@da groups=(list path) content=(unit ?) ~]]
=/  our=@p  p.bec
:: check given path has `csv` mark
::
?>  =(%csv (snag (dec (lent csv-path)) csv-path))
:: get all graph associations ship is a part of
::
=/  associations=associations:ms
  (~(scry-graph-associations crunch [our now]))
:: filter by input groups, if any (default: all from scry)
::
=/  filtered-associations=associations:ms
  ?~  groups
    associations
  %+  filter-associations-by-group-resources.crunch
    associations
  (paths-to-resources.crunch groups)
:: walk graphs to extract content
::
=/  file-content=wain
  %:  ~(walk-graph-associations crunch [our now])
    filtered-associations
    ?~  content  %.n  u.content
    from
    ?:  =(*@da to)  now  to
  ==
[%helm-pass (note-write-csv-to-clay.crunch csv-path file-content)]
