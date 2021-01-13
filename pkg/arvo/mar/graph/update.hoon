/+  *graph-store
=*  as-octs  as-octs:mimes:html
::
|_  upd=update
++  grad  %noun
++  grow
  |%
  ++  noun  upd
  ++  json  (update:enjs upd)
  ++  resource
    ?+  -.q.upd  !!
      ?(%run-updates %add-nodes %remove-nodes %add-graph)  resource.q.upd
      ?(%remove-graph %archive-graph %unarchive-graph)     resource.q.upd
      ?(%add-tag %remove-tag)                              resource.q.upd
      ?(%add-signatures %remove-signatures)                resource.uid.q.upd
    ==
  ++  mime  [/application/x-urb-graph-update (as-octs (jam upd))]
  --
::
++  grab
  |%
  ++  noun  update
  ++  json  update:dejs
  ++  mime  |=([* =octs] ;;(update (cue q.octs)))
  --
--
