`:tree` is the web filesystem interface.

# Internals

somethins something coffeescript

## `/[desk]/tree/{path}.json`
tree/json.hook accepts a schema in light noun encoding¹

    ++  schema  (dict ,[term $|(mark schema)])
    ++  dict    |*(a=_,* $&([a (dict a)] a))

which is normalized and type-checked(request types are `%t` text, `%r` html-derived tree, and `%j` arbitrary json) to a `query` list of
-  `[%kids query]`, the only recursive value, which executes for all subpaths
   XX descent is only currently supported to a single level as a performance optimization
-  `[%name %t]`, the node name
-  `[%snip %r]`, a snippet, extracted via `react-snip-json`
-  `[%head %r]`, the first header, extracted via `react-head-json`
-  `[%body %r]`, the `react-json` body
-  `[%meta %j]`, json frontmatter per the `mdy` mark definition
-  `[%index %j]`, a search index generated from the first header of each file in a path below the request one (SLOW)

Per this specification, the current access points will be migrated:
-  (json.hook)`""` to `body:'r' kids:{name:'t'}` `"?body.r__kids_name.t"` 
-  `"?kids"` to `kids:{name:'t' body:'r'}` `"?kids_name.t_body.r"`
-  `"?snip"` to `kids:{name:'t' body:'r' head:'r' meta:'j'}` `"?kids_name.t_body.r"`
-  `"?heads"` to `index:'j'` `"?index.j"`

¹In the examples,

    =<  (cook to-noun (cook to-tree apex))
    |%
    ++  data  $|(term [n=@ l=data r=data])
    ++  apex  ;~(plug sym (star ;~(plug delim sym)))
    ++  delim  ;~(pose (cold 0 dot) (cook lent (plus cab)))
    ++  to-noun  |=(a=data ?@(a a [$(a l.a) $(a r.a)]))
    ++  to-tree
      |=  [acc=data a=(list ,[p=@u q=term])]
      %+  roll  a  =<  .(acc ^acc)
      |=  [[n=@u v=term] acc=data]
      ?@  acc            [n acc v]
      ?:  (gth n n.acc)  [n acc v]
      acc(r $(acc r.acc))
    --

Curtis, this can be full coin %many if you wish.
