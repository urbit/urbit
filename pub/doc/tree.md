# Tree

`:tree` is the web filesystem interface.

# Data retrieval interface
Async provides loading by schema

`{path name sein sibs next prev}` are all immediately accesible from the store

a `getPath` method, if present (defaulting to current url), is used to determine the query root node.

# Internals

something something coffeescript

## `/[desk]/tree/{path}.json`
tree/json.hook accepts a query string schema `q` in light noun encoding¹

    ++  schema  (dict ,[term $|(mark schema)])
    ++  dict    |*(a=_,* $&([a (dict a)] a))

which is normalized and type-checked(request types are `%t` text, `%r` html-derived tree, and `%j` arbitrary json) to a `query` list of
-  `[%kids query]`, the only recursive value, which executes for all subpaths
   XX descent is only currently supported to a single level as a performance optimization
-  `[%name %t]`, the node name
-  `[%path %t]`, the current path
-  `[%snip %r]`, a snippet, extracted via `react-snip-json`
-  `[%head %r]`, the first `<h1/>`, extracted via `react-head-json`
-  `[%body %r]`, the `react-json` body
-  `[%meta %j]`, json frontmatter per the `mdy` mark definition

Per this specification, the current access points will be migrated:
-  (json.hook)`""` to `body:'r' kids:{name:'t'}` `"?body.r__kids_name.t"` 
-  `"?kids"` to `kids:{name:'t' body:'r'}` `"?kids_name.t_body.r"`
-  `"?snip"` to `kids:{name:'t' snip:'r' head:'r' meta:'j'}` `"?kids_name.t_snip.r_head.r_meta.j"`

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

XX Curtis, this can be full coin `%many` if you wish.
