# Tree

`:tree` is the web filesystem interface.

# Data retrieval interface
Async provides loading by schema

`{path name sein sibs next prev}` are all immediately accesible from the store

a `getPath` method, if present (defaulting to current url), is used to determine the query root node.

# Internals

something something coffeescript

## `/[desk]/tree/{path}.json`
tree/json.hook accepts a query string schema `q` in light noun encoding

    ++  schema  (dict ,[term $|(mark schema)])
    ++  dict    |*(a=_,* $&([a (dict a)] a))

which is normalized and type-checked to a `query` list of
-  `[%kids query]`, the only recursive value, which executes for all subpaths
   XX descent is only currently supported to a single level as a performance optimization
-  `[%name %t]`, the node name
-  `[%path %t]`, the current path
-  `[%snip %r]`, a snippet, extracted via `react-snip-json`
-  `[%head %r]`, the first `<h1/>`, extracted via `react-head-json`
-  `[%body %r]`, the `react-json` body
-  `[%meta %j]`, json frontmatter per the `mdy` mark definition

The request types above are `%t` text, `%r` html-derived tree, and `%j`
arbitrary json; an example query, used by the main content renderer, is
`"q=body.r__kids_name.t"` (`body:'r' kids:{name:'t'}` )
