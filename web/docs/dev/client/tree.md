# `:tree`

`:tree` is the web filesystem interface. Odds are this file has been rendered for you by `:tree`. 

`:tree` is a single-page app that uses a backend in `/home/tree` to load
contents from `%clay` as the user navigates around as `%json`. The frontend
lives in `/home/pub/tree` and is a fairly straightforward
[React](https://facebook.github.io/react/) /
[Flux](https://facebook.github.io/flux/) app.

## Frontend

The frontend code for `:tree` can be found in `/home/pub/tree/src/`.

### CSS

The CSS is written in [Stylus](https://learnboost.github.io/stylus/). The main entry point is `main.styl` and can be compiled with `stylus main.styl` which should output a `main.css`

### JS

The JS is written in [CoffeeScript](http://coffeescript.org/) and packaged with
[Browserify](http://browserify.org/). The main entry point is `main.coffee` and
is compiled with `browserify -t coffeeify main.coffee > main.js`. You'll need
to `npm install` first.

Each page is loaded as JSON and then rendered using React on the page. This
allows us to write JSX in our markdown to implement simple components. Check
out `/home/pub/tree/src/js/components` to see the component library. 

You'll notice that some of these doc pages use things like `<list>` in the raw markdown files. 

## JSON API
Async provides loading by schema

`{path name sein sibs next prev}` are all immediately accesible from the store

a `getPath` method, if present (defaulting to current url), is used to determine the query root node.

## JSON Internals

### `/[desk]/tree/{path}.json`
`tree/json.hook` accepts a query string schema `q` in light noun encoding

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
