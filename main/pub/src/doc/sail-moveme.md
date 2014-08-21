Structures
==========

The fundamental hoon structure used to generate XML is a manx. A manx is 
composed of a `marx` and a `marl`, representing an XML node: 


```
++  manx  ,[g=marx c=marl]                              ::  XML node
++  marx  ,[n=mane a=mart]                              ::  XML tag
++  mane  $|(@tas [@tas @tas])                          ::  XML name/space
```

Names, manes
------------

The most basic `manx` consists of an XML node name, called a `mane`, an empty 
`marl`, and an empty `mart`:

```
~zod/try=> ;div
[[%div ~] ~]
~zod/try=> ;namespaced_div;
[[[%namespaced %div] ~] ~]
```

This represents an XML node, an opening and a closing tag with no attributes
nor children:

```
~zod/try=> (xmlt | ;div; "") 
"<div></div>"
~zod/try=> (xmlt | ;namespaced_div; ~)
"<namespaced:div></namespaced:div>"
```

**Note**: `++xmlt` takes three arguments: a loobean determening whether text should be
xml-escaped, the `manx` to be rendered, and a tape onto which to append the
results. `<script>` and `<style>` tags are unescaped by default. For more
information, see the XML documentation in Volume 3, section 3bD.

However, `%gall` applications operate on nouns of the type `[%hymn manx]`,
making direct calls to `++xmlt` rare.

Attributes and child nodes
--------------------------

To represent XML nodes with children and/or attributes, `marl` and `mart` are
used respectively:

```
++  marl  (list manx)                                   ::  XML node list
++  mart  (list ,[n=mane v=tape])                       ::  XML attributes
```

Each child node is simply another manx, and attributes are pairs of key `mane`
to value `tape`. `marl` and `mart` are lists of these respective values. 

Wide Form
=========

Children
--------

The XML templating syntax begins with a `';'` followed by a `term`.
In wide form, a child node is appended with a `':'`:

```
~zod/try=> ;div:p;
[[%div ~] [[%p ~] ~] ~]
~zod/try=> :poxo ;div:p;
<div><p></p></div>
```

**Note:**`/~zod/try/1/bin/poxo/hoon` is a shell script that applies `(curr (cury xmlt |) ~)` 
to a `manx`, rendering it as a `tape`.

Multiple nodes can be represented inside of parentheses, separated by single spaces:

```
~zod/try=> ;div:(p ul:(li li))
[[%div ~] [[%p ~] ~] [[%ul ~] [[%li ~] ~] [[%li ~] ~] ~] ~]
~zod/try=> :poxo ;div:(p ul:(li li))
<div><p></p><ul><li></li><li></li></ul></div>
```

Tags must always be closed. In wide form, this is done either with a `';'` if the 
node contains no children, or with a `':'`, followed by one or more children.

In wide form, terminating `';'`s are implied: `;div:(p p p)` is equivalent to
`;div:(p; p; p;)`.
However, in instances where there are a chain of nodes where each node 
contains only one child, as with `;div:p;`, it is good practice to include the
final `';'` to avoid confusion with an unclosed single tag.

Attributes
----------

Attributes are included as `','`-separated pairs of `mane` and and a `tape` producing twig. These
pairs are contained within parentheses, which immediately follow a `mane`:

```
~zod/try=> ;div(id "me", class "elem");
[[%div [%id "me"] [%class "elem"] ~] ~]
~zod/try=> :poxo ;div(class "elem", id "me"); ~))
<div class="elem" id="me"></div>
```

The characters `'.'`, `'#'`, and `'/'` are short forms of the attributes `class`, 
`id`, and `href` respectively:

```
~zod/try=> ;div.elem#me;
[[%div [%id "me"] [%class "elem"] ~] ~]
~zod/try=> ;a/"//google.com";
[[%a [%href "//google.com"] ~] ~]
```

As demonstrated above, the short forms of both `class` and `id` accept unquoted
terms instead of tapes.
The resulting XML is, of course, identical:

```
~zod/try=> :poxo ;div.elem#me;
<div id="me" class="elem"></div>
```

Character data
--------------

Nodes can also contain text:

```
~zod/try=> ;p:"contents of paragraph"
[[%p ~] [[%~. [%~. "contents of paragraph"] ~] ~] ~]
~zod/try=> :poxo ;p:"contents of paragraph"
<p>contents of paragraph</p>
```

Text is stored as a `mars` (a cdata node):

```
++  mars  ,[t=[n=%$ a=[i=[n=%$ v=tape] t=~]] c=~]       ::  XML cdata
```

Techincally, a `mars` is a `manx` with an empty `mane`, one empty-named attribute 
containing the text, and no children. Equivalently, it is a `_:/(*tape)`.

To insert text with newlines, it is surrounded in triple quotes:

```
;code:"""
      block
      with newlines
      """
```

This is rendered as:

```
<code>block
with newlines</code>
```

Interpolation
-------------

Similar to the syntax for `tape` interpolation,

```
~zod/try=> "Tape with {(weld "inter" (trip %polate))}d text"
"Tape with interpolated text"
```

there exists a `manx` syntax to insert XML produced by twigs. The simplest form
is that of embedded nodes:

```
~zod/try=> ;p:"Text text ;{b "bolded text"} more text"
[ [%p ~]
  [[%~. [%~. "Text text "] ~] ~]
  [[%b ~] [[%~. [%~. "bolded text"] ~] ~] ~]
  [[%~. [%~. " more text"] ~] ~]
  ~
]
~zod/try=> :poxo ;p:"Text text ;{b "bolded text"} more text"
<p>Text text <b>bolded text</b> more text</p>
```

Notice that the syntax calls for a `marx`, followed by zero or more wide-form children 
`manx`, placed within `';{}'`.

Dynamic data can also be interpolated. This is done by prefixing the `{}` wrapped twig with one of the following glyphs: `-, +, *, or %`.

A `{}` prefixed by a `'-'` accepts a `tape`: 

```
~zod/try=>  ;p:"Paragraph with -{(weld "inter" (trip %polate))}d text"
[ [%p ~]
  [[%~. [%~. "Paragraph with "] ~] ~]
  [[%~. [%~. ~[~~i ~~n ~~t ~~e ~~r ~~p ~~o ~~l ~~a ~~t ~~e]] ~] ~]
  [[%~. [%~. "d text"] ~] ~]
  ~
]
```

If no glyph prefix is present, a`'-'` is assumed:

```
~zod/try=> ;p:"Paragraph with {(weld "inter" (trip %polate))}d text"
[ [%p ~]
  [[%~. [%~. "Paragraph with "] ~] ~]
  [[%~. [%~. ~[~~i ~~n ~~t ~~e ~~r ~~p ~~o ~~l ~~a ~~t ~~e]] ~] ~]
  [[%~. [%~. "d text"] ~] ~]
  ~
]
~zod/try=> :poxo ;p:"Paragraph with {(weld "inter" (trip %polate))}d text"
<p>Paragraph with interpolated text</p>
```

A prefix of `'+'` accepts a `manx`:

```
~zod/try=> ;p:"This text +{?:((gth 2 1) ;b:"be bold" ;i:"be scared")}"
[[%p ~] [[%~. [%~. "This text "] ~] ~] [[%b ~] [[%~. [%~. "be bold"] ~] ~] ~] ~]
~zod/try=> :poxo ;p:"This text +{?:((gth 2 1) ;b:"be bold" ;i:"be scared")}"
<p>This text <b>be bold</b></p>
```

A prefix of `'*'` accepts a `marl`:

```
~zod/try=> ;p:"Today *{(turn `wain`~[%live %love] |=(a=@tas ;span:"we {(trip a)}, "))}" 
[ [%p ~]
  [[%~. [%~. "Today "] ~] ~]
  [ i=[[%span ~] [[%~. [%~. "we "] ~] ~] [[%~. [%~. "live"] ~] ~] [[%~. [%~. ", "] ~] ~] ~]
      t
    [ i=[[%span ~] [[%~. [%~. "we "] ~] ~] [[%~. [%~. "love"] ~] ~] [[%~. [%~. ", "] ~] ~] ~]
      t=~
    ]
  ]
]
~zod/try=> :poxo ;p:"Today *{(turn `wain`~[%live %love] |=(a=@tas ;span:"we {(trip a)}, "))}" 
<p>Today <span>we live, </span><span>we love, </span></p>
```

A prefix of `'%'` accepts `$+(marl marl)`, a gate with both a sample and product of `marl`. The gate is then slammed by the nodes that follow it:

```
~zod/try=> ;p:"%{|=(a=marl (weld a a))}dup"
[ [%p ~]
  ~[[g=[n=%$ a=~[[n=%$ v="dup"]]] c=~] [g=[n=%$ a=~[[n=%$ v="dup"]]] c=~]]
]
~zod/try=> :poxo ;p:"%{|=(a=marl (weld a a))}dup"
<p>dupdup</p>
```

Interpolation can be disabled by replacing double quotes with single quotes.
For example, `;unescaped:'Kel {}'` becomes `<unescaped>Kel {}</unescaped>`,
and:

```
;script:'''
        function(){
          console.log(42)
        }
        '''
```

when rendered, is: 

```
<script>function(){
  console.log(42)
}</script>
```

Tall form
==========

Most development is done in tall form:

```
;html
  ;head
    ;title
      ; Hi
    ==
  ==
  ;body
    ;p
      ; Hello world
    ==
  ==
==
```

This produces the `manx`:

```
g=[n=%html a=~]
    c
  ~[
    [ g=[n=%head a=~]
      c=~[[g=[n=%title a=~] c=~[[g=[n=%$ a=~[[n=%$ v="Hi
"]]] c=~]]]]
    ]
    [ g=[n=%body a=~]
      c=~[[g=[n=%p a=~] c=~[[g=[n=%$ a=~[[n=%$ v="Hello world
"]]] c=~]]]]
    ]
  ]
]
```

When the `manx` is rendered, it produces:

```
<html><head><title>Hi
</title></head><body><p>Hello world
</p></body></html>
```

As previously mentioned, a tag must always be closed. In tall form, this is accomplished by a `'=='`, which should align directly beneath it; any node in between the two is a subnnode. 

As demonstrated in the example above, the tall form for character data
is opened by `'; '` (note the space). Unlike wide form, the text that follows
the `'; '` is not wrapped in quotes, but is instead terminated with a new
line. The syntax for interpolation remains the same.

The final way in which a tag may be closed is with `': '`, which is equivalent
to a `manx` with a single child declared by `'; '`. As with `'; '`, the text
 declared by the `': '` is terminated by a new line:

```
;p: par contents
:: <p>par contents</p>
```

Another syntax that is unique to tall form is`';='`, which describes a `marl`
with no tag:

```
!: 
=-  [- (xmll | - ~)]
;=
  ;p: node
  ;node(with "attribute");
==
```

This produces the `marl`:

```
[[[%p ~] [[%~. [%~. "node"] ~] ~] ~] [[%node [%with "attribute"] ~] ~] ~]
```


Notice how this renders two XML nodes without an outer tag:

```
 "<p>node</p><node with="attribute"></node>"
```

Tall interpolation
------------------

A ';' followed by an interpolation glyph(`-, +, *, or %`) and two or more spaces, 
accepts a twig and interpolates it into the surrounding structure:

```
;div#demo
  ;-  "foo"
  ;+  ;p:"bar"
  ;*  :-  ;p:"baz"
      ?:  &
        ~
      ~[;hi;]
  ;%  |=  a=marl
      (weld a a)
  ; dup
==
```

This produces the `manx`:

```
[ g=[n=%div a=~[[n=%id v="demo"]]]
      c
    ~[
      [g=[n=%$ a=~[[n=%$ v="foo"]]] c=~]
      [g=[n=%p a=~] c=~[[g=[n=%$ a=~[[n=%$ v="bar"]]] c=~]]]
      [g=[n=%p a=~] c=~[[g=[n=%$ a=~[[n=%$ v="baz"]]] c=~]]]
      [g=[n=%$ a=~[[n=%$ v="dup
"]]] c=~]
      [g=[n=%$ a=~[[n=%$ v="dup
"]]] c=~]
    ]
  ]
```

When the `manx` is rendered, it produces:

```
<div id="demo">foo<p>bar</p><p>baz</p>dup
dup
</div>
```

Attributes
----------

The tall form syntax for attributes is `'='` followed by a `mane` (the key), two 
or more spaces, and a tall twig that produces a tape: 

```
;div
  =id  "foo"
  =class  "bar"
  ;p: baz`
==
```

This produces the `manx`:

```
[ g=[n=%div a=~[[n=%id v="foo"] [n=%class v="bar"]]]
    c=~[[g=[n=%p a=~] c=~[[g=[n=%$ a=~[[n=%$ v="baz"]]] c=~]]]]
]
```

When rendered, this `manx` produces:

```
<div id="foo" class="bar"><p>baz</p></div>
```

The tall-form syntax can be combined with the wide-form attribute syntax (and all of its short forms):  

```
;html
  ;div#foo(tag "bar")
    =attr  "exceedingly value in here"
    ; tall form
    ;p: contents
  ==
==
```

This produces the `manx`:

```
[ [%html ~]
  [ [%div [%id "foo"] [%tag "bar"] [%attr "hella long value in here"] ~]
    [[%~. [%~. "tall form
"] ~] ~]
    [[%p ~] [[%~. [%~. "contents"] ~] ~] ~]
    ~
  ]
  ~
]
```

When rendered, this `manx` produces:

```
"<html><div id="foo" tag="bar" attr="hella long value in here">tall form
<p>contents</p></div></html>"
```



