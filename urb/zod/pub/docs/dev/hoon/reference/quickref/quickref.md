
##Quick references

###Lexical syntax

####Comments

`::` all code following '::' on a line is a comment.

By convention, comments should begin on either column 39 or
column 57.

####Runes (Expressions)

- Most runes have a predetermined amount of children, and thus do not need to be closed.

- Runes that accept an arbitrary number of children are typically closed with
  `==`, with the exception being the `|` runes, some of which are closed with
  `--`.

- In some cases, the second glyph denotes how many 'children' a rune accepts:

  `-` two 
  `+` three
  `^` four

- Backstep pattern: in order to avoid excess parentheses as well as code that
  flows off the page, the last child of a rune should fall directly below it.
  The first child should be the furthest to the right, with every subsequent
  rune 'backstepping' two spaces towards the last child.
  
  XX

The first glyph in a rune defines a rough semantic category of expressions:

##### ? : Conditional statements

`?&` Logical "and" 

  - Irregular: `&`

`?|` Logical "or"

  - Irregular: `|.`

`?!` Logical "not"

  - Irregular: `!`


`?:` - If-then-else

`?.` - Inverted if-then-else

`?@` - If atom-then-else

`?^` - If cell-then-else


`?-` - Switch (on type)

`?+` - Swith (on type) with default


`?>` - Assert `p` is false.

`?<` - Assert `p` is true.


`?~` - if null-then-else Note: don't forget, changes type
`?=` - is `q` of type `p`Note: don't forget, changes the type

##### % : Code execution

`%-` Call `p` with 1 arg

  - Irregular: `(p q)` Note: irregular form can accept n children.

`%.` Inverted `%-`: call `q` with arg `p`

`%+` Call `p` with 2 args

`%^` Call `p` with 3 args


`%~` Call one arm `p` within object/core `q` with arg `r`

  - Irregular: `~(p q r)`

`%*` Same as `%~` except `s` is a list of changes that can be made anywhere in
the context, not just to input data.


`%=` Evaluate `p` with changes specified in `q`

  - Irregular: `a(p q.p, q p.a)`, where `a` is the code to be executed with changes
    specified in the  `()` 

`%_` Same as `%=`, except that product is cast back to type of `p`.

##### : : Tuple construction

`:-` Tuple of two

  - Irregular `[p q ..]` Note: the irregular form can make tuples of arbitrary size

`:_` Inverted tuple of two

`:+` Tuple of three

`:^` Tuple of four


`:*` Tuple of arbitrary number of elements, closed with `==`

`:~` Null terminated tuple of arbitrary number of elements, closed with `==`.


`:/` XX

##### | : Object aka "core" construction

`|%` Generic core, closed with `--`

`|_` Core with input data (aka 'sample), closed with `--`


`|=` Function with type-checking at compile time (aka 'dry gate'); called automatically

`|*` Function with type-checking at run time (aka 'wet gate'); called automatically

`|.` Function (like `|=`) with no argument; called automatically

`|-` Function with no arguments (like `|.`); not called automatically


`|/` Same as `|_`, but with well typed output, closed with `--`

`|^` Same as `|-`, but can contain internal arms, closed with `--`

`|+` Function with unreadable argument(s) (aka 'sample'), so as to avoid type conflicts

`|?` Same as `|_` but both the input and 'context' are hidden to avoid type conflicts

##### $ : Type definitions

`$|` Union between atom and cell

`$_` 'Unbunt'; create type from example

  - Irregular: `_p`, but only when in mold syntax

`$:` Tuple of types (aka 'molds')

  - Irregular: `{i.p i.t.p i.t.t.p}`

`$%` Tagged union

`$+` Type of function (type signature)

`$&` Union of two cells: one w/cellular head and other w/atomic head

`$*` 'Bunt'; produce default value of a type

  - Irregular: `*([mold])`

`$=` Add variable name; technically changes the type as well

  - Irregular: `p=q`, but only when in mold syntax

`$?` Union of types; non-empty list of types ('molds'). Default value ('bunt') is the value of the first element

  - Irregular: `?([list of molds])`, but only when in mold syntax

##### = : Subject ("context") modification XX

`=>` Resolve code; use the product of `p` as the subject of `q`

  - Irregular: `p:q`

`=<` Inverted `=>`, used for code readability


`=|` Push default value of type ('mold') onto subject 

`=+` Push variable/value onto subject

`=-` Inverted `=+`; push product of `q` onto subject `p`


`=.` One change to subject; produce `r` with `p` in the context (in hoon the
'subject') set to `q`

`=:` Multiple changes to subject; Make a list of changes to the context (in hoon the 'subject')

  XX how do we talk about subject and context.


`=^` Used to push a new variable `q` with face `p` onto the subject `s`, as
well as to change the value of another. `r` is a function that accepts `q` and
produces a cell of a new value of `q`, and a new subject. XX

`=~` Compose a list of twigs, applying `=>` to the list, using each result as the subject to the subsequent expressions.

`=*` Make `p` an alias of `q`

##### ^ : Casting

`^|` Make core context unreadable so as to avoid type conflicts

`^.` Cast `q` to the product type of `p`. Useful when you don't actually want to run a funciton at run time

`^-` Traditional Cast `q` to the type of the default value ('bunt') of `p`

  - `\`p\`q`

`^+` Cast `q` to the type of `p`. Used to cast to types that have previously been made into validator functions with `$,`.

`^&` Convert object/core from invariant to covariant

`^~` Try to execute `p` statically at compile time. If this fails, `p` remains dynamic

`^=` Put on variable name ('face')

  - Irregular: `p=q`

`^?` Convert core with unreadable context to a core whose input ('sample') and context are unreadable. Used to avoid type conflicts

##### . : Nock operations

`.^` Nock 11; loads a file from `%clay`

`.+` Nock 4; increments an atom

  - Irregular: `+(p)`

`.*` Compute nock; formula `q` on subject `p`

`.=` Nock 5; test `p` and `q` for equality

  - Irregular: `=(p q)`

`.?` Test if noun is cell or an atom

##### # : Pretty printing

`#<` Pretty print tape; only used in its irregular form

  - Irregular: `<i.p i.t.p i.t.t.p>`

`#>` Pretty print `++tank`; used only in it's irregular form

  - Irregular: `>i.p i.t.p i.t.t.p<`

##### / : Library access

`//` Load a file between twigs

`/~` Cached computation, similar to a `=+` at the top of the file

`/$` Pull in dynamic request data, slam it through a gate

`/=` Horn form of `^=`. Puts a face on a cached computation

`/^` Horn form of `^-`

`/;` Apply gate to result of horn

`/,` Appen path to current working beam before computing horn

`/:` Replace beam

`/%` Reifiy the 'heel' virtual path, letting any requested resource interpret it further

`/.` Construct list of specified horns

`/*` similar to `/.`, but produces a map instead

`/_` descend into each available subdirectory, construct a map from all successfully building versions

`/&` Parse directory name as `@da`

`/|` Parse directory name as `@dr`

##### ~ : Compiler hints

`~$` Label computation `q` as `p` for profiling

`~|` `p` in stacktrace if `q` crashes

`~_` Preformatted stackframe `p` (a trap producing a `++tank`) into stacktrace

`~%` i.p i.t.p i.t.t.pRegister jet in core `s`

`~/` XX

`~<` Hint to product; applies arbitrary hint `p` to the product of `q`, which is computed beforehand

`~>` Arbitrary hint `p` to `q`

`~+` Memoize computation of `p`

`~&` Printf

`~=` Hint to avoid duplication

`~?` PrintF if `q` is true

`~!` Print type on compilation fail

##### ! : Special runes

`!_` Path and range in stack trace

`!>` Produces a cell of both the type and value of `p` (called a `++vase`).
Useful for debugging.

`!=` Show nock of `p`

`!?` Enforce a Hon version restriction

`!!` Crash


####Data types

#####Strings

`++tape` - List of chars. Used when you want to manipulate and interpolate text - `"[text]"`
  See [list]() section for operations.

`@t` - UTF-8 text; aka `++cord` - `'[text]'`
    `++trip`: cord to string XX
    `++taco`: cord to octet stream (for http requests) XX
    `++slav`: parse cord to an atom of input odor XX

`@ta` - Valid Hoon ASCII symbols - `~.[text]` + {`a-z`, `0-9`, `~`, `-`, `.`, `_`.}
`@tas` - Hoon identifier ASCII subset - `%` + `a-z`, `-` (not first), `1-9` (not first)

XX Would love to add half a liine about the difference between cords and tapes.

#####Numbers

`@`   - Generic atom (defaults to unsigned integer) - `42`
`@u`  - Unsigned integer - `42`
`@sd` - Signed decimal - `-42`(-), `--42` see `++si` for operations
`@rd` - Single precision floating point - `.-42.75` (-), `.42.75` (+), see `++rd` for operations

#####Other Important Atoms
`@da` - absolute date - `~~[year].[month].[date]..[hour].[minute].[second]..[fractionals]` 
      `++year`: `++date` to `@d` XX
      `++yore`: `@d` to `++date` XX
      note: year month day is mandatory, others are optional
`@dr` - relative date (timespan) - `~d[day].h[hour]m[minute].s[second]..[fractionals]`
      note: every measurement optional.
      `++add`: can add `@dr`s together XX
      `++yell`: `@d` to `++tarp`       XX
`@p` - phonemic base - `~[valid @p]`
      `(scot %p [number])` prints a number as a `@p`

#####Data structures

######Tuples are made with the `:` runes and their irregular `[]` wide forms

 To construct, use [`[]`]() (irregular form of [`:` runes]()).

`[3 'hello']`

Special syntaxes:

`~[1 2 3]` > `[1 2 3 ~]`

`[1 2 3]~` > `[[1 2 3] ~]`

`~[1 2 3]~` > `[[1 2 3 ~] ~]`

######Autocons

To eliminate need for excess `[]`, tuples automatically group to
the right:

`[1 [2 [3 ~]]]` == `[1 2 3 ~]`

######pair, trel, quad put faces on tuples

``=+ a=`(pair term cord)`[%h 'j']`` >  `[p=%h q='j']`

Faces allow you to access specific axes within your context: 

`p.a` > `%h` and `p:[p=%h q='j']` > `%h`

######Lists are null-terminated and have faces of `i` and `t`

- Definition:

`++  list  |*  a=_,*                                     ::  null-term list
          $|(~ [i=a t=(list a)])                        ::
`

- Constructor: `++limo` makes a list out of a null terminated tuple.

`(limo [67 55 201 ~])` > `[i=67 t=[i=55 t=[i=201 t=~]]]`

``(list ,@t)`(list ,@)\`~[67 55] >  <|C 7|>``

- Operations:

`++weld`: concatenate
`++scag`: slice up to (and including) index number args: (index, list)
`++slag`: slice list after index, args: (index, list)
`++turn`: map over list, args: (list, function)
`++reel`: right fold args: (list, function)
`++roll`: left fold args: (list, function)
`++sort`: quicksort, args: (list, comparator-function)

######Map

- Definition:

` ++  map  |*  [a=_,* b=_,*]                              ::  associative tree
         $|(~ [n=[p=a q=b] l=(map a b) r=(map a b)])    ::
`

- Constructor: `++mo` produces a map of key-val pairs from left-right cell pairs of a list

`
    ~zod/try=> (mo \`(list ,[@t *])\`[[a 1] [b 2] ~])
    {[p=a q=1] [p=b q=2]}
`

- Operations: contained within the `++by` core

`++by` accepts a map, then we use `%~` to pull the desired arm, which we then
pass the remainder of the arguments

`
> =m (mo \`(list ,[@t *])\`[['a' 1] ['b' 2] ~])
> (~(got by m) 'a')
1
`

`+-all:by`: logical 'AND'
`+-any:by`: logical 'OR'
`+-del:by`: delete at key
`+-dig:by`: axis of key
`+-gas:by`: concatenate: insert a list of key-value pairs into map
`+-get:by`: pull out value at key as a unit
`+-got:by`: assert: pull value at key, crash if key doesn't exist
`+-has:by`: check if key exists
`+-int:by`: intersection: if keys match, use value from `b`
`+-put:by`: add key value pair
`+-rep:by`: fold over map
`+-run:by`: 'map' over map
`+-tap:by`: convert map to list
`+-uni:by`: produce union between keys; if both share keys, use value from `a`
`+-urn:by`: 'map', with key included with value as input to mapping function
`+-wyt:by`: produce depth of tree of map

######Set

- Definition:

` 
    ++  set  |*  a=_,*                                      ::  set
             $|(~ [n=a l=(set a) r=(set a)])                ::
`

- Constructor: `++sa`

`
    ~zod/try=> (sa \`(list ,@)\`[1 2 3 4 5 ~])
    {5 4 1 3 2}
`

- Operations: contained within the `++in` core

The `++in` core accepts a set, then we use `%~` to pull desired operation out
of it, which we then pass the remainder of the arguments.

    > =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
    > (~(has in a) `a`)
    %.y


`+-all:in`: logical 'AND'
`+-any:in`: logical 'OR'
`+-del:in`: delete val at key
`+-dif:in`: difference between sets
`+-dig:in`: produce axis `b` in set
`+-gas:in`: concatenate: insert elements of list `b` into set
`+-has:in`: check if element exists in set
`+-int:in`: intersection
`+-put:in`: add an element to set
`+-rep:in`: fold over set
`+-tap:in`: convert set to list
`+-uni:in`: produces the union between two sets
`+-wyt:in`: Produces the number of elements in set

#####Addressing within nouns

There are several ways you can address an axes of a noun:

1. `[face].[variable-name]`

2. `-.+.+.a`: 'the head, within the tail of the tail of `a`.'

Can also intersperse faces: `+.a.b` reads 'the tail of `a` within `b`.

3. `->+.a`: 'tail within the head of the head of `a`.'

Must start with either `-` (head) or `+` (tail), and then follow with either `<` (head) or `>` (tail), so
as to not repeat two symbols. 

4. `|[number].[var]` returns the elements that follow the nth element in a list

5. `&[number]` returns the nth element in a list. Cannot return the last element.

####Error messages

  i. hoon-how to generate the list of errors (Henry) 

  ii. arvo
    %a. Curtis
    %b. Philip
    %c. PHilip
    %d. Curtis || Anton
    %e. Anton
    %f. Curtis || Anton
    %g. Curtis || Anton



