Lexicon: Nouns
=======

Atom Syntax
----------


###Canonical Atom Odors

An odor is an atom format that specifies an atomic subtype.

```
@c              UTF-32 codepoint
@d              date
  @da           absolute date
  @dr           relative date (ie, timespan)
@f              yes or no (inverse boolean)
@n              nil
@p              phonemic base
@r              IEEE floating-point
  @rd           double precision  (64 bits)
  @rh           half precision (16 bits)
  @rq           quad precision (128 bits)
  @rs           single precision (32 bits)
@s              signed integer, sign bit low
  @sb           signed binary
  @sd           signed decimal
  @sv           signed base32
  @sw           signed base64
  @sx           signed hexadecimal
@t              UTF-8 text (cord)
  @ta           ASCII text (span)
    @tas        ASCII symbol (term)
@u              unsigned integer
  @ub           unsigned binary
  @ud           unsigned decimal
  @uv           unsigned base32
  @uw           unsigned base64
  @ux           unsigned hexadecimal
```

###Unsigned decimal, @ud

Hoon's unsigned decimal format is the normal Continental syntax. It differs
from the Anglo-American only in the use of periods, rather than commas, between
groups of 3:

    ~zod/try=> 19
    19
    ~zod/try=> 1.024
    1.024

An unsigned decimal not broken into groups is a syntax error. Also, whitespace
or even linebreaks can appear between the dot and the next group.

    ~zod/try=> 65.  536
    65.536

###Unsigned hexadecimal, @ux

@ux has the same syntax as @ud, except that it's prefixed by 0x and uses groups
of four. Hex digits are lowercase only.

    ~zod/try=> 0x0
    0x0
    ~zod/try=> `@ud`0x17
    23

###Unsigned base64 and base32, @uv, @uw

The prefix is 0w for base64 and 0v for base32. The digits for @uw are, in
order: 0-9, a-z, A-Z, -, ~:

    ~zod/try=> `@ud`0w-
    62

For @uv, the digits are 0-9, a-v.

Signed integers, @sd, @sx, @sw, @sv, @sb

Obviously, without finite-sized integers, the sign extension trick does not
work. A signed integer in Hoon is a different way to use atoms than an unsigned
integer; even for positive numbers, the signed integer cannot equal the
unsigned.

The prefix for a negative signed integer is a single - before the unsigned
syntax. The prefix for a positive signed integer is --. The sign bit is the low
bit:

    ~zod/try=> -1
    -1
    ~zod/try=> --1
    --1
    ~zod/try=> `@ud`-1
    1
    ~zod/try=> `@ud`--1
    2
    ~zod/try=> `@ud`-2
    3
    ~zod/try=> `@ud`--2
    4
    ~zod/try=> `@ux`-0x10
    0x1f
    ~zod/try=> `@ux`--0x10
    0x20
    ~zod/try=> `@ud`--0w-
    124
    ~zod/try=> `@sw`124
    --0w-

###Absolute date, @da

Urbit dates represent 128-bit chronological time, with 2^64 seconds from the
start of the universe to the end. 2^127 is 3:30:08 PM on December 5, AD 226,
for reasons not clear or relevant:

    ~zod/try=> `@da`(bex 127)
    ~226.12.5..15.30.08

    ~zod/try=> `@da`(dec (bex 127))
    ~226.12.5..15.30.07..ffff.ffff.ffff.ffff

The time of day and/or second fragment is optional:

    ~zod/try=> `@ux`~2013.12.7
    0x8000.000d.2140.7280.0000.0000.0000.0000

    ~zod/try=> `@ux`~2013.12.7..15.30.07
    0x8000.000d.2141.4c7f.0000.0000.0000.0000

    ~zod/try=> `@ux`~2013.12.7..15.30.07..1234
    0x8000.000d.2141.4c7f.1234.0000.0000.0000

We also do BC:

    ~zod/try=> `@ux`~226-.12.5
    0x7fff.fffc.afb1.b800.0000.0000.0000.0000

The semantics of the time system are that UGT (Urbit Galactic Time) is GMT/UTC
as of leap second 25. UGT is chronological and will never add leap seconds,
even if UTC continues this mistake. If a gap appears, it must be resolved in
the presentation layer, with timezones and other human curiosities.

See section 2cH of the source for more details.

###Relative date, @dr

It's also nice to have a syntax for basic time intervals:

    ~zod/try=> `@ux`~s1
    0x1.0000.0000.0000.0000

    ~zod/try=> `@ux`~m1
    0x3c.0000.0000.0000.0000

    ~zod/try=> (div ~m1 ~s1)
    60

    ~zod/try=> (div ~h1 ~m1)
    60

    ~zod/try=> (div ~h1 ~s1)
    3.600

    ~zod/try=> (div ~d1 ~h1)
    24

    ~zod/try=> `@da`(add ~2013.11.30 ~d1)
    ~2013.12.1

There are no @dr intervals under a second or over a day. Since the resolution
is so high, though, (div ~s1 1.000.000) produces a pretty accurate microsecond.

###Loobean, @f

A loobean, or just bean, is 0 or 1. 0 is yes, 1 is no:

    ~zod/try=> `@ud`.y
    0
    ~zod/try=> `@ud`.n
    1

###Nil, @n

Nil indicates an absence of information, as in a list terminator. The only
value is ~, 0.

  ~zod/try=> `@ud`~
  0

###Unicode text, @t

@t is a sequence of UTF-8 bytes, LSB first - sometimes called a cord. For
lowercase numbers and letters, the canonical syntax is ~~text:

    ~zod/try=> ~~foo
    'foo'

Note that the prettyprinter makes an unprincipled exception and prints the text
in a noncanonical format:

    ~zod/try=> `@ux`~~foo
    0x6f.6f66

We want to be able to encode an arbitrary Unicode string as a single URL-safe
token, using no punctuation but .~-, in @t. Space is ., . is ~., ~ is ~~, - is
-:

    ~zod/try=> ~~foo.bar
    'foo bar'
    ~zod/try=> ~~foo.bar~.baz~~moo-hoo
    'foo bar.baz~moo-hoo'

For all other ASCII/Unicode characters, insert the Unicode codepoint in
lower-case hexadecimal, followed by .. For example, for U+2605 "BLACK STAR",
write:

    ~zod/try=> ~~foo~2605.bar
    'foo★bar'

This UTF-32 codepoint is of course converted to UTF-8:

    ~zod/try=> `@ux`~~foo~2605.bar
    0x72.6162.8598.e26f.6f66

###URL-safe ASCII text, @ta

@ta encodes the ASCII subset that all canonical atom syntaxes restrict
themselves to. The prefix is ~.. There are no escape sequences except ~~, which
means ~, and ~-, which means \_. - and . encode themselves. No other characters
besides numbers and lowercase letters need apply.

    ~zod/try=> `@t`~.foo
    'foo'
    ~zod/try=> `@t`~.foo.bar
    'foo.bar'
    ~zod/try=> `@t`~.foo~~bar
    'foo~bar'
    ~zod/try=> `@t`~.foo~-bar
    'foo_bar'
    ~zod/try=> `@t`~.foo-bar
    'foo-bar'

A @ta atom is called a span.

###Codepoint, @c

Normally when we build atoms of Unicode text, we use a UTF-8 bytestream, LSB
first. But sometimes it's useful to build atoms of one or more UTF-32 words.

The codepoint syntax is the same as @t, except with a ~- prefix. Let's repeat
our examples, with hex display:

    ~zod/try=> `@ux`~-foo
    0x6f.0000.006f.0000.0066

    ~zod/try=> `@ux`~-foo.bar
    0x72.0000.0061.0000.0062.0000.0020.0000.006f.0000.006f.0000.0066

###Phonemic, @p

We've seen @p used for ships, of course. But it's not just for ships - it's for
any short number optimized for memorability, not for arithmetic. @p is great
for checksums, for instance.

That said, @p is subtly customized for the sociopolitical design of Urbit as a
digital republic. For example, one feature we don't want is the ability to see
at a glance which carrier and cruiser issued a destroyer. Consider the carrier
0x21:

    ~zod/try=> `@p`0x21
    ~mep

It issues 255 cruisers, including 0x4321:

    ~zod/try=> `@p`0x4321
    ~pasnut

Which issues 65.535 destroyers, including 0x8765.4321 and several successors:

    ~zod/try=> `@p`0x8765.4321
    ~famsyr-dirwes
    ~zod/try=> `@p`0x8766.4321
    ~lidlug-maprec
    ~zod/try=> `@p`0x8767.4321
    ~tidlus-roplen
    ~zod/try=> `@p`0x8768.4321
    ~lisnel-lonbet

Of course, anyone who can juggle bits can see that ~famsyr-dirwes is a close
cousin of ~lidlug-maprec. But she actually has to juggle bits to do it.
Obfuscation does not prevent calculated associations, just automatic ones.

But at the yacht level, we actually want to see a uniform 32-bit space of
yachts directly associated with the destroyer:

    ~zod/try=> `@p`0x9.8765.4321
    ~talfes-sibzod-famsyr-dirwes
    ~zod/try=> `@p`0xba9.8765.4321
    ~tacbep-ronreg-famsyr-dirwes
    ~zod/try=> `@p`0xd.cba9.8765.4321
    ~bicsub-ritbyt-famsyr-dirwes
    ~zod/try=> `@p`0xfed.cba9.8765.4321
    ~sivrep-hadfeb-famsyr-dirwes

###IPv4 and IPv6 addresses, @if, @is

Urbit lives atop IP and would be very foolish to not support a syntax for the
large atoms that are IPv4 and IPv6 addresses.

@if is the standard IPv4 syntax, prefixed with .:

    ~zod/try=> `@ux`.127.0.0.1
    0x7f00.0001

@is is the same as @if, but with 8 groups of 4 hex digits:

    ~zod/try=> `@ux`.dead.beef.0.cafe.42.babe.dead.beef
    0xdead.beef.0000.cafe.0042.babe.dead.beef


###Floating Point, @rs, @rd, @rq, @rh

The syntax for a single-precision float is the normal English syntax, with a . prefix:

    .6.2832             ::  τ as @rs
    .-6.2832            ::  -τ as @rs
    .~6.2832            ::  τ as @rd
    .~-6.2832           ::  -τ as @rd
    .~~6.2832           ::  τ as @rh
    .~~~6.2832          ::  τ as @rq

(Hoon is a Tauist language and promotes International Tau Day.)

###Transparent cell syntax

By adding _, we can encode arbitrary nouns in our safe subset. The prefix to a
canonical cell is ._; the separator is _; the terminator is __. Thus:

    ~zod/try=> ._3_4__
    [3 4]

    ~zod/try=> :type; ._.127.0.0.1_._0x12_19___~tasfyn-partyv__
    [.127.0.0.1 [0x12 19] ~tasfyn-partyv]
    [@if [@ux @ud] @p]

Those who don't see utility in this strange feature have perhaps never needed
to jam a data structure into a URL.

###Opaque noun syntax

Speaking of jam, sometimes we really don't care what's inside our noun. Then,
the syntax to use is a variant of @uw prefixed by ~, which incorporates the
built-in jam and cue marshallers:

    ~zod/try=> (jam [3 4])
    78.241
    ~zod/try=> `@uw`(jam [3 4])
    0wj6x
    ~zod/try=> (cue 0wj6x)
    [3 4]
    ~zod/try=> ~0wj6x
    [3 4]

Noncanonical Syntax
--------------------

These are syntaxes for constants which don't fit the canonical character-set
constraints.

###Cubes, @tas

@tas, a term, is our most exclusive odor. The only characters permitted are
lowercase ASCII, - except as the first or last character, and 0-9 except as the
first character.

The syntax for @tas is the text itself, always preceded by %. This means a term
is always cubical. You can cast it to @tas if you like, but we just about
always want the cube:

    ~zod/try=> %dead-fish9
    %dead-fish9

    ~zod/try=> -:!>(%dead-fish9)
    [%cube p=271.101.667.197.767.630.546.276 q=[%atom p=%tas]]

The empty @tas has a special syntax, $:

    ~zod/try=> %$
    %$

A term without % is not a constant, but a name:

    ~zod/try=> dead-fish9
    ! -find-limb.dead-fish9
    ! find-none
    ! exit

A common structure in Hoon is a noun with a cubical head and an arbitrary tail:

    ~zod/try=> [%foo 'bar']
    [%foo 'bar']

This structure may be generated with the following irregular syntax:

    ~zod/try=> a/'bar'
    [%a 'bar']

###Loobeans, @f

    .y is a little cumbersome, so we can say & and |. The % prefix cubes as usual.

    ~zod/try=> `@ud`&
    0
    ~zod/try=> `@ud`|
    1

###Cords, @t

The canonical ~~ syntax for @t, while it has its place, is intolerable in a
number of ways - especially when it comes to escaping capitals. So @t is both
printed and parsed in a conventional-looking single-quote syntax:

    ~zod/try=> 'foo bar'
    'foo bar'
    ~zod/try=> `@ux`'foo bar'
    0x72.6162.206f.6f66
    Escape ' with \:

    ~zod/try=> 'Foo \'bar'
    'Foo \'bar'
    ~zod/try=> `@ux`'\''
    0x27

###Strings

Text in Hoon is generally manipulated in two ways, depending on what you're
doing: as an atomic cord/span/term, or as a tape which is a list of bytes (not
codepoints).

To generate a tape, use double quotes:

  ~zod/try=> "foo"
  "foo"
  ~zod/try=> `*`"foo"
  [102 111 111 0]

We're getting off the constant reservation, but strings also interpolate with curly-braces:

    ~zod/try=> "hello {(weld "wor" "ld")} is a fun thing to say"
    "hello world is a fun thing to say"

And they can be joined across space or lines with a .:

    ~zod/try=> "hello"."world"
    "helloworld"
    ~zod/try=> "hello". "world"
    "helloworld"

Lists
-----

A list in Hoon is a null-terminated tuple. See section 2bB for Hoon's List library. 

    ~zod/try=> :type; `(list)`[3 4 ~]
    ~[3 4]
    it(*)
  
The list type can be further specified by a subtype:

    ~zod/try=> :type; `(list ,@ud)`[3 4 ~]
    ~[3 4]
    it(@ud)

The above example is a list of @ud, meaning the all values in the list must be of type @ud.

Polymorphic lists can be specified by constructing a more complex type:

    ~zod/try=> :type; `(list ?(@ud @ux))`[3 0xf ~]
    ~[3 15]
    it({@ud @ux})

Not specifing a more complex type defaults to a list of raw nouns:

    ~zod/try=> :type; `(list)`[3 0xf ~]
    ~[3 15]
    it(*)

Null-terminated tuples may be generated with the following syntax:

    ~zod/try=> ~[3 0xf]
    [3 0xf ~]

    ~zod/try=> :type; ~[3 0xf]
    [3 0xf ~]
    [@ud @ux %~]

Note that this syntax is not automatically typed as a list, but may be cast as such:

    ~zod/try=> :type; `(list ?(@ux @ud))`~[3 0xf]
    ~[0x3 0xf]
    it({@ux @ud})

Furthermore, a different syntax may be used to group the entire noun in the
head of a null terminated tuple:


    ~zod/try=> [3 0xf]~
    [[3 0xf] ~]

This is often used to easily generate a list from a single noun.

~zod/try=> :type; `(list ,[@ud @ux])`[3 0xf]~
~[[3 0xf]]
it([@ud @ux])

The above is typed as a list of decimal hexadecimal pairs.

Units
-----

A Unit is Hoon's "maybe" type. As in, either some value or null. See section 2bA for Hoon's Unit library. Units are represented by a cell whose head is null and whose tail is some noun:

    ~zod/try=> `(unit ,@ud)`[~ 3]
    [~ 3]

The unit type can be further specified by a subtype. The above example is a unit of @ud, meaning the optional value must be of type @ud.

    ~zod/try=> `(unit ,@ud)`[~ [3 3]]
    ! type-fail
    ! exit

    ~zod/try=> `(unit ,^)`[~ [3 3]]
    [~ [3 3]]

For convenience, a null-headed noun may be specified with the following irregular syntax:

    ~zod/try=> `3
    [~ 3]

    ~zod/try=> :type; `3
    [~ 3]
    [%~ @ud]

Note that this syntax is not automatically typed as a Unit, but may be cast as such:

    ~zod/try=> :type; `(unit)``3
    [~ 3]
    u(*)




