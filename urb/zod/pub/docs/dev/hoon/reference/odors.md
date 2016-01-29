Odors
=====

Overview
--------

Since Everything in Hoon is a natural number, the interpreter needs to
know both how to render them and subject them to type enforcement.
Odors, which are just ASCII spans beginning with a `@`, carry all of the
information necessary for the interpreter to do this. For instance, the
interpreter knows to render an atom of odor `@t` as UTF-8 text.

The span composing the Odor consists of two parts: a lowercase prefix
carrying type information, and an upper-case suffix containing
information about its size. The prefix is a taxonomy that grows more
specific to the right. For example, atoms of the odor `@ta` are URL-safe
ASCII text, and Atoms of the odor `@tas` are the more specific subset of
ASCII text that is acceptable in hoon.

The general principle of type enforcement is that atoms change freely
either up or down the taxonomy, but not across. You can treat a `@tas`
as a `@t`, as in a strong type system; but you can also treat a `@t` as
a `@tas`, or an `@` as anything. However, passing a `@t` to a function
that expects an `@ux` is a type error.

XXDIAGRAMXX

For example, you can cast a `@t` to a `@tas`, or vice-versa:

    ~zod/try=> =a `@t`'permitted'
    ~zod/try=> a
    'permitted'
    ~zod/try=> `@tas`a
    %permitted

However, you cannot pass a `@ux` to a function that expects a `@t`
without casting it to a `@` first:

    ~zod/try=> (|=(a=@t [%foo a]) 0x20)
    ! type-fail
    ! exit
    ~zod/try=> =a 0x21
    ~zod/try=> (|=(a=@t [%foo a]) `@t`a)
    [%foo '!']

Note that when explicitly casting a `@ux` to a `@t`, the interpreter
automatically casts the `@ux` to a `@` first.

Comprehensive list of the Hoon Odors
------------------------------------

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

Odor Size Suffixes
------------------

The suffix of an odor, if present, is a single upper-case character A-Z
`c`, which indicates the size of an atom. This is possible, because in
Hoon, a letter maps to an ASCII code number, thus a number.

Size is specified in bits in the form of `1 << (c - 'A')` resp. `2^c`,
since most data aligns to the power of 2 or can be composed of such
blocks.

The size of a block of size `N` can be calculated for example using
`(bex (sub 'N' 'A'))` in bits or `(div (bex (sub 'N' 'A')) 8)` in bytes.

Thus, `@tD` is one UTF-8 byte (whatever that means) and `@tN` is 1
kilobyte or less of UTF-8.

For reference:

    A   1 bit
    B   2 bits
    C   4 bits
    D   1 byte
    E   2 bytes
    F   4 bytes
    G   8 bytes
    H   16 bytes
    I   32 bytes
    J   64 bytes
    K   128 bytes
    L   256 bytes
    M   512 bytes
    N   1K
    O   2K
    P   4K
    Q   8K
    R   16K
    S   32K
    T   64K
    U   128K
    V   256K
    W   512K
    X   1MB
    Y   2MB
    Z   4MB

It is possible to construct an atom bigger than 4Mb in size, but the
type system would not be able to express an odor size for it.

There is also the datatype `++bloq` to hold a to-the-power-of block size
(though it is just an alias for `@`).

------------------------------------------------------------------------

### @c

UTF-32 codepoint

Atoms of the odor `@c` represent Unicode text, constructed with a UTF-32
bytestream, with the lowest-significant bit first. Although we usually
use a UTF-8 bytestream, sometimes it's useful to build atoms of one or
more UTF-32 words.

##### Forms

`~-[text]`

##### Examples

    ~zod/try=> ? ~-foo
    ~-foo
    @c
    ~zod/try=> ~-i~2764.u
    ~-i~2764.u
    ~zod/try=> (tuft ~-i~2764.u)
    'i❤u'
    ~zod/try=> `@ux`~-foo
    0x6f.0000.006f.0000.0066
    ~zod/try=> `@ux`~-i~2764.u
    0x75.0000.2764.0000.0069
    ~zod/try=> `@ux`(tuft ~-i~2764.u)
    0x75.a49d.e269

------------------------------------------------------------------------

### @d

Date

#### @da

Absolute date

Atoms of the odor `@da` represent absolute Urbit dates. Urbit dates
represent 128-bit chronological time, with 2\^64 seconds from the start
of the universe. For example, 2\^127 is 3:30:08 PM on December 5, AD
226. The time of day and/or second fragment is optional. As the last
example shows, BC times are also possible.

##### Forms

`~~[year].[month].[date]..[hour].[minute].[second]..[millisecond]`

Note: the time of day and/or millisecond fragment is optional.

##### Examples

    ~zod/try=> ~2014.1.1
    ~2014.1.1
    ~zod/try=> ? ~2014.1.1
    ~2014.1.1
    @da
    ~zod/try=> ~2014.1.1..01.01.01
    ~2014.1.1..01.01.01
    ~zod/try=> ? ~2014.1.1..01.01.01
    ~2014.1.1..01.01.01
    @da
    ~zod/try=> ~2014.1.1..01.01.01..1234
    ~2014.1.1..01.01.01..1234
    ~zod/try=> `@da`(bex 127)
    ~226.12.5..15.30.08
    ~zod/try=> `@da`(dec (bex 127))
    ~226.12.5..15.30.07..ffff.ffff.ffff.ffff
    ~zod/try=> `@ux`~2013.12.7
    0x8000.000d.2140.7280.0000.0000.0000.0000
    ~zod/try=> `@ux`~2013.12.7..15.30.07
    0x8000.000d.2141.4c7f.0000.0000.0000.0000
    ~zod/try=> `@ux`~2013.12.7..15.30.07..1234
    0x8000.000d.2141.4c7f.1234.0000.0000.0000
    ~zod/try=> `@ux`~2013.12.7..15.30.07..1234
    0x8000.000d.2141.4c7f.1234.0000.0000.0000

------------------------------------------------------------------------

#### @dr

Relative date (ie, timespan)

Atoms of the odor `@dr` atoms represent basic time intervals in
milliseconds. There are no `@dr` intervals under a second or over a day
in length.

##### Forms

`~d[day].h[hour]m[minute].s[second]..[fractionals]`

Note: Every measurement is optional, so long as those that are present
are in order. The largest measurement is preceded by a `~`.

##### Examples

    ~zod/try=> ~d1.h19.m5.s29
    ~d1.h19.m5.s29
    ~zod/try=> ? ~d1.h19.m5.s29
    ~d1.h19.m5.s29
    @dr
    ~zod/try=> `@dr`(div ~s1 1.000)
    ~.s0..0041.8937.4bc6.a7ef
    ~zod/try=> ~d1.h19.m5.s29..0041
    ~d1.h19.m5.s29..0041
    ~zod/try=> `@ux`~s1
    0x1.0000.0000.0000.0000
    ~zod/try=> `@ux`~m1
    0x3c.0000.0000.0000.0000
    ~zod/try=> `@dr`(div ~d1 5)
    ~h4.m48
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

------------------------------------------------------------------------

#### @f

Loobean(inverse boolean)

Atoms of the odor `@f` represent loobeans, where `0` is yes and `1` is
no. Loobeans are often represented in their cubic and runic forms shown
below.

##### Forms

`0`, `1` as numbers. `%.y`, `%.n` as [`%cube`]()s. `&`, `|` as short
forms.

##### Examples

    ~zod/try=> `@ud`.y
    0
    ~zod/try=> ? .y
    %.y
    {%.y %.n}
    ~zod/try=> `@ud`.n
    1
    ~zod/try=> .y
    %.y
    ~zod/try=> &
    %.y
    ~zod/try=> |
    %.n

------------------------------------------------------------------------

### @n

Nil

Atoms of the odor `@n` indicate an absence of information, as in a list
terminator. The only value is `~`, which is just `0`.

    ~zod/try=> ? ~
    ~
    %~
    ~zod/try=> `@ud`%~
    0
    ~zod/try=> `@ud`~
    0

------------------------------------------------------------------------

### @p

Phonemic base

Atoms of `@p` are primarily used to represent ships names, but they can
be used to optimize any short number for memorability. For example, it
is great for checksums.

##### Forms

`~[phonemic]`

Every syllable is a byte. Pairs of two bytes are separated by `-`, and
phrases of four pairs are separated by `--`.

    ~zod/try=> ~pasnut
    ~pasnut
    ~zod/try=> ? ~pasnut
    ~pasnut
    @p
    ~zod/try=> `@p`0x4321
    ~pasnut
    ~zod/try=> `@p`0x8765.4321
    ~famsyr-dirwes
    ~zod/try=> `@p`0x8766.4321
    ~lidlug-maprec
    ~zod/try=> `@p`(shaf %foo %bar)
    ~ralnyl-panned-tinmul-winpex--togtux-ralsem-lanrus-pagrup

------------------------------------------------------------------------

### @r

IEEE floating-points

Hoon does not yet support floating point, so these syntaxes don't work
yet. But the syntax for a single-precision float is the normal English
syntax, with a `.` prefix:

------------------------------------------------------------------------

### @s

Signed integer, sign bit low

Without finite-sized integers, the sign extension trick obviously does
not work. A signed integer in Hoon is a different way to use atoms than
an unsigned integer; even for positive numbers, the signed integer
cannot equal the unsigned.

The prefix for a negative signed integer is a single `-` before the
unsigned syntax. The prefix for a positive signed integer is `--`. The
least significant represents the sign. The representation is similar to
a folded number line.

#### @sb

Signed binary

Atoms of the odor `@sb` represent signed binary numbers.

#### Forms

`-0b[negative_binary]` `--0b[postive_binary]`

#### Examples

    ~zod/try=> ? -0b1
    -0b1
    @sb
    ~zod/try=> `@sd`-0b1
    -1
    ~zod/try=> `@sd`--0b1
    --1
    ~zod/try=> `@sd`--0b11
    --3
    ~zod/try=> `@sb`(sum:si -0b10 -0b10)
    -0b100

------------------------------------------------------------------------

#### @sd

Signed decimal

Atoms of odor `@sd` represent signed decimal numbers.

#### Forms

`-[negative[decimal]()]` `--[postive_[decimal]()]`

#### Examples

    ~zod/try=> -234
    -234
    ~zod/try=> ? -234
    -234
    @sd
    ~zod/try=> ? --234
    --234
    @sd
    ~zod/try=> (sum:si -234 --234)
    --0

------------------------------------------------------------------------

#### @sv

Signed base32

Atoms of odor `@sv` represent signed base32 numbers.

##### Forms

`-0v[negative_base32]` The digits are, in order, `0-9`, `a-v`.
`--0v[positive_base32]`

##### Examples

    ~zod/try=> -0vv
    -0vv
    ~zod/try=> ? -0vv
    -0vv
    @sv
    ~zod/try=> --0vb
    --0vb
    ~zod/try=> `@sd`-0vv
    -31
    ~zod/try=> `@sd`--0vb
    --11
    ~zod/try=> `@sd`(sum:si -0vv --0vb)
    -20

------------------------------------------------------------------------

#### @sw

Signed base64

Atoms of odor `@sw` represent base64 numbers.

##### Forms

`-0w[negative_base64]` The digits are, in order, `0-9`, `a-z`,
`A-Z`,`-`, and `~`. `--0w[positive_base64]` The digits are, in order,
`0-9`

##### Examples

    ~zod/try=> -0w--
    -0w--
    ~zod/try=> ? -0w--
    -0w--
    @sw
    ~zod/try=> `@sd`(sum:si -0w-A -0w--)
    -8.034

#### @sx

Signed hexadecimal

Atoms of odor `@sx` represent signed hexadecimal numbers.

##### Forms

`-[negative_hexadecimal]` `--[positive_hexadecimal]`

##### Examples

    ~~zod/try=> -0x0
    --0x0
    ~zod/try=> `@sd`--0x17
    --23
    ~zod/try=> `@ux`(bex 20)
    0x10.0000
    ~zod/try=> 0x10.  0000
    0x10.0000
    ~zod/try=> `@sd`(sum:si --0x17 -0x0)
    --23
    ~zod/try=> `@sd`(sum:si --0x17 -0xa)
    --13

------------------------------------------------------------------------

### @t

UTF-8 text (cord)

Atoms of the odor `@t` represent a
[cord](http://en.wikipedia.org/wiki/Rope_data_structure), sequence of
UTF-8 bytes, LSB first. It is sometimes called a cord.

##### Forms

`~~[text]` `'[text]'`

##### Examples

    ~zod/try=> ~~foo
    'foo'
    ~zod/try=> ? 'foo'
    'foo'
    @t
    ~zod/try=> ? ~~foo
    'foo'
    @t

------------------------------------------------------------------------

#### @ta

ASCII text (span)

Atoms of the odor `@ta` represent the ASCII text subset used in hoon
literals: `a-z`, `0-9`, `~`, `-`, `.`, `_`.

##### Forms

`~.[text]` There are no escape sequences.

##### Examples

    ~zod/try=> ~..asdf
    ~..asdf
    ~zod/try=> ? ~.asdf
    ~.asdf
    @ta
    ~zod/try=> `@t`~.asdf
    'asdf'

------------------------------------------------------------------------

#### @tas

ASCII symbol (term)

Atoms of `@tas` represent [`++term`]()s, the most exclusive text odor.
The only characters permitted are lowercase ASCII, except as the first
or last character, and 0-9, except as the first character.

##### Forms

`%[text]` This means a term is always [cubical]().

##### Examples

    ~zod/try=> %dead-fish9
    %dead-fish9
    ~zod/try=> -:!>(%dead-fish9)
    [%cube p=271.101.667.197.767.630.546.276 q=[%atom p=%tas]]

------------------------------------------------------------------------

### @u

Unsigned integer

------------------------------------------------------------------------

#### @ub

Unsigned binary

Atoms of the odor `@ub` represent unsigned binary numbers.

#### Forms

`0b[number]` Numbers are least-significant bit first.

##### Examples

    ~zod/try=> `@`0b1
    1
    ~zod/try=> ? 0b1
    0b1
    @ub
    ~zod/try=> `@`0b10
    2
    ~zod/try=> `@`0b100
    4

------------------------------------------------------------------------

#### @ud

Unsigned decimal

Atoms of `@ud` represent unsigned decimal numbers. It is the default
print format for both `@u` and and `@u`, namely unsigned numbers with no
printing preference, as well as opaque atoms.

##### Forms

Numbers of more than three digits must be delimited by `.`s. Whitespace
and linebreaks can appear between the dot and the next group.

    ~zod/try=> 0
    0
    ~zod/try=> 19
    19
    ~zod/try=> ? 19
    19
    @ud
    ~zod/try=> 1.024
    1.024
    ~zod/try=> 65.536
    65.536
    ~zod/try=> (bex 20)
    1.048.576

------------------------------------------------------------------------

#### @uv

Unsigned base32

Atoms of the odor `@uv` represent unsigned base64 numbers.

##### Forms

`0v[number]` The digits are, in order, `0-9`, `a-v`.

##### Examples

    ~zod/try=> `@ud`0vv
    31
    ~zod/try=> ? 0vv
    0vv
    @uv
    ~zod/try=> `@ud`(add 0vv 0v9)
    40

------------------------------------------------------------------------

#### @uw

Unsigned base64

##### Forms

`ow[number]` The digits are, in order, `0-9`, `a-z`, `A-Z`,`-`, and `~`.

##### Examples

    ~zod/try=> 0w~
    0w~
    ~zod/try=> ? 0w~
    0w~
    @uw
    ~zod/try=> `@uv`(add 0w~ 0wZ)
    0v3s
    ~zod/try=> `@ud``@uv`(add 0w~ 0wZ)
    124

------------------------------------------------------------------------

#### @ux

Unsigned hexadecimal

Atoms of the odor `@ux` represent hexadecimal numbers.

##### Forms

`0x`number. Numbers with more than four digits must be delimited by
`.`s. Hex digits are lowercase only.

    ~zod/try=> 0x0
    0x0
    ~zod/try=> ? 0x0
    0x0
    @ux
    ~zod/try=> `@ud`0x17
    23
    ~zod/try=> `@ux`(bex 20)
    0x10.0000
    ~zod/try=> 0x10.  0000
    0x10.0000

------------------------------------------------------------------------
