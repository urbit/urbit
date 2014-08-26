Lexicon
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

###Noncanonical syntaxes

These are syntaxes for constants which don't fit the canonical character-set
constraints.

####Hoon symbol, @tas

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

####Loobeans, @f

    .y is a little cumbersome, so we can say & and |. The % prefix cubes as usual.

    ~zod/try=> `@ud`&
    0
    ~zod/try=> `@ud`|
    1

####Cords, @t

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

####Strings

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

Runes:

    |  bar    core construction
      |_    dry %gold door
      |%    generic %gold core
      |.    dry %gold trap
      |/    vulcanized gold door
      |^    kick a %gold book
      |-    kick a %gold trap
      |+    dry %iron gate
      |*    vulcanized wet gate
      |=    dry %gold gate
      |?    dry %lead trap

    $  buc    tiles and tiling
      $_    bunt a tile
      $,    clam a tile
      $@    whip a wing into a tile
      $*    bunt a tile statically
      $!    bunt an axil

      Tile runes:
      $^    plant a %herb 
      $:    build a tile 
      $=    plant a %bark
      $&    plant a %bush
      $?    plant a %fern 
      $%    plant a %kelp
      $|    plant a %reed
      @     atom axil
      ^     cell axil
      *     noun axil
      ?     bean axil
      ~     null axil

    %  cen    invocations
      %_    invoke with changes and cast
      %:    pull %$ of a door with a sample
      %.    inverse order %-
      %-    slam a core with a sample
      %*    
      %^
      %+
      %~
      %=

    :  col    tuples
      :_      reverse pair [q p]
      :%      
      :/
      :^
      :-
      :+
      :~
      :*

    .  dot    nock operators
      .^
      .+
      .*
      .=
      .?

    ^  ket    type conversions
      ^|
      ^.
      ^+
      ^-
      ^&
      ^~
      ^=
      ^?

    ;  sem    miscellaneous macros
      ;:
      ;.
      ;"
      ;~
      ;;

    ~  sig    hints
      ~|
      ~_
      ~%
      ~/
      ~<
      ~>
      ~$
      ~+
      ~&
      ~=
      ~?
      ~!

    =  tis    compositions
      =|
      =:
      =%
      =.
      =/
      =>
      =<
      =-
      =^
      =+
      =&
      =@
      =*
      =~

    ?  wut    conditionals, booleans, tests
      ?|
      ?-
      ?:
      ?.
      ?^
      ?>
      ?<
      ?+
      ?&
      ?~
      ?=
      ?!

    !  zap    special operations
      !:
      !_
      !,
      !%
      !/
      !;
      !?
      !!

Wing Runes

Limbs
  Names
    a
    ^a

  Axes
    .
    +3
    &3
    ->-

Wings: concatenate limbs separated by .

Punctuation runes

++  dry arm
+-  wet arm

==  terminate list
--  terminate map or set


Irregular Rune Forms

,p           $,(p)
*p           $*(p)
_p           $_(p)
p@q          $@(p q)
!p           ?!(p)
&(p q)       ?&(p q)
|(p q)       ?|(p q)
`p`q         ^-(p q)
p=q          ^=(p q)
~[p q]       :~(a b)
[p q]~       :~(a)
`[p]         [~ p]
p^q          [p q]
[p q]        :*(p)
+(p)         .+(p)
=(p q)       .=(p)
p:q          =<(p q)
p(q r)       %=(p q r)
(p list)     %-(p)
~(p q r)     %~(p q r)
>p<          #<(p)
<p>          #>(p)
:(p q)       ;:(p q)

Twigs
-----

There are 112 cases of ++twig

    [p=twig q=twig] 

A twig can be a pair of twigs.

    [%$ p=axis]

Refers to a nock axis. 

    [%bccb p=tile]

$_ ("buccab") is a synthetic hoon that produces the bunt (default
value) for p.

        Tall
          $_  p

        Wide
          $_(p)

        Irregular
          _p
        
        Reduction
          See ++open 

    [%bccm p=tile]

$, ("buccom") is a synthetic rune that produces a normalizing gate
(clam) for p.

        Talln
          $,  p

        Wide
          none

        Irregular
          ,p

        Reduction
          ~(clam al p) 
        
        See ++clam in ++al. 

    [%bcpt p=wing q=tile]

$@ ("bucpat") is a (just barely) natural hoon that whips wing p
into tile q.

        Tall
          $@  p
          q

        Wide
          $@(p q)

        Irregular
          p@q

        Reduction
          none, natural

    [%bctr p=tile]

$* ("buctar") is a synthetic rune that produces the bunt (default
value) for p as a compile-time constant.

        Tall
          $*  p

        Wide
          $*(p)

        Reduction
          ^~ ~(bunt al p)
            
          See ++bunt in al.

    [%bczp p=base]

$! ("buczap") is a synthetic internal rune that produces the bunt
(default value) for [%axil p].

        Reduction
          See ++open

    [%brcb p=tile q=(map term foot)]

|_ ("barcab") is a synthetic rune that produces a %gold tray with
sample p, arms q. q is an associative array of names and
expressions, each pair of which is called an arm. After any number
of dry (%ash, ++) and/or wet (%elm, +-) arms, the array is
terminated with --

        Tall
          |_  p
          ++  p.n.q
            q.n.q
          --

        Wide
          none
      
        Irregular
          none

        Reduction
          See ++open

    [%brcn p=(map term foot)]

|% ("barcen") is a natural rune that produces a %gold core from an
associative array of names and expressions, each pair of which is
called an arm. After any number of dry (%ash, ++) and/or wet (%elm,
+-) arms, the array is terminated with --

        Tall
          |%
          ++  p.n.q
            q.n.q
          +-  p.n.l.q
            q.n.l.q
          --

        Wide
          none

        Irregular
          none

        Reduction
          none, natural

    [%brdt p=twig]

|. ("bardot") is a synthetic rune that produces a dry %gold trap
from twig p.

        Tall
          |.  p

        Wide
          |.(p)

        Irregular
          none

        Reduction
          See ++open

    [%brfs p=tile q=(map term foot)]

|/ ("barfas") is a synthetic rune that produces a vulcanized %gold
tray with arms q, sample p.

        Tall
          |/  p
          +-  p.n.q
            q.n.q
          --

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%brkt p=twig q=(map term foot)]

|^ ("barket") is a synthetic rune that produces a %gold book with
arms q, with p as %$, and kicks it.

        Tall
          |^  p
          ++  p.n.q
            q.n.q
          -- 
     
        Wide
          none

        Irregular
          none

        Reduction
          See ++open 

    [%brhp p=twig]

|- ("barhep") is a synthetic rune that produces a dry %gold trap
from twig p, and kicks it.

        Tall
          |-
          p

        Wide
          |-(p)

        Irregular
          none

        Reduction
          See ++open

    [%brls p=tile q=twig]

|+ ("barlus") is a synthetic rune that produces a dry %iron gate
with arm q, sample p.

        Tall
          |+  p
          q

        Wide
          |+(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brpt p=tile q=tile r=twig]

         XX not used

    [%brtr p=tile q=twig]

|* ("bartar") is a synthetic rune that produces a vulcanized wet
gate with arm q, sample p.

        Tall
          |*  p
          q

        Wide
          |*(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brts p=tile q=twig]

|= ("bartis") is a synthetic hoon that produces a dry %gold gate
with arm q, sample p.

        Tall
          |=  p
          q

        Wide
          |=(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brwt p=twig]

|? ("barwut") is a synthetic rune that produces a dry %lead trap.

        Tall
          |?  p

        Wide
          |?(p)

        Irregular
          none

        Reduction
          See ++open

    [%clcb p=twig q=twig]

:_ ("colcab") is a synthetic rune that produces the cell [q p].

        Tall
          :_  p
          q

        Wide
          :_(p q)

        Irregular
          none

        Reduction
          See ++open

    [%clcn p=tusk]

:% ("colcen") is a synthetic rune that produces a cell [[p ~] ~]
from a list of twigs p, terminated by a ==

        Tall
          :%  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :%(i.p i.t.p i.t.t.p)

        Irregular
          %[i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%clfs p=twig]

:/ ("colfas") is a synthetic rune that, given a twig p, produces
[%$ [%$ p ~] ~], i.e., [0 [0 p 0] 0]. Used in practice only in
string interpolation.

        Tall
          :/  p

        Wide
          :/(p)

        Irregular
          none

        Reduction
          See ++open
         
    [%clkt p=twig q=twig r=twig s=twig]

:^ ("colket") is a synthetic rune that produces a cell [p q r s]
from twigs p, q, r, and s.

        Tall
          :^    p     
              q
            r
          s
        
        Wide
          :^(p q r s)

        Irregular
          none

        Reduction
          See ++open

    [%clhp p=twig q=twig]

:- ("colhep") is a synthetic rune that produces the cell [p q] from
twigs p and q.

        Tall
          :-  p
          q

        Wide
          :-(p q)

        Irregular
          [p q]

        Reduction
          See ++open

    [%clls p=twig q=twig r=twig]

:+ ("collus") is a synthetic rune that produces a cell [p q r] from
twigs p, q, and r.

        Tall
          :+  p
            q
          r

        Wide
          :+(p q r)
        
        Irregular
          none

        Reduction
          See ++open

    [%clsg p=tusk]

:~ ("colsig") is a synthetic rune that produces a null-terminated
tuple of a list of twigs p.

        Tall
          :~  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :~(i.p i.t.p i.t.t.p)
        
        Irregular
          ~[i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%cltr p=tusk]

:* ("coltar") is a synthetic hoon that produces a tuple from p, a
list of twigs.

        Tall
          :*  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :*(i.p i.t.p i.t.t.p)

        Irregular
          [i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%clzz p=tusk]

"colzaz" is a synthetic internal rune that promotes its tusk p
within a %clsg or %cltr tusk.

        Not used at present.

    [%cncb p=wing q=tram]                  

%_ ("cencab") is a synthetic rune that evaluates the wing p with
the changes specified in tram q, then casts the product back to p.

        Tall
          %_  p
            p.i.q  q.i.q
            p.i.t.q  q.i.t.q
          ==

        Wide
          %_(p p.i.q q.i.q, p.i.t.q q.i.t.q)

        Irregular
          none

        Reduction            
          See ++open

    [%cncl p=twig q=twig]                  

%: ("cencol") is a synthetic rune that pulls %$ from the twig p
with the with its sample set to q.

        Tall
          %:  p
          q

        Wide
          %:(p q)
        
        Irregular
          none
      
        Reduction
          See ++open

    [%cndt p=twig q=twig]                  

%. ("cendot") is a synthetic rune that slams the gate q with 
[%cltr p]. The dual of %cnhp.

        Tall
          %.  p
          q

        Wide
          %.(p q)

        Irregular
          none

        Reduction
          %-  q
          p

          See ++open

    [%cnhp p=twig q=tusk]                  

%- ("cenhep") is a synthetic rune that slams the gate p with 
[%cltr q].

        Tall
          %-  p
          q

        Wide
          %-(p q)

        Irregular
          (p q)

        Reduction
          See ++open

    [%cntr p=wing q=twig r=tram]           

%* is a synthetic rune that pulls the wing p from tray q with changes r.

        Tall
          %*  p  q
          p.i.r  q.i.r
          p.i.t.r  q.i.t.r
          ==

        Wide
          %*(p q p.i.r q.i.r, p.i.t.r q.i.t.r)

        Irregular
          none

        Reduction
          See ++open

    [%cnkt p=twig q=twig r=twig s=twig]         

%^ ("cenket") is a synthetic rune that slams gate p with [%cntr q r s].

        Tall
          %^    p
              q
            r
          s

        Wide
          %^(p q r s)
      
        Irregular
          none  

        Reduction
          See ++open

    [%cnls p=twig q=twig r=twig]                

%+ ("cenlus") is a synthetic rune that slams gate p with [%cntr q r].

        Tall
          %+  p
            r
          s

        Wide
          %+(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%cnsg p=wing q=twig r=twig]                

%~ ("censig") is a synthetic rune that pulls p from the tray q with its
sample set to r.

        Tall
          %~  p
            q
          r

        Wide
          %~(p q r)

        Irregular
          ~(p q r)

        Reduction
          See ++open

    [%cnts p=wing q=tram]                       

%= ("centis") is a natural rune that evaluates p with the changes 
specified in q.

        Tall
          %=  p
            p.i.q    q.i.q
            p.i.t.q  q.i.t.q
          ==

        Wide
          %=(p p.i.q q.i.q, p.i.t.q q.i.t.q)

        Irregular
          p(p.i.q q.i.q, p.i.t.q q.i.t.q)

        Reduction
          See ++open

    [%cnzy p=term]                              

"cenzey" is a synthetic internal rune that pulls limb p from the subject.
        
        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%cnzz p=wing]                              

"cenzaz" is a synthetic internal rune that pulls wing p from the subject.
        
        Form
          none, internal

        Reduction
          See ++open

    [%dtkt p=twig]                              

.^ ("dotket") is a natural rune that generates Nock operator 11, which in
virtual userspace Nock (++mock) loads a file from the global namespace.

        Tall
          .^  p

        Wide
          .^(p)

        Irregular
          ^:type/path

          ^/path

        Reduction
          none, natural

    [%dtls p=twig]                              

.+ ("dotlus") is a natural rune that generates Nock operator 4, which 
increments an atomic operand.
      
        Tall
          .+  p

        Wide
          .+(p)

        Irregular
          +(p)

        Reduction
          none, natural

    [%dtzy p=term q=@]                          

"dotzey" is a natural internal rune that produces a non-cubed atomic 
constant of odor p and value q.

        Tall/Wide/Irregular
          none, internal

        Reduction
          none, natural
        
    [%dtzz p=term q=*]                          

"dotzaz" is a natural internal rune that produces a cubed noun constant of
value q and odor p, if q is an atom.

        Tall/Wide/Irregular
          none, internal

        Reduction
          none, natural

    [%dttr p=twig q=twig]                       

.* ("dottar") is a natural rune that calculates the Nock of subject p,
formula q.
     
        Tall
          .*  p
              q

        Wide
          .*(p q)

        Irregular
          none

        Reduction
          none, natural

    [%dtts p=twig q=twig]                       

.= ("dottis") is a natural rune that applies Nock 5 (equals) to determine
if the products of p and q are equivalent.

        Tall
          .=  p
              q

        Wide
          .=(p q)

        Irregular
          =(p q)

        Reduction
          none, natural

    [%dtwt p=twig]                              

.? ("dotwut") is a natural hoon that applies Nock 3 to a noun: if the 
noun is a cell, it returns the loobean & (true); if the noun is an atom, 
it returns the loobean | (false).

        Tall
          .?  p

        Wide
          .?(p)

        Irregular
          none

        Reduction
          none, natural

    [%hxgl p=tusk]                              

#< ("haxgal") is a synthetic rune that slams the assumed gate noah on
[%zpgr %cntr p]. See the Biblical names.

        Tall/Wide
          none

        Irregular
          >i.p i.t.p i.t.t.p<

        Reduction
          See ++open

    [%hxgr p=tusk]                              

#> ("haxgar") is a synthetic rune that slams the assumed gate cain on
[%zpgr %cntr p]. See the Biblical names.

        Tall/Wide
          none

        Irregular
          <i.p i.t.p i.t.t.p>

        Reduction
          See ++open

    [%ktbr p=twig]                              

^| ("ketbar") is a natural rune that converts a %gold core into an %iron
core. See geometric polymorphism.

        Tall
          ^|  p

        Wide
          ^|(p)

        Irregular
          none

        Reduction
          none, natural
        
    [%ktdt p=twig q=twig]                       

^. ("ketdot") is a synthetic rune that casts q to the type of (p q).

        Tall
          ^.  p
              q

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%ktls p=twig q=twig]                       

^+ ("ketlus") is a natural rune that casts the product of q to the 
type of p, verifying that it contains the type of q.

        Tall
          +^  p
              q

        Wide
          ^+(p q)

        Irregular
          none

        Reduction
          none, natural

    [%kthp p=tile q=twig]                       

^- ("kethep") is a synthetic rune that casts q to ~(bunt al p), 
i.e., the icon of p.

        Tall
          ^-  p
              q

        Wide
          ^-(p q)

        Irregular
          `p`q

        Reduction
          See ++open

    [%ktpm p=twig]                              

^& ("ketpam") is a natural rune that converts a %gold core to %zinc core.
See geometric polymorphism.

        Tall
          ^&  p

        Wide
          ^&(p)

        Irregular
          none

        Reduction
          none, natural

    [%ktsg p=twig]                              

^~ ("ketsig") is a natural rune that tries to execute p statically at
compile time; if this fails, p remains dynamic.

        Tall
        ^~  p

        Wide
        ^~(a)

        Irregular
          none

        Reduction
          none, natural

    [%ktts p=toga q=twig]                       

^= ("kettis") is a natural rune that wraps q in the toga p. The 
toga is a powerful naming device that can assign an entire name 
tree to a properly typed result. For instance, if foo produces 
an unlabeled tuple [x y z], [a b=[c d]]=foo produces 
[a=x b=[c=y d=z]].

        Tall
          ^=  p
              q

        Wide
          ^=(p q)

        Irregular
          none

        Reduction
          none, natural

    [%ktwt p=twig]                              

^? ("ketwut") is a natural hoon that converts a %gold core into a
%lead core. See geometric polymorphism.

        Tall
          ^?  p

        Wide
          ^?(p)

        Irregular
          none

        Reduction
          none, natural

    [%sgbr p=twig q=twig]                       

~| ("sigbar") is a synthetic rune that presents the product of p
in the stack trace if q crashes. Only performed as needed. 
Generates %cain - see the Biblical names.

        Tall
          ~|  p
              q

        Wide
          ~|(p q)

        Irregular
          none

        Reduction
          See ++open, ++feck

    [%sgcb p=twig q=twig]                       

~_ ("sigcab") is a synthetic rune that inserts p, a trap producing a tank,
into the trace of q.

        Tall
          ~_  p
              q

        Wide
          ~_(p q)

        Irregular
          none

        Reduction
          See ++open

    [%sgcn p=chum q=twig r=tyre s=twig]         

~% ("sigcen") is a synthetic rune that identifies a core for specific
optimization. See jet propulsion.

        Tall
          ~%    p
              q 
            ==
              p.i.r  q.i.r
              p.i.t.r  q.i.t.r
            ==
          s

          ~%    p
              q
            ~
          s

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%sgfs p=chum q=twig]                       

~/ ("sigfas") is a synthetic rune that identifies an arm for specific
optimization. See jet propulsion.

        Tall
          ~/  p
          q

        Wide
          ~/(p q)

        Irregular
          none

        Reduction
          See ++open

    [%sggl p=$|(term [p=term q=twig]) q=twig]   

~< ("siggal") is a synthetic rune that applies arbitrary hint p to 
the product of q. Does not wake the hint engine until the
computation is finished.

        Tall
          ~<  p
              q

        Wide
          ~<(p q)

        Irregular
          none

        Reduction
          See ++open
        
    [%sggr p=$|(term [p=term q=twig]) q=twig]
        
~> ("siggar") is a natural rune that applies arbitrary hint p to q.
         
        Tall
          ~>  p
              q

        Wide
          ~>(p q)

        Irregular
          none

        Reduction
          See ++open             

    [%sgbc p=term q=twig]
   
~$ ("sigbuc") is a synthetic rune that labels computation q as p 
for profiling (not currently enabled).

        Tall
          ~$  p
              q
        
        Wide
          ~$(p q)
        
        Irregular
          none

        Reduction
          See ++open
  
    [%sgls p=@ q=twig]
        
XX Solve        ~+ ("siglus") is a synthetic rune that memoizes computation q
        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%sgpm p=@ud q=twig r=twig]

~& ("sigpam") is a synthetic rune that prints q on the console
before computing r. p is the log priority 0-3, defaulting to 0.

        Tall
          0, debug
            ~&  q
            r

          1, notice
            ~&  >  q
            r

          2, warning
            ~&  >>  q
            r

          3, alarm
            ~&  >>>  q
            r              

        Wide
          ~&(>>> q r)

        Irregular
          none

        Reduction
          See ++open

    [%sgts p=twig q=twig]

~= ("sigtis") is a synthetic rune that hints to the interpreter
that q may produce a noun equal to the already existing p,
avoiding duplication.

        Tall
          ~=  p
              q

        Wide
          ~=(p q)
        
        Irregular
          none

        Reduction
          See ++open

    [%sgwt p=@ud q=twig r=twig s=twig]

~? ("sigwut") is a synthetic rune that prints r to the console
before computing s, iff q produces yes. p is the log priority,
0-3, 0 by default

        Tall
          0, debug
            ~?  q
              r
            s                

          1, notice
            ~?  >  q
              r 
            s

          2, warning
            ~?  >>  q                
              r
            s

          3, alarm
            ~?  >>>  q
              r
            s
            
        Wide
          ~?(>>> q r s)

        Irregular
          none

        Reduction
          See ++open

    [%sgzp p=twig q=twig]

~! ("sigzap") is a natural rune for debugging uses only,
semantically equivalent to its own twig q. Should compilation
fail within q, ~! will show the type of p on the stacktrace.

        Tall
          ~!  p
              q

        Wide
          ~!(p q)

        Irregular
          none

        Reduction
          none, natural

    [%smcl p=twig q=tusk]

;: ("semcol") is a synthetic gate that applies p, a binary gate,
to the n-ary tuple q.

        Tall
          ;:  p
            i.q
            i.t.q
            i.t.t.q
          ==

        Wide
          ;:(p i.q i.t.q i.t.t.q)

        Irregular
          :(p i.q i.t.q i.t.t.q)

        Reduction
          See ++open

    [%smdt p=twig q=tusk]
XX determine function

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smdq p=(list beer)]
XX determine if internal/external

;" ("semdoq") is a synthetic rune used to make strings,
interpolated or not.

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smsg p=twig q=tusk]

XX to do            

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smsm p=twig q=twig]

;; ("semsem") is a synthetic rune that types q as a fixpoint of p.
Semantically identical to ((hard p) q).

        Tall
          ;;  p
              q

        Wide
          ;;(p q)

        Irregular
          none

        Reduction
          See ++open

    [%tsbr p=tile q=twig]

=| ("tisbar") is a synthetic rune that pushes ~(bunt al p) on the
subject and sends it to q.

        Tall
          =|  p
              q

        Wide
          =|(p q)

        Irregular
          none

        Reduction
          =+(_p q)
          See ++open, ++bunt in ++al

    [%tscl p=tram q=twig]

=: ("tiscol") is a synthetic rune that produces q with the subject
by p. Uses %cncb, and so cannot change the subject type.

        Tall
          =:  p.i.p  q.i.p
              p.i.t.p  q.i.t.p
              p.i.t.t.p  q.i.t.t.p
            ==
          q

        Wide
          none

        Irregular
          noen

        Reduction
          See ++open

    [%tscn p=twig q=twig]
XX to do
        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%tsdt p=wing q=twig r=twig]

=. ("tisdot") is a synthetic rune that produces r with p in the
subject set to q. Uses %cncb, and so cannot change the subject
        p.

        Tall
          =.  p
            q
          r
        
          =.  p  q
          r

        Wide
          =.(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%tsfs p=twig q=twig]
        XX not used

    [%tsgl p=twig q=twig]

=< ("tisgal") is a synthetic rune that uses the product of q as 
the subject of p.

        Tall
          =<  p
          q
        
        Wide
           =<(p q)

        Irregular

        Reduction
          See ++open

    [%tshp p=twig q=twig]

=- ("tishep") is a synthetic rune that pushes q on the subject
and sends it to p. Dual of =+ ("tislup")
        

        Tall
          =-  p
          q

        Wide
          =-

        Irregular
          none

        Reduction
          See ++open

    [%tsgr p=twig q=twig]

=> ("tisgar") is a natural rune that uses the product of p as the
subject of q.

        Tall
          =>  p
          q

        Wide
          =>(p q)

        Irregular
          none

        Reduction
          none, natural

    [%tskt p=twig q=twig r=twig s=twig]

=^ ("tisket") is a synthetic rune that handles a product which is
a cell of the new result, and a mutation to the subject.

        Tall
          Kingside
            =^    p
                q
              r
           s

          Queenside
            =^  p  q
              r
            s      

        Wide
          =^(p q r s)

        Irregular
          none

        Reduction
          See ++open

    [%tsls p=twig q=twig]

=+ ("tislus") is a synthetic rune that pushes p on the subject
and sends it to q. Semantically equavlent to Nock 8.Dual of =- ("tishep")

        Tall
          =+  p
          q

        Wide
          =+(p q)

        Irregular
          none           

        Reduction
          See ++open

    [%tspm p=tile q=twig]

        XX not used

    [%tspt p=tile q=twig]

        XX not used

    [%tstr p=term q=wing r=twig]

=* ("tistar") is a natural rune that creates a %bull, or alias,
type.

        Tall
          =*  p  q
              r

        Wide
          =*(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%tssg p=tusk]

=~ ("tissig") is a synthetic rune that composes a list of twigs.

        Tall
          Kingside
            =~    i.p
                i.t.p
                i.t.t.p
            ==

          Queenside
            =~  i.p
                i.t.p
                i.t.t.p
            ==

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%wtbr p=tusk]

?| ("wutbar") is a synthetic rune that computes the "or" of the
loobeans in p.

        Tall
          ?|  i.p
              i.t.p
              i.t.t.p
          ==            

        Wide
          ?|(i.p i.t.p i.t.t.p)

        Irregular
          |(i.p i.t.p i.t.t.p)

        Reduction
          See ++open

    [%wthp p=wing q=tine]

?- ("wuthep") is a synthetic rune that selects a case in q for
the actual type of p.

        Tall
          Kingside
            ?-  p
              p.i.q  q.i.q
              p.i.t.q  q.i.t.q
              p.i.t.t.q  q.i.t.t.q
            ==  
          
          Queenside
            ?-    p
                p.i.q
              q.i.q
                p.i.t.q
              q.i.t.q
                p.i.t.t.q
              q.i.t.t.q
            ==

        Wide
          ?-(p p.i.q q.i.q, p.i.t.q q.i.t.q, p.i.t.t.q q.i.t.t.q) 

        Irregular
          none
        
        Reduction
          See ++open

    [%wthz p=tiki q=tine]

"wuthaz" is a synthetic internal rune that selects a case in q 
for the actual type of p.

        Tall/Wide/Irregular
          none, internal            

        Reduction
          See ++open

    [%wtcl p=twig q=twig r=twig]

?: ("wutcol") is a natural rune that produces q if p is yes (&, 0),
or r if p is no (|, 1).

        Tall
          ?:  p
            q 
          r

        Wide
          ?:(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%wtdt p=twig q=twig r=twig]

?. ("wutdot") is a synthetic rune that prduces r if p is yes 
(&, 0), of q if p is no (|, 1).

        Tall
          ?.  p
            q
          r

        Wide
          ?:(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%wtkt p=wing q=twig r=twig]

?^ ("wutkey") is a synthetic rune that evaluates r if p is 
equivalent to the bunt for its tile, otherwise q is evaluted.

        Tall
          ?^  p
            q
          r

        Wide
          ?^(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtkz p=tiki q=twig r=twig]

"wutkaz" is a synthetic, internal rune that evaluates r if p is
equivalent to the bunt for its tile, otherwise q is evaluated.
See tikis.            

        Tall
          ?^  p
            q
          r

        Wide
          ?^(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtgl p=twig q=twig]

?< ("wutgal") is a synthetic hoon that produces q, asserting that
p is no (|, 1).

        Tall
          ?<  p
              q

        Wide
          ?<(p q)

        Irregular
          none 

        Reduction
          See ++open

    [%wtgr p=twig q=twig]

?> ("wutgar") is a synthetic hoon that produces q, asserting that
p is yes (&, 0).

        Tall
          ?>  p
              q

        Wide
          ?>(p q)

        Irregular
          none

        Reduction
          See ++open

    [%wtls p=wing q=twig r=tine]

?+ ("wutlus") is a synthetic rune that selects a case in q for
the actual type of p.

        Tall
          Kingside
            ?+  p
              q
              p.i.r  q.i.r
              p.i.t.r  q.i.t.r
              p.i.t.t.r  q.i.t.t.r
            ==

          Queenside
            ?+    p
              q
                p.i.r
              q.i.r
                p.i.t.r
              q.i.t.r
                p.i.t.t.r
              q.i.t.t.r
            ==

        Wide
          ?+(p p.i.r q.i.r, p.i.t.r q.i.t.r, p.i.t.t.r q.i.t.t.r)

        Irregular
          none
        
        Reduction
          See ++open

    [%wtlz p=tiki q=twig r=tine]
        
"wutlaz" is a synthetic, internal rune that selects a case in q for
the actual type of p.

        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%wtpm p=tusk]

?& ("wutpam") is a synthetic hoon that computes the "and" of the 
loobeans in p.

        Tall
          ?&  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          ?&(i.p i.t.p i.t.t.p)

        Irregular
          none

        Reduction
          See ++open

    [%wtpt p=wing q=twig r=twig]

?@ ("wutpat") is a synthetic hoon that produces q if p is an 
atom, r otherwise.

        Tall
          Kingside
            ?@  p
              q
            r
      
          Queenside
            ?@  p
              q
            r

        Wide
          ?@(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtpz p=tiki q=twig r=twig]

"wutpaz" is a synthetic hoon that produces q if p is an atom, r 
otherwise.

        Tall
          ?@  p
            q
          r

        Wide
          ?@(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtsg p=wing q=twig r=twig]

?~ ("wutsig") is a synthetic rune that produces q if p is ~, r
otherwise.

        Tall
          ?~  p
            q
          r

        Wide
          ?~(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtsz p=tiki q=twig r=twig]

"wutsaz" is a synthetic internal rune that produces q if p is ~, 
r otherwise.

        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%wtts p=tile q=wing]

?= ("wuttis") is a natural rune that produces true if the leg at
wing q is in tile p.

        Tall
          ?=  p
              q

        Wide
          ?=(p q)

        Irregular
          none

        Reduction
          none, natural

    [%wtzp p=twig]

?! ("wutzap") is a synthetic rune that produces the logical "not"
of p. 

        Tall
          ?!  p

        Wide
          ?!(p)

        Irregular
          !p

        Reduction
          See ++open

    [%zpcb p=spot q=twig]
XX tall/wide form
!_ ("zapcab") is a natural rune that puts debugging information
in the stack trace.

        Tall
             
        Wide

        Irregular
          none

        Reduction
          none, natural

    [%zpcm p=twig q=twig]

!, ("zapcom") is a natural rune that inserts twig q as a 
constant, typed with the type of twig p.

        Tall
          !,  p
              q

        Wide
          !,(p q)

        Irregular
          none

        Reduction
          none, natural

    [%zpcn ~]
XX determine function
!% ("zapcen")

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%zpfs p=twig]
XX tall/wide
!/ ("zapfas") is a natural rune that should never be compiled. 
When compiled with error checking turned on, it reports its 
subject as an error.

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%zpgr p=twig]

!> ("zapgar") is a synthetic rune that produces a vase (a 
[type noun] cell) with the value p.

        Tall
          !>  p

        Wide
          !>(p)

        Irregular
          none

        Reduction
          See ++open

    [%zpsm p=twig q=twig]

!; ("zapsem") is a natural rune that produces the product of twig
q as a [type noun] pair, with twig p defining the type of the type.

        Tall
          !;  p
              q

        Wide
          !;(p q)

        Irregular
          none
        
        Reduction
          none, natural

    [%zpts p=twig]

!= ("zaptis") is a natural rune that produces the formula of twig
p as a noun.

        Tall
          !=  p

        Wide
          !=(p)

        Irregular
          none

        Reduction
          none, natural

    [%zpwt p=$|(p=@ [p=@ q=@]) q=twig]

!? ("zapwut") is a synthetic rune that enforces a Hoon version
restriction.

        Tall
          !?  p
          q

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%zpzp ~]

!! ("zapzap") is a natural rune that always causes a crash when
executed. 

        Tall
          none

        Wide
          !!

        Irregular
          none

        Reduction
          none, natural

---

