volume 1, Hoon Structures
=======================

XX Introduction/overview of structures goes here.

---

###abel

    ++  abel  typo                                          ::  original sin: type

Aliases ++typo. [#typo]

--- 

###axis

    ++  axis  ,@                                            ::  tree address

A Nock axis, an address inside a Nock noun. 

Clammed atom [#axil].

---

###also

    ++  also  ,[p=term q=wing r=type]                       ::  alias

XX unused?

---

###base

    ++  base  ?([%atom p=odor] %noun %cell %bean %null)     ::  axils, @ * ^ ? ~

The cases of an %axil tile [#tile]. 

See %axil in ++tile. 

--- 

###beer

    ++  beer  $|(@ [~ p=twig])                              ::  simple embed

Used in string interpolation, section 2fD
A literal cord, or a twig producing a tape.

---

###beet

    ++  beet  $|  @                                         ::  advanced embed
              $%  [%a p=twig]                               ::
                  [%b p=twig]                               ::
                  [%c p=twig]                               ::
                  [%d p=twig]                               ::
                  [%e p=twig q=(list tuna)]                 ::
              ==                                            ::

Used in xml interpolation, section 2fD
A literal cord, or a twig producing xml.

---

###bloq

    ++  bloq  ,@                                            ::  blockclass

An atom representing a blocksize, by convention expressed as a power of 2.
Used in section 2cA: bit surgery.

Clammed atom [#axil].

++met measures how many bloqs long an atom is. It takes a bloq
and an atom. In the below example, the 256 is 2 bloqs of 2^3 long. That
is, it takes two bytes to represent the atom 256

    ~zod/try=> (met 3 256) 
    2

---

###calf

    ++  calf  ,[p=(map ,@ud wine) q=wine]                   ::

Used in the type system, section 2fC

See [#wine]

---

###char

    ++  char  ,@tD                                          ::  UTF-8 byte

A single character.

Used in tape parsing, section 2eC

Atom with odor [#type]. A @tD is a single Unicode byte. 

++tape [#tape], which is a string type, is a list of chars.

---

###chub

    ++  chub                                                ::  registered battery
              $:  p=(pair chum tyre)                        ::  definition
                  q=*                                       ::  battery
                  r=(unit (pair axis chub))                 ::  parent
              ==

Used (implicitly?) by compiler to attach jets, section 2fB

---

###chum

    ++  chum  $?  lef=term                                  ::  jet name
                  [std=term kel=@]                          ::  kelvin version
                  [ven=term pro=term kel=@]                 ::  vendor and product
                  [ven=term pro=term ver=@ kel=@]           ::  all of the above
              ==                                            ::

The jet hint information that must be present in the body of a ~/
or ~% rune.

Used by compiler to attach jets, section 2fB

A %fern of the following cases:

-  the jet's name
-  the jet's name and the kelvin number
-  the jet's vendor, name and kelvin number
-  the jet's vendor, name, legacy major.minor version and kelvin number

---

###claw

    ++  claw  
        XX  unused?

---

###clue

    ++  clue  ,[p=axis q=chum r=tyre]                       ::  battery definition

Used (implicitly?) by compiler to attach jets, section 2fB

---

###coat  

    ++  coat  ,[p=path q=vase]                              ::

Used in section 4aG: ames protocol engine

---

###coil  

    ++  coil  $:  p=?(%gold %iron %lead %zinc)              ::
                  q=type                                    ::
                  r=[p=?(~ ^) q=(map term foot)]            ::
              ==                                            ::

Core. XX

Variance, type, and arms XX

Used in intermediate parsing and then converted to a %core twig, section 2fA

---

###coin  

    ++  coin  $%  [%$ p=dime]                               ::
                  [%blob p=*]                               ::
                  [%many p=(list coin)]                     ::
              ==                                            ::

Literal syntax primitive XX

Used to parse `0x2b59` or `~s1`, in section 2fD

---


###cord  

    ++  cord  ,@t                                           ::  text atom (UTF-8)

One of Hoon's two string types (the other being ++tape). A cord is an
atom of UTF-8 text.

Used everywhere XX

Atom with odor. @t is a Unicode atom. The order of bytes in a @t are
little-endian, i.e. the first character in the text is the low byte of
the atom.

    ~zod/try=> `@ux`'foobar'
    0x7261.626f.6f66

---

###date  

    ++  date  ,[[a=? y=@ud] m=@ud t=tarp]                   ::  parsed date

A point in time. 

Used in sections 2cH and 3bc, coversion between @da and other formats.

A loobean designating AD or BC, a year atom, a month atom, and a ++tarp, which
is a day atom and a time.

++yell produces a ++date from a @da (a date atom)

    ~zod/try=> (yell ~2014.6.6..21.09.15..0a16)
    [d=106.751.991.820.172 h=21 m=9 s=15 f=~[0xa16]]

---

###dime  

    ++  dime  ,[p=@ta q=@]                                  ::

Odor-atom pair, used in parsing/printing, section 2fD

---

### dram 

    ++  dram  $%  [| p=(map ,@tas dram)]                    ::  simple unix dir
                  [& p=@ud q=@]                             ::

The structure of a unix filesystem tree.

One of two cases: 

- `|` a directory - a map of names to deeper tree structures.
- `&` a file - a numbered atom of data.

Cards %dire and %pour in zuse require a ++dram argument to target.

---

###each  

    ++  each  |*([a=$+(* *) b=$+(* *)] $%([& p=a] [| p=b])) ::

Dicriminated fork between two types XX

Used everywhere XX

###edge

    ++  edge  ,[p=hair q=(unit ,[p=* q=nail])]              ::  parsing output
    
Half-parsed tape with location metadata XX

Used in section 2eD: combinators, and implicitly everywhere a ++rule is used.

###foot

    ++  foot  $%  [%ash p=twig]                             ::  dry arm, geometric
                  [%elm p=twig]                             ::  wet arm, generic
                  [%oak ~]                                  ::  XX not used
                  [%yew p=(map term foot)]                  ::  XX not used
              ==                                            ::

Arm with wetness XX

Used in chapter 2f: Hoon proper

###gate

    ++  gate  $+(* *)                                       ::  general gate

Used everywhere XX

### gear  

    ++  gear  |*  a=_,*                                     ::  XX list generator
              $_                                            ::
              =|  b=*                                       ::
              |?                                            ::
              ?@  b                                         ::
                ~                                           ::
              [i=(a -.b) t=^?(..$(b +.b))]                  ::

Unused.
              1
###hair  

    ++  hair  ,[p=@ud q=@ud]                                ::  parsing trace

Line and column number.

Used to track position in parsing, section 2eB and 2eC

###hapt  

    ++  hapt  (list ,@ta)                                   ::  XX not used

###like  

    ++  like  |*  a=_,*                                     ::  generic edge
              |=  b=_`*`[(hair) ~]                          ::
              :-  p=(hair -.b)                              ::
              ^=  q                                         ::
              ?@  +.b  ~                                    ::
              :-  ~                                         ::
              u=[p=(a +>-.b) q=[p=(hair -.b) q=(tape +.b)]] ::
              
Type to "parser to that type" type XX

Used for funky vulcan stuff, section 2eC

###limb  

    ++  limb  $|(term $%([%& p=axis] [%| p=@ud q=term]))    ::
    
Reference into subject by name/axis

Used in compilation and grammar, section 2fC-2fD


###line  

    ++  line  ,[p=[%leaf p=odor q=@] q=tile]                ::  %kelp case
    
Dicriminated union unionee XX

Used in compilation and grammar, section 2fC-2fD

###list  

    ++  list  |*  a=_,*                                     ::  null-term list
              $|(~ [i=a t=(list a)])                        ::

Used everywhere XX

###lone

    ++  lone  |*(a=$+(* *) ,p=a)                            ::  just one thing

Used nowhere XX

###mane  

    ++  mane  $|(@tas [@tas @tas])                          ::  XML name/space

Used in XML, section 3bD

###manx  

  ++  manx  ,[g=marx c=marl]                              ::  XML node

Used in XML, section 3bD

###marl  

    ++  marl  (list manx)                                   ::  XML node list

Used in XML, section 3bD

###mars  

    ++  mars  ,[t=[n=%$ a=[i=[n=%$ v=tape] t=~]] c=~]       ::  XML cdata

Used in XML, section 3bD

###mart  

    ++  mart  (list ,[n=mane v=tape])                       ::  XML attributes

Used in XML, section 3bD

###marx  

    ++  marx  ,[n=mane a=mart]                              ::  XML tag

Used in XML, section 3bD

###metl  

    ++  metl  ?(%gold %iron %zinc %lead)                    ::  core variance

Used implicitly in ++coil

###noun

    ++  noun  ,*                                            ::  any noun

Used nowhere XX

###null  

    ++  null  ,~                                            ::  null, nil, etc

Used nowhere XX

###odor  

    ++  odor  ,@ta                                          ::  atom format

Used in ++base

###tarp  

    ++  tarp  ,[d=@ud h=@ud m=@ud s=@ud f=(list ,@ux)]      ::  parsed time

Used in ++date, consequently sections 2cH and 3bc

###time  
###tree  
###nail  
###numb  
###pair  
###pass  
###path  
###pint  
###port  
###post  
###prop  
###qual  
###rege  
###ring  
###rule  
###span
    A restricted text atom for canonical atom syntaxes. The prefix is `~.`.
    There are no escape sequences except `~~`, which means `~`, and `~-`,
    which means `_`. - and .  encode themselves. No other characters
    besides numbers and lowercase letters are permitted.
    ---
    ~zod/try=> `@t`~.foo
    'foo'
    ---
    ~zod/try=> `@t`~.foo.bar
    'foo.bar'
    ---
    ~zod/try=> `@t`~.foo~~bar
    'foo~bar'
    ---
    ~zod/try=> `@t`~.foo~-bar
    'foo_bar'
    ---
    ~zod/try=> `@t`~.foo-bar
    'foo-bar'
    ---
++  spot  
++  tank  
++  tape  
    One of Hoon's two string types (the other being ++cord). A tape is a
    list of chars.
    ---
    ~zod/try=> `(list ,char)`"foobar"
    "foobar"
    ---
    ~zod/try=> `(list ,@)`"foobar"
    ~[102 111 111 98 97 114]
    ---
++  term  
    A restricted text atom for Hoon constants. The only characters
    permitted are lowercase ASCII, - except as the first or last character,
    and 0-9 except as the first character.

    The syntax for @tas is the text itself, always preceded by %. This
    means a term is always cubical. You can cast it to @tas if you like,
    but we just about always want the cube:
    ---
    ~zod/try=> %dead-fish9
    %dead-fish9
    ---
    ~zod/try=> -:!>(%dead-fish9)
    [%cube p=271.101.667.197.767.630.546.276 q=[%atom p=%tas]]
    ---
    The empty @tas has a special syntax, $:

    ~zod/try=> %$
    %$
    ---
    A term without % is not a constant, but a name:

    ~zod/try=> dead-fish9
    ! -find-limb.dead-fish9
    ! find-none
    ! exit
    ---

++  tiki

++  tile  

    A tile is a convenient way of making a well-typed noun.  It can be
    reduced in four ways - cryptically called bunt, clam,
    fish, and whip. each tile corresponds to a well-defined
    type, called its icon. A tile is converted statically into a twig,
    which in turn may (depending on the conversion) produce the icon, test
    for it, etc. And always, the icon is some function of the tile and its
    subject.
    ---
    There are nine cases within ++tile:

    [p=tile q=tile]

        Tiles autocons, just like twigs - a cell of tiles is a tile of a
        cell. 

        (,[@ @] [4 5]) is [4 5], which is the same as [(,@ 4) (,@ 5)],
        producing [4 5]. Clearly, (,[@ @] [4 5])  should not be the same as
        [(,@ [4 5]) (,@ [4 5])] - which would produce merely [0 0].)

        The irregular wide syntax for tile autocons is the same as the
        syntax for twig autocons - eg, [@ @], a cell of atoms. But there is
        also a regular tall/wide tuple syntax, with $: (buccol, %bccl).
        Thus instead of [@ @] we could write:

        $:  @ 
            @ 
        ==

    [%axil p=base]

        An %axil is a simple built-in mechanism for a few basic icons: an
        atom of any odor (@odor, or just @ for the odorless base atom); a
        noun (*); a cell of nouns (^); a loobean ?; and null ~.

    [%bark p=term q=tile]

        Wrap a name round a tile. a=* parses as [%bark %a %noun].

        This is another case where the tile syntax matches the twig syntax,
        but only in the irregular form. The twig equivalent of %bark is of
        course ^= (kettis, %ktts). But the tile is $= (buctis):

        $=  a
        *

        Obviously a silly syntactic arrangement. But you can need it if q
        is really big.

    [%bush p=tile q=tile]

        A %bush is a tile in which there are two kinds of nouns: cells
        whose head is a cell (tile p) and cells whose head is an atom (tile
        q). Its default value is the value of q.

        We don't have to look very far to find a %bush - ++tile is one, as
        is ++twig and ++nock. The rune is $& (bucpam). See ++tile above - p
        is [p=tile q=tile], q is the $%. There is no irregular form.

        What's the use of a %bush? Often in a variety of data structures we
        have something like autocons, in which forming a cell of two
        instances has an obvious default semantics.

        Sure, we could attach these semantics to an atom, and just use a
        %kelp. In twigs, tiles, or nock formulas, we could have an explicit
        cons stem of some sort. But it would be a bulky as compared to
        autocons.

    [%fern p=[i=tile t=(list tile)]]

        A %fern is a non-empty list of cases; its icon is naturally a
        %fork. The programmer is responsible for ensuring that the cases
        are actually orthogonal (unlike with the structured forks, %bush,
        %kelp and %reed). A good general practice is to use %ferns only
        with %leafs.

        For example, a fern that could be %foo or %bar has the irregular
        form ?(%foo %bar), or the regular form

        $?  %foo
            %bar
        ==

        The default value is the first - in this case, %foo.

    [%kelp p=[i=line t=(list line)]]          

        A kelp is the workhorse of tiles - it provides the most common data
        structure in any language, the discriminated union. 

        In Hoon, the head (which must be a leaf) is called the stem. The
        tail (which can be anything) is the bulb. Cases of a kelp are known
        inevitably as fronds. 

        (Yes. We're aware that "kelp" is not properly a singular noun. In
        Hoon - it is properly a singular noun. And that's that. And oddly,
        it's not that hard to run out of four-letter plants.)

        $%, buccen, is a tile rune that produces a %kelp. $% takes a list
        of lines, which are labelled cases, closed by ==.
                                              
        $%  p 
            q 
        ==
    [%leaf p=term q=@]

        A %leaf is an atomic constant of value q and odor p. Obviously its
        icon is a %cube.

        The syntax for a leaf is the same as the twig syntax, except that %
        is never required to generate a cube. For instance, as a twig, 7
        has a type of [%atom %ud]; %7 has a type of [%cube 7 [%atom %ud]].
        But the icon of the leaf 7 is, again, [%cube 7 [%atom %ud]].

    [%reed p=tile q=tile]

        A %reed is a tile whose icon contains two kinds of nouns: atoms of
        tile p and cells of tile q.

        There is no irregular form of %reed. The regular form is:

        $|  ~ [@ @] ==

        or in wide mode $|(~ [@ @])

    [%herb p=twig]

        You can write your own tile which is just a gate, accepting a
        sample of * and normalizing it as you choose. If you use a twig as
        a tile, it's treated as an herb.

        For example, when we define a gate like ++base, as defined above
        (remember that when we use a tile as a twig, we get the clam, ie,
        the normalizing gate) base is just an arm which produces a gate.
        Nothing has any idea that this gate is built from a tile of its
        own.

        So when we parse [p=base q=base] as a tile, the parser builds the
        noun:

        [[%bark %p %herb %cnzy %base] [%bark %q %herb %cnzy %base]] In
        other words, base in p=base is actually a twig, but this twig
        happens to produce a normalizing gate generated by clamming a tile.
        In time this will come to seem totally straightforward, but don't
        be surprised if it confuses you now.

        The important thing to remember about %herb is that the actual twig
        we provide will be applied when we whip or clam. Hence, arbitrary
        normalization and/or verification procedures may be part of the
        herbaceous custom tile.
    ---

++  toga  
++  trel  
++  tuna  
    
    An XML template tree.  

    Leaf %a contains plain-text, %b an empty tag, %c a static list, %d a 
    dynamic list, %e a full node element containing a twig and a list of
    tuna, and %f is a empty node.
    
++  twig    TODO: delete Form, normalize indentation

    A twig is an abstract syntax tree or AST, which we produce when we
    parse a Hoon expression, file, etc.  A twig is a noun that's converted
    into a Nock formula, with the assistance of a type which describes the
    subject of the formula:

    [subject-type twig] => formula

    But actually this isn't quite right, because Hoon does something called
    "type inference." When we have a type that describes the subject for
    the formula we're trying to generate, as we generate that formula we
    want to also generate a type for the product of that formula on that
    subject. So our compiler computes:

    [subject-type twig] => [product-type formula]

    As long as subject-type is a correct description of some subject, you
    can take any twig and compile it against subject-type, producing a
    formula such that *(subject formula) is a product correctly described
    by product-type.

    Actually, this works well enough that in Hoon there is no direct syntax
    for defining or declaring a type. There is only a syntax for
    constructing twigs. Types are always produced by inference.
    ---

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

++  tine  
++  tusk  
++  tyre  
++  tyke  
++  tram  
++  tone  
++  nock  
++  toon  
++  tune  
++  twin  
++  type  

    A type is a noun that:

    One, it defines a set of nouns. Any finite noun is either in this set, or
    not in it.

    Two, it ascribes semantics to all nouns in this set. For example, a Hoon
    type exports a semantic namespace.

    A lot of other languages use dynamic types, in which the type of a
    value is carried along with the data as you use it. Even languages like
    Lisp, which are nominally typeless, look rather typed from the Hoon
    perspective. For example, a Lisp atom knows dynamically whether it's a
    symbol or an integer. A Hoon atom is just a Nock atom, which is just a
    number. So without a static type, Hoon doesn't even know how to print
    an atom properly.
    ---
    
    There are 9 cases of ++type:

    ?(%noun %void)

        Either %noun or %void. %noun is the set of all nouns. %void is the
        set of no nouns. 

        ~zod/try=> :type; *
        0
        *

        ~zod/try=> -:!>(*)
        %noun

        ~zod/try=> :type; `*`%noun
        1.853.189.998
        *

        ~zod/try=> -:!>(`*`%noun)
        %noun

        We can't show any examples producing %void - by definition, none of
        them would terminate. Because that's what %void means. All other
        cases in ++type are subsets of %noun.

    [%atom p=term]

        An atom is a natural number, with a term (known as an
        odor) that specificies an atomic subtype.

        An odor is an ASCII span. This span is a taxonomy which
        grows more specific to the right. For instance, @t
        for UTF-8 text, @ta for URL-safe ASCII text, @tas for
        a Hoon symbol; or @u for an unsigned integer, @ux for
        an unsigned integer formatted as hexadecimal.

        Hoon knows about the following odors, with defined meanings:

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

        Atoms change freely either up or down the taxonomy,
        but not across. You can treat a @tas as
        a @t, as in a strong type system; but you can also
        treat a @t as a @tas, or an @ as anything. However,
        passing a @t to a function that expects an @ux is a
        type error.

        Each of these forms has a URL-safe syntax. Each
        parses as an atomic constant in Hoon, and each is
        printed by the Hoon prettyprinter.
        ---

        ~zod/try=> :type; 0x42
        0x42
        @ux

        ~zod/try=> `@ud`0x42
        66

        ~zod/try=> :type; 'foo'
        'foo'
        @ta

        ~zod/try=> `@ud`'foo'
        7.303.014

        ~zod/try=> :type; ~2013.12.6
        ~2013.12.6
        @da

        ~zod/try=> `@ud`~2013.12.6
        170.141.184.500.724.667.905.957.736.036.171.776.000

        ~zod/try=> `@ud`.127.0.0.1
        2.130.706.433

        ~zod/try=> :type; .127.0.0.1
        .127.0.0.1
        @if

        ~zod/try=> :type; ~m45
        ~m45
        @dr

        ~zod/try=> `@ud`~m45
        49.806.208.999.015.789.363.200

        ~zod/try=> :type; `@da`(add ~2013.12.6 ~m45)
        ~2013.12.6..00.45.00
        @da

        ---
        The variety of units and formats which an atom can
        represent is essentially infinite. The set of
        syntaxes which Hoon can parse and print is
        fundamentally limited.

        For instance, Hoon has no syntax which means "number
        of miles." But within your program, nothing stops you
        from using the odor system to distinguish a number of
        miles from, for instance, a number of kilometers:

        ~zod/try=> `@udm`25.717
        25.717
        ~zod/try=> `@udk`25.717
        25.717

        The printer has no idea what a @udm is, but it knows
        what a @ud is and can print accordingly. Then, if you
        have a function which expects a @udm and you try to
        pass it a @udk, it will fail.
        ---

        Besides these prefixes, which indicate the rendering
        and/or meaning of atoms, the odor system has another
        orthogonal mechanism to restrict the size of atoms.
        Like the prefix, this mechanism is weak - it is not
        enforced and trivially evaded.

        An odor span contains two parts, both optional: a
        lowercase prefix and an uppercase suffix. The suffix,
        if present, is a single character A-Z c which
        indicates an atom of size less than or equal to n
        bits, where n is 1 << (c - 'A'). Thus, @tD is one
        UTF-8 byte (whatever that means); @tN is a kilobyte
        or less of UTF-8.

        When enforcing conversions, @t has no size
        information and can be used as @tD; and @tD, of
        course, can be used as @t. But using @tN as @tD is an
        error. There is no way to generate the smell of size
        from a constant without a cast. And of course
        arithmetic results have no odor at all.

        A full table for convenience:

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

        You of course can build an atom larger than 4MB, but
        the type system cannot express a size odor above 4MB.


      [%bull p=twin q=type]

      [%cell p=type q=type]

          A pair of types. Set: all cells of p and q. 

          ~zod/try=> :type; [3 4]
          [3 4]
          [@ud @ud]

          ~zod/try=> -:!>([3 4])

          ~zod/try=> :type; [3 4]
          [3 4]
          [@ud @ud]

          ~zod/try=> -:!>([3 4])
          [%cell p=[%atom p=%ud] q=[%atom p=%ud]]

      [%core p=type q=coil]

      [%cube p=* q=type]

          When we enter an ordinary constant, like 42, its type [%atom %ud]
          is the set of all atoms (with odor @ud, but any atom can have that
          or any odor). Its type is certainly not the set consisting
          exclusively of the value 42.

          But here's how we produce this "cubical" constant:

          ~zod/try=> :type; %42
          %42
          %42

          ~zod/try=> -:!>(%42)
          [%cube p=42 q=[%atom p=%ud]]
          
          In general, a %cube type contains p, a single noun, and q, a base
          type which provides semantics.

          Syntactically, any atomic constant can be preceded by % to generate
          a cube. The exception is @tas, which always needs % and is always
          cubical.

    [%face p=term q=type]

        A type is not just a set of nouns - it's also a meaning which makes
        sense of any noun in that set. The typed noun exports a namespace -
        give it a name, and it gives you another noun.

        ~zod/try=> foo=42
        foo=42
        ~zod/try=> :type; foo=42
        foo=42
        foo=@ud
        ~zod/try=> -:!>(foo=42)
        [%face p=%foo q=[%atom p=%ud]]

        With %face, we've simply wrapped a label around another type. Note
        that this doesn't impair our ability to compute with the value.
        Computationally, foo=42 is just 42:

         ~zod/try=> (add 17 foo=42)
         59
         ---

    [%fork p=type q=type]

        A union type. [%fork p q] means "it could be a p, or maybe a q."

        Any branching computation in which different branches produce
        different types will generate a fork. For example:

        ~zod/try=> :type; ?:(& %foo [13 10])
        %foo
        { %foo [@ud @ud] }

        ~zod/try=> -:!>(?:(& %foo [13 10]))
        [ %fork
          p=[%cube p=7.303.014 q=[%atom p=%tas]]
          q=[%cell p=[%atom p=%ud] q=[%atom p=%ud]]
         ]

        Here we start to understand why the type renderer is useful, as 
        { %foo [@ud @ud] } (which is not in any way Hoon syntax) is a 
        little easier to read than the actual type noun.

        (Readers of a mathematical bent may ask: since Hoon has a union
        type, where is the intersection type? There is none. Hoon is not
        one of these languages whose goal is to be as mathematically
        powerful as possible. Since a programming language is a UI for
        programmers, and programmers are not mathematicians, Hoon is
        designed to be as powerful as it has to be - and no more.)

    [%hold p=(list ,[p=type q=twig])]

++  typo  
++  udal  
++  udon  
++  umph  
++  unce  
++  unit  
++  upas  
++  urge  
++  vase  
++  vise  
++  wall  
++  wain  
++  wing  
++  wine  
++  woof  
++  wonk  
++  map  
++  qeu  
++  set  
++  jar  
++  jug  


