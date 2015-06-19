section 2eG, parsing (whitespace)
=================================

<h3 id="++dog"><code>++dog</code></h3>

`.` optional gap

    ++  dog  ;~(plug dot gay)                               ::

Dot followed by an optional gap, used in numbers.

    /~zod/try=> 1.234.
                703
    1.234.703
    ~zod/try=> (scan "a.        " ;~(pfix alf dog))
    [~~~. ~]

------------------------------------------------------------------------

<h3 id="++doh"><code>++doh</code></h3>

`@p` separator

    ++  doh  ;~(plug ;~(plug hep hep) gay)                  ::

Phonetic base phrase separator

    /~zod/try=> ~nopfel-botduc-nilnev-dolfyn--haspub-natlun-lodmur-holtyd
    ~nopfel-botduc-nilnev-dolfyn--haspub-natlun-lodmur-holtyd
    /~zod/try=> ~nopfel-botduc-nilnev-dolfyn--
                haspub-natlun-lodmur-holtyd
    ~nopfel-botduc-nilnev-dolfyn--haspub-natlun-lodmur-holtyd
    ~zod/try=> (scan "--" doh)
    [[~~- ~~-] ~]
    ~zod/try=> (scan "--      " doh)
    [[~~- ~~-] ~]

------------------------------------------------------------------------

<h3 id="++dun"><code>++dun</code></h3>

`--` to `~`

    ++  dun  (cold ~ ;~(plug hep hep))                      ::  -- (phep) to ~

Parse phep, `--`, to null, `~`.

    ~zod/try=> (scan "--" dun)
    ~
    ~zod/try=> (dun [[1 1] "--"])
    [p=[p=1 q=3] q=[~ u=[p=~ q=[p=[p=1 q=3] q=""]]]]

------------------------------------------------------------------------

<h3 id="++duz"><code>++duz</code></h3>

`==` to `~`

    ++  duz  (cold ~ ;~(plug tis tis))                      ::  == (stet) to ~

Parse stet, `==`, to null `~`.

    ~zod/try=> (scan "==" duz)
    ~
    ~zod/try=> (duz [[1 1] "== |=..."])
    [p=[p=1 q=3] q=[~ u=[p=~ q=[p=[p=1 q=3] q=" |=..."]]]]

------------------------------------------------------------------------

<h3 id="++gah"><code>++gah</code></h3>

Newline or ' '

    ++  gah  (mask [`@`10 ' ' ~])                           ::  newline or ace

Whitespace component, either newline or space.

    /~zod/try=> ^-  *  ::  show spaces
                """
                   -
                 -
                  -
                """
    [32 32 32 45 10 32 45 10 32 32 45 0]
    /~zod/try=> ^-  *
                """

                """
    [32 32 32 10 32 10 32 32 0]
    /~zod/try=> ^-  (list ,@)
                %-  scan  :_  (star gah)
                """

                """
    ~[32 32 32 10 32 10 32 32]

------------------------------------------------------------------------

<h3 id="++gap"><code>++gap</code></h3>

Plural whitespace

    ++  gap  (cold ~ ;~(plug gaq (star ;~(pose vul gah))))  ::  plural whitespace

Separates tall runes

------------------------------------------------------------------------

<h3 id="++gaq"><code>++gaq</code></h3>

End of line

    ++  gaq  ;~  pose                                       ::  end of line
                 (just `@`10)
                 ;~(plug gah ;~(pose gah vul))
                 vul
             ==

Two spaces, a newline, or comment.

------------------------------------------------------------------------

<h3 id="++gaw"><code>++gaw</code></h3>

Classic whitespace

    ++  gaw  (cold ~ (star ;~(pose vul gah)))               ::  classic white

Terran whitespace

------------------------------------------------------------------------

<h3 id="++gay"><code>++gay</code></h3>

Optional gap.

    ++  gay  ;~(pose gap (easy ~))                          ::

Optional gap.

------------------------------------------------------------------------

<h3 id="++vul"><code>++vul</code></h3>

    ++  vul  %-  cold  :-  ~                                ::  comments
             ;~  plug  col  col
               (star prn)
               (just `@`10)
             ==

Parse comments and produce a null. Note that a comment must be ended
with a newline character.

    ~zod/try=> (scan "::this is a comment \0a" vul)
    ~
    ~zod/try=> (scan "::this is a comment " vul)
    ! {1 21}
    ! exit

------------------------------------------------------------------------
