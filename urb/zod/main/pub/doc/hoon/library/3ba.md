section 3bA, lite number theory
===============================

<h3 id="++fu"><code>++fu</code></h3>

    ++  fu                                                  ::  modulo (mul p q)
      |=  a=[p=@ q=@]
      =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
      |%

XX document

<h3 id="++dif"><code>++dif</code></h3>

      ++  dif
        |=  [c=[@ @] d=[@ @]]
        [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
      ::

XX document

<h3 id="++exp"><code>++exp</code></h3>

      ++  exp
        |=  [c=@ d=[@ @]]
        :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
        (~(exp fo q.a) (mod c (dec q.a)) +.d)
      ::

XX document

<h3 id="++out"><code>++out</code></h3>

      ++  out                                               ::  garner's formula
        |=  c=[@ @]
        %+  add
          +.c
        (mul q.a (~(pro fo p.a) b (~(dif fo p.a) -.c (~(sit fo p.a) +.c))))
      ::

XX document

<h3 id="++pro"><code>++pro</code></h3>

      ++  pro
        |=  [c=[@ @] d=[@ @]]
        [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
      ::

XX document

<h3 id="++sum"><code>++sum</code></h3>

      ++  sum
        |=  [c=[@ @] d=[@ @]]
        [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
      ::

XX document

<h3 id="++sit"><code>++sit</code></h3>

      ++  sit
        |=  c=@
        [(mod c p.a) (mod c q.a)]

XX document
