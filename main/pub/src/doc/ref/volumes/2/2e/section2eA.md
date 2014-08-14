section 2eA, packing          

---

##++  cue

Unpack an atom to a noun.  The inverse of jam.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a`.
        Yield noun.
        Push `b` is 0.
        Push `m` is empty map of type (map ,@ ,*).
        Seek subject for q.
        Kick dry %gold trap, yield tuple [p=@ q=* r=_m]
        If (0=(cut 0 [b 1] a)),
                Then, push `c` is (rub +(b) a).
                Produce

####Examples        
 
        ~midlys-rocpet/try=> (cue (jam 1))
        1
        ~midlys-rocpet/try=> (cue 4.657)
        [1 2]
        ~midlys-rocpet/try=> (cue (jam [1 1]))
        [1 1]
        ~tadbyl-hilbel/try=> (cue 39.689)
        [0 19]

---

##++  jam       

Compress a noun to an atom.  The inverse of cue.

####Summary

        Activate jet.
        Build wet %gold gate with sample noun `a`.
        Yield atom.
        Push `b` is 0.
        Push `m` is empty may of type (map ,@ ,*).
                
####Examples

        ~midlys-rocpet/try=> (jam 1)
        12
        ~midlys-rocpet/try=> (jam [1 1])
        817
        ~tadbyl-hilbel/try=> (jam [~ u=19])
        39.689

---

##++  mat       

Encodes length.  Only used internally as helper function to jam and cue.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom a.
        Yield atom a, atom b.
        If: a is 0.
                Then: Produce [1 1]
        Else, push `b` is (met 0 a), the number of bits in `a`.
        Push `c` is (met 0 b), the number of bits in `b`.
        Produce pair: 
                (add (add c c) b) and
                (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))

####Examples

---

##++  rub 

Decodes length.  Only used internally as a helper function to jam and cue.

####Summary

        Activate jet.
        Build wet %gold gold with sample atom a, atom b.
        Yield atom p, atom q.
        Push label `c` on:
                Push `c` is 0, m is (met 0 b), the number of bits in `b`.
                Kick dry %gold trap.  Deny that (gth c m), `c` is greater than `m`.
                Unless:  (cut 0 [(add a c) 1] b)) is 0,
                        Then:  `c`
                Else:  Slam trap with +(c)
        If: c is 0,
        Then: Produce [1 0].
        Else, push `d` is (add a +(c))
        Push `e` is (add (bex (dec c)) (cut 0 [d (dec c)] b)).
        Produce [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]

####Examples

---


