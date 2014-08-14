section 2eO, virtualization
---

##++mack

Accpet a nock subject-formula cell.
Produce a unit result, treating 11 as a crash (i.e. pure nock).

####Summary

        Creates a dry %gold gate accepting cell ['sub' 'fol'].
        Its output is a unit (of a noun).
        Let 'ton' be the result of minking the sample, with a sky that produces
        ~ on any input, halting interpretation.
        Unless ton has stem 0, produce the empty unit, otherwise produce one
        containing ton's bulb.

####Examples

        ~zod/try=> (mack [[1 2 3] [0 1]])
        [~ [1 2 3]]
        ~zod/try=> (mack [41 4 0 1])
        [~ 42]
        ~zod/try=> (mack [4 0 4])
        ~
        ~zod/try=> (mack [[[0 2] [1 3]] 4 4 4 4 0 5])
        [~ 6]
        ~zod/try=> ;;((unit ,@tas) (mack [[1 %yes %no] 6 [0 2] [0 6] 0 7]))
        [~ %no]

##++mink

Bottom-level mock (virtual nock) interpreter.

####Summary

        Accepts a nock subject-formula cell, and an %iron gate which 
          accepts any noun and produces a unit, which is defined to be mock 11.
        Produces a ++tone, which is the result of the virtualized computation.
        ---
        For clarity, a ++tone with stem %0 will be referred to as a "success",
        one with stem %1 as a "block", and one with stem %2 as a "crash".
        ---
        Activate jet.
        Creates a dry %gold gate accepting cell ['sub' 'fol'] and gate 'sky'.
        Let 'tax' be a statically bunted list of term-noun pairs. (hint list)
        Do (recursion point) produce a tone:
        If fol is an atom
          Produce a crash of fol.
        Else if the head of fol is a cell
          Let hed be the result of recurring with fol replaced by its head.
          If hed is a crash
            Yield it
          Otherwise let 'tal' be the result of recurring with fol replaced 
          by its tail.
          Switch on the type of tal by stem:
            If tal is a success
              If hed is a block produce hed.
              Else (success) produce a success of a cell of the bulbs of hed 
              and tal.
            If tal is a block
              If hed is a success produce tal.
              Else (block) produce a block of welding the bulbs of hed and tal.
            Else (crash) produce tal
        Otherwise (the head of fol is an atom) switch on fol,
          by default producing a crash of tax.
            If fol has stem 0 and an atom bulb we name 'b'
          If b is 0 produce a crash of tax.
          If b is 1 produce a success of sub.
          If sub is an atom produce a crash of tax
          Otherwise let 'now' be the cap of b, and 'lat' be the mas of b
          Tail-recur with b replaced by lat, and sub replaced by: if now is 2,
          its head, else its tail.
            If fol has stem 1 and a bulb we name 'b'
          Produce a success of b
            If fol has stem 2 and a bulb whose head is a cell.
          Let 'ben' be the result of recurring with fol replaced by its bulb.
          Unless ben is a success, produce ben.
          Else assert that ben contains a cell, and tail-recur with 
          sub and fol replaced by the head and tail of ben's bulb
            If fol has stem 3 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else produce a success of (loobean) whether ben contains a cell.
            If fol has stem 4 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else unless ben contains an atom produce a crash of tax.
          Otherwise produce a success of ben's contents, incremented.
            If fol has stem 5 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else unless ben contains a cell produce a crash of tax.
          Otherwise produce a success of (loobean) whether the bulb of ben has
          a tail equal to its head.
            If fol has stem 6, 7, 8, or 9
          Tail-recur with its bulb expanded per nock specification.
            If fol has stem 10 and a cell bulb whose head is an atom
          Tail-recur with for replaced by its bulb's tail
            If fol has stem 10 and a bulb that can be destructured as [[b c] d]
          Let ben be the result of recurring with fol replaced by v.
          Unless ben is a success, produce ben.
          If b is %hunk, %lose, %mean, or %spot
            Tail-recur with fol replaced by d and tax prepended with a pair of
            b and the bulb of ben.
          Else tail-recur with just fol replaced by d.

####Examples

        XX

##++mock

Accepts a nock subject-formula cell and an %iron gate which
accepts any noun and produces a unit (this is used as nock 11).
Produces a ++toon, which is a sucesful, blocked, or crashed result.

####Summary

        Compose ++mook and ++mink.

####Examples 

        ~zod/try=> (mock [5 4 0 1] ,~)
        [%0 p=6]
        ~zod/try=> (mock [~ 11 1 0] |=(* `999))
        [%0 p=999]
        ~zod/try=> (mock [~ 0 1.337] ,~)
        [%2 p=~]
        ~zod/try=> (mock [~ 11 1 1.337] ,~)
        [%1 p=~[1.337]]
        ~zod/try=> (mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[+<]))
        [%0 p=14]
        ~zod/try=> (mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[<+<>]))
        [%0 p=[49 52 0]]
        ~zod/try=> ;;(tape +:(mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[<+<>])))
        "14"

####++mook

Intelligently render crash annotation.

####Summary

        Accepts a ++tone, produces a ++toon
        ---
        Create a dry %gold gate accepting a tone we name 'ton'
        Its output is a toon.
        Unless the stem of ton is %2, produce ton.
        Produce a frond with a stem of 2 and the following bulb:
        Let yel be the length of ton's bulb.
        Replace the bulb of ton,
          If yel > 256 (otherwise keep it static)
          With the weld of
            its top 128 elements
          And a list of term-noun pairs:
          The last 128 elements of ton's bulb
          Preceded by a %lose-stemmed frond of
          A cord from the tape
            "[skipped "
            A @ud rendering of yel - 256
            " frames]"
        Do (*recursion point*) produce a list of tanks:
        For each element in the bulb of ton
        Switch on its leaf, by default leaving the element out of the product
          For each %hunk, clam it to a tank
          For each %lose, produce a leaf with the element clammed to an atom 
          and tripped (to a tape).
          For each %mean, if the elment is an atom treat it as a %lose
              Otherwise let mac be the element macked by its tail.
              If the computation fails, produce a "####" leaf, else clam
              the result to a tank.
          For each %spot, let sot be the element clammed to a spot.
              Produce a leaf with
              The weld of
                The path in sot converted to a tank and then a tape
                ":<["
                [[p.p ] ] in the pint in sot rendered as @ud
                " "
                [[ q.p] ] in the pint in sot rendered as @ud
                "].["
                [ [p.q ]] in the pint in sot rendered as @ud
                " "
                [ [ q.p]] in the pint in sot rendered as @ud
                "]>"
                
####Examples 

        ~zod/try=> (mook [%0 5 4 5 1])
        [%0 p=[5 4 5 1]]
        ~zod/try=> (mook [%2 ~[[%hunk %rose ["<" "," ">"] ~[[%leaf "err"]]]]])
        [%2 p=~[[%rose p=[p="<" q="," r=">"] q=[i=[%leaf p="err"] t=~]]]]
        ~zod/try=> (mook [%2 ~[[%malformed %elem] [%lose 'do print']]])
        [%2 p=~[[%leaf p="do print"]]]
        ~zod/try=> (mook [%2 ~[[%spot /b/repl [[1 1] 1 2]] [%mean |.(!!)]]])
        [%2 p=~[[%leaf p="/b/repl/:<[1 1].[1 2]>"] [%leaf p="####"]]]

---

####++mang

Work just like in `++makc`, but accept a `++sky`.
Produce a unit computation result.

####Summary

        Creates a dry %gold gate accepting cell ['sub' 'fol'] and an
        %iron unit-clam 'sky'.
        Its output is a unit (of a noun).
        Let 'ton' be the result of monging the sample.
        Unless ton has stem 0, produce the empty unit, otherwise produce one
        containing ton's bulb.
---

##++mung

---

##++mule 

---

##++mute 

---

