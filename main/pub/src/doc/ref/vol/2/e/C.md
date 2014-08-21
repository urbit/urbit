section 2eC, parsing (custom rules)   

---

##++  cold  

Build gate to parse a nail with a rule, then replaced the parsed texted with a constant.

####Summary

        Activate jet.
        Build wet %gold gate with sample noun `cus`, bunt of a rule `sef`.
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`.
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: q.vex is an atom,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]]]

####Examples

        ~midlys-rocpet/try=> ((cold %foo (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%foo q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cold %foo (just `a`)) [[1 1] "bc"])
        [p=[p=1 q=1] q=~]

---

##++  cook

Build gate to parse a nail with a rule, then slam a gate with the parsed text.

####Summary

        Activate jet.
        Build wet %gold gate with sample clam gate `poq`, bunt of a rule `sef`.
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`.
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=[~ u=[p=(poq p.u.q.vex) q=q.u.q.vex]]],
                where (poq p.u.q.vex) is gate `poq` slammed with the parsed text.

####Examples

        ~midlys-rocpet/try=> ((cook ,@ud (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook ,@tas (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook |=(a=@ +(a)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=98 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook |=(a=@ `@t`+(a)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=`b` q=[p=[p=1 q=2] q="bc"]]]]

---

##++  easy

Succeed but consume no characters - Produce an edge at the same text position 
with the text to parse unchanged, but with a

####Summary

        Activate jet.
        Build wet %gold gate with sample noun, `huf`, a noun to produce as the parsed value.
        Activate extra parsing jet.
        Build dry %gold date with sample nail, `tub`
        Yield edge of type `huf`
        Produce [p=p.tub q=[~ u=[p=huf q=tub]]], the edge with the noun `huf` as it's parsed value and `tub` as unparsed.

####Examples

        ~tadbyl-hilbel/try=> ((easy %foo) [[1 1] "abc"])
        [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="abc"]]]]
        ~tadbyl-hilbel/try=> ((easy %foo) [[1 1] "bc"])
        [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="bc"]]]]
        ~tadbyl-hilbel/try=> ((easy 'a') [[1 1] "bc"])
        [p=[p=1 q=1] q=[~ [p='a' q=[p=[p=1 q=1] q="bc"]]]]

---

##++  fail  

Fail to parse - Produce a nail at the same text position but with null text.

####Summary

        Build wet %gold gate with sample nail, `tub`.
        Produce nail [p=p.tub q=~].

####Examples

        ~tadbyl-hilbel/try=> (fail [[1 1] "abc"])
        [p=[p=1 q=1] q=~]
        ~tadbyl-hilbel/try=> (fail [[p=1.337 q=70] "Parse me, please?"])
        [p=[p=1.337 q=70] q=~]

---

##++  full  

Demand politely that the parsing rule parse the entire sample nail, produce a null edge otherwise.

####Summary

        Build wet %gold gate with sample rule, `sab`
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule slammed with the text to parse.
        If: Parse of `vex` is null,
                Then: Produce `vex`
        Else: If: The unparsed text in the produced edge is nulll,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=~], the edge with a null unit nail.

####Examples

        ~tadbyl-hilbel/try=> ((full (just 'a')) [[1 1] "ab"])
        [p=[p=1 q=2] q=~]
        ~tadbyl-hilbel/try=> ((full (jest 'ab')) [[1 1] "ab"])
        [p=[p=1 q=3] q=[~ u=[p='ab' q=[p=[p=1 q=3] q=""]]]]
        ~tadbyl-hilbel/try=> ((full ;~(plug (just 'a') (just 'b'))) [[1 1] "ab"])
        [p=[p=1 q=3] q=[~ u=[p=[~~a ~~b] q=[p=[p=1 q=3] q=""]]]]

---

##++  funk

Prepend a tape to the text to be parsed, then parse the new tape.

####Summary

        Build wet %gold gate with sample tape `pre`, rule `sef`
        Build dry %gold gate with sample nail, `tub`
        Produce the rule slammed with the hair index of `tub` and the concatenation of 
        the prefix tape and the `tub` tape.

####Examples

        ~tadbyl-hilbel/try=> ((funk "abc prefix-" (jest 'abc')) [[1 1] "to be parsed"])
        [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=" prefix-to be parsed"]]]]
        ~tadbyl-hilbel/try=> ((funk "parse" (just 'a')) [[1 4] " me"])
        [p=[p=1 q=4] q=~]

---

##++  here  

Apply rule if parsing within a specific line and column range.

####Summary

        Activate jet.
        Build wet %gold gate with sample bunted gate accepting pint `a`, 
        noun `b` and producing cell [a b], and bunt of rule `sef`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`,
        Else: Produce the hair,
                p is `q.tub`
                q is the unit:
                        Null
                        u is: 
                                p is `hez` slammed with the pint cell of tub's position and vex's position.
                                q is `q.u.q.vex`, the text not parsed.

####Examples

---
        
##++  inde

Apply rule to indented block starting at current column number,
omitting the leading whitespace.

####Summary

        Build wet %gold gate with sample rule, 'sef'
        Build dry %gold gate with sample nail and the same product type as sef.
        Let 'har' and 'tap' be p and q within that nail
        Let 'lev' be ' ' repeated once less than the column number in har.
        Let roq be the result of parsing the nail as any number of either
          Printable characters, or
          Newlines followed by ignored lev.
        If roq is a failure produce it.
        Let 'vex' be the result of parsing with sef, with column restarted to
        1, the result in roq which must be a tape.
        If vex is a failure produce it with its p set to that of roq.
        Produce an edge with parse reach from roq, succesful result from vex,
        hair from vex with column number advanced by q.har - 1 (which was 
        subtracted prior passing it to sef), and a continuation tape of:
          Let res be the continuation in vex(section of block unconsumed).
          Build an kick a trap (Do):
            If res is empty produce the continuation in roq.
            Unless res starts with newline produce the head of res followed
            by the result of tossing res for its tail.
            Welp together a newline, lev, and the result of tossing res for
            its tail.

####Examples

---

##++  jest  

Match and consume a cord.

####Summary

        Build dry %gold gate with sample cord `daf`
        Build dry %gold gate with sample nail `tub`
        Push `fad` is `daf`
        Kick dry %gold trap.  Yield edge of cord text.
        If: `daf` is 0,
                Then: Produce the edge with:
                        p is `p.tub`
                        q is the unit:
                                Null
                                u is the cell [p=fad q=tub]
        Else: If: fish for null in q.tub OR compile to Nock the last byte in `daf` and the 
                Then: Produce the failed parse of `tub`,
        Else: Toss `p.tub` for the index of the next character to be parsed, 
        `q.tub` for the tail of `q.tub`, `daf` for the single byte right-shift of `daf`

####Examples

        ~tadbyl-hilbel/try=> ((jest 'abc') [[1 1] "abc"])
        [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "abc" (jest 'abc'))
        'abc'
        ~tadbyl-hilbel/try=> (scan "abc" (jest 'acb'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> ((jest 'john doe') [[1 1] "john smith"])
        [p=[p=1 q=6] q=~]
        ~tadbyl-hilbel/try=> ((jest 'john doe') [[1 1] "john doe"])
        [p=[p=1 q=9] q=[~ [p='john doe' q=[p=[p=1 q=9] q=""]]]]

---

##++  just

Match and consume a single character.

####Summary

        Activate jet.
        Build dry %gold gate with sample char `daf`
        Activate extra parsing jet.
        Build dry %gold gate wtih sample nail `tub`
        Yield char edge.
        If: `q.tub` is null,
                Then: Produce the failed parse of `tub`,
        Else:  Unless: `daf` is `i.q.tub`,
                Then: Produce the failed parse of `tub`,
        Else: Produce the parse of the next character of `tub`

###Examples

        ~tadbyl-hilbel/try=> ((just 'a') [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~tadbyl-hilbel/try=> (scan "abc" (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "a" (just 'a'))
        ~~a
        ~tadbyl-hilbel/try=> (scan "%" (just '%'))
        ~~~25.

---

##++  knee

Callback 

####Summary

        Build wet %gold gate with sample noun `gar`, rule trap `sef`
        Build dry %gold gate with sample nail `tub`
        Yield char edge.
        Produce `tub` slammed to `sef`

####Examples

---

##++  mask  

Match the next char to a list of chars, a tape.

####Summary

        Activate jet.
        Build wet %gold gate with sample (list char) `bud`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Yield char edge.        
        If: `q.tub` is an atom,
                Then: Produce the failed parse of `tub`
        Else: Unless: 
                
####Examples

        ~tadbyl-hilbel/try=> (scan "a" (mask "cba"))
        ~~a
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "bbc"])
        [p=[p=1 q=2] q=[~ [p=~~b q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "dbc"])
        [p=[p=1 q=1] q=~]

---

##++  next  

Always succeeds and consumes a character.

####Summary

        Build dry %gold gate with sample nail `tub`
        Yield char edge.
        If: The text to parse `q.tub` is an atom,
                Then: Produce the failed parse of `tub`
        Else: Push `zac` is lust slammed with:
                The first chaarcter to parse (The head of `q.tub`) and its location in the text.
        Produce the edge with the hair `zac` and unit nail with:
                The character successfully consumed, the head of the text to parse.
                A nail of hair index `zac`, text to be parsed `t.q.tub` (The tail of the text to parse.)

####Examples

        ~tadbyl-hilbel/try=> (next [[1 1] "ebc"])
        [p=[p=1 q=2] q=[~ [p=~~e q=[p=[p=1 q=2] q="bc"]]]] 
        ~tadbyl-hilbel/try=> (next [[1 1] "john jumps jones"])
        [p=[p=1 q=2] q=[~ [p=~~j q=[p=[p=1 q=2] q="ohn jumps jones"]]]]

---

##++  sear  

Conditional cook - Produce the slam of the parsed texted to `b` only if the result is not null.
Else, produce null.

####Summary

        Activate jet.
        Build wet %gold gate with sample tile of gate accepting a noun and producing a unit `pyq`, rule `sef`
        Activate extra parsing jet.
        Build dry %gold with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: The text to be parsed is an atom,
                Then: Produce `vex`,
        Else: Push `gey` is `pyq` slammed with the 
        If: `gey` is an atom,
                Then: Produce the cell with:
                        p is the hair index of the parse failure.
        Else: Produce the cell with:
                p is the hair index of the parse.
                q is the unit with value u is:
                        p is the value of the parsed text slammed to `pyq`
                        q is the value of the unparsed text.

####Examples

        ~midlys-rocpet/try=> ((sear |=(a=* ?@(a (some a) ~)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((sear |=(a=* ?@(a [~ u=a] ~)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]

---

##++  shim  

Match characters within a range.

####Summary

        Activate jet.
        Build wet %gold gate with sample atom `les`, atom `mos`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail, `tub`
        Yield char edge.
        If: `q.tub` is an atom,
                Then: Produce the failed parse of `tub`,
        Else: Unless: `i.q.tub` is greater than or equal to `les` AND `i.q.tub` is less than or equal to `mos`,
                Then: Produce the failed parse of `tub`,
        Else: Produce the single character parse of `tub`

####Examples

        ~midlys-rocpet/try=> ((shim `a` 'z') [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((shim `a` 'Z') [[1 1] "abc"])
        [p=[p=1 q=1] q=~]
        ~midlys-rocpet/try=> ((shim `a` 'Z') [[1 1] "Abc"])
        [p=[p=1 q=2] q=[~ [p=~~~41. q=[p=[p=1 q=2] q="bc"]]]]

---

##++  stag  

Add a label to an edge parsed by a rule.

####Summary

        Activate jet.
        Build wet %gold gate with sample noun `gob`, bunt of a rule `sef`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`
        Else: Produce the edge with hair `p.vex` and unit with value hair u=[p=[gob p.u.q.vex] q=q.u.q.vex]

###Examples

        ~tadbyl-hilbel/try=> ((stag %foo (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=[%foo ~~a] q=[p=[p=1 q=2] q="bc"]]]]
        ~tadbyl-hilbel/try=> ((stag "xyz" (jest 'abc')) [[1 1] "abc"])
        [p=[p=1 q=4] q=[~ u=[p=["xyz" 'abc'] q=[p=[p=1 q=4] q=""]]]]
        ~tadbyl-hilbel/try=> ((stag 10.000 (shim 0 100)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=[10.000 ~~a] q=[p=[p=1 q=2] q="bc"]]]]

---

##++  stet

Listify a list of text position and bunt of rule pairs.

####Summary

        Build wet %gold gate with sample list of position and bunt of rule pairs `leh`
        Kick dry %gold trap.
        If: `leh` is null,
                Then: Produce null.
        Else: Produce the cell,
                with head: The cell of the head of the head of `leh`, p=-.i.leh, the tail of the head of `leh, q=+.i.leh.
                with tail: Toss `leh` for `t.leh`

####Examples

        ~tadbyl-hilbel/try=> (stet (limo [[5 (just 'a')] [1 (jest 'abc')] [[1 1] (shim 0 200)] 
        [[1 10] (cold %foo (just 'a'))]~]))
        ~[
          [p=5 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=1 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=[1 10] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
        ]
        ~tadbyl-hilbel/try=> (stet (limo [[[1 1] (just 'a')] [[2 1] (shim 0 200)] ~]))
        ~[
          [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>] 
          [p=[2 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
        ]

---

##++  stew
        
####Summary

        Activate jet.
        Build wet %gold gate with sample list of position and bunt of rule pairs `leh`
        Push label `wor` on:
                Build dry %gold gate with sample fork between `ort` , fork `wan`

####Examples

---

##++  stir
        
####Summary

        Activate jet.
        Build wet %gold gate with sample noun `rud`, gate accepting two nouns and producing , rule `fel`
        Activate extra parsing jet.
        Build dry %gold with sample nail `tub`
        Yield edge of type of `rud`
        Push `vex` is the rule `fel` slammed with the nail `tub`A
        If: The parsed text is null,
                Then: Produce the edge of unit nail `rud` and `tub` at the hair index of `vex`
        Else: Push `wag` is the toss of `tub` for the unparsed text in the unit nail of `vex`
        Assert that the value 

        Produce the edge with:
                The farthest along hair index of `vex` and `wag`
                The unit nail of 

####Examples

---
        
##++  stun  

Parse several times

####Summary

        Activate jet.
        Build wet %gold gate with sample atom `les`, atom `mos`, rule `fel`
        Activate extra parsing jet.
        Build wet %gold gate with sample nail `tub`
        Yield edge of 
        
        If: `mos` is 0,
                Then: Produce the edge with 

####Examples

---
