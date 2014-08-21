section 2eI, parsing (external)       

##++rash
        
Parse a cord with a given rule and crash if the cord isn't entirely parsed.

####Summary

        Build wet %gold gate with sample atom `naf`, rule `sab`
        Produce the slam of scan with:
                Trip slammed with `naf`, to turn `naf` into a tape.
                The rule `sab`

####Examples

        ~tadbyl-hilbel/try=> (rash 'I was the world in which I walked, and what I saw' (star (shim 0 200)))
        "I was the world in which I walked, and what I saw"
        ~tadbyl-hilbel/try=> (rash 'abc' (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (rash 'abc' (jest 'abc'))
        'abc'
        `~tadbyl-hilbel/try=> (rash 'abc' (jest 'ab'))
        ! {1 3}
        ! 'syntax-error'
        ! exit

##++rush

Parse a given with a given rule and produce null if the cord isn't entirely parsed.

####Summary

        Build wet %gold gate with sample atom `naf`, rule `sab`
        Produce the slam of scan with:
                Trip slammed with `naf`, to turn `naf` into a tape.
                The rule `sab`

####Examples

        ~tadbyl-hilbel/try=> (rush 'I was the world in which I walked, and what I saw' (star (shim 0 200)))
        [~ "I was the world in which I walked, and what I saw"]
        ~tadbyl-hilbel/try=> (rush 'abc' (just 'a'))
        ~
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'abc'))
        [~ 'abc']
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'ac'))
        ~
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'ab'))
        ~

##++rust

Parse a tape with a given rule and produce null if the tape isn't entirely parsed.

####Summary

        Build wet %gold gate with sample tape `los`, rule `sab`
        Push `vex` is the rule (full sab) slammed with the beginning of the `los` tape.
        If: `q.vex`, the parsed result, is null,
                Then: Produce null.
        Else: Produce the unit with value 'p.u.q.vex', the parsed text.

####Examples

        ~tadbyl-hilbel/try=> (rust "I was the world in which I walked, and what I saw" (star (shim 0 200)))
        [~ "I was the world in which I walked, and what I saw"]
        ~tadbyl-hilbel/try=> (rust "Or heard or felt came not but from myself;" (star (shim 0 200)))
        [~ "Or heard or felt came not but from myself;"]
        ~tadbyl-hilbel/try=> (rust "And there I found myself more truly and more strange." (jest 'And there I'))
        ~

++  scan

Parse a tape with a given rule and crash if the tape isn't entirely parsed.

####Summary

        Build wet %gold gate with sample tape `los`, rule `sab`
        Push `vex` is the rule (full sab) slammed with the beginning of the `los` tape.
        If: `q.vex` is null,
                Then: Add to the crash with message 'syntax-error''s trace:
                        show slammed with [%m '{%d %d}'], `p.p.vex`, `q.p.vex`, and null
        Else: Produce the parsing output of `vex`

####Examples

        ~tadbyl-hilbel/try=> (scan "I was the world in which I walked, and what I saw" (star (shim 0 200)))
        "I was the world in which I walked, and what I saw"
        ~tadbyl-hilbel/try=> (scan "Or heard or felt came not but from myself;" (star (shim 0 200)))
        "Or heard or felt came not but from myself;"
        ~tadbyl-hilbel/try=> (scan "And there I found myself more truly and more strange." (jest 'And there I'))
        ! {1 12}
        ! 'syntax-error'
        ! exit


