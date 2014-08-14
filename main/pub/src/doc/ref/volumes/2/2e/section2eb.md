section 2eB, parsing (tracing)

---

##++  last

Compare two [line column] pairs and produce the one which is farther along in text.

####Summary

        Build dry %gold gate with sample hair `zyc`, hair `naz`
        Yield hair.
        If: p.zyc is p.naz,
                Then: If: q.zyc is greater than q.naz,
                        Then: Produce zyc,
                      Else:  Produce naz.
        Else: If: p.zyc is greater than p.naz,
                        Then: Produce zyc,
              Else: Produce naz.

####Examples

        ~tadbyl-hilbel/try=> (last [1 1] [1 2])
        [p=1 q=2]
        ~tadbyl-hilbel/try=> (last [2 1] [1 2])
        [p=2 q=1]
        ~tadbyl-hilbel/try=> (last [0 0] [99 0])
        [p=99 q=0]
        ~tadbyl-hilbel/try=> (last [7 7] [7 7])
        [p=7 q=7]

---

##++  lust

Produce the beginning of the next line after a newline character or increment the 
column number - The index of the next character to be parsed.

####Summary

        Build dry %gold gate with sample char `weq`, hair `naz`
        Yield hair.
        If: `weq` is 10,
                Then: Produce [+(p.naz) 1].
        Else: Produce [p.naz +(q.naz)].

####Examples

        ~tadbyl-hilbel/try=> (lust `a` [1 1])
        [p=1 q=2]
        ~tadbyl-hilbel/try=> (lust `@t`10 [1 1])
        [p=2 q=1]
        ~tadbyl-hilbel/try=> (lust '9' [10 10])
        [p=10 q=11]
        ~tadbyl-hilbel/try=> (lust `@t`10 [0 0])
        [p=1 q=1]

---


