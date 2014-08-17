section 2eE, parsing (composers)

---

##++  bass

####Summary

        Build wet %gold gate with sample atom `wuc`, rule `tyd`
        Slam cook with:
                Build dry %gold gate with sample list of atoms, `waq`
                Slam roll with:
###Examples

---

##++  boss

####Summary

        Build wet %gold gate with sample atom `wuc`, rule `tyd`

####Examples

---

##++  ifix
        
####Summary

        Build wet %gold gate with sample cell of rules `fel`, rule `hof`
        Produce pfix gonadified with:
            `p.fel`, the first rule in `fel`
            Gonadify sfix with `hof` and `q.fel`, the second rule in `fel`

####Examples

---
        
##++  more

####Summary

        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce the gonadified:

###Examples

---

##++  most

Parse to a list elements of the second rule seperated by the second.

####Summary

        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce gonadified:
                Plug slammed with `fel`,
                        star slammed with gonadified:
                                pfix slammed with `bus` and `fel`, `bus` added as the prefix of `fel`
###Examples

---
        
##++  plus  

Like 'star', but "one or more" instead of "0 or more"

####Summary

        Build wet %gold gate with sample rule `fel`
        Produce gonadified:
                plug slammed with `fel` and star slammed with `fel`, the repeated application of `fel`.

####Examples

---
        
##++  slug

####Summary

        Build wet %gold gate with sample noun `rud`, gate accepting  cell of two nouns and producing [a b] `raq`
        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce the gonadified:
                comp slammed with `raq`, 
                        slammed with `fel`, 
                                slammed with,
                                        stir slammed with `rud`, `raq`, and `fel` prefixed with `bus`
####Examples

---
        
##++  star

Apply the parsing rule repeatedly until it fails.

####Summary

        Build wet %gold gate with sample rule `fel,
        Produce stir slammed with:
                The list of elements of type of the icon of `fel` slammed to `wonk`
        

####Examples

        ~tadbyl-hilbel/try=> (scan "aaaaa" (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "aaaaa" (star (just 'a')))
        "aaaaa"
        ~tadbyl-hilbel/try=> (scan "abcdef" (star (just 'a')))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "abcabc" (star (jest 'abc')))
        <|abc abc|>
        ~tadbyl-hilbel/try=> (scan "john smith" (star (shim 0 200)))
        "john smith"

---


