section 2eJ, formatting (basic text)  

##++cass

Produce the case insensitive (all lowercase) cord of a tape.

####Summary

        Build wet %gold gate with sample tape `vib`
        Slam rap with:
                3, to rap by bytes
                Slam turn with:
                        `vib`
                        Build dry %gold gate with sample atom `a`,
                                Unless: `a` is greater than or equal to 'A' or less than or equal to 'Z',
                                        Then: Produce `a`,
                                Else: Produce the difference between `a` and 32.

####Examples

       ~tadbyl-hilbel/try=> (cass "john doe")
        7.309.170.810.699.673.450
        ~tadbyl-hilbel/try=> `cord`(cass "john doe")
        'john doe'
        ~tadbyl-hilbel/try=> (cass "abc, 123, !@#")
        2.792.832.775.110.938.439.066.079.945.313
        ~tadbyl-hilbel/try=> `cord`(cass "abc, 123, !@#")
        'abc, 123, !@#' 

##++cuss

Turn all occurances of lowercase letters in any tape into uppercase letters, as a cord.

####Summary

        Build dry %gold gate with sample tape `vib`
        Yield cord
        Slam rap with:
                3, to rap by bytes
                Slam turn with:
                        `vib`
                        Build dry %gold gate with sample atom `a`,
                                Unless: `a` is greater than or equal to 'A' or less than or equal to 'Z',
                                        Then: Produce `a`,
                                Else: Produce the difference between `a` and 32.

####Examples

        ~tadbyl-hilbel/try=> (cuss "john doe")
        'JOHN DOE'
        ~tadbyl-hilbel/try=> (cuss "abc ABC 123 !@#")
        'ABC ABC 123 !@#'
        ~tadbyl-hilbel/try=> `@ud`(cuss "abc")
        4.407.873
        ~tadbyl-hilbel/try=> (cuss "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsQqRrVvWwXxYyZz")
        'AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSQQRRVVWWXXYYZZ'

##++crip

Produce the cord of a tape.

####Summary

        Build dry %gold with sample tape `a`
        Produce the rap of `a` by bytes, cast to a cord.

####Examples

        ~tadbyl-hilbel/try=> (crip "john doe")
        'john doe'
        ~tadbyl-hilbel/try=> (crip "abc 123 !@#")
        'abc 123 !@#'
        ~tadbyl-hilbel/try=> `@ud`(crip "abc")
        6.513.249

##++mesc

##++runt

##++sand

##++sane

##++trim

##++trip

##++teff

##++turf

##++tuba

##++tufa

##++tuft

##++wack

##++wick

##++woad

##++wood


