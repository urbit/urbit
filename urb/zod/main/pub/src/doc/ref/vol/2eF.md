section 2eF, parsing (ascii)          

---

##++  ace

Parse ASCII character 32, ace.

####Summary

        Produce the rule just slammed with ' '

###Examples

        ~tadbyl-hilbel/try=> (scan " " ace)
        ~~. 
        ~tadbyl-hilbel/try=> `cord`(scan " " ace)
        ' '
        ~tadbyl-hilbel/try=> (ace [[1 1] " "])
        [p=[p=1 q=2] q=[~ [p=~~. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ace [[1 1] " abc "])
        [p=[p=1 q=2] q=[~ [p=~~. q=[p=[p=1 q=2] q="abc "]]]]

---

##++  bar 

Parse ASCII character 124, bar.

####Summary

        Produce the rule just slammed with '|'

####Examples

       ~tadbyl-hilbel/try=> (scan "|" bar)
        ~~~7c. 
        ~tadbyl-hilbel/try=> `cord`(scan "|" bar)
        '|'
        ~tadbyl-hilbel/try=> (bar [[1 1] "|"])
        [p=[p=1 q=2] q=[~ [p=~~~7c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (bar [[1 1] "|="])
        [p=[p=1 q=2] q=[~ [p=~~~7c. q=[p=[p=1 q=2] q="="]]]]

---

##++  bas 

Parse ASCII character 92, bas.
Note the extra '\' in the slam of bas with just is to escape the escape character, bas.

####Summary

        Produce the rule just slammed with '\\'

###Examples

        ~tadbyl-hilbel/try=> (scan "\\" bas)
        ~~~5c.
        ~tadbyl-hilbel/try=> `cord`(scan "\\" bas)
        '\'
        ~tadbyl-hilbel/try=> (bas [[1 1] "\"])
        ~ <syntax error at [1 18]>
        ~tadbyl-hilbel/try=> (bas [[1 1] "\\"])
        [p=[p=1 q=2] q=[~ [p=~~~5c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (bas [[1 1] "\""])
        [p=[p=1 q=1] q=~]

---

##++  buc 

Parse ASCII character 36, buc.

####Summary

        Produce the rule just slammed with '$'

####Examples

        ~tadbyl-hilbel/try=> (scan "$" buc)
        ~~~24.
        ~tadbyl-hilbel/try=> `cord`(scan "$" buc)
        '$'
        ~tadbyl-hilbel/try=> (buc [[1 1] "$"])
        [p=[p=1 q=2] q=[~ [p=~~~24. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (buc [[1 1] "$%"])
        [p=[p=1 q=2] q=[~ [p=~~~24. q=[p=[p=1 q=2] q="%"]]]]

---

##++  cab 

Parse ASCII character 95, cab.

####Summary

        Produce the rule just slammed with '_'

###Examples

        ~tadbyl-hilbel/try=> (scan "_" cab)
        ~~~5f.
        ~tadbyl-hilbel/try=> `cord`(scan "_" cab)
        '_'
        ~tadbyl-hilbel/try=> (cab [[1 1] "_"])
        [p=[p=1 q=2] q=[~ [p=~~~5f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (cab [[1 1] "|_"])
        [p=[p=1 q=1] q=~]

---

##++  cen 

Parse ASCII character 37, cen.

####Summary

        Produce the rule just slammed with '%'

####Examples

        ~tadbyl-hilbel/try=> (scan "%" cen)
        ~~~25.
        ~tadbyl-hilbel/try=> `cord`(scan "%" cen)
        '%'
        ~tadbyl-hilbel/try=> (cen [[1 1] "%"])
        [p=[p=1 q=2] q=[~ [p=~~~25. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (cen [[1 1] "%^"])
        [p=[p=1 q=2] q=[~ [p=~~~25. q=[p=[p=1 q=2] q="^"]]]] 

---

##++  col 

Parse ASCII character 58, col.

####Summary

        Produce the rule just slammed with ':'

###Examples

        ~tadbyl-hilbel/try=> (scan ":" col)
        ~~~3a.
        ~tadbyl-hilbel/try=> `cord`(scan ":" col)
        ':'
        ~tadbyl-hilbel/try=> (col [[1 1] ":"])
        [p=[p=1 q=2] q=[~ [p=~~~3a. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (col [[1 1] ":-"])
        [p=[p=1 q=2] q=[~ [p=~~~3a. q=[p=[p=1 q=2] q="-"]]]]

---

##++  com 

Parse ASCII character 44, com.

####Summary

        Produce the rule just slammed with ','

####Examples

        ~tadbyl-hilbel/try=> (scan "," com)
        ~~~2c.
        ~tadbyl-hilbel/try=> `cord`(scan "," com)
        ','
        ~tadbyl-hilbel/try=> (com [[1 1] ","])
        [p=[p=1 q=2] q=[~ [p=~~~2c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (com [[1 1] "not com"])
        [p=[p=1 q=1] q=~]

---

##++  doq 

Parse ASCII character 34, doq.

####Summary

        Produce the rule just slammed with '"'
####Examplse

       ~tadbyl-hilbel/try=> (scan "\"" doq)
        ~~~22.
        ~tadbyl-hilbel/try=> `cord`(scan "\"" doq)
        '"'
        ~tadbyl-hilbel/try=> (doq [[1 1] "\""])
        [p=[p=1 q=2] q=[~ [p=~~~22. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (doq [[1 1] "not successfully parsed"])
        [p=[p=1 q=1] q=~]
        ~tadbyl-hilbel/try=> (scan "see?" doq)
        ! {1 1}
        ! 'syntax-error'
        ! exit 

---

##++  dot 

Parse ASCII character 46, dot.

####Summary

        Produce the rule just slammed with '.'

####Examples

        ~tadbyl-hilbel/try=> (scan "." dot)
        ~~~.
        ~tadbyl-hilbel/try=> `cord`(scan "." dot)
        '.'
        ~tadbyl-hilbel/try=> (dot [[1 1] "."])
        [p=[p=1 q=2] q=[~ [p=~~~. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (dot [[1 1] ".^"])
        [p=[p=1 q=2] q=[~ [p=~~~. q=[p=[p=1 q=2] q="^"]]]]

---

##++  fas 

Parse ASCII character 47, fas.

####Summary

        Produce the rule just slammed with '/'

###Examples

        ~tadbyl-hilbel/try=> (scan "/" fas)
        ~~~2f.
        ~tadbyl-hilbel/try=> `cord`(scan "/" fas)
        '/'
        ~tadbyl-hilbel/try=> (fas [[1 1] "/"])
        [p=[p=1 q=2] q=[~ [p=~~~2f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (fas [[1 1] "|/"])
        [p=[p=1 q=1] q=~]

---

##++  gal 

Parse ASCII character 60, gal.

####Summary

        Produce the rule just slammed with '<'

####Examples

        ~tadbyl-hilbel/try=> (scan "<" gal)
        ~~~3c.
        ~tadbyl-hilbel/try=> `cord`(scan "<" gal)
        '<'
        ~tadbyl-hilbel/try=> (gal [[1 1] "<"])
        [p=[p=1 q=2] q=[~ [p=~~~3c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (gal [[1 1] "<+"])
        [p=[p=1 q=2] q=[~ [p=~~~3c. q=[p=[p=1 q=2] q="+"]]]]
        ~tadbyl-hilbel/try=> (gal [[1 1] "+<"])
        [p=[p=1 q=1] q=~]

---

##++  gar 

Parse ASCII character 62, gar.

####Summary

        Produce the rule just slammed with '>'

####Examples

        ~tadbyl-hilbel/try=> (scan ">" gar)
        ~~~3e.
        ~tadbyl-hilbel/try=> `cord`(scan ">" gar)
        '>'
        ~tadbyl-hilbel/try=> (gar [[1 1] ">"])
        [p=[p=1 q=2] q=[~ [p=~~~3e. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (gar [[1 1] "=>"])
        [p=[p=1 q=1] q=~]

---

##++  hax 

Parse ASCII character 35, hax.

####Summary

        Produce the rule just slammed with '#'

####Examples

        ~tadbyl-hilbel/try=> (scan "#" hax)
        ~~~23.
        ~tadbyl-hilbel/try=> `cord`(scan "#" hax)
        '#'
        ~tadbyl-hilbel/try=> (hax [[1 1] "#"])
        [p=[p=1 q=2] q=[~ [p=~~~23. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (hax [[1 1] "#!"])
        [p=[p=1 q=2] q=[~ [p=~~~23. q=[p=[p=1 q=2] q="!"]]]]

---

##++  kel 

Parse ASCII character 123, kel.
Note that this, with ker, opens and closes a Hoon expression for Hoon string interpolation.  Escape kel to parse it.

####Summary

        Produce the rule just slammed with '{'

####Examples

        ~tadbyl-hilbel/try=> (scan "\{" kel)
        ~~~7b.
        ~tadbyl-hilbel/try=> `cord`(scan "\{" kel)
        '{'
        ~tadbyl-hilbel/try=> (kel [[1 1] "\{"])
        [p=[p=1 q=2] q=[~ [p=~~~7b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (kel [[1 1] " \{"])
        [p=[p=1 q=1] q=~]

---

##++  ker 

Parse ASCII character 125, ker.

####Summary

        Produce the rule just slammed with '}'

###Examples

        ~tadbyl-hilbel/try=> (scan "}" ker)
        ~~~7d.
        ~tadbyl-hilbel/try=> `cord`(scan "}" ker)
        '}'
        ~tadbyl-hilbel/try=> (ker [[1 1] "}"])
        [p=[p=1 q=2] q=[~ [p=~~~7d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ker [[1 1] "\{}"])
        [p=[p=1 q=1] q=~]

---

##++  ket 

Parse ASCII character 94, ket.

####Summary

        Produce the rule just slammed with '^'

####Examples

        ~tadbyl-hilbel/try=> (scan "^" ket)
        ~~~5e.
        ~tadbyl-hilbel/try=> `cord`(scan "^" ket)
        '^'
        ~tadbyl-hilbel/try=> (ket [[1 1] "^"])
        [p=[p=1 q=2] q=[~ [p=~~~5e. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ket [[1 1] ".^"])
        [p=[p=1 q=1] q=~]

---

##++  lus 

Parse ASCII character 43, lus.

####Summary

        Produce the rule just slammed with '+'

###Examples

        ~tadbyl-hilbel/try=> (scan "+" lus)
        ~~~2b.
        ~tadbyl-hilbel/try=> `cord`(scan "+" lus)
        '+'
        ~tadbyl-hilbel/try=> (lus [[1 1] "+"])
        [p=[p=1 q=2] q=[~ [p=~~~2b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (lus [[1 1] ".+"])
        [p=[p=1 q=1] q=~]

---

##++  hep 

Parse ASCII character 45, hep.

####Summary

        Produce the rule just slammed with '-'

####Examples

        ~tadbyl-hilbel/try=> (scan "-" hep)
        ~~-
        ~tadbyl-hilbel/try=> `cord`(scan "-" hep)
        '-'
        ~tadbyl-hilbel/try=> (hep [[1 1] "-"])
        [p=[p=1 q=2] q=[~ [p=~~- q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (hep [[1 1] ":-"])
        [p=[p=1 q=1] q=~]

---

##++  pel 

Parse ASCII character 40, pel.

####Summary

        Produce the rule just slammed with '('

####Examples

        ~tadbyl-hilbel/try=> (scan "(" pel)
        ~~~28.
        ~tadbyl-hilbel/try=> `cord`(scan "(" pel)
        '('
        ~tadbyl-hilbel/try=> (pel [[1 1] "("])
        [p=[p=1 q=2] q=[~ [p=~~~28. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pel [[1 1] ";("])
        [p=[p=1 q=1] q=~]

---

##++  pam 

Parse ASCII character 38, pam.

####Summary

        Produce the rule just slammed with '&'

####Examples

        ~tadbyl-hilbel/try=> (scan "&" pam)
        ~~~26.
        ~tadbyl-hilbel/try=> `cord`(scan "&" pam)
        '&'
        ~tadbyl-hilbel/try=> (pam [[1 1] "&"])
        [p=[p=1 q=2] q=[~ [p=~~~26. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pam [[1 1] "?&"])
        [p=[p=1 q=1] q=~]

---

##++  per 

Parse ASCII character 41, per.

####Summary

        Produce the rule just slammed with ')'

###Examples

        ~tadbyl-hilbel/try=> (scan ")" per)
        ~~~29.
        ~tadbyl-hilbel/try=> `cord`(scan ")" per)
        ')'
        ~tadbyl-hilbel/try=> (per [[1 1] ")"])
        [p=[p=1 q=2] q=[~ [p=~~~29. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (per [[1 1] " )"])
        [p=[p=1 q=1] q=~]

---

##++  pat 

Parse ASCII character 64, pat.

####Summary

        Produce the rule just slammed with '@'

####Examples

        ~tadbyl-hilbel/try=> (scan "@" pat)
        ~~~4.
        ~tadbyl-hilbel/try=> `cord`(scan "@" pat)
        '@'
        ~tadbyl-hilbel/try=> (pat [[1 1] "@"])
        [p=[p=1 q=2] q=[~ [p=~~~4. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pat [[1 1] "?@"])
        [p=[p=1 q=1] q=~]

---

##++  sel

Parse ASCII character 91, sel.

####Summary

        Produce the rule just slammed with '['

####Examples

        ~tadbyl-hilbel/try=> (scan "[" sel)
        ~~~5b.
        ~tadbyl-hilbel/try=> `cord`(scan "[" sel)
        '['
        ~tadbyl-hilbel/try=> (sel [[1 1] "["])
        [p=[p=1 q=2] q=[~ [p=~~~5b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sel [[1 1] "-["])
        [p=[p=1 q=1] q=~]

---

##++  sem 

Parse ASCII character 59, sem.

####Summary

        Produce the rule just slammed with ';'

###Exampels

        ~tadbyl-hilbel/try=> (scan ";" sem)
        ~~~3b.
        ~tadbyl-hilbel/try=> `cord`(scan ";" sem)
        ';'
        ~tadbyl-hilbel/try=> (sem [[1 1] ";"])
        [p=[p=1 q=2] q=[~ [p=~~~3b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sem [[1 1] " ;"])
        [p=[p=1 q=1] q=~]

---

##++  ser 

Parse ASCII character 93, ser.

####Summary

        Produce the rule just slammed with ']'

####Examples

        ~tadbyl-hilbel/try=> (scan "]" ser)
        ~~~5d.
        ~tadbyl-hilbel/try=> `cord`(scan "]" ser)
        ']'
        ~tadbyl-hilbel/try=> (ser [[1 1] "]"])
        [p=[p=1 q=2] q=[~ [p=~~~5d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ser [[1 1] "[ ]"])
        [p=[p=1 q=1] q=~]

---

##++  sig 

Parse ASCII character 126, sig.

####Summary

        Produce the rule just slammed with '~'

####Examples

        ~tadbyl-hilbel/try=> (scan "~" sig)
        ~~~~
        ~tadbyl-hilbel/try=> `cord`(scan "~" sig)
        '~'
        ~tadbyl-hilbel/try=> (sig [[1 1] "~"])
        [p=[p=1 q=2] q=[~ [p=~~~~ q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sig [[1 1] "?~"])
        [p=[p=1 q=1] q=~]

---

##++  soq 

Parse ASCII character 39, soq.
Note the extra '\' in the slam of soq with just is to escape the first soq because soq denotes a crip.

####Summary

        Produce the rule just slammed with '\''

####Examples

        ~tadbyl-hilbel/try=> (scan "'" soq)
        ~~~27.
        ~tadbyl-hilbel/try=> `cord`(scan "'" soq)
        '''
        ~tadbyl-hilbel/try=> (soq [[1 1] "'"])
        [p=[p=1 q=2] q=[~ [p=~~~27. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (soq [[1 1] ">'"])
        [p=[p=1 q=1] q=~]

---

##++  tar 

Parse ASCII character 42, tar.

####Summary

        Produce the rule just slammed with '*'

####Examples

        ~tadbyl-hilbel/try=> (scan "*" tar)
        ~~~2a.
        ~tadbyl-hilbel/try=> `cord`(scan "*" tar)
        '*'
        ~tadbyl-hilbel/try=> (tar [[1 1] "*"])
        [p=[p=1 q=2] q=[~ [p=~~~2a. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tar [[1 1] ".*"])
        [p=[p=1 q=1] q=~]

---

##++  tec 

Parse ASCII character 96, tec.

####Summary

        Produce the rule just slammed with '`'

####Examples

        ~tadbyl-hilbel/try=> (scan "`" tec)
        ~~~6.
        ~tadbyl-hilbel/try=> `cord`(scan "`" tec)
        '`'
        ~tadbyl-hilbel/try=> (tec [[1 1] "`"])
        [p=[p=1 q=2] q=[~ [p=~~~6. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tec [[1 1] " `"])
        [p=[p=1 q=1] q=~]

---

##++  tis 

Parse ASCII character 61, tis.

####Summary

        Produce the rule just slammed with '='

####Examples

        ~tadbyl-hilbel/try=> (scan "=" tis)
        ~~~3d.
        ~tadbyl-hilbel/try=> `cord`(scan "=" tis)
        '='
        ~tadbyl-hilbel/try=> (tis [[1 1] "="])
        [p=[p=1 q=2] q=[~ [p=~~~3d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tis [[1 1] "|="])
        [p=[p=1 q=1] q=~]
---

##++  wut 

Parse ASCII character 63, wut.

####Summary

        Produce the rule just slammed with '?'

###Examples

        ~tadbyl-hilbel/try=> (scan "?" wut)
        ~~~3f.
        ~tadbyl-hilbel/try=> `cord`(scan "?" wut)
        '?'
        ~tadbyl-hilbel/try=> (wut [[1 1] "?"])
        [p=[p=1 q=2] q=[~ [p=~~~3f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (wut [[1 1] ".?"])
        [p=[p=1 q=1] q=~]

---

##++  zap 

Parse ASCII character 33, zap.

####Summary

        Produce the rule just slammed with '!'

###Examples

        ~tadbyl-hilbel/try=> (scan "!" zap)
        ~~~21.
        ~tadbyl-hilbel/try=> `cord`(scan "!" zap)
        '!'
        ~tadbyl-hilbel/try=> (zap [[1 1] "!"])
        [p=[p=1 q=2] q=[~ [p=~~~21. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (zap [[1 1] "?!"])
        [p=[p=1 q=1] q=~]

---


