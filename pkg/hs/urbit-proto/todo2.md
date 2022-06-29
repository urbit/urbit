* fix bug in exhaustiveness checker re recursive types
- finish mint
- make gate bodies like gold cores and recursive
- every arm requires a type annotation; relax this.
- write an elminator for types
  - `?/`, nest check
  - stubbed and jetted; nest not generated in nock, instead a jet %nest
    - to be clear, this is just for convenience in the prototype
    - in the actual compiler written in hoon, we will generate a formula which
      computes nest and probably is not even jetted.
    - specifically a rune `#:` will produce a nock 1 which evaluates to the noun
      of the subhoun
    - or nest is a biblical
  - which in the dashboard, involves UNREADING type nouns into Semis and calling
  - fits.
  - Consider:
    ```
    {my-type x/my-type}
    ?:  ?/  my-type  (list @)
      do-thing-with-list
    do-other-thing
    ```
- `$@` Both
  - `|=  a=$(*)  $@  a  ^`
* `%=`
  - eta-beta (XX what is the problem here again?)
  * gold cores
- solve the abstract core ("variance") problem
  * think up solution
  - code up solution
- fix extra face on type `=/`, e.g. in nest-face.hoon
- make printing suck less
  - loft throughout
  - actually write a special "printing loft"
  - print names on holds instead of that awful large noise
  - don't print subjects or make it less awful
  - and get rid of "----"
  - figure out indentation issues in long hop prefix, maybe also palms
  - refactor that awful printing library
- figure out why exhaustiveness checking is slow
- figure out why `comp.hoon` is slow ugh
- fix linearity in retcon
- investigate typed delimited continuation scry
- investigate type-safe code ingestion
- investigate quantitative type theory
- investigate pattern matching on types
- investigate elaboration
- investigate collection runes
- cache model
- self hosting
- hoon 140 edition
