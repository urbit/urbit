# Roadmap for language work

## Stuff that needs to be done before we can begin converting everything

### Add remaining features supported by nock

- $@, ?@, @, .+
- should be quite easy

### Support for vases and molds

- This is done by having a way to represent type values as nouns in the runtime
- and a way to inspect, pattern match, and nest-check these values
  - nest checking is used for vases, which are dependent cell types [|t/# t|] where the head type is represented at runtime
  - pattern matching is used for implementing `mold` as a library function:
    mold/<|a/# (unit <|noun (unit a)|>)|>, where the a argument is a reified type
- be able to say which arguments are reified and which erased in the type system

### Syntactic sugar for common 141 patterns

- =^ etc
- goal: "local conversion"
- there are things we won't want to locally convert, like arvo sump

## Stuff that should be done beforehand to make sure it's done right

### Type inference

- "elaboration": allow you to say (map <a/@ +(a)> xs) instead of (map @ @ <a/@ +(a)> xs)
- bidirectional type inference: let you leave off the types of lambda arguments in some situations
- may require us to back out of subtyping

### Pattern matching

- full haskell-style pattern matching
- include matching on types
- serious test of the dependent approach to sums, etc
- may require us to back out of subtyping

## Write some code

- Standard library
- Arvo
- A vane or two

## Subsequent features

- Type classes
- generative data types; mark reform
