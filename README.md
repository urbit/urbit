# Lagoon for Urbit
## Linear Algebra in hOON

LAGOON, the _Linear AlGebra in hOON_ library, implements a suite of basic matrix operations for numerical applications. Lagoon endeavors to be mathematically accurate and notationally convenient. The fundamental problem Lagoon solves is representing memory-adjacent array data in a uniformly accessible way to Hoon.

What I do have done:
- vector library + jets in `@rs` and `@rd`
- matrix library + jets in `@rs`

What I don't have done:
- matrix library + jets in `@rd`
- profiling data on performance
- some utility routines, in particular pretty-printing and import/export

I considered adding in ATLAS or another BLAS package as a dependency but ultimately decided not to for the alpha version.  Instead everything is done via SoftFloat as with other Urbit floating-point operations.  I have some basic unit test setups but as of Ford Fusion it seems that unit testing is itself broken.  Large tests and rigorous profiling will require import/export routines as well to avoid tedious manual entry.  I'd also like jet-by-jet comparison with the Hoon, which I didn't have time to finish.

The future roadmap:
- Full unit testing suite compatible with whatever the recommended practice is or will be.
- Prospectus for sister library of scientific algorithms (based on `lazytrig` but jetted), provisionally Saloon (Scientific ALgorithms in hOON).
- Vectors and matrices in other significant types (`@ud`, `@rh`, `@rq` come to mind).  Creation of a complex type, without which certain operations (like eigenvalues/eigenvectors) will not be supportable.
- Code refactor of Lagoon:  I have done some terrible terrible things, like being inconsistent on 0-indexing and 1-indexing.
- I'd like to write a blog post on jetting for those interested.

(Do not, of course, include this branch's README file in any PRs.)
