Urbit
=====

This is an implementation of C. Guy Yarvin's [Nock][1] axioms, a compiler from
the "Watt" language to Nock, and other foundations of the [Urbit][2] static
functional namespace.

The interpreter depends on the following packages:

-  readline library
-  GNU Multiprecision library
-  bison
-  libsigsegv
-  openssl
-  libssl-dev
-  ncurses

You may be able to install these by running

    sudo apt-get install libreadline-dev libgmp3-dev bison libev-dev \
                         libsigsegv-dev openssl libssl-dev libncurses5-dev


To build the system, run `make OS=osx` or `make OS=linux`.

[1]: http://moronlab.blogspot.com/2010/01/nock-maxwells-equations-of-software.html
[2]: http://moronlab.blogspot.com/2010/01/urbit-functional-programming-from.html
