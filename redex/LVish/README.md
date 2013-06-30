# LVish in Redex

A PLT Redex model of the LVish family of languages.

### Version requirements

The code has been tested under **Racket v5.3.5** (released June 2013).
Other versions may also work.

It will _not_ work under versions prior to 5.3.2 (released January
2013).  This is because version 5.3.2 added support for the Redex
`boolean` pattern, which the code makes use of.

### Building and running

Running `make all` in this directory will build all the LVish
languages and run their test suites.

### Automated testing

The LVish tests run regularly on [a Jenkins continuous integration
server][jenkins].

[jenkins]: http://tester-lin.soic.indiana.edu:8080/job/LVish-redex/
