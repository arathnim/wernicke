# Wernicke
## parsing framework for cl

Wernicke is a framework made to ease development of parsing languages, such as regular expressions and Instaparse,
by providing a highly optimized s-expression format to represent common parsing constructs.
This core language is fully extensible, the internal functions used to define parsers are exposed.
Wernicke also provides several advanced features, like error handling, custom error formatting, parsing from streams,
incremental reparsing, and scanning.

It takes advantage of CL's flexible type system to give parse results in a natural format, that can be easily
manipulated with normal code, and uses CL's macros to create clean, powerful interfaces to the parsing library.
See the examples for more neat things Wernicke can do, or the tutorials to get started making a simple
regex-like parsing language in Wernicke.

## dependencies and installation

Wernicke requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
to install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow the instructions there.
Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable when you start your interpreter.

To use wernicke, clone the repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'wernicke)`.
