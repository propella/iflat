-*- outline -*-

This directory is a note for reading "Implementing functional languages: a tutorial"
and the implementation in Haksell.

* Textbook:

Implementing functional languages: a tutorial
Simon Peyton Jones and David Lester. Published by Prentice Hall, 1992.
http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/

* Found bugs:

** p27: "+ 2" was necessary for proper indent.

flatten col ((IIndent seq, indent) : seqs)
    = flatten col ((seq, col + 2) : seqs)

** p33: definition of pVar is missing.

** p40: assembleOp only handles right associative infix operators


