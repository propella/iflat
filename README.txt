-*- outline -*-

This directory is a note for reading "Implementing functional languages: a tutorial"
and the implementation in Haksell.

* Textbook:

Implementing functional languages: a tutorial
Simon Peyton Jones and David Lester. Published by Prentice Hall, 1992.
http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/

* Found bugs:

** p27: was + 2 necessary?.

flatten col ((IIndent seq, indent) : seqs)
    = flatten col ((seq, col + 2) : seqs)
