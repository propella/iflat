Implementing functional languages: a tutorial
Simon Peyton Jones and David Lester. Published by Prentice Hall, 1992.
http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/

found bugs:

p27: + 2 was necessary.

flatten col ((IIndent seq, indent) : seqs)
    = flatten col ((seq, col + 2) : seqs)

