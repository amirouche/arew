Usage:

  arew compile [--dev] [--optimization-level=n] [extensions-or-directories ...] program.scm a.out [-- cc-argument ...]

Given a Scheme program inside the file `program.scm`, produce a standalone executable in the current directory called `a.out`. `arew compile` will look for the necessary files using `extensions-or-directories`.

With the flag `--dev`, the executable will include the machinery to generate allocation and instruction count.

The arguments `extensions-or-directories` will replace the default extensions, source and library directories. If you want to compile a project that has both `.sld` and `.scm` with libraries in the current directory, to output a binary executable called `a.out`, you need to use something along the line of:

  arew compile --dev --optimization-level=3 .sld .scm . program.scm a.out

Any `cc-arguments` will be passed to the default C compiler.

That's all folks!
