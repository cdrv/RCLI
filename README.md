RCLI
----

This is a lightweight, very opinionated package for generating R
scripts that can be run from the command line using `Rscript`. It allows
us to generate scripts with a minimal amount of boilerplate for nicer usage
from the command line.

Using this package, we can write R scripts of the form (say in a file `echo.R`):

    #!/usr/bin/env Rscript
    
    library(RCLI)
    description("A simple script that echos its arguments.")
    usage("Takes arguments of the form 'a=b', and outputs the variable name 'a' and the value 'b'.")
    args <- parse_args()
    
    if( length(args) == 0 ) {
      print_description()
      print_usage()
    }
    
    for( i in seq_along(args) ) {
      arg <- args[[i]]
      name <- names(args)[i]
      cat("Argument number ", i, " has name ", name, " and value ", arg, ".\n", sep="")
    }

Then one could run this from the command line as either:

    Rscript echo.R a=1 b=2 c=3
    
or, with `+x` permissions:

    echo.R a=1 b=2 c=3
    
One can also get the description and usage information printed out using
`-h` or `--help` as a command line argument; this is currently the only
`-` style argument supported.

    echo.R --help
    
prints

    Description: A simple script that echos its arguments. 
    Usage: Takes arguments of the form 'a=b', and outputs the variable name 'a' and the value 'b'.
    
Note that it is (currently) a requirement that all arguments are passed as
named arguments; this may change in the future, however, currently I a strict
simplicity over complexity of other solutions.
