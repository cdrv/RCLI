#!/usr/bin/env Rscript

library(RCLI)
description("A simple script that echos its arguments.")
#usage(input="Path to input file.", output="Path to output file.")
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