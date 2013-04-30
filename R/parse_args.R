.RCLI.env <- new.env()
assign("description", NULL, envir=.RCLI.env)
assign("usage", NULL, envir=.RCLI.env)

## this flag helps us keep track of whether usage and description have been
## printed
assign("help_flag", FALSE, envir=.RCLI.env)

#' Parse the Command Line Arguments
#' 
#' Ths function takes command line arguments of the form \code{var=value} from
#' the command line, then puts it into a list.
#'
#' @export 
parse_args <- function() {
  
  ## to ensure that we capture the Rscript call correctly, we use both
  ## commandArgs(trailingOnly=FALSE) and commandArgs(trailingOnly=TRUE)
  c1 <- commandArgs(trailingOnly=FALSE)
  c2 <- commandArgs(trailingOnly=TRUE)
  
  R_call_args <- paste(c1[1:(length(c1)-length(c2))])
  user_args <- c2
  
  ## if '-h' or '--help' is passed in the command line args,
  ## print out the usage and description
  if( any( c('-h', '--help' ) %in% user_args ) ) {
    print_description()
    print_usage()
    assign("help_flag", TRUE, envir=.RCLI.env)
    return( invisible(NULL) )
  }
  
  ## if '-h' or '--help' in command line args, remove them
  user_args <- user_args[ !(user_args %in% c('-h', '--help')) ]
  
  ## check that each argument has an equals sign
  for( i in seq_along(user_args) ) {
    arg <- user_args[i]
    if( !length( grep( "=", arg, fixed=TRUE ) ) )
      stop("Argument ", i, " (", arg, ") is malformed: expected '=' but none found")
  }
  
  ## split according to first equals sign, and send to output list
  out <- vector("list", length(user_args))
  for( i in seq_along(user_args) ) {
    arg <- user_args[i]
    param_name <- gsub( "=.*", "", arg, perl=TRUE)
    param_value <- sub("(.*?)=(.*?)", "\\2", arg, perl=TRUE)
    out[i] <- param_value
    names(out)[i] <- param_name
  }
  
  return(out)
  
}

#' Set the Script Expected Parameters
#' 
#' This function checks the passed parameters against what is expected for a
#' particular script. See examples for usage.
#' 
#' @param usage Either a single list of arguments, or a character string.
#' @export
#' @examples
#' expected_args <- list( 
#'   input="Location of input file.",
#'   output="Location of output file."
#' )
#' usage(expected_args)
usage <- function(...) {
  args <- list(...)
  if( length(args) == 1 )
    assign( "usage", args[[1]], envir=.RCLI.env )
  else
    assign( "usage", args, envir=.RCLI.env )
  return( invisible(NULL) )
}

#' Print Usage
#' 
#' Prints the usage, as set through \code{\link{usage}}.
#' 
#' @export
#' @seealso \code{\link{usage}}
print_usage <- function() {
  if( get("help_flag", envir=.RCLI.env) ) {
    return( invisible(NULL) )
  }
  usage <- get('usage', envir=.RCLI.env)
  if( !is.null(usage) ) {
    if( is.list(usage) ) {
      cat("Usage:\n")
      for( i in seq_along( usage ) ) {
        arg <- usage[[i]]
        name <- names(usage)[i]
        cat("  ", name, ": ", arg, "\n", sep="")
      }
    } else {
      cat("Usage:", usage, "\n")
    }
  }
}

#' Set the Description
#' 
#' This function \code{cat}s the description for an R script.
#' 
#' @param desc A single string to be output as the description.
#' @export
description <- function(desc) {
  stopifnot( is.character(desc) && length(desc) == 1 )
  assign( "description", desc, envir=.RCLI.env )
  return( invisible(NULL) )
}

#' Print Description
#' 
#' Prints the description, as set through \code{\link{description}}.
#' 
#' @export
#' @seealso \code{\link{description}}
print_description <- function() {
  if( get("help_flag", envir=.RCLI.env) ) {
    return( invisible(NULL) )
  }
  desc <- get('description', envir=.RCLI.env)
  if( !is.null(desc) )
    cat("Description:", desc, '\n')
}
