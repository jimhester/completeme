#' Completion helpers
#'
#' @param env The completion environment, see `?rc.status()` for details.
#' @name helpers
NULL


#' @describeIn helpers Returns `TRUE` if within single or double quotes.
#' @importFrom utils head
#' @export
# Adapted from utils:::isInsideQuotes
inside_quotes <- function(env) {
  (env[["start"]] > 0 && {
    linebuffer <- env[["linebuffer"]]
    lbss <- head(unlist(strsplit(linebuffer, "")),
      env[["end"]])
    ((sum(lbss == "'") %% 2 == 1) || (sum(lbss == "\"") %% 2 == 1))
  })
}


#' Return from a function unless a condition is met
#'
#' This function can be used to return early from a function unless a condition
#' is met. It is equivalent to `if (!x) return()`.
#' @param x Condition to be evaluated.
#' @importFrom rlang return_to
#' @export
return_unless <- function(x) {
  if (!isTRUE(x)) {
    return_from(parent.frame(2))

  }
}

remove_quotes <- function(x) {
  # This handles escaped quotes as well
  # https://stackoverflow.com/a/4925400/2055486
  re <- '["\'](([^\"\\\\]+|\\\\.)*)["\']'

  gsub(re, "", x)
}

#' @describeIn helpers Returns `TRUE` if the current context is a comment.
#' @export
is_comment <- function(env) {
  buffer <- env[["linebuffer"]]
  !is.na(rematch2::re_match(remove_quotes(buffer), "#")$.match)
}

#' @describeIn helpers Returns `TRUE` if the current context is a roxygen comment.
#' @export
is_roxygen_comment <- function(env) {
  buffer <- env[["linebuffer"]]
  !is.na(rematch2::re_match(remove_quotes(buffer), "#'")$.match)
}


arg_names <-
  function(fname)
  {
    args <- do.call(argsAnywhere, list(fname))
    if (is.null(args))
      character()
    else if (is.list(args))
      unlist(lapply(args, function(f) formals(f)))
    else
      formals(args)
  }

#'@export
utils:::normalCompletions()

#'@export
normal_completions <- getFromNamespace("normalCompletions","utils")

#' @describeIn helpers Checks to see if we are in a function.
#'    If we are then it returns a list loaded with information
#'    about what has been typed so far.
#'@author person("Adam", "Wheeler", email = "ajwtech@gmail.com", role = c("aut", "cre"))
#' @export
# Adopted and expanded from utils:::inFunction()
in_function <- function(env) {
    line <- env[["linebuffer"]]
    cursor <- env[["start"]]
    inFun <- list()
    ## are we inside a function? Yes if the number of ( encountered
    ## going backwards exceeds number of ).  In that case, we would
    ## also like to know what function we are currently inside
    ## (ideally, also what arguments to it have already been supplied,
    ## but let's not dream that far ahead).
    ## I started dreaming. This function has been heavily modified from
    ## the utils packageto be aware of the cursor position, the
    ## related argument, all the arguments possible
    ## and the values of such arguments
    ## (does not yet handle recursing through dots I may need to add this though)

    parens <-
      sapply(c("(", ")"),
             function(s) gregexpr(s, substr(line, 1L, cursor), fixed = TRUE)[[1L]],
             simplify = FALSE)
    ## remove -1's
    parens <- lapply(parens, function(x) x[x > 0])

    ## The naive algo is as follows: set counter = 0; go backwards
    ## from cursor, set counter-- when a ) is encountered, and
    ## counter++ when a ( is encountered.  We are inside a function
    ## that starts at the first ( with counter > 0.

    temp <-
      data.frame(i = c(parens[["("]], parens[[")"]]),
                 c = rep(c(1, -1), lengths(parens)))
    if (nrow(temp) == 0) return(FALSE)
    temp <- temp[order(-temp$i), , drop = FALSE] ## order backwards
    wp <- which(cumsum(temp$c) > 0)
    if (length(wp)) # inside a function
    {
      ## return guessed name of function, letting someone else
      ## decide what to do with that name

      index <- temp$i[wp[1L]]
      prefix <- substr(line, 1L, index - 1L)
      suffix <- substr(line, index + 1L, cursor + 1L)



      possible <- suppressWarnings(strsplit(prefix, "[^\\.\\w]", perl = TRUE))[[1L]]
      possible <- possible[nzchar(possible)]
      if (length(possible))
        inFun$currentFunction <- tail(possible, 1)

      arguments <- arg_names(inFun$currentFunction)
      if (length(arguments) < 1L){
        ## We retrieved a function name but there isn't a function installed in
        ## the current libraries by that name. for now just return the function
        ## name we found.
        ## TODO use this information to allow the user to insert a function
        ## snippet for a new function at the current location, new file or
        ## existing file.
        return(inFun)
      }
      currArgs <- list()

      length(currArgs) <- length(arguments)
      names(currArgs) <- names(arguments)

      for (x in strsplit(sub(")","", suffix),",")) {
        subArg <- strsplit(x,"[=|]")
        i <- 1L
        for (y in subArg) {
         if(length(y) > 1){
            currArgs[trimws(y[1])] <- trimws(y[2])
         }else{
           currArgs[i] <- trimws(y)
         }
          i = i + 1L
        }
      }

      inFun$currArgs <- currArgs

      ## Determine if we are at the first argument and it is not named In this situation
      ## we would return the name of the first item in currArgs
      ## If not the first argument then first we can check to see if our current
      ## argument is not named. In this sutuation we would find how many commas
      ## have been typed and return name in currArgs at the index of the comma
      ## if it is named then we grab the name then look to see the starting position
      ## of our current argument, get the length of the match trim the ws and return the result.

      suffixPos <- cursor - nchar(prefix) -1L

      prevCommaPos <- gregexpr(",", substr(suffix,0, suffixPos ), fixed = TRUE)
      prevEqualPos <- gregexpr("=", substr(suffix,0, suffixPos ), fixed = TRUE)
      #Check to see if the suffix position is less than the first comma in the suffix
      if (prevCommaPos[[1]][1] == -1L)
      {
        # Check to see if there is an equal sign
        if (prevEqualPos[[1]][1] == -1L)
        {
          # this is the first argument
          inFun$currentArg <- names(currArgs)[1]
          #there is no comma and no equal sign -- function(|)
          inFun$IsFirstArg <- TRUE

        }else{
          #there is no comma and there is an equal sign --function(x=|)
          inFun$currentArg <- trimws(substr(suffix,0,prevEqualPos[[1]][1]-1))
          inFun$IsFirstArg <- identical(inFun$currentArg,names(currArgs)[1])
        }
      }else if(prevEqualPos[[1]][length(prevEqualPos[[1]])] > prevCommaPos[[1]][length(prevCommaPos[[1]])]){
          #There is a comma and and an equal sign after it
          #so lets get what is between the comma and the equal sign
          inFun$currentArg <- trimws(substr(suffix,prevCommaPos[[1]][which.max(prevCommaPos[[1]])]+1,prevEqualPos[[1]][which.max(prevEqualPos[[1]])]-1))
          inFun$IsFirstArg <- identical(inFun$currentArg,names(currArgs)[1])
        }else{
          #There is a comma and no equal sign so get the argument by position
          inFun$currentArg <- names(currArgs)[length(prevCommaPos)+1]
          inFun$IsFirstArg <- FALSE
        }

      # Assign default values where the current value is NULL.
      inFun$currArgs <- modifyList(inFun$currArgs, arguments[intersect(names(Filter(is.null, inFun$currArgs)),names(Filter(Negate(is.null),arguments)))])

      return(inFun)
  }
  else # not inside function
  { # dont think there is a path to this code
      return(FALSE)
  }
}
