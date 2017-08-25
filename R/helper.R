#' Completion helpers
#'
#' @param env The completion environment, see `?rc.status()` for details.
#' @name helpers
NULL

#' @describeIn helpers Returns the current function call, or `""` if
#' not within a call.
#' @export
current_function <- function(env) {
  buffer <- env[["linebuffer"]]
  fun <- rematch2::re_match(buffer, "(?<fun>[^[:space:](]+)[(][^(]*$")$fun
  if (is.na(fun)) {
    return("")
  }
  fun
}

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

#' @describeIn helpers Returns `TRUE` if currently completing the first argument.
#' @export
# utils:::getIsFirstArg doesn't seem to actually work
is_first_argument <- function(env) {
  buffer <- env[["linebuffer"]]
  !is.na(rematch2::re_match(buffer, "[^[:space:](]+[(][^[:space:]=,]*$")$.match)
}

#' @describeIn helpers Returns the current named argument, or `""` if not
#' completing a named argument.
#' @export
current_argument <- function(env) {
  buffer <- env[["linebuffer"]]
  arg <- rematch2::re_match(buffer, "\\b(?<arg>[^(=[:space:]]+)[[:space:]]*=(?:[^=]|==)*$")$arg
  if (!is.na(arg)) {
    return(arg)
  }
  ""
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
    return_to(parent.frame(2))
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
