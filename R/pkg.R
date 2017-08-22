#' @importFrom glue glue
NULL

the <- new.env(parent = emptyenv())
the$completions <- list()

#' @importFrom glue collapse single_quote
vals <- function(x) {
  collapse(single_quote(x), sep = ", ", last = " and ")
}

#' @importFrom glue glue
#' @importFrom rlang abort
halt <- function(msg, type = NULL) {
  abort(do.call(glue, c(msg, .envir = parent.frame())), type = type)
}

register_completion <- function(...) {
  funs <- list(...)

  nms <- names(funs)
  if (is.null(nms) || any(nms == "" | is.na(nms))) {
    wch <- if (is.null(nms)) 1 else which(nms == "" | is.na(nms))
    halt("All arguments must be named. Unnamed arguments at position {vals(wch)}.", "argument_error")
  }

  old <- the$completions
  the$completions <- modifyList(the$completions, funs)

  invisible(old)
}

complete <- function(env) {
  for (fun in the$completions) {
    if (isTRUE(fun(env))) {
      return()
    }
  }
  # Fall back to using the default completer
  on.exit(rc.options(custom.completer = complete))
  rc.options(custom.completer = NULL)
  complete_token()
}

#' @importFrom rlang warn
.onLoad <- function(x, y) {
  if (!is.null(default <- rc.getOption("custom.completer"))) {
    if (!isTRUE(all.equal(default, complete))) {
      warn("Found default custom.completer, registering as 'default'")
      #register_completion(default = default)
    }
  }
  rc.options(custom.completer = complete)
}

# from https://github.com/jimhester/readxl/blob/87b4d03fcb1952ef75cc5359cf2e0bf936a7abbd/R/example.R
readxl_example_completer <- function(env) {
  env2 <<- env
  fun <- in_function_call()
  if (length(fun) > 0 && fun == "readxl_example" && inside_quotes()) {

    # Completion within readxl_example uses the example filenames
    comps <- grep(paste0("^\"?", env$token, "."), readxl_example(), value = TRUE)
    env[["fileName"]] <- comps
    env[["comps"]] <- comps
    return (TRUE)
  }
  return (FALSE)
}

register_completion(a = readxl_example_completer)

in_function_call <- get("inFunction", asNamespace("utils"))
inside_quotes <- get("isInsideQuotes", asNamespace("utils"))
complete_token <- get(".completeToken", asNamespace("utils"))
