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
#' @importFrom utils modifyList
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

#' @importFrom utils rc.options
completeme <- function(env) {
  for (fun in the$completions) {
    if (isTRUE(fun(env))) {
      return()
    }
  }
  # Fall back to using the default completer
  on.exit(rc.options(custom.completer = completeme))
  rc.options(custom.completer = NULL)
  complete_token()
}

#' @importFrom rlang warn
#' @importFrom utils rc.getOption rc.options
.onLoad <- function(lib, pkg) {
  if (!is.null(default <- rc.getOption("custom.completer"))) {
    if (!isTRUE(all.equal(default, completeme))) {
      warn("Found default custom.completer, registering as 'default'")
      #register_completion(default = default)
    }
  }
  rc.options(custom.completer = completeme)
}

in_function_call <- get("inFunction", asNamespace("utils"))
inside_quotes <- get("isInsideQuotes", asNamespace("utils"))
complete_token <- get(".completeToken", asNamespace("utils"))
