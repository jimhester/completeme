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

#' Register completion functions
#'
#' Completion functions should take one parameter `env`, the completion
#' environment, see `?rc.settings` for details of this environment. They should
#' simply return any completions found or `return(NULL)` otherwise.
#'
#' If all registered completions do not have any completions for a given
#' context, than R's standard completions are used.
#' @param ... One or more completion functions specified as named parameters.
#' @export
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
#' @importFrom rlang %||%
completeme <- function(env) {
  env$fileName <- FALSE
  for (fun in the$completions) {
    env$comps <- fun(env)
    if (length(env$comps) > 0) {
      attributes(env$comps) <- list(class = "completions", type = attr(env$comps, "type") %||% 15)
      return(invisible(env$comps))
    }
  }

  env$comps <- character()

  # if in the IDE, throw an error to fallback on normal completion
  if (rstudioapi::isAvailable()) {
    abort("No custom completions", type = "no_completions")
  }

  # If on the command line, fall back to using the default completer
  on.exit(rc.options(custom.completer = completeme))
  rc.options(custom.completer = NULL)
  complete_token()

  invisible(env$comps)
}

unique.completions <- function(x, ...) {
  attrs <- attributes(x)
  res <- unique(unclass(x))
  mostattributes(attrs) <- attrs
  x
}

#' @importFrom rlang warn
#' @importFrom utils rc.getOption rc.options
.onLoad <- function(lib, pkg) {
  if (!is.null(default <- rc.getOption("custom.completer"))) {
    if (!isTRUE(all.equal(default, completeme))) {
      warn("Found default custom.completer, registering as 'default'")
      #TODO: turn this on
      # register_completion(default = default)
    }
  }
  rc.options(custom.completer = completeme)
}

complete_token <- get(".completeToken", asNamespace("utils"))
