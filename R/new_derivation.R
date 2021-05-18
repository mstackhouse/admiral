new_derivation <- function(fun) {

  if (names(formals(fun))[1L] != "obj") {
    abort("First argument of `fun` must be named `obj`.")
  }

  if ("obj" %in% all.vars(body(fun))) {
    abort("Derivations are nor allowed to reference the `obj` variable in their function body.")
  }

  derivation_fun <- function(...) {
    args <- as.list(match.call()[-1L])
    list2env(obj, envir = environment(fun))
    existing_vars <- colnames(dataset)

    obj$dataset <- do.call(fun, args)

    new_vars <- setdiff(colnames(dataset), existing_vars)
    for (var in new_vars) {
      attr(dataset[[var]], "label") <- metadata %>%
        filter(Variable == var) %>%
        pull(Label)
    }

    obj
  }

  formals(derivation_fun) <- formals(fun)
  class(derivation_fun) <- c("admiral_function", "function")

  derivation_fun
}
