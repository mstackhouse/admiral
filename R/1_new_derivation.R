new_derivation <- function(name, fun) {

  if (names(formals(fun))[1L] != "obj") {
    abort("First argument of `fun` must be named `obj`.")
  }

  if ("obj" %in% all.vars(body(fun))) {
    abort("Derivations are nor allowed to reference the `obj` variable in their function body.")
  }

  fun_impl_name <- paste0(name, "_impl")
  assign(fun_impl_name, fun)

  derivation_fun <- function(...) {
    args <- as.list(match.call()[-1L])
    list2env(obj, envir = environment(fun))
    existing_vars <- colnames(dataset)

    obj$dataset <- do.call(fun_impl_name, args)

    # Add labels
    new_vars <- setdiff(colnames(obj$dataset), existing_vars)
    for (var in new_vars) {
      attr(obj$dataset[[var]], "label") <- metadata %>%
        filter(Variable == var) %>%
        pull(Label)
    }

    # Drop temporary variables
    # tmp_vars <- colnames(obj$dataset) %!in% obj$metadata$Variable
    # for (var in tmp_vars) {
    #   obj$dataset[[var]] <- NULL
    # }

    obj
  }

  formals(derivation_fun) <- formals(fun)
  class(derivation_fun) <- c("admiral_function", "function")

  assign(name, derivation_fun, envir = parent.frame())
}
