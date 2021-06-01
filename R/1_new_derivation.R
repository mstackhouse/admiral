.obj_env <- new.env(parent = environment())

new_derivation <- function(fun) {

  if (names(formals(fun))[1L] != "obj") {
    abort("First argument of `fun` must be named `obj`.")
  }

  overwrites_obj <- function(expr) {
    is.call(expr) &&
      expr[[1L]] == quote(`<-`) &&
      grepl("^obj[$|\\[]?", deparse(expr[[2L]]))
  }

  if (any(map_lgl(as.list(body(fun)), overwrites_obj))) {
    abort("Derivations are nor allowed to overwrite the `obj` variable in their function body.")
  }

  derivation_fun <- function(...) {
    call <- match.call()
    fun_name <- call[[1L]]
    args <- as.list(call[-1L])

    fun_impl_name <- paste0(fun_name, "_impl")
    assign(fun_impl_name, fun)
    eval(bquote({
      environment(.(as.symbol(fun_impl_name))) <- .obj_env
    }))

    existing_vars <- colnames(obj$dataset)

    obj$dataset <- do.call(fun_impl_name, args)

    # Add labels
    new_vars <- setdiff(colnames(obj$dataset), existing_vars)
    for (var in new_vars) {
      attr(obj$dataset[[var]], "label") <- obj$metadata %>%
        filter(Variable == var) %>%
        pull(Label)
    }

    # Drop temporary variables
    tmp_vars <- setdiff(colnames(obj$dataset), obj$metadata$Variable)
    obj$dataset[, tmp_vars] <- NULL

    obj
  }

  formals(derivation_fun) <- formals(fun)
  class(derivation_fun) <- c("admiral_function", "function")

  derivation_fun
}

is_adam <- function(x) {
  inherits(x, "adam")
}

get_dataset <- function(obj) {
  assert_that(is_adam(obj))
  obj$dataset
}

get_source_dataset <- function(obj, name) {
  assert_that(is_adam(obj))
  obj$source_datasets[[name]]
}

get_metadata <- function(obj) {
  assert_that(is_adam(obj))
  obj$metadata
}
