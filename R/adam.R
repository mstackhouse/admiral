print.adam <- function(x, ...) {
  cat("Dataset:", attr(x, "name"), "\n")
  cat("Source Datasets:", enumerate(toupper(names(x[["source_datasets"]])), identity), "\n")
  print(x[["dataset"]])
}

print.admiral_function <- function(x, ...) {
  print(get("fun", envir = environment(x)))
}
