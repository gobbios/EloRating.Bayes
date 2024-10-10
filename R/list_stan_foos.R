#' list and print (via \code{cat()}) Stan functions in the package
#'
#'
#' @param foo name of stan function
#' @return textual output
#' @aliases cat_foo
#' @export
#'

list_stan_foos <- function() {
  fp <- file.path(system.file(package = "EloRating.Bayes"), "extdata/stan_functions")
  x <- list.files(fp, pattern = ".stan", full.names = TRUE)
  if (length(x) == 0) {
    return(NULL)
  }

  z <- lapply(x, function(xfile){
    z <- readLines(xfile)
    z <- z[!grepl("^//", z, perl = TRUE)]
    c(paste0("- ", basename(xfile), ":\n"), paste0("    ", z[1]), "\n")
  })

  invisible(lapply(z, cat))
}

#' @export
#' @rdname list_stan_foos
cat_foo <- function(foo) {
  fp <- file.path(system.file(package = "EloRating.Bayes"), "extdata/stan_functions")
  x <- list.files(fp, pattern = foo, full.names = TRUE)
  z <- readLines(x)
  cat(paste(z, "\n"))
}
