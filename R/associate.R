## S4 method to associate between all types of variables
#' @export
associate <- function(a, b, ...) standardGeneric("associate")

#' @export
setMethod(
  "associate", 
  signature(a = "numeric", b = "numeric"),
  function(a, b, method = "spearman") {
    cor.test(a, b, method = method, exact = FALSE)[["p.value"]]
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "character", b = "ANY"),
  function(a, b) {
    associate(factor(a), b)
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "ANY", b = "character"),
  function(a, b) {
    associate(a, factor(b))
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "ANY", b = "factor"),
  function(a, b) {
    associate(b, a)
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "factor", b = "numeric"),
  function(a, b) {
    anova(lm(b ~ a))[["Pr(>F)"]][[1]]
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "logical", b = "numeric"),
  function(a, b) {
    associate(factor(a, b))
  }
)
#' @export
association_plot <- function(dataframe, ...) {
  pvals <- generate_pvalues(dataframe, dataframe)
  diag(pvals) <- NA
  pvalue_heatmap(pvals, ...)
}
