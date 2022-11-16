#' Statistical test of association between arbitrary vector types.
#' @param a,b Vectors of covariates of arbitrary type.
#' See Details for the specific tests used.
#' @param ... Passed to specific methods.
#' @details
#' For numeric vs numeric, a correlation test.
#' For factor vs factor, a chisq test.
#' For numeric vs factor/character/logical, ANOVA.
#' @return A p-value of an association test.
#' @export
setGeneric("associate", function(a, b, ...) standardGeneric("associate"))

#' @export
setMethod(
  "associate",
  signature(a = "numeric", b = "numeric"),
  function(a, b, method = "spearman") {
    stats::cor.test(a, b, method = method, exact = FALSE)[["p.value"]]
  }
)

#' @export
setMethod(
  "associate",
  signature(a = "factor", b = "numeric"),
  function(a, b) {
    if (length(unique(a)) == 1) NA
    stats::anova(stats::lm(b ~ a))[["Pr(>F)"]][[1]]
  }
)
#' @export
setMethod(
  "associate",
  signature(a = "numeric", b = "factor"),
  function(a, b) {
    stats::anova(stats::lm(a ~ b))[["Pr(>F)"]][[1]]
  }
)

#' @export
setMethod(
  "associate",
  signature(a = "factor", b = "factor"),
  function(a, b) {
    if (length(unique(a)) == 1 || length(unique(b)) == 1) NA
    stats::chisq.test(a, b, simulate.p.value = TRUE)[["p.value"]]
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
  signature(a = "character", b = "character"),
  function(a, b) {
    associate(factor(a), factor(b))
  }
)

#' @export
setMethod(
  "associate",
  signature(a = "ANY", b = "character"),
  function(a, b) {
    associate(factor(b), a)
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
  signature(a = "logical", b = "ANY"),
  function(a, b) {
    associate(factor(a), b)
  }
)

#' @export
setMethod(
  "associate",
  signature(a = "ANY", b = "logical"),
  function(a, b) {
    associate(a, factor(b))
  }
)
