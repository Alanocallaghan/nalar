#' Statistical test of association between arbitrary vector types.
#' @param a,b Vectors of covariates of arbitrary type.
#' See Details for the specific tests used.
#' @param method For numeric vs numeric, the correlation method (passed to
#' \code{\link[stats]{cor.test}})
#' @param ... Passed to specific methods.
#' @details
#' For numeric vs numeric, a correlation test.
#' For factor vs factor, a chisq test.
#' For numeric vs factor/character/logical, ANOVA.
#' @return A p-value of an association test.
#' @export
setGeneric("associate", function(a, b, ...) standardGeneric("associate"))

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "numeric", b = "numeric"),
    function(a, b, method = "spearman") {
        stats::cor.test(a, b, method = method, exact = FALSE)[["p.value"]]
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "factor", b = "numeric"),
    function(a, b) {
        if (length(unique(a)) == 1) {
            return(NA)
        }
        stats::anova(stats::lm(b ~ a))[["Pr(>F)"]][[1]]
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "numeric", b = "factor"),
    function(a, b) {
        stats::anova(stats::lm(a ~ b))[["Pr(>F)"]][[1]]
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "factor", b = "factor"),
    function(a, b) {
        if (length(unique(a)) == 1 || length(unique(b)) == 1) {
            return(NA)
        }
        stats::chisq.test(a, b, simulate.p.value = TRUE)[["p.value"]]
    }
)
#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "character", b = "ANY"),
    function(a, b) {
        associate(factor(a), b)
    }
)
#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "Date", b = "ANY"),
    function(a, b) {
        associate(as.numeric(a), b)
    }
)
#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "ANY", b = "Date"),
    function(a, b) {
        associate(a, as.numeric(b))
    }
)


#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "character", b = "character"),
    function(a, b) {
        associate(factor(a), factor(b))
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "ANY", b = "character"),
    function(a, b) {
        associate(factor(b), a)
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "ANY", b = "factor"),
    function(a, b) {
        associate(b, a)
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "logical", b = "ANY"),
    function(a, b) {
        associate(factor(a), b)
    }
)

#' @rdname associate
#' @export
setMethod(
    "associate",
    signature(a = "ANY", b = "logical"),
    function(a, b) {
        associate(a, factor(b))
    }
)
