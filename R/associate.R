## S4 method to associate between all types of variables
associate <- function(a, b, ...) standardGeneric("associate")

setMethod(
  "associate", 
  signature(a = "numeric", b = "numeric"),
  function(a, b, method = "spearman") {
    cor.test(a, b, method = method, exact = FALSE)[["p.value"]]
  }
)

setMethod(
  "associate", 
  signature(a = "character", b = "ANY"),
  function(a, b) {
    associate(factor(a), b)
  }
)

setMethod(
  "associate", 
  signature(a = "ANY", b = "character"),
  function(a, b) {
    associate(a, factor(b))
  }
)

setMethod(
  "associate", 
  signature(a = "ANY", b = "factor"),
  function(a, b) {
    associate(b, a)
  }
)

setMethod(
  "associate", 
  signature(a = "factor", b = "numeric"),
  function(a, b) {
    anova(lm(b ~ a))[["Pr(>F)"]][[1]]
  }
)

setMethod(
  "associate", 
  signature(a = "logical", b = "numeric"),
  function(a, b) {
    associate(factor(a, b))
  }
)

association_plot <- function(dataframe) {
  pvals <- sapply(
    1:ncol(dataframe),
    function(i) {
      sapply(1:ncol(dataframe),
        function(j) {
          associate(dataframe[[i]], dataframe[[j]])
        }
      )
    }
  )
  dimnames(pvals) <- list(colnames(dataframe), colnames(dataframe))
  diag(pvals) <- NA
  pvalue_heatmap(pvals)
}
