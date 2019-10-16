## S4 method to associate between all types of variables
#' @export
associate <- function(a, b, ...) standardGeneric("associate")

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
    stats::anova(stats::lm(b ~ a))[["Pr(>F)"]][[1]]
  }
)
#' @export
setMethod(
  "associate", 
  signature(a = "factor", b = "numeric"),
  function(a, b) {
    stats::chisq.test(a, b, simulate.p.value = TRUE)[["p.value"]]
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
association_plot <- function(a, b, progress_bar = FALSE, ...) {
  pvals <- associate_dfs(a, b, progress_bar = progress_bar)
  pvalue_heatmap(pvals, ...)
}


#' @export
association_table <- function(a, b, progress_bar = FALSE) {
  pvals <- associate_dfs(a, b, progress_bar = progress_bar)
  mdf <- reshape2::melt(pvals)
}

associations <- function(a, b, associate_dfs) {
  pvals <- associate_dfs(a, b, progress_bar = progress_bar)
  structure(
    pvalues = pvals,
    a = a,
    b = b,
    class = "associations"
  )
}

associate_dfs <- function(
    a,
    b,
    progress_bar = FALSE, 
    symmetric = identical(a, b)
    ) {

  if (progress_bar) {
    pb <- progress::progress_bar$new(total = ncol(a) * ncol(b))
  }
  if (symmetric) {
    combs <- utils::combn(seq_len(ncol(a)), 2)
    pvals <- sapply(seq_len(ncol(combs)), 
      function(n) {
        i <- combs[1, n]
        j <- combs[2, n]
        if (progress_bar) {
          pb$tick()
        }
        associate(a[, i, drop = TRUE], b[, j, drop = TRUE])
      }
    )
    out <- matrix(NA, 
      ncol = ncol(a),
      nrow = ncol(b),
      dimnames = list(colnames(b), colnames(a))
    )
    out[lower.tri(out)] <- pvals
    out <- reflect_matrix(out)
  } else {
    out <- sapply(
      seq_len(ncol(a)), 
      function(i) {
        sapply(seq_len(ncol(b)),
          function(j) {
            if (progress_bar) {
              pb$tick()
            }
            associate(a[, i, drop = TRUE], b[, j, drop = TRUE])
          }
        )
      }
    )
    dimnames(out) <- list(colnames(b), colnames(a))
  }
  out
}


reflect_matrix <- function(mat) {
  mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)] 
  mat
}

