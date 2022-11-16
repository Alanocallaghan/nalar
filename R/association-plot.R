#' Graphical and tabular summaries of association between one or two tables.
#' @param a A data.frame of covariates.
#' @param b An optional data.frame of covariates.
#' @param progress_bar Show a progress bar when calculating associations?
#' @param ... Passed to \code{pvalue_heatmap}.
#' @return A ggplot or table showing the p-values of association between table
#' columns.
#' @details
#' For information on the tests used, see \code{\link{associate}}.
#' @rdname association-summaries
#' @export
association_plot <- function(a, b, progress_bar = FALSE, ...) {
  pvals <- associate_dfs(a, b, progress_bar = progress_bar)
  pvalue_heatmap(pvals, ...)
}

#' @rdname association-summaries
#' @export
association_table <- function(a, b = a, progress_bar = FALSE) {
  pvals <- associate_dfs(a, b, progress_bar = progress_bar)
  if (missing(b)) {
    pvals[lower.tri(pvals)] <- NA
  }
  mdf <- reshape2::melt(pvals)
  colnames(mdf) <- c("Variable 1", "Variable 2", "p-value")
  mdf <- mdf[!is.na(mdf$"p-value"), ]
  mdf
}

## todo: plot and table methods
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
    b = a,
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
        # cat(i, "vs", j, "\n")
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
