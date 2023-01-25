#' Graphical and tabular summaries of association between one or two tables.
#' @param a A data.frame of covariates.
#' @param b An optional data.frame of covariates.
#' @param n An integer controlling the maximum number of associations shown.
#' @param progress_bar Show a progress bar when calculating associations?
#' @param verbose Logical flag that controls whether the indices being tested
#' are printed at each iteration. Useful mainly for debugging.
#' @param ... Passed to \code{pvalue_heatmap}.
#' @return A ggplot or table showing the p-values of association between table
#' columns.
#' @details
#' For information on the tests used, see \code{\link{associate}}.
#' @examples
#' mat <- matrix(rnorm(1000), ncol = 10)
#' association_plot(mat)
#' ## only the top 5 associations
#' association_plot(mat, n = 5)
#' @rdname association-summaries
#' @export
association_plot <- function(
        a,
        b = a,
        n = 30,
        progress_bar = FALSE,
        verbose = FALSE,
        ...
    ) {
    pvals <- associate_dfs(a, b, progress_bar = progress_bar, verbose = verbose)
    ind <- rank(apply(pvals, 2, min, na.rm = TRUE)) <= n
    pvals <- pvals[ind, ind]
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
    mdf[order(mdf$"p-value"), ]
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
        verbose = FALSE,
        progress_bar = FALSE,
        symmetric = identical(a, b)) {

    if (progress_bar) {
        pb <- progress::progress_bar$new(total = ncol(a) * ncol(b))
    }
    if (symmetric) {
        combs <- utils::combn(seq_len(ncol(a)), 2)
        pvals <- vapply(
            seq_len(ncol(combs)),
            function(n) {
                i <- combs[1, n]
                j <- combs[2, n]
                if (verbose) {
                    cat(i, "vs", j, "\n")
                }
                if (progress_bar) {
                    pb$tick()
                }
                associate(a[, i, drop = TRUE], b[, j, drop = TRUE])
            }, numeric(1)
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
                sapply(
                    seq_len(ncol(b)),
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
