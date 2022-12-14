#' Maximise the trace of a matrix.
#'
#' Maximising the trace of a matrix amounts to maximising the diagonal elements.
#' This is useful when comparing clustering methods, or comparing clustering
#' results with known ground truth labels.
#'
#' @param mat Input matrix
#' @return A matrix with rows permuted such that tr(mat) is maximised.
#' @examples
#' # say we have the output of a clustering algorithm on 100 samples
#' a <- sample(letters[1:5], 100, replace = TRUE)
#' # now imagine we repeat it and get different labels for the same clusters
#' releveler <- setNames(letters[1:5], sample(letters[1:5]))
#' b <- releveler[a]
#' # comparing the results, it can be hard to spot which cluster is which
#' table(a, b)
#' # if we permute the rows to maximise the trace it's pretty obvious
#' maximise_trace(table(a, b))
#' @export
maximise_trace <- function(mat) {
    diag <- diag(1, nrow(mat))
    permute_matrix(mat, diag)
}

permute_matrix <- function(a, b) {
    # finds the permutation P of A such that ||PA - B|| is min in Frobenius norm
    # Uses the linear-sum assignment problem (LSAP) solver in the "clue" package

    # Returns P%*%A and the permutation vector `pvec' such that
    # A[pvec, ] is the permutation of A closest to B
    n <- nrow(a)
    d <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            d[j, i] <- sum((b[j, ] - a[i, ])^2)
        }
    }
    vec <- c(clue::solve_LSAP(d))
    a[vec, ]
}
