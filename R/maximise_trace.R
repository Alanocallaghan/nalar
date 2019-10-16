#' Maximise the trace of a matrix.
#' 
#' Maximising the trace of a matrix amounts to maximising the diagonal elements.
#' This is useful when comparing clustering methods, or comparing clustering 
#' results with known ground truth labels.
#' 
#' @param mat Input matrix
#' @return A matrix with rows permuted such that tr(mat) is maximised.
#' @export
maximise_trace <- function(mat) {
  diag <- diag(1, nrow(mat)) # this choice of B maximizes the trace of permuted A
  permute_matrix(mat, diag)
}


permute_matrix <- function(A, B) {
  ## https://r.789695.n4.nabble.com/reordering-of-matrix-rows-to-maximize-the-sum-of-the-diagonal-td2062867.html
  # finds the permutation P of A such that ||PA - B|| is minimum in Frobenius norm
  # Uses the linear-sum assignment problem (LSAP) solver in the "clue" package

  require("clue")  # need this package to solve the LSAP
  # Returns P%*%A and the permutation vector `pvec' such that
  # A[pvec, ] is the permutation of A closest to B
  n <- nrow(A)
  D <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      D[j, i] <- sum((B[j, ] - A[i, ]) ^ 2)
    } 
  }
  vec <- c(clue::solve_LSAP(D))
  A[vec, ]
}
