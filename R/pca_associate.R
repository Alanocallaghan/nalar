pca_associate <- function(a, b, ...) standardGeneric("pca_associate")

setMethod(
  "pca_associate",
  signature(a = "data.frame", b = "matrix"),
  function(a, b, 
      method = c("irlba", "prcomp"), 
      npcs = min(ncol(a) - 1, nrow(a) - 1, 50), 
      scale = TRUE, 
      max_iterations = 100000, 
      ...) {
    
    method <- match.arg(method)
    ## -1 because irlba has to be truncated
    pcs <- switch(method,
      "irlba" = {
        prcomp_irlba(b, n = npcs, 
          scale. = scale,
          maxit = max_iterations)
      },
      "prcomp" = {
        prcomp(b,
          scale. = scale)
      }
    )
    pca_associate(a, pcs, npcs = npcs, ...)
  }
)

setMethod(
  "pca_associate",
  signature(a = "matrix", b = "ANY"),
  function(a, b, ...) {
    pca_associate(as.data.frame(a), b, ...)
  }
)


setMethod(
  "pca_associate",
  signature(a = "data.frame", b = "missing"),
  function(a, b, ...) {
    pca_associate(a, a, ...)
  }
)

setMethod(
  "pca_associate",
  signature(a = "ANY", b = "data.frame"),
  function(a, b, ...) {
    pca_associate(a, as.matrix(b), ...)
  }
)

setMethod(
  "pca_associate",
  signature(a = "data.frame", b = "irlba_prcomp"),
  function(a, b, ...) {
    pcs <- b$x
    pvals <- generate_pvalues(a, pcs)
    pvalue_heatmap(pvals)
  }
)


setMethod(
  "pca_associate",
  signature(a = "data.frame", b = "prcomp"),
  function(a, b, npcs, ...) {
    pcs <- b$x[, seq_len(npcs)]
    pvals <- generate_pvalues(a, pcs)
    pvalue_heatmap(pvals)
  }
)

generate_pvalues <- function(a, b) {
  pvals <- sapply(
    seq_len(ncol(a)), 
    function(i) {
      sapply(seq_len(ncol(b)),
        function(j) {
          associate(a[, i, drop = TRUE], b[, j, drop = TRUE])
        }
      )
    }
  )
  dimnames(pvals) <- list(colnames(b), colnames(a))
  pvals
}

pvalue_heatmap <- function(pvalues) {
  stopifnot(inherits(pvalues, "matrix"))

  mdf <- reshape2::melt(pvalues)
  ggplot(mdf, aes_string(x = "Var1", y = "Var2", fill = "value")) + 
    geom_tile() +
    scale_fill_distiller(palette = "YlGnBu", name = "p-value", trans = "log10", limits = c(min(pvalues), 1)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
