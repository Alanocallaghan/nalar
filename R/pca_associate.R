pca_associate <- function(a, b, ...) standardGeneric("pca_associate")

setMethod(
  "pca_associate",
  signature(a = "data.frame", b = "matrix"),
  function(a, b, ...) {
    pca_associate(a, prcomp(b), ...)
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
  signature(a = "data.frame", b = "prcomp"),
  function(a, b, ...) {
    pcs <- b$x
    pvals <- sapply(
      seq_len(ncol(a)), 
      function(i) {
        sapply(seq_len(ncol(pcs)),
          function(j) {
            associate(a[[i]], pcs[, j])
          }
        )
      }
    )
    colnames(pvals) <- colnames(a)
    rownames(pvals) <- colnames(pcs)
    pvalue_heatmap(pvals)
  }
)


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
