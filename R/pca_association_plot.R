#' @export
pca_association_plot <- function(a, b, ...) standardGeneric("pca_association_plot")
#' @export
setMethod(
  "pca_association_plot",
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
    pca_association_plot(a, pcs, npcs = npcs, ...)
  }
)
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "matrix", b = "ANY"),
  function(a, b, ...) {
    pca_association_plot(as.data.frame(a), b, ...)
  }
)
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "missing"),
  function(a, b, ...) {
    pca_association_plot(a, a, ...)
  }
)
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "ANY", b = "data.frame"),
  function(a, b, ...) {
    pca_association_plot(a, as.matrix(b), ...)
  }
)
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "irlba_prcomp"),
  function(a, b, progress_bar = TRUE, npcs = ncol(b$x), ...) {
    pcs <- b$x[, seq_len(npcs), drop = FALSE]
    pvals <- generate_pvalues(a, pcs, progress_bar = progress_bar)
    pvalue_heatmap(pvals)
  }
)
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "prcomp"),
  function(a, b, npcs, progress_bar = TRUE, ...) {
    pcs <- b$x[, seq_len(npcs), drop = FALSE]
    pvals <- generate_pvalues(a, pcs, progress_bar = progress_bar)
    pvalue_heatmap(pvals, ...)
  }
)


pvalue_heatmap <- function(pvalues, ...) {
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
