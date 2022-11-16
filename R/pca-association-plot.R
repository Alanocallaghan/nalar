#' Graphical test between principal components and a table of covariates.
#' 
#' @param a A matrix or data.frame-alike of covariates.
#' @param b A matrix, or the output of \code{\link[stats]{prcomp}}.
#' @param method The method used to calculate PCs. Can be \code{"irlba"}
#' for \code{\link[irlba]{prcomp_irlba}} for truncated PCA or
#' \code{\link[stats]{prcomp}} for standard PCA.
#' @param center,scale Passed to PCA methods. Should the data be scaled and
#' centered before performing PCA?
#' @param npcs The number of PCs to truncate the results to. For large datasets
#' visualising >50 PCs is unwieldy so setting this to (e.g.) 20 can be very
#' useful.
#' @param max_iterations Passed to \code{\link[irlba]{prcomp_irlba}}.
#' @param progress_bar Show a progress bar when testing associations? Useful
#' for very large datasets.
#' @param ... Passed to specific methods.
#' @return The output of plot_grid.
#' @export
setGeneric("pca_association_plot", function(a, b, ...) {
  standardGeneric("pca_association_plot")
})

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "matrix"),
  function(a, b,
      method = c("irlba", "prcomp"),
      npcs = min(ncol(a) - 1, nrow(a) - 1, 50),
      center = TRUE,
      scale = TRUE,
      max_iterations = 100000,
      ...) {

    method <- match.arg(method)
    ## -1 because irlba has to be truncated
    pcs <- switch(method,
      "irlba" = {
        prcomp_irlba(b,
          n = npcs,
          center = center,
          scale. = scale,
          maxit = max_iterations)
      },
      "prcomp" = {
        prcomp(b,
          center. = center,
          scale. = scale
        )
      }
    )
    pca_association_plot(a, pcs, npcs = npcs, ...)
  }
)

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "ANY", b = "ANY"),
  function(a, b, ...) {
    pca_association_plot(as.data.frame(a), b, ...)
  }
)

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "missing"),
  function(a, b, ...) {
    pca_association_plot(a, a, ...)
  }
)

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "ANY", b = "data.frame"),
  function(a, b, ...) {
    pca_association_plot(a, as.matrix(b), ...)
  }
)

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "irlba_prcomp"),
  function(a, b, progress_bar = TRUE, npcs = ncol(b$x), ...) {
    pcs <- b$x[, seq_len(npcs), drop = FALSE]
    pvals <- associate_dfs(a, pcs, progress_bar = progress_bar)

    eigs <- (b$sdev^2)[seq_len(npcs)]
    varexp <- eigs / sum(eigs)

    hm <- pvalue_heatmap(pvals, ...)
    add_varexp(hm, varexp)
  }
)

#' @rdname pca_association_plot
#' @export
setMethod(
  "pca_association_plot",
  signature(a = "data.frame", b = "prcomp"),
  function(a, b, npcs = ncol(b$x), progress_bar = TRUE, ...) {
    pcs <- b$x[, seq_len(npcs), drop = FALSE]
    pvals <- associate_dfs(a, pcs, progress_bar = progress_bar)

    eigs <- (b$sdev^2)[seq_len(npcs)]
    varexp <- eigs / sum(eigs)

    hm <- pvalue_heatmap(pvals, ...)
    add_varexp(hm, varexp)
  }
)

add_varexp <- function(heatmap, varexp) {
  barplot <- ggplot() +
    aes(
        factor(paste0(seq_along(varexp)), levels = paste0(seq_along(varexp))),
        varexp
    ) +
    geom_col() +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "% variance explained")
  plot_grid(barplot, heatmap, align = "v", ncol = 1, axis = "tblr")
}

pvalue_heatmap <- function(pvalues, varexp, ...) {
  stopifnot(inherits(pvalues, "matrix"))

  mdf <- reshape2::melt(pvalues)
  ggplot(mdf, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_distiller(
      palette = "YlGnBu",
      name = "p-value",
      trans = "log10",
      limits = c(min(pvalues), 1)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(hjust = 1, angle = 45),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
