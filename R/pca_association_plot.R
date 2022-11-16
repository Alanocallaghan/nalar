#' @export
setGeneric("pca_association_plot", function(a, b, ...) standardGeneric("pca_association_plot"))
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
        prcomp_irlba(b, 
          n = npcs, 
          scale. = scale,
          maxit = max_iterations)
      },
      "prcomp" = {
        prcomp(b,
          scale. = scale
        )
      }
    )
    pca_association_plot(a, pcs, npcs = npcs, ...)
  }
)

#' @export
setMethod(
  "pca_association_plot",
  signature(a = "ANY", b = "ANY"),
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
    pvals <- associate_dfs(a, pcs, progress_bar = progress_bar)
    eigs <- (b$sdev^2)[seq_len(npcs)]
    varexp <- eigs / sum(eigs)
    hm <- pvalue_heatmap(pvals, ...)
    add_varexp(hm, varexp)
  }
)
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
    theme(axis.text.x = element_blank()) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = "% variance explained")
  plot_grid(barplot, heatmap, align = "v", ncol = 1, axis = "tblr")
}

pvalue_heatmap <- function(pvalues, varexp, ...) {
  stopifnot(inherits(pvalues, "matrix"))

  mdf <- reshape2::melt(pvalues)
  ggplot(mdf, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile() +
    scale_fill_distiller(palette = "YlGnBu", name = "p-value", trans = "log10", limits = c(min(pvalues), 1)) +
    theme(
      axis.text.x = element_text(hjust = 1, angle = 45)
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
