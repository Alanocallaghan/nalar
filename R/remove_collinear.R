remove_collinear <- function(x) {
    combs <- utils::combn(seq_len(ncol(x)), 2)
    collinear <- sapply(
        seq_len(ncol(combs)),
        function(i) {
            print(i)
            compare_columns(x[[combs[1, i]]], x[[combs[2, i]]])
        }
    )
    drop <- combs[2, collinear]
    if (length(drop)) {
        x[, -drop]
    } else {
        x
    }
}


#' @export
compare_columns <- function(a, b) {
    a <- to_numeric(a)
    b <- to_numeric(b)
    length(stats::na.omit(unique(a - b))) == 1
}

to_numeric <- function(x) {
    if (is.character(x)) {
        x <- factor(x)
    }
    if (is.factor(x) || is.logical(x)) {
        x <- as.numeric(x)
    }
    x
}
