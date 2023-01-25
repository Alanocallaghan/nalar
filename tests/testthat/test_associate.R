context("associations")

test_that("reflect_matrix works", {
    mat <- as.matrix(mtcars[1:10, 1:10])
    mat <- reflect_matrix(unname(mat))
    expect_true(isSymmetric(mat))
})

test_that("associate_dfs works", {
    asymm <- associate_dfs(mtcars, mtcars, symmetric = FALSE)
    diag(asymm) <- NA
    symm <- associate_dfs(mtcars, mtcars, symmetric = TRUE)
    expect_identical(asymm, symm)
})

test_that("association_plot works", {
    g <- association_plot(mtcars)
    expect_is(g, "gg")
})

test_that("pcaassociation_plot works", {
    g <- pca_association_plot(mtcars, prcomp(mtcars))
    expect_is(g, "gg")
})


test_that("inscrutable s4 error from logical input", {
    mtcars$vs <- as.logical(mtcars$vs)
    expect_error(associate_dfs(mtcars), NA)
})

test_that("associate doesn't fail with low levels", {
    a <- rep("a", 10)
    b <- rep("b", 10)
    c <- rep(1, 10)
    expect_error(associate(a, b), NA)
    expect_equal(associate(a, b), NA)
    expect_error(associate(a, c), NA)
    expect_equal(associate(a, c), NA)
})

test_that("associate with NAs", {
    x <- c(rep("a", 2), rep("b", 8), rep("c", 10))
    y <- c(rep("d", 2), rep(NA, 18))
    expect_error(associate(x, y), NA)
    expect_equal(associate(x, y), NA)
})
