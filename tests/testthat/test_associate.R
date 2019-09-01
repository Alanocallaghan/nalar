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
