test_that("sreserve works correctly", {
  Lambda <- function(x) {
    A <- matrix(0, 3, 3)
    A[1, 2] <- (0.0004 + 10^(4.54 + 0.06 * (x + 30) - 10)) * ifelse(x <= 35, 1, 0)
    A[1, 3] <- A[1, 2]
    A[2, 1] <- 2.0058 * exp(-0.117 * (x + 30)) * ifelse(x <= 35, 1, 0)
    A[2, 3] <- A[1, 3] * (1 + ifelse(x <= 35, 1, 0))
    diag(A) <- -rowSums(A)
    return(A)
  }
  R <- function(x, mu) {
    if (x <= 35) {
      return(diag(c(-mu, 400000, 0)))
    } else {
      return(diag(c(400000, 400000, 0)))
    }
  }
  rentefun <- function(x) { 0.01 + 0.001 * x }  # Dynamic interest rate

  result <- sreserve(0, 80, Lambda, R, mu = 200000, r = rentefun, n = 1000)

  # Check that result is a matrix
  expect_true(is.matrix(result))

  # Check that dimensions match the expected size
  expect_equal(dim(result), c(3, 3))

})
