test_that("equiv_premium works correctly", {
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
  dR <- function(x, mu) {
    if (x <= 35) {
      return(diag(c(-1, 0, 0)))
    } else {
      return(diag(c(0, 0, 0)))
    }
  }
  result <- equiv_premium(0, 1, Lambda, R, dR, 0.05, 0.03, 5000)
  expect_true(!any(is.na(result)), "Result contains NA values")
  expect_true(is.numeric(result))
})
