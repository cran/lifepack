test_that("reserve works correctly", {
  Lambda <- function(x) {

    A <- matrix(0, 3, 3)


    A[1, 2] <- (0.0004 + 10^(4.54 + 0.06*(x+30)-10))*ifelse(x <= 35, 1, 0)
    A[1,3]  <- A[1,2]
    A[2,1] <- 2.0058 * exp(-0.117*(x+30)) * ifelse(x <= 35,1,0)
    A[2,3] <- A[1,3]*(1+ifelse(x <= 35,1,0))

    row_sums <- rowSums(A)
    diag(A) <- -row_sums

    return(A)
  }
  R <- function(x, mu) {
    if (x <= 35) {
      return(diag(c(-mu , 400000, 0)))
    } else {
      return(diag(c(400000, 400000,0)))
    }
  }
  result <- reserve(0,80, Lambda, R, mu=200000, r = 0.01, n = 1000)

  expect_true(is.matrix(result))

})
