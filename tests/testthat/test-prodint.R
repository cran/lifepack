test_that("prodint works correctly", {
  Lambda <- function(x) {
    A <- matrix(0, 3, 3)
    A[1, 2] <- (0.0004 + 10^(4.54 + 0.06 * (x + 30) - 10)) * ifelse(x <= 35, 1, 0)
    A[1, 3] <- A[1, 2]
    A[2, 1] <- 2.0058 * exp(-0.117 * (x + 30)) * ifelse(x <= 35, 1, 0)
    A[2, 3] <- A[1, 3] * (1 + ifelse(x <= 35, 1, 0))
    diag(A) <- -rowSums(A)
    return(A)
  }

  result <- prodint(Lambda, 0, 80, 1000)
  print(class(result))

  # Check that result is a matrix
  expect_true(is.matrix(result))

  # Check that there are no NA values in the result
  expect_false(any(is.na(result)))
})
