#' Productintegral
#' This function calculates the productintegral of a matrix from s to t with a given number of steps for the runge kutta
#'
#' @param A intensity matrix
#' @param s initial timepoint
#' @param t end timepoint
#' @param n number of steps for the runge kutta algorithm
#' @return returns a matrix (if using an intensity matrix as A you are given the transition probabilities)
#' @examples
#' Lambda <- function(x) matrix(c(-0.1, 0.1, 0, -0.1), 2, 2)
#' prodint(Lambda, 0, 80, 100)
#' @export
prodint <- function(A, s, t, n) {
  x0 <- s
  y0 <- diag(nrow(A(s)))
  h <- (t - s) / n
  for (i in 1:n) {
    s1 <- h * y0 %*% A(x0)
    s2 <- h * (y0 + s1 / 2) %*% A(x0 + h / 2)
    s3 <- h * (y0 + s2 / 2) %*% A(x0 + h / 2)
    s4 <- h * (y0 + s3) %*% A(x0 + h)
    y0 <- y0 + s1 / 6 + s2 / 3 + s3 / 3 + s4 / 6
    x0 <- x0 + h
  }
  return(y0)
}
