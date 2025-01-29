#' Reserve with Dynamic Rate
#'
#' This function calculates the reserve matrix using a dynamic interest rate function.
#' It extends the functionality of the `reserve` function by allowing the rate to vary over time.
#'
#' @param t Initial timepoint
#' @param TT End timepoint
#' @param Lambda Intensity matrix
#' @param R Reward matrix
#' @param mu Equivalence premium
#' @param r A function of time that returns the interest rate
#' @param n Number of steps for the Runge-Kutta algorithm
#' @return A matrix representing statewise reserves
#' @examples
#' Lambda <- function(x) matrix(c(-0.1, 0.1, 0, -0.1), 2, 2)
#' R <- function(x, mu) matrix(c(0, 0, 0, mu), 2, 2)
#' rentefun <- function(x) { 0.01 + 0.001 * x }  # Dynamic interest rate
#' sreserve(0, 80, Lambda, R, 200000, rentefun, 1000)
#' @export
sreserve <- function(t, TT, Lambda, R, mu, r, n) {
  dim <- nrow(Lambda(t))
  A11 <- function(x) {
    return(Lambda(x) - r(x) * diag(1, nrow = dim))
  }
  RM <- function(x) {
    cbind(rbind(A11(x), matrix(0, dim, dim)), rbind(R(x, mu), Lambda(x)))
  }
  PRM <- prodint(RM, t, TT, n)
  RES <- PRM[1:dim, (dim + 1):(2 * dim)]
  return(RES)
}
