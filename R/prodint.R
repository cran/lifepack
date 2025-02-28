#' Productintegral (C++ optim)
#'
#' This function calculates the product integral of a matrix function from s to t.
#' It uses a Runge-Kutta method implemented in C++ for efficiency.
#'
#' @param A A function returning a matrix (intensity matrix)
#' @param s Initial timepoint
#' @param t End timepoint
#' @param n Number of steps for the Runge-Kutta algorithm
#' @return A matrix (transition probabilities if A is an intensity matrix)
#' @export
prodint <- function(A, s, t, n) {
  return(prodint_cpp(A, s, t, n))
}
