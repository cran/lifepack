#' Equivalence Premium
#'
#' This function calculates the equivalence premium for an insurance contract.
#' @param s Initial timepoint
#' @param t End timepoint
#' @param Lambda Intensity matrix
#' @param R Reward matrix
#' @param dR Differential of reward matrix
#' @param mu Equivalence premium guess
#' @param r Constant rate as a scalar
#' @param n Number of steps for the Runge-Kutta algorithm
#' @return A scalar
#' @examples
#' Lambda <- function(x) matrix(c(-0.1, 0.1, 0.05, -0.05), nrow = 2)
#' R <- function(x, mu) matrix(c(mu, 0, 0, mu), nrow = 2) # Corrected
#' dR <- function(x, mu) matrix(c(0.1, 0, 0, 0.1), nrow = 2)
#' equiv_premium(0, 80, Lambda, R, dR, 0.05, 0.03, 100)
#' @export
equiv_premium <- function(s, t, Lambda, R, dR, mu, r, n) {
  # Compute the reserve matrices
  b <- reserve(s, t, Lambda, R, mu, r, n)
  a <- reserve(s, t, Lambda, dR, mu, r, n)

  # Define a column vector of ones of appropriate size
  ones <- rep(1, nrow(b))  # `nrow(b)` gives `p`

  # Aggregate the matrices into scalars using vector multiplication
  b_scalar <- sum(b %*% ones)
  a_scalar <- sum(a %*% ones)

  # Return the equivalence premium as a scalar
  return(-b_scalar / a_scalar)
}
