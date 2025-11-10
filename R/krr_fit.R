#' Fit Kernel Ridge Regression model
#'
#' This function fits a Kernel Ridge Regression (KRR) model using
#' a Gaussian kernel.
#'
#' @param X Matrix of input covariates (n x d)
#' @param y Response vector of length n
#' @param rho Kernel scale parameter
#' @param lambda Regularization parameter
#'
#' @return An object of class \code{"krr"} containing fitted coefficients,
#'         fitted values, training data, and kernel parameters.
#' @examples
#' X = matrix(runif(100, -1, 1), ncol = 1)
#' y = sin(2*pi*X) + rnorm(100, 0, 0.1)
#' model = krr_fit(X, y, rho = 1, lambda = 1e-4)
#' @export
krr_fit = function(X, y, rho = 1, lambda = 1e-4) {
  n = nrow(X)
  K = matrix(0, n, n)
  for (i in 1:n)
    for (j in 1:n)
      K[i, j] = exp(-rho * sum((X[i, ] - X[j, ])^2))

  coef_hat = solve(K + lambda * diag(n), y)

  result = list(
    coefficients = coef_hat,
    fitted_values = K %*% coef_hat,
    rho = rho,
    X = X,
    y = y
  )
  class(result) = "krr"
  return(result)
}
