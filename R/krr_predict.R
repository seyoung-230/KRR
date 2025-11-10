#' Predict method for Kernel Ridge Regression
#'
#' Compute predicted responses for new data using a fitted KRR model.
#'
#' @param object A fitted object of class \code{"krr"}.
#' @param newdata A matrix of new covariate values.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A numeric vector of predicted values.
#' @examples
#' X = matrix(runif(100, -1, 1), ncol = 1)
#' y = sin(2*pi*X) + rnorm(100, 0, 0.1)
#' model = krr_fit(X, y)
#' X_new = matrix(seq(-1, 1, length.out = 50), ncol = 1)
#' preds = predict(model, X_new)
#' @export
#' @method predict krr
predict.krr = function(object, newdata, ...) {
  X_train = object$X
  n_train = nrow(X_train)
  n_new = nrow(newdata)

  K_new = matrix(0, n_new, n_train)
  for (i in 1:n_new)
    for (j in 1:n_train)
      K_new[i, j] = exp(-object$rho * sum((newdata[i, ] - X_train[j, ])^2))

  preds = K_new %*% object$coefficients
  return(as.vector(preds))
}
