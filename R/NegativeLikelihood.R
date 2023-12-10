#' Calculates the Negative log likelihood value from a set of params
#' @param params A vector of length three in the order LS_p , tau , mu.
#' @param proximity A proximity matrix
#' @return A numeric value representing the value of negative likelihood
#' @noRd
Negative_Likelihood = function(params, proximity) {

  ## Initializing standardized proximity matrix
  standardized_proximity = Proximity_standardize(proximity)

  ## Initializing rowSums of non-standardized matrix
  rowSums_proximity = rowSums(proximity)

  ## Initializing Identity matrix
  I = diag(nrow(proximity))

  ## Defining the three parameters of the function (p,tau,mu) that will be changed as we
  ## iterate through our MLE algorithm
  LS_p = params[1]
  tau = params[2]
  mu = params[3]

  # Defining a as Y
  a = Y - mu
  b = proximity %*% a

  # Ensuring rho(p) is between -1 and 1 to ensure that our sigma matrix is Positive Definite
  if (abs(params[1]) < 1) {

    # First term of negative log likelihood function as defined in readme
    l1 = n * log(abs(tau^2))
    l1 = as.numeric(l1)

    # Second term of negative log likelihood function as defined in readme
    I_pW = I - LS_p * standardized_proximity
    l2 = log(abs(det(I_pW)))

    # Final/third term of negative low likelihood function as defined in readme
    sum3 = sum((a - (LS_p * standardized_proximity %*% a)) * (a * rowSums_proximity))
    l3 = (1/(tau^2)) * sum3
    l3 = as.numeric(l3)

    # Scalar value obtained from negative log likelihood function with set parameters
    NLL_value = l1 - l2 + l3
    return(NLL_value)
  }
  # If rho not within -1 to 1 , returning a large positive value so the optimization algorithm strays away from those values in future iterations
  else {
    return(1e10)
  }
}
