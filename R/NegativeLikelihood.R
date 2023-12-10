#' Calculates the Negative log likelihood value from a set of params
#' @param params A vector of length three in the order LS_p , tau , mu.
#' @param proximity A proximity matrix
#' @return A numeric value representing the value of negative likelihood
#' @noRd
Negative_Likelihood = function(params, proximity, Y) {

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

  # Defining a as standardized Y value
  Y_std = Y - mu
  # Defining b as the proximity matrix multiplied by standardized Y vector
  b = proximity %*% Y_std

  # Ensuring rho(p) is between -1 and 1 to ensure that our sigma matrix is Positive Definite
  if (abs(params[1]) < 1) {

    # First term of negative log likelihood function as defined in readme
    l1 = nrow(proximity) * log(abs(tau^2))
    # l1 is the scalar value of the third term of the NLL
    l1 = as.numeric(l1)

    # Second term of negative log likelihood function as defined in readme
    I_pW = I - LS_p * standardized_proximity
    # l2 is the scalar value of the third term of the NLL
    l2 = log(abs(det(I_pW)))

    # Final/third term of negative low likelihood function as defined in readme
    sum3 = sum((Y_std - (LS_p * standardized_proximity %*% Y_std)) * (Y_std * rowSums_proximity))
    # l3 is the scalar value of the third term of the NLL
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
