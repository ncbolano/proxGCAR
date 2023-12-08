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

  a = Y - mu
  b = proximity %*% a

  if (abs(params[1]) < 1) {

    l1 = n * log(abs(tau^2))
    l1 = as.numeric(l1)

    I_pW = I - LS_p * standardized_proximity
    l2 = log(abs(det(I_pW)))

    sum3 = sum((a - (LS_p * standardized_proximity %*% a)) * (a * rowSums_proximity))
    l3 = (1/(tau^2)) * sum3

    l3 = as.numeric(l3)

    equation = l1 - l2 + l3
    return(equation)
  }
  else {
    return(1e10)
  }
}
