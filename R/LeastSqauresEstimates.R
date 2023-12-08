#' Calculates the mean of our Y vector
#' @param Y A nx1 vector that represents an observation of our data
#' @returns A scalar value representing the mean of vector Y.
#' @noRd
Calculate_mu = function(Y) {

  # Calculating the predicted mu for our observed data Y
  mu = sum(Y) / length(Y)

  return(mu)
}

#' Calculates the least squares estimate of rho(p)
#' @param Y A nx1 vector that represents an observation of our data
#' @param proximity A proximity matrix
#' @param mu A scalar value that represents the mean of Y
#' @param Proximity_std Standardized proximity matrix
#' @returns A scalar value representing the Least Squares extimate of rho(p)
#' @noRd
Calculate_initial_p = function(Y, proximity, mu, Proximity_std) {

  # Calculating rowSums of our proximity matrix
  rowSums_proximity = rowSums(proximity)

  a = Y - mu
  b = Proximity_std %*% a

  LS_p = (t(a) %*% b)/(t(b) %*% b)
  LS_p = as.numeric(LS_p)

  return(LS_p)
}
#' Calculates the Least Squares estimate of tau
#' @param Y A nx1 vector that represents an observation of our data
#' @param proximity A proximity matrix
#' @param LS_p A scalar value representing the LS estimate of rho(p)
#' @param mu A scalar value that represents the mean of Y
#' @param Proximity_std Standardized proximity matrix
#' @return Least squares estimate of tau
#' @noRd
Calculate_initial_tau = function(Y,proximity,LS_p,mu,proximity_std){

  # mu = Calculate_mu(Y)
  a = Y - mu
  b = Proximity_std %*% a

  rowsums_proximity = rowSums(proximity)
  c = (a - (LS_p * b))

  tau_sq_est = sum(rowsums_proximity * (c^2))/n
  tau = sqrt(tau_sq_est)
  tau = as.numeric(tau)

  return(tau)
}
