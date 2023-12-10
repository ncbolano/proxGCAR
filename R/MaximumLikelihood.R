#' Computes the Maximum Likelihood Estimators (mu,rho,tau) for a Gaussian CAR model and the estimator's associated Variances. Returns these estimates alongside calculated Least Squares estimates.
#' @param Y An nx1 vector that represents an singular observation of
#' @param proximity A matrix with a neighboring proximity structure
#' @return A matrix of LS_estimators , MLE's , and MLE variances for rho(p) , tau , mu
#' @export
Maximum_Likelihood = function(Y, proximity) {

  proximity_std = Proximity_standardize(proximity)

  mu = Calculate_mu(Y)

  LS_p = Calculate_initial_p(Y, proximity,mu , proximity_std)

  tau = Calculate_initial_tau(Y, proximity, LS_p, mu, proximity_std)

  initial_values = c(LS_p,tau,mu)

  # NLM is a function which is a part of the Stats package. It iterates through the negative log likelihood function given a set of parameters
  nlm_output = nlm(Negative_Likelihood, initial_values, proximity = proximity, hessian = TRUE)

  # Grabbing the mu , tau , rho(p) from the nlm_output
  optimized_mu_p_tau = nlm_output$estimate
  MLEp = optimized_mu_p_tau[1]
  MLEtau = optimized_mu_p_tau[2]
  MLEmu = optimized_mu_p_tau[3]

  # NLM also stores the hessian, which we invert to get variance estimates for our MLE's
  inv_hessian = solve(nlm_output$hessian)
  MLEp_var = inv_hessian[1,1]
  MLEtau_var = inv_hessian[2,2]
  MLEmu_var = inv_hessian[2,2]

  # Putting our LS , MLE , MLE_VAR into a table and returning it
  data = c(mu, LS_p, tau, MLEmu, MLEp , MLEtau, MLEmu_var , MLEp_var , MLEtau_var)
  mat = matrix(data, nrow = 3, ncol = 3, byrow = TRUE,
               dimnames = list(c("Least Squares", "MLE", "MLE variance"),
                               c("Mu", "Rho", "Tau")))

  # Print the matrix

  return(mat)
}
