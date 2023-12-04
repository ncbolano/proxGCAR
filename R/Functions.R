# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
### Needs mvtnorm

Proximity_check = function(proximity) {
  # Checking matrix is square (1)
  if(nrow(proximity) != ncol(proximity)) {
    stop("Your proximity matrix must be a square matrix.
    For further aid towards construction of the proximity matrix, refer to the
    package description.")
  }
  # Checking matrix has all values 0 or 1 (2)
  if (any(proximity != 0 & proximity != 1)) {
    stop("Your proximity matrix contains a value that is not ( 0 or 1 ).
    For further aid towards construction of the proximity matrix, refer to the
    package description.")
  }
  # Checking matrix has all 0 values for it's diagonals (3)
  if(any(diag(proximity) != 0)) {
    stop("Your proximity matrix should have all diagonal entries equal to 0. For
    further aid towards construction of the proximity matrix, refer to the
    package description.")
  }
  # Checking matrix is symmetric (4)
  if(!isSymmetric(proximity)) {
    stop("Your proximity matrix should be symmetric. For further aid towards
    construction of the proximity matrix, refer to the package description.")
  }
  # Checking matrix has all rowSums > 0. (No check for colSums since symmetric (5)
  if(!all(rowSums(proximity) > 0)){
    stop("Your proximity matrix should have at least one conditional dependance
    for each row. For further aid towards construction of the proximity matrix,
    refer to the package description.")
  }
}

Proximity_standardize = function(proximity) {

  # Obtains the rowSums of the matrix and saves them as a vector
  rowSums_vector = rowSums(proximity)

  # Used element wise division to efficiently compute this standardized matrix
  proximity_std = (proximity / rowSums_vector)
  return(proximity_std)
}



Calculate_mu = function(Y) {

  mu = sum(Y) / length(Y)
  return(mu)

}

Calculate_initial_p = function(Y,proximity) {

  mu = Calculate_mu(Y)
  rowSums_vector = rowSums(proximity)
  proximity_std = (proximity / rowSums_vector)

  a = Y - mu
  b = Proximity_std %*% a

  LS_p = (t(a) %*% b)/(t(b) %*% b)
  LS_p = as.numeric(LS_p)

  return(LS_p)
}

Calculate_initial_tau = function(Y,proximity,LS_p){

  a = Y - mu
  b = Proximity_std %*% a

  rowsums_v = rowSums(proximity)
  c = (a - (LS_p * b))

  tau_sq_est = sum(rowsums_v * (c^2))/n
  tau = sqrt(tau_sq_est)
  tau = as.numeric(tau)

  return(tau)
}

# ------ newer section
Negative_Likelihood = function(params, proximity) {

  ## Initializing standardized proximity matrix
  standardized_proximity = Proximity_standardize(proximity)

  ## Initializing rowSums of non-standardized matrix
  rowSums_vector = rowSums(proximity)

  ## Initializing Identity matrix
  I = diag(nrow(proximity))

  p = params[1]
  tau = params[2]
  print(params)
  a = Y - mu
  b = proximity %*% a
  if (abs(params[1]) < 1) {

    l1 = log(abs(tau^2))
    l1 = as.numeric(l1)

    I_pW = I - p * standardized_proximity
    l2 = log(abs(det(I_pW)))

    sum3 = sum((a - (p * standardized_proximity %*% a)) * (a * rowSums_vector))
    l3 = 1/(tau^2) * sum3
    l3 = as.numeric(l3)

    equation = l1 - l2 + l3
    return(equation)
  }
  else {
    return(1e10)
  }
}

Maximum_Likelihood = function(Y, proximity) {

  LS_p = Calculate_initial_p(Y, proximity)

  tau = Calculate_initial_tau(Y, proximity, LS_p)

  initial_values = c(LS_p,tau)

  nlm_output = nlm(Negative_Likelihood, initial_values, proximity = proximity)

  optimized_p_tau = nlm_output$estimate

  return(optimized_p_tau)
}


Calculate_sigma_matrix = function(Y, proximity){
  standardized_proximity = Proximity_standardize(proximity)
  rowSums_vector = rowSums(proximity)
  I = diag(nrow(proximity))
  p = Calculate_initial_p(Y,proximity)
  tau = Calculate_initial_tau(Y,proximity)
  Sigma_inv = tau^-2 * diag(rowSums_vector) %*% (I - (p * standardized_proximity)) # By remark 4.3.2
  return(Sigma_inv)
}

Calculate_Yt_Sigma_Y = function(Y, proximity) {
  p = Calculate_initial_p(Y, proximity)
  mu = Calculate_mu(Y)
  tau = Calculate_initial_tau(Y, proximity)
  Sigma_inv = Calculate_sigma_matrix(Y, proximity)
  Y_adj = Y - mu
  Yt_Sigma_Y = t(Y_adj) %*% Sigma_inv %*% (Y_adj)
  return(Yt_Sigma_Y)
}

Calculate_log_term = function(Y, proximity) {
  Sigma_inv = Calculate_sigma_matrix(Y, proximity)
  log_term = log(abs(det(Sigma)))
  return(log_term)
}

Negative_Log_Likelihood = function(Y, proximity) {
  Yt_Sigma_Y = Calculate_Yt_Sigma_Y(Y, proximity)
  log_term = Calculate_log_term(Y, proximity)
  LL =  (log_term - Yt_Sigma_Y)
  Negative_LL = -1 * LL
  return(Negative_LL)
}

