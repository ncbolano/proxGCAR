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
  proximity = (proximity / rowSums_vector)
  return(proximity)
}
Calculate_mu = function(Y) {
  mu = sum(Y) / length(Y)
  return(mu)
}

#Calculate_initial_p = function(Y,proximity) {
#  standardized_proximity = Proximity_standardize(proximity)
#  p = 0
#  learning_rate = .01
#  iterations = 100
#  for (i in 1:iterations) {
#    sum = standardized_proximity %*% Y
#    derivative = -2 * t(Y - (p * sum)) %*% sum
#    derivative = as.numeric(derivative)
#    p = p - (learning_rate * derivative)
#  }
#  return(p)
#}

Calculate_initial_p = function(Y,proximity) {
  a = Y - mu
  b = Proximity_std %*% a
  LS_p = (t(a) %*% b)/(t(b) %*% b)
  as.numeric(LS_p)
  return(LS_p)
}

Calculate_initial_tau = function(Y, proximity){
  rowsums_v = rowSums(adj_matrix)
  c = (a - (LS_p * b))
  tau_sq_est = sqrt(rowsums_v %*% (c^2) / n)
  tau = sqrt(tau_sq_est)
  return(tau)
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

# ------ newer section
Negative_Likelihood = function(Y, proximity) {
  function(parameters) {
    ## Initializing standardized proximity matrix
    standardized_proximity = Proximity_standardize(proximity)
    ## Initializing rowSums of non-standardized matrix
    rowSums_vector = rowSums(proximity)
    ## Initializing Identity matrix
    I = diag(nrow(proximity))
    p = parameters[1]
    tau = parameters[2]
    ## Initializing mu
    mu = Calculate_mu(Y)
    #equation = log(abs(det(tau^-2 * diag(rowSums_vector) %*% (I - (p * standardized_proximity)))))
    equation_1 = log(abs(tau)^2)
    equation_2 = log(abs(det(I - p * standardized_proximity)))
    equation_3 = t(Y - mu) %*% (tau^-2 * diag(rowSums_vector) %*% (I - (p * standardized_proximity))) %*% (Y - mu)
    equation = equation_1 - equation_2 + equation_3
    return(equation)
  }
}

Maximum_Likelihood = function(Y, proximity) {
  p = Calculate_initial_p(Y, proximity)
  tau = Calculate_initial_tau(Y, proximity)
  initial_values = c(0,0)
  initial_values[1] = p
  initial_values[2] = tau
  Objective_function = Negative_Likelihood(Y, proximity)
  nlm_output = nlm(Objective_function, p = initial_values)
  optimized_p_tau = nlm_output$estimate
  return(optimized_p_tau)
}



