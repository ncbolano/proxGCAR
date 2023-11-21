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
  standardized_proximity = Proximity_standardize(proximity)
  p = 0
  learning_rate = .01
  iterations = 100
  for (i in 1:iterations) {
    Y_adj = Y - mu
    sum = standardized_proximity %*% Y_adj
    derivative = -2 * t(Y_adj - (p * sum)) %*% sum
    derivative = as.numeric(derivative)
    p = p - (learning_rate * derivative)
  }
  return(p)
}

#Calculate_mu = function(Y,proximity) {
#  standardized_proximity = Proximity_standardize(proximity)
#  p = Calculate_initial_p(Y,proximity)
#  mu = p * ((standardized_proximity) %*% Y)
#  return(mu)
#}

#Calculate_initial_tau = function(Y, proximity){
#  tau = 0
#  rowSums_vector = rowSums(proximity)
#  standardized_proximity = Proximity_standardize(proximity)
#  p = Calculate_initial_p(Y,proximity)
#  mu = Calculate_mu(Y,proximity)
#  tau = sqrt((1/length(Y)) * sum(rowSums_vector * (Y - mu)^2))
#  return(tau)
#}

Calculate_initial_tau = function(Y, proximity){
  tau = 0
  rowSums_vector = rowSums(proximity)
  standardized_proximity = Proximity_standardize(proximity)
  rowSums_vector_std = rowSums(standardized_proximity)
  p = Calculate_initial_p(Y,proximity)
  mu = Calculate_mu(Y)
  Y_adj = Y - mu
  tau_sq = (1/length(Y)) * sum(rowSums_vector  * (Y_adj - (p * sum(rowSums_vector_std * (Y_adj))))^2)
  tau = sqrt(tau_sq)
  return(tau)
}

Calculate_sigma_matrix = function(Y, proximity){
  standardized_proximity = Proximity_standardize(proximity)
  rowSums_vector = rowSums(proximity)
  I = diag(nrow(proximity))
  p = Calculate_initial_p(Y,proximity)
  tau = Calculate_initial_tau(Y,proximity)
  Sigma_inv = tau^-2 * diag(rowSums_vector) %*% (I - (p * standardized_proximity)) # By remark 4.3.2
  Sigma = solve(Sigma_inv)
  return(Sigma)
}

Calculate_Yt_Sigma_Y = function(Y, proximity) {
  p = Calculate_initial_p(Y, proximity)
  mu = Calculate_mu(Y, proximity)
  tau = Calculate_initial_tau(Y, proximity)
  Sigma = Calculate_sigma_matrix(Y, proximity)
  Y_adj = Y - mu
  Yt_Sigma_Y = t(Y_adj) %*% solve(Sigma) %*% (Y_adj)
  return(Yt_Sigma_Y)
}

Calculate_log_term = function(Y, proximity) {
  Sigma = Calculate_sigma_matrix(Y, proximity)
  log_term = log(abs(det(solve(Sigma))))
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
    standardized_proximity = Proximity_standardize(proximity)
    rowSums_vector = rowSums(proximity)
    I = diag(nrow(proximity))
    p = parameters[1]
    tau = parameters[2]
    #equation = log(abs(det(tau^-2 * diag(rowSums_vector) %*% (I - (p * standardized_proximity)))))
    equation_1 = log(abs(tau)^2)
    equation_2 = log(abs(det(I - p * standardized_proximity)))
    Calculate_sigma_matrix(Y, proximity)
    equation_3 = t(Y - mu) * solve(sigma)
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



