# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
### Needs mvtnorm

library(usethis)
use_test()
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

### Might have to move learning rate and iterations inside of this function
Calculate_initial_p = function(Y,proximity) {
  standardized_proximity = Proximity_standardize(proximity)
  p = 0
  learning_rate = .01
  iterations = 100
  for (i in 1:iterations) {
    sum = standardized_proximity %*% Y
    derivative = -2 * t(Y - (p * sum)) %*% sum
    derivative = as.numeric(derivative)
    p = p - (learning_rate * derivative)
  }
  return(p)
}

Calculate_mu = function(Y,proximity) {
  standardized_proximity = Proximity_standardize(proximity)
  p = Calculate_initial_p(Y,proximity, learning_rate = .01, iterations = 100)
  mu = p * ((standardized_proximity) %*% Y)
  return(mu)
}

Calculate_initial_tau = function(Y, proximity){
  tau = 0
  rowSums_vector = rowSums(proximity)
  standardized_proximity = Proximity_standardize(proximity)
  p = Calculate_initial_p(Y,proximity, learning_rate = .01, iterations = 100)
  mu = Calculate_mu(Y,proximity,p)
  tau = (1/length(Y)) * rowSums_vector * (Y - mu)^2
  return(tau)
}

Calculate_sigma_matrix = function(Y, proximity){
  standardized_proximity = Proximity_standardize(proximity)
  rowSums_vector = rowSums(proximity)
  I = diag(nrow(proximity))
  p = Calculate_initial_p(Y,proximity)
  tau = Calculate_initial_tau(Y,proximity)
  sigma_inv = tau^-2 * diag(rowSums_vector) %*% (I - p * standardized_proximity) # By remark 4.3.2
  sigma = solve(sigma_inv)
  return(sigma)
}

Calculate_Yt_Sigma_Y = function(Y, proximity) {
  p = Calculate_initial_p(Y, proximity)
  mu = Calculate_mu(Y, proximity)
  tau = Calculate_initial_tau(Y, proximity)
  Sigma = Calculate_sigma_matrix(Y, proximity)
  Yt_Sigma_Y = t((Y - mu)) * Sigma * (Y - mu)
  return(Yt_Sigma_Y)
}

Calculate_log_term = function(Y, proximity) {
  Sigma = Calculate_sigma_matrix(Y, proximity)
  log_term = log(det(Sigma))
  return(log_term)
}

Log_Likelihood = function(Y, proximity) {
  log_term = Calculate_log_term(Y,proximity,p,t)
  Yt_Sigma_Y = Calculate_Yt_Sigma_Y(Y,proximity,p,t)
  -1 * (log_term + Yt_Sigma_Y)
}


# ------ newer section

Maximum_Likelihood = function()
nlm()
