# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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

Simulating_CAR = function(proximity) {
  standardized_proximity = Proximity_standardize(proximity)
  I = diag(nrow(standardized_proximity))
  rowSums_vector = rowSums(proximity)
  t = 2
  D = (t^2) *(diag(rowSums_vector))
  n = 100
  mu = 0
  p = .5
  Sigma = ((I - (p*standardized_proximity))^-1)*(D)
  mvrnorm_data = mvrnorm(n, mu = mu, Sigma = Sigma)
  return(mvrnorm_data)
}
summation_term_LL = function(Y,rho,proximity) {
  for (k in 1:nrow(Y)) # Observation amount {
    for (i in 1:ncol(Y)) {
      for (j in 1:ncol(Y)) {
        rho_term[i] = proximity[i,j] * Y[k,j]
      }
    }
}
sum((Y[k,i]) - rho * sum(proximity[i,j] * Y[k,j]) * Y[k,i] * rowSums(proximity[i]))
Log_likelihood = function(tau, rho, Y, proximity) {
  standardized_proximity = Proximity_standardize(proximity)
  LL = (-2 * log(tau)) + log(det(diag(n) - (rho * standardized_proximity))) -
    ((tau)^-2)*(
}


