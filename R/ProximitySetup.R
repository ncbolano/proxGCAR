#' Checking the proximity matrix provided is valid
#' @param proximity A proximity matrix
#' @noRd
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


#' Standardizes valid proximity matrix by row
#' @param proximity A proximity matrix
#' @returns The standardized version of the matrix
#' @noRd
Proximity_standardize = function(proximity) {

  # Obtains the rowSums of the matrix and saves them as a vector
  rowSums_proximity = rowSums(proximity)

  # Used element wise division to efficiently compute this standardized matrix
  proximity_standardized = (proximity / rowSums_proximity)

  return(proximity_standardized)
}
