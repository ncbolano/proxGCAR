# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Proximity_check = function(proximity) {
  # Checking matrix is square (2)
  if(nrow(proximity) != ncol(proximity)) {
    stop("Your proximity matrix must be a square matrix.
    For further aid towards construction of the proximity matrix, refer to the
    package description.")
  }
  # Checking matrix has all values 0 or 1 (3)
  if (any(proximity != 0 & proximity != 1)) {
    stop("Your proximity matrix contains a value that is not ( 0 or 1 ).
    For further aid towards construction of the proximity matrix, refer to the
    package description.")
  }
  # Checking matrix has all 0 values for it's diagonals (4)
  if(any(diag(proximity) != 0)) {
    stop("Your proximity matrix should have all diagonal entries equal to 0. For
    further aid towards construction of the proximity matrix, refer to the
    package description.")
  # Checking matrix is symmetric (5)
  if(!isSymmetric(proximity)) {
    stop("Your proximity matrix should be symmetric. For further aid towards construction of the proximity matrix,
    refer to the package description.")
  }
  # Checking matrix has all rowSums > 0. (No check for colSums since symmetric (6)
  if(!all(rowSums(proximity) > 0)){
    stop("Your proximity matrix should have at least one conditional dependance
    for each row. For further aid towards construction of the proximity matrix,
    refer to the package description.")
  }


  }
}
