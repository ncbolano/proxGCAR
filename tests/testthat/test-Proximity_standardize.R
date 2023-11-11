test_that("Proximity_standardize standardizes each row in the proximity matrix correctly", {
  # Test 1 (2x2)
  proximity1 = matrix(c(0,1,1,0), nrow = 2)
  resulting_matrix1 = Proximity_standardize(proximity1)
  expected_matrix1 = matrix(c(0,1,1,0), nrow = 2)

  # Test 2 (3x3)
  proximity2 = matrix(c(0,1,1,1,0,1,1,1,0), nrow = 3)
  resulting_matrix2 = Proximity_standardize(proximity2)
  expected_matrix2 = matrix(0,1/2,1/2,1/2,0,1/2,1/2,1/2,0)
})
