test_that("Proximity_standardize standardizes each row in the proximity matrix correctly", {
  # Test (1-3) (2x2,3x3,4x4 matrices that should still remain symmetric after standardization)
  proximity1 = matrix(c(0,1,1,0), nrow = 2, byrow = TRUE)
  resulting_matrix1 = Proximity_standardize(proximity1)
  expected_matrix1 = matrix(c(0,1,1,0), nrow = 2, byrow = TRUE)
  expect_equal(resulting_matrix1, expected_matrix1)

  proximity2 = matrix(c(0,1,1,1,0,1,1,1,0), nrow = 3, byrow = TRUE)
  resulting_matrix2 = Proximity_standardize(proximity2)
  expected_matrix2 = matrix(c(0,1/2,1/2,1/2,0,1/2,1/2,1/2,0),nrow = 3, byrow = TRUE)
  expect_equal(resulting_matrix2, expected_matrix2)

  proximity3 = matrix(c(0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0), nrow = 4, byrow = TRUE)
  resulting_matrix2 = Proximity_standardize(proximity3)
  expected_matrix3 = matrix(c(0,1/2,0,1/2,1/2,0,1/2,0,0,1/2,0,1/2,1/2,0,1/2,0), nrow = 4,
                            byrow = TRUE)
  expect_equal(resulting_matrix3, expected_matrix3)

  # Test (4) (Matrix that will not remain symmetric after standardization)
  proximity4 = matrix(c(0,1,0,1,0,
                        1,0,1,1,1,
                        0,1,0,0,1,
                        1,1,0,0,1,
                        0,1,1,1,0), nrow = 5, byrow = TRUE)
  resulting_matrix2 = Proximity_standardize(proximity3)
  expected_matrix3 = matrix(c(0,1/2,0,1/2,0,1/4,0,1/4,1/4,1/4,0,1/2,0,0,1/2,
                              1/3,1/3,0,0,1/3,0,1/3,1/3,1/3,0),
                               nrow = 5, byrow = TRUE)
  expect_equal(resulting_matrix4, expected_matrix4)
})
