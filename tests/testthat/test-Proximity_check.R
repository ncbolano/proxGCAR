test_that("Proximity_setup handles matrices correctly", {
  # Test 1 : (Given invalid matrix , nrow(proximity) != ncol(proximity))
  proximity1 = matrix(c(0,1,1,1,0,1), nrow = 2)
  expect_error(Proximity_check(proximity1))

  # Test 2 : (Given invalid matrix , some value not equal to (0 or 1))
  proximity2 = matrix(c(0,2,2,0), nrow = 2)
  expect_error(Proximity_check(proximity2))

  # Test 3 : (Given invalid matrix , some diagonal value not equal to 0)
  proximity3 = matrix(c(1,1,1,1), nrow = 2)
  expect_error(Proximity_check(proximity3))

  # Test 4 : (Given invalid matrix , matrix not symmetric)
  proximity4 = matrix(c(0,0,1,1,0,0,1,0,0), nrow = 3)
  expect_error(Proximity_check(proximity4))

  # Test 5 : (Given invalid matrix , one or more rowSums has value = 0)
  # Also prevents an empty matrix from passing checking tests
  proximity5 = matrix(c(0,0,1,0,0,0,1,0,0), nrow = 3)
  expect_error(Proximity_check(proximity5))

  # Test 6 - 8 : (Given valid matrices)
  proximity6 = matrix(c(0,1,1,0), nrow = 2)
  expect_silent(Proximity_check(proximity6))
  proximity7 = matrix(c(0,1,1,1,0,1,1,1,0), nrow = 3)
  expect_silent(Proximity_check(proximity7))
  proximity8 = matrix(c(0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0), nrow = 4)
  expect_silent(Proximity_check(proximity8))

})
