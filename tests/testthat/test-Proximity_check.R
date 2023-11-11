test_that("Proximity_setup handles matrices correctly", {
  # Test 1 : (Given valid matrix)
  proximity1 = matrix(c(0,1,1,0), nrow = 2)
  expect_silent(Proximity_check(proximity1))

  # Test 2 : (Given invalid matrix , nrow(proximity) != ncol(proximity))
  proximity2 = matrix(c(0,1,1,1,0,1), nrow = 2)
  expect_error(Proximity_check(proximity2))

  # Test 3 : (Given invalid matrix , some value not equal to (0 or 1))
  proximity3 = matrix(c(0,2,2,0), nrow = 2)
  expect_error(Proximity_check(proximity3))

  # Test 4 : (Given invalid matrix , some diagonal value not equal to 0)
  proximity4 = matrix(c(1,1,1,1), nrow = 2)
  expect_error(Proximity_check(proximity4))
})
