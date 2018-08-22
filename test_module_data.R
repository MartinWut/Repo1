context("modul_data function")

# test, if output is as expected (i.e. output the same as entered directly on FlexStat)
test_that("Testing if the grade mean output of the function is the same as in FlexStat", {
  expect_equal(module_data(65, 12, 31463)[,8], "1.369") #SS2017, Wiwi Fac, Advanced Statistical Programming with R
  expect_equal(module_data(66, 12, 217)[,8], c("3.263", "3.039"))   #WS17/18, WiWi Fac., Econometrics I
  expect_equal(module_data(67, 4, 58844)[,8], c("1.100", "2.050", "-")) #SS2018, Phil. Fac, Basic seminar linguistics 1.2
  expect_equal(module_data(63, 6, 9039)[,8], c("-", "-")) #SS2016, Fac. for Physics, Physics II3428
  expect_equal(module_data(65, 5, 3428)[,8], "1.404") #SS2017, Fac, for Mathem. and Inform., module "Datenschutz und Datensicherheit"
})


# test, if error messages are produced, when arguments not entered correctly
test_that("Error messages if arguments are not entered correctly", {
  expect_error(module_data(85, 12, 217), 
               "The chosen semester value was not entered in the correct form or does not exist.") # not existing semester value
  expect_error(module_data("one", 12, 217), 
               "The chosen semester value was not entered in the correct form or does not exist.") # semester value entered in wrong form
  expect_error(module_data(64, 3289, 217), 
               "The chosen faculty value was not entered in the correct form or does not exist.") # not existing faculty value
  expect_error(module_data(64, "twelve", 217), 
               "The chosen faculty value was not entered in the correct form or does not exist.") # faculty value entered in wrong format
  expect_error(module_data(64, 12, 1000000000), 
               "The chosen module value was not entered in the correct form or does not exist for the chosen faculty.") # not existing module value
  expect_error(module_data(64, 12, "Econometrics I"), 
               "The chosen module value was not entered in the correct form or does not exist for the chosen faculty.") # module entered in wrong form
})


## Save unit test file in package --> Wickham: 'R packages': ##

# To set up your package to use testthat, run:

#devtools::use_testthat()

#This will:
  
#  1. Create a tests/testthat directory.

#  2. Adds testthat to the Suggests field in the DESCRIPTION.

#  3.Creates a file tests/testthat.R that runs all your tests when R CMD check runs. (You’ll learn more about that in automated checking.)

#Once you’re set up the workflow is simple:
  
#  1. Modify your code or tests.

#  2. Test your package with Ctrl/Cmd + Shift + T or devtools::test().

#  3. Repeat until all tests pass.
