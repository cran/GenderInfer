context("Calculate baseline")

test_that("baseline is numeric", {
  name <- c("anna", "john", "david", "ernest", "colin", "aileen")
  gender <- c("F", "M", "M", "M", "M", "F")
  df <- data.frame(FirstName = name,
                   gender = gender,
                   stringsAsFactors = FALSE)

  baselin <- baseline(df, gender_col = "gender")
  expect_is(baselin, "numeric")
})
