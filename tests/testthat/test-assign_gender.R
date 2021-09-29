context("Assign gender to names")

test_that("gender is correct", {
  name <- c("aileen", "anna", "colin", "david", "ernest", "john")
  df <- data.frame(FirstName = name, stringsAsFactors = FALSE)
  gender <- c("F", "F", "M", "M", "M", "M")
  assigngender <- assign_gender(data_df = df, first_name_col = "FirstName")
  expect_is(assigngender, "data.frame")
  expect_is(assigngender$gender, "character")
  expect_equal(assigngender$gender, gender)
}
)
