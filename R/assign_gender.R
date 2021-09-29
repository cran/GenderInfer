#' Assign gender by first name
#'
#' @name assign_gender
#' @description This function use the data source based on combined US/UK
#' censor data to assign gender based on first name.
#' @param data_df, input dataframe containing the first name
#' @param first_name_col, first name column's name to assign gender to
#' @return The input data frame with the gender column:
#'
#'          gender - assigned gender (F/M/U)
#' @examples
#' gender <- assign_gender(authors, "first_name")
#'
#' @export

assign_gender <- function(data_df, first_name_col) {

  ## read the data frame and convert special character to ASCII to better
  ## assign the gender. The first name need to be without accent or special
  ## character, this is why we use here the function iconv.
  UKUS_Gender <- Name <- NULL
  df <- data_df
  df$Name <- tolower(iconv(df[, first_name_col], from = "UTF-8",
                           to = "ASCII//TRANSLIT"))
  df <- merge(df, gender_names, by.x = "Name", by.y = "Name", all.x = TRUE)
  df$UKUS_Gender[is.na(df$UKUS_Gender)] <- "U"
  df$gender <- ifelse(df$UKUS_Gender == "Male", "M",
                      ifelse(df$UKUS_Gender == "Female", "F", "U"))
  df <- subset(df, select = -c(UKUS_Gender, Name))
  df
}
