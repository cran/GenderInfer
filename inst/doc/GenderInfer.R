## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(GenderInfer)
library(dplyr)
library(ggplot2)

## ---- warning=FALSE, message=FALSE--------------------------------------------
head(authors)

## ---- warning=FALSE, message=FALSE--------------------------------------------
authors_df <- assign_gender(data_df = authors, first_name_col = "first_name")
  
head(authors_df)

## ---- warning=FALSE, message=FALSE--------------------------------------------
## Count how many female, male and unknown gender there are in the data
authors_df %>% count(gender)

## per gender and country
authors_df %>% count(gender, country_code)


## ----  warning=FALSE, message=FALSE-------------------------------------------
## calculates baseline for the year range 2016-2019
baseline_female <- baseline(data_df = authors_df %>% 
                              filter(publication_years %in% seq(2016, 2019)),
                            gender_col = "gender")
baseline_female


## -----------------------------------------------------------------------------
## Create a data frame that containing only the data from 2020 and
## the count of the variable gender.
female_count_2020 <- authors_df %>% 
  filter(publication_years == 2020) %>%
  count(gender)

## create a new data frame to be used for the binomial calculation.
df_gender <- reshape_for_binomials(data = female_count_2020,
                                   gender_col = "gender",
                                   level = 2020)
#df_gender <- test(female_count_2020, "gender", 2020)

df_gender

## ---- fig.width=6, fig.height=4-----------------------------------------------
## Calculate the binomial
## Create a new column with the baseline and calculate the binomial.

df_gender <- calculate_binom_baseline(data_df = df_gender,
                                      baseline_female = baseline_female)

df_gender

## Reshape first the dataframe using `gender_total_df` and afterwards create a
## bar chart of showing the number of male, female and unknown gender with `gender_bar_chart`
gender_total <- total_gender_df(data_df = df_gender, level = "level")

bar_chart(data_df = gender_total, x_label = "Year", 
                 y_label = "Total number")


## ---- fig.width=6-------------------------------------------------------------
## reshape the dataframe using the function `percent_df`.
## Add to `stacked_bar_chart` coord_flip() from ggplot2 to invert the xy axis.
# percent_df(data_df = df_gender)
percent_data <- percent_df(data_df = df_gender) 
stacked_bar_chart(percent_data, baseline_female = baseline_female,
                    x_label = "Year", y_label = "Percentage of authors",
                    baseline_label = "Female baseline 2016-2019:") +
  coord_flip() 
 


## -----------------------------------------------------------------------------
## calculate binomials for us and uk. 
## Reshape the dataframe and filter it country UK and US and year 2020 and count
## gender per countries.
# as.data.frame(t(with(authors_df, tapply(n, list(gender), c))))

UK_US_df <- reshape_for_binomials(data_df = authors_df %>%
                                   filter(country_code %in% c("UK", "US"),
                                          publication_years == 2020) %>%
                                    count(gender, country_code),
                                 gender_col = "gender", level = "country_code")

## To calculate the baseline for each country we can use the function `sapply`
baseline_uk_us <- sapply(UK_US_df$level, function(x) {
  baseline(data_df = authors_df %>%
            filter(country_code %in% x, publication_years %in% seq(2016, 2019)),
           gender_col = "gender")
})

baseline_uk_us

UK_US_binom <- calculate_binom_baseline(data_df = UK_US_df,
                                        baseline_female = baseline_uk_us)

UK_US_binom

## ---- fig.width=6, fig.height=4-----------------------------------------------
percent_uk_us <- percent_df(UK_US_binom)

bullet_chart <- bullet_chart(data_df = percent_uk_us,
                             baseline_female = baseline_uk_us,
                             x_label = "Countries", y_label = "% Authors",
                             baseline_label = "Female baseline for 2016-2019")
bullet_chart

## -----------------------------------------------------------------------------
## calculate binomials for US and UK

UK_df <- reshape_for_binomials(data_df = authors_df %>%
                                     filter(country_code == "UK") %>% 
                                     count(gender, publication_years),
                               "gender", "publication_years")

UK_df

## create a baseline vector containing values for each year from 2016 to 2020.
## using as country to compare France.
baseline_fr <- sapply(seq(2016, 2020), function(x) {
  baseline(data_df = authors_df %>%
             filter(country_code == "FR", publication_years %in% x), 
           gender_col = "gender")
})
baseline_fr

UK_binom <- calculate_binom_baseline(UK_df, baseline_female = baseline_fr)
UK_binom

## ---- fig.width=7, fig.height=5-----------------------------------------------
## Calculate the total number of submission per country and per year
percent_uk <- percent_df(UK_binom)
## calculate the number of submission from UK
total_uk <- authors_df %>%
  filter(country_code == "UK") %>%
  count(publication_years) %>%
  mutate(x_values = factor(publication_years,
                                    levels = publication_years))
## conversion factor to create the second y-axis
c <- min(total_uk$n) / 100
bullet_line_chart(data_df = percent_uk, baseline_female = baseline_fr,
                  x_label = "year", y_bullet_chart_label = "Authors submission (%)",
                  baseline_label = "French Female baseline",
                  line_chart_df = total_uk,
                  line_chart_scaling = c, y_line_chart_label = "Total number",
                  line_label = "Total submission UK")

