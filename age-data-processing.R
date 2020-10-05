## This script calculates some summary statistics of the age distribution in
## census tracts and writes the result to a CSV.
##
library(dplyr)
library(reshape2)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(sf)



#' Return a numeric approximation of the age group based on the middle of the
#' interval and truncating ages to 79.
#'
#' @param age_groups a character vector
#'
age_group_as_numeric <- function(age_groups) {
  .f <- function(age_group) {
    switch(age_group,
           idade_0a9 = 4.5,
           idade_10a14 = 12,
           idade_15a19 = 17,
           idade_20a29 = 24.5,
           idade_30a39 = 34.5,
           idade_40a49 = 44.5,
           idade_50a59 = 54.5,
           idade_60a69 = 64.5,
           idade_70 = 74.5)
  }

  if (length(age_groups) == 1) {
    .f(age_groups)
  } else {
    sapply(age_groups, .f)
  }
}

#' Return a numeric vector which has been normalised as a probability
#' distribution.
#'
#' @param xs a numeric vector with non-negative values.
#'
normalise <- function(xs) {
  xs / sum(xs)
}

main <- function() {
  ## Read in the data and select the relevant columns because otherwise we end up
  ## with a bunch of other columns we do not care about for this step of the
  ## process.
  age_data_wide <- readRDS("covariates_sp_21sept.rds") %>%
    select(code_tract,
           pop_total,
           starts_with("idade"))
  
  ## Little check to help understanding the way in which data may be missing to
  ## make it easier to work with.
  stopifnot(age_data_wide %>% filter(not(is.na(idade_0a9))) %>% is.na %>% any %>% not)

  ## Convert the age data to a long format to make it easier to work with. We
  ## make the age group an ordered factor so that it is easier to work with
  ## these later.
  age_data_long <- age_data_wide %>%
    filter(not(is.na(idade_0a9)),
           pop_total > 0) %>%
    melt(id.vars = c("code_tract", "pop_total"),
         value.name = "num_people",
         variable.name = "age_group") %>%
    mutate(age_group = factor(age_group, ordered = TRUE))

  ## Little check to see how much of the data is incomplete so we can understand
  ## if the following steps are reasonable.
  tmp <- age_data_long %>%
    group_by(code_tract) %>%
    summarise(sum_num_people = sum(num_people),
              unique_pop_total = unique(pop_total))
  cat("Check how often the age distribution consists of the total population\n")
  print(table(tmp$sum_num_people == tmp$unique_pop_total))
  rm(tmp)


  ## Make a plot of the total age distribution
  plot_df_1 <- age_data_long %>%
    group_by(age_group) %>%
    summarise(total_people = sum(num_people))
  fig_1 <- ggplot(plot_df_1) +
    geom_col(mapping = aes(x = age_group, y = total_people))
  ggsave("age-data-fig-1.png", fig_1)

  ## Compute the mean and mode age in each tract as potential covariates for
  ## subsequent analysis. We also compute the proportion of the census tracts'
  ## population that is at least 60 years of age.
  partial_result <- age_data_long %>%
    mutate(age_group_val = age_group_as_numeric(as.character(age_group))) %>%
    group_by(code_tract) %>%
    summarise(mean_age = age_group_val %*% normalise(num_people),
              mode_age = age_group[which.max(num_people)],
              prop_geq_60 = sum(num_people[age_group >= "idade_60a69"]) / unique(pop_total))

  ## Make a plot to look at the correlation between the summary statistics to
  ## check that it makes sense.
  fig_2 <- ggplot(partial_result) +
    geom_point(mapping = aes(x = mode_age, y = mean_age))
  ggsave("age-data-fig-2.png", fig_2)

  ## Calculate some central values to use in the situation where there is
  ## missing data as an imputed value so that we can return a value for every
  ## tract. Note that we can take the mean here because the partial result does
  ## not contain the values from any census tract where there is missing data.
  imputed_mean <- mean(partial_result$mean_age)
  mode_tbl <- table(partial_result$mode_age)
  imputed_mode <- names(mode_tbl)[which.max(mode_tbl)]
  imputed_prop_geq_60 <- mean(partial_result$prop_geq_60)

  ## Construct the final data frame with all the census tracts with the missing
  ## values replaced by the imputed values so that this is easy to use in
  ## subsequent analyses.
  result <- left_join(x = select(age_data_wide, code_tract),
                      y = partial_result,
                      by = "code_tract")
  result$imputed <- is.na(result$mean_age)
  result[result$imputed, "mean_age"] <- imputed_mean
  result[result$imputed, "mode_age"] <- imputed_mode
  result[result$imputed, "prop_geq_60"] <- imputed_prop_geq_60

  ## Write the results to CSV so they are easy to use in the future.
  write.table(x = result,
              file = "age-data.csv",
              sep = ",",
              row.names = FALSE)
}

if (!interactive()) {
  main()
}
