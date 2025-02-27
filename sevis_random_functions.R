generate_random_bdate <- function(start_date = as.Date("1970-09-01"), end_date =  as.Date("2005-09-01"), separation_days = 365) {
  # Calculate the number of days between the start and end dates
  total_days <- as.integer(end_date - start_date)
  
  # Calculate the maximum number of intervals that can fit within the total number of days
  max_intervals <- floor(total_days / separation_days)
  
  # Generate random intervals between 0 and the maximum number of intervals
  intervals <- sample(0:max_intervals, 1, replace = FALSE)
  
  # Calculate the dates by adding the random intervals to the start date
  random_dates <- start_date + intervals * separation_days
  
  return(random_dates)
}

generate_random_bdate <- function() {
  sample(seq(as.Date('1970-09-01'), as.Date('2005-09-01'), by="day"), 1)
}

generate_random_pastdate <- function(start_date = as.Date("1970-09-01"), end_date =  today(), separation_days = 365) {
  # Calculate the number of days between the start and end dates
  total_days <- as.integer(end_date - start_date)
  
  # Calculate the maximum number of intervals that can fit within the total number of days
  max_intervals <- floor(total_days / separation_days)
  
  # Generate random intervals between 0 and the maximum number of intervals
  intervals <- sample(0:max_intervals, 1, replace = FALSE)
  
  # Calculate the dates by adding the random intervals to the start date
  random_dates <- start_date + intervals * separation_days
  
  return(random_dates)
}

generate_random_futuredate <- function(start_date = today(), end_date =  as.Date(today()+1825), separation_days = 365) {
  # Calculate the number of days between the start and end dates
  total_days <- as.integer(end_date - start_date)
  
  # Calculate the maximum number of intervals that can fit within the total number of days
  max_intervals <- floor(total_days / separation_days)
  
  # Generate random intervals between 0 and the maximum number of intervals
  intervals <- sample(0:max_intervals, 1, replace = FALSE)
  
  # Calculate the dates by adding the random intervals to the start date
  random_dates <- start_date + intervals * separation_days
  
  return(random_dates)
}

# MAIN SCRIPT EXECUTION

start_date <- as.Date("1970-09-01")  # Example start date
end_date <- as.Date("2005-09-01")    # Example end date
separation_days <- 365                # Example minimum separation in days

random_dates <- generate_random_bdate(start_date, end_date, separation_days)

generate_random_bdate()
generate_random_pastdate()
generate_random_futuredate()


cat("Randomly generated dates:\n")
print(random_dates)


generateRandomDate <- function(start_date, end_date) {
  random_days <- sample(0:(as.numeric(end_date - start_date)), 1)
  start_date + days(random_days)
}

generateRandomDate(start_date = start_date, end_date = end_date)


sevis_data_download_date_columns <- sevis_data_download |> 
  select(contains("Date")) |> 
  clean_names() |> 
  group_by(row_number()) |> 
  mutate(status_change_date = generate_random_bdate(),
         date_of_birth = generate_random_pastdate(),
         passport_expiration_date = generate_random_futuredate(),
         date_of_last_event = generate_random_pastdate())


#### The SEVIS data was messy and ended up coming in as lists from the randomly generator info. The codes below flatten the list ####
# sevis_data_download <- tibble(googlesheets4::read_sheet('1SwpHoNYX2ZC7vYgbMJJqI5ByEqPfdO4lsg7Wcfom4Y0', sheet = 'Sheet1'))
# sevis_country_codes <- googlesheets4::read_sheet('1SwpHoNYX2ZC7vYgbMJJqI5ByEqPfdO4lsg7Wcfom4Y0', sheet = 'country_codes')
# 
# sevis_data_download$`SEVIS Legacy First Name` <- vapply(sevis_data_download$`SEVIS Legacy First Name` , paste, collapse = ", ", character(1L))
# sevis_data_download$`Foreign Postal Code` <- vapply(sevis_data_download$`Foreign Postal Code` , paste, collapse = ", ", character(1L))
# sevis_data_download$`Passport Number` <- vapply(sevis_data_download$`Passport Number`, paste, collapse = ", ", character(1L))
# sevis_data_download$`Visa Number` <- vapply(sevis_data_download$`Visa Number`, paste, collapse = ", ", character(1L))
# sevis_data_download$`I-94 Admission Number` <- vapply(sevis_data_download$`I-94 Admission Number`, paste, collapse = ", ", character(1L))
# sevis_data_download$`CPT Approved` <- vapply(sevis_data_download$`CPT Approved`, paste, collapse = ", ", character(1L))


#### Wrote the parqute files for faster loading after the sevis df was flattened ####
# write_parquet(sevis_data_download, here("data/sevis_data_download.parquet"))
# write_parquet(sevis_country_codes, here("data/sevis_country_codes.parquet"))

