# Assignment 4 - Martians are Coming!

# 1. Read the data into a data frame (make sure that column names do not have spaces in them)

library(readr)
ufo <- read_csv("~/Desktop/ufo_subset.csv") # This line will work if the csv is saved on your desktop
colnames(ufo) <- gsub (" ", "_", colnames(ufo)) # Remove spaces from column names 
View(ufo)
data.frame(ufo)

    # Don't forget to visually inspect and compare your data frame to the original csv 
          # DONE 

# 2. Find the rows where Shape information is missing and impute with "unknown"

ufo$shape <- ifelse(is.na(ufo$shape), "unknown", ufo$shape) # Replace NA values in the shape column with "unknown"
View(ufo) # Updated datset 

# 3. Remove the rows that do not have Country information

ufo <- ufo[!is.na(ufo$country), ] # Remove rows with missing country information
View(ufo) # Updated dataset 

# 4. Convert Datetime and Date_Posted columns into appropriate formats 

ufo$datetime <- as.POSIXct(ufo$datetime, format = "%Y-%m-%d %H:%M") # Convert Datetime column to the appropriate format (as.POSIXct)
ufo$date_posted <- as.Date(ufo$date_posted, format = "%d-%m-%Y") # Convert Date_posted column to the appropriate format (as.Date)
View(ufo) # Updated dataset 

# 5. Figure out a way to identify possible hoax reports. Create a new boolean column "is_hoax", and populate this column with TRUE if the sighting is a possible hoax, FALSE if otherwise 

# Initialize the is_hoax column as FALSE for all rows 
ufo$is_hoax <- FALSE 

for (i in 1:nrow(ufo)) {
  if (!is.na(ufo$comments[i]) && # Check if the column is not NA and contains the words HOAX, Hoax, or hoax 
      (grepl("HOAX", ufo$comments[i], ignore.case = TRUE) ||
       grepl("Hoax", ufo$comments[i], ignore.case = TRUE) ||
       grepl("hoax", ufo$comments[i], ignore.case = TRUE))) {
    ufo$is_hoax[i] <- TRUE # Set the value to TRUE if these words are found within the comments
  }
}

View(ufo) # Updated dataset

# 6. Create a table reporting the percentage of hoax sightings per country 

hoax_counts <- aggregate(is_hoax ~ country, data = ufo, FUN = sum) # Calculate the number of hoax sightings per country
total_counts <- table(ufo$country) # Calculate the total number of sightings per country
percentage_hoax <- round(hoax_counts$is_hoax / total_counts * 100, 2) # Calculate the percentage of hoax sightings per country and round to the nearest hundredth

# Create the summary table
summary_table <- data.frame(
  country = names(total_counts),
  hoax_sightings = hoax_counts$is_hoax,
  total_sightings = as.numeric(total_counts),
  percentage_hoax = percentage_hoax
)

# Remove the percentage_hoax.Var1 column 
summary_table$percentage_hoax.Var1 <- NULL 

# Print the table
print(summary_table)

# 7. Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported 

# Convert columns to POSIXIt type 
ufo$datetime <- as.POSIXlt(ufo$datetime)
ufo$date_posted <- as.POSIXlt(ufo$date_posted)

# Calculate the report delay in days and round to the nearest day - this helps especially for rows where the dateposted and datetime are the same to account for the additional time values (i.e. hours, mins, secs) found within the datetime column
ufo$report_delay <- round(as.numeric(difftime(ufo$date_posted, ufo$datetime, units = "days")))

View(ufo) # Updated dataset

# 8. Remove the rows where the sighting was reported before it happened 

ufo <- ufo[ufo$report_delay >= 0 | as.Date(ufo$date_posted) == as.Date(ufo$datetime), ] # Keeps all the rows where the report delay is greater than or equal to 0 and all row where the dates of date_posted and datetime are the same 
rownames(ufo) <- NULL # Reset row index after removing rows 
View(ufo) # Updated dataset 

# 9. Create a table reporting the average report_delay per country 

avg_report_delay <- aggregate(report_delay ~ country, data = ufo, FUN = mean) # Calculate the average report_delay per country
avg_report_delay$report_delay <- round(avg_report_delay$report_delay) # Round the average report_delay to the nearest day for continuity 

# Print the table
print(avg_report_delay)

# 10. Check the data quality (missingness, format, range, etc) of the "duration seconds" column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments 

# Missingness - 1 NA value 
missing_values <- sum(is.na(ufo$duration_seconds)) # Displays that only 1 row contains a missing value 
missing_values
rows_missing <- which(is.na(ufo$duration_seconds)) # Indicates which row that is (row 8937) - there is no information found within this row therefore it will be removed from the dataset 
rows_missing

    # Remove row 
ufo <- ufo[!is.na(ufo$duration_seconds), ] # Removing row with NA value 
rownames(ufo) <- NULL # Reseting row index 
View(ufo) # Updated dataset 

# Format - numbers are not rounded to the same number of decimal points within the column
ufo$duration_seconds <- as.numeric(ufo$duration_seconds) # Convert "duration_seconds" column to numeric
ufo$duration_seconds <- sprintf("%.2f", ufo$duration_seconds) # Format numbers in duration_seconds to two decimal places
View(ufo) # Updated dataset 

# Range - does not provide the true minimum and maximum values 
min_duration <- min(ufo$duration_seconds)
max_duration <- max(ufo$duration_seconds)

    # Clean the duration_seconds column
ufo$duration_seconds <- gsub("[^0-9.]", "", ufo$duration_seconds)
ufo$duration_seconds <- as.numeric(ufo$duration_seconds)
min_duration_2 <- min(ufo$duration_seconds)
min_duration_2
max_duration_2 <- max(ufo$duration_seconds)
max_duration_2

    # Actual Range 
print(paste("The Range for duration_seconds is", min_duration_2, "-", max_duration_2, "seconds"))

# 11. Create a histogram using the "duration seconds" column