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

# 4. Convert Datetime and Date_Posted columns into appropriate formats 

# 5. Figure out a way to identify possible hoax reports. Create a new boolean column "is_hoax", and populate this column with TRUE if the sighting is a possible hoax, FALSE if otherwise 

# 6. Create a table reporting the percentage of hoax sightings per country 

# 7. Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported 

# 8. Remove the rows where the sighting was reported before it happened 

# 9. Create a table reporting the average report_delay per country 

# 10. Check the data quality (missingness, format, range, etc) of the "duration seconds" column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments 

# 11. Create a histogram using the "duration seconds" column