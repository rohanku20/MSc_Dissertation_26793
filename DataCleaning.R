library(openxlsx)
library(tidyr)
library(dplyr)

coaches <- read.xlsx("coaches.xlsx") # reading file

# diagnostics
na_count <- sum(is.na(coaches$staff_text))
na_values <- coaches[is.na(coaches$staff_text), c("year", "org")]
wrong_values <- c(157, 189, 197, 210, 217, 245, 357, 360, 464, 487, 502, 523, 
                  526, 535, 545, 547, 551, 563)

# basic cleaning - split on empty space, clean extra lines, remove empty
cleaner <- function(input) {
  split_string <- strsplit(input, "\n")[[1]]
  cleaned_string <- gsub("\n", "", split_string)
  cleaned_string <- cleaned_string[cleaned_string != ""]
  return(cleaned_string)
}

cleaned_staff_text <- lapply(coaches$staff_text, function(x) cleaner(x))
coaches$cleaned_staff_text <- cleaned_staff_text
coaches$test <- coaches$cleaned_staff_text

# remove headings - leaving only title
headings <- c("Offensive coaches", "Front office", "Defensive coaches", "Special teams coaches", "Head coaches")
remove_headings <- function(x) {
  for (split_str in headings) {
    x <- unlist(strsplit(x, split_str))
  }
  return(x)
}
coaches$test <- lapply(coaches$test, remove_headings)

# drop old columns
cols_to_drop <- c("staff_text", "cleaned_staff_text") 
coaches <- coaches[, !names(coaches) %in% cols_to_drop]

# rename columns for clarity
old_names <- c("year", "org", "test")
new_names <- c("season", "franchise", "staff")
colnames(coaches)[colnames(coaches) %in% old_names] <- new_names

# Explode the staff list column
coaches_exploded <- tidyr::unnest(coaches, staff)
coaches_split <- separate(coaches_exploded, staff, into = c("title", "name"), sep = " – ")

# Drop rows where the title column contains a number
coaches_filtered <- coaches_split %>%
  filter(!grepl("\\d", title))

write.xlsx(coaches_filtered, "cleaned_data.xlsx")

na_count_FINAL <- sum(is.na(coaches_filtered$name))
na_values <- coaches_filtered[is.na(coaches_filtered$title), c("season", "franchise")]

manual <- read.xlsx("Manual Wiki Data.xlsx")
wikiAPI <- read.xlsx("cleaned_data.xlsx")

# adding/binding data
manual <- subset(manual, select = -X5)
coaches_full <- rbind(wikiAPI, manual)
coaches_full <- coaches_full[complete.cases(coaches_full$name), ]

#########################################################################

# splitting dataset into year-by-year frames
coach_total <- read.xlsx("coaches_full_final.xlsx") # further cleaned excel file
coach_total <- na.omit(coach_total)

coach_total <- arrange(coach_total, name, season)

coach_total <- coach_total %>%
  arrange(name, season) %>%
  group_by(name) %>%
  mutate(promotion = ifelse((grepl("coordinator$", title, ignore.case = TRUE) & 
                               lag(!grepl("coordinator$", title, ignore.case = TRUE), default = FALSE)) |
                              (grepl("head coach", title, ignore.case = TRUE) & 
                                 lag(!grepl("head coach", title, ignore.case = TRUE), default = FALSE)), 
                            1, 0)) %>%
  ungroup()

unique_seasons <- unique(coach_total$season)

# splitting data into years
for (year in unique_seasons) {
  assign(paste0("coach_", year), subset(coach_total, season == year))
}

#######################################################################
years <- 2013:2023

# Function to split name column into first_name and last_name
split_names <- function(df) {
  df %>% separate(name, into = c("first_name", "last_name"), sep = " ", extra = "merge")
}

# Loop through each year and process the corresponding dataset
for (year in years) {
  # Dynamically get the dataset
  dataset_name <- paste0("coach_", year)
  dataset <- get(dataset_name)
  dataset <- na.omit(dataset)
  
  # Split the name column
  dataset <- split_names(dataset)
  
  # Assign the modified dataset back to the original variable name
  assign(dataset_name, dataset)
}

#################################################################### ethnicity

library(rethnicity)

add_predicted_ethnicity <- function(df, first_name_col, last_name_col) {
  df <- df %>%
    rowwise() %>%
    mutate(predicted_ethnicity = predict_ethnicity(!!sym(first_name_col), !!sym(last_name_col))[[1, 7]])
  return(df)
}

datasets <- paste0("coach_", 2013:2023)

# Loop through each dataset and apply add_predicted_ethnicity function
for (dataset in datasets) {
  # Read the dataset into a variable
  df <- get(dataset)
  df <- na.omit(df)

  # Add predicted_ethnicity column
  df <- add_predicted_ethnicity(df, "first_name", "last_name")

  # Print or save the updated dataset
  assign(dataset, df)  # Update the dataset in the global environment
  print(paste("Updated", dataset))
}

coach_total <- bind_rows(coach_2013, coach_2014, coach_2015, coach_2016, 
                             coach_2017, coach_2018, coach_2019, coach_2020, 
                             coach_2021, coach_2022, coach_2023)

write.xlsx(coach_total, "promotion_race.xlsx")