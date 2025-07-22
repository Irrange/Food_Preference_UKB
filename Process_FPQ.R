data_dir <- "./data/"

# Extract Food Preference Questionnaire (FPQ) data
food_pre_raw <- fread(file.path(data_dir, "ukb_food_preference_raw.csv"))

# FPQ Questionnaire Preprocessing ----

## Remove participants who didn't complete the questionnaire
food_pre_raw1 <- food_pre_raw %>% filter(!is.na(`20750-0.0`))

# Save and load intermediate data in CSV format
fwrite(food_pre_raw1, file.path(data_dir, "food_pre_raw1.csv"))
food_pre_raw1 <- fread(file.path(data_dir, "food_pre_raw1.csv"))

# Create food item information vector
food_info <- fread(file.path(data_dir, "food_group_information.csv"))

# Remove non-dietary questions
food_pre_raw2 <- food_pre_raw1 %>%
  dplyr::select(-c(`20614-0.0`, `20641-0.0`, `20656-0.0`, `20657-0.0`, `20668-0.0`,
                   `20669-0.0`, `20670-0.0`, `20733-0.0`, `20741-0.0`, `20749-0.0`))

# Calculate proportion of "prefer not to answer" and "never tried" responses per participant
target_columns <- names(food_pre_raw2)[-c(1:3)]
food_pre_raw2[, exclude_percentage := apply(.SD, 1, function(row) 
  round(mean(row %in% c(-121, -818)), 2)), .SDcols = target_columns]

# Filter out participants with >25% excluded responses
food_pre_raw3 <- food_pre_raw2[exclude_percentage < 0.25]
food_pre_raw3 <- na.omit(food_pre_raw3)

# Recode response values
food_pre_raw3[, (target_columns) := lapply(.SD, function(x) 
  fifelse(x %in% c(-121, -818), 0, x)), .SDcols = target_columns]

# Remove identifier columns and exclusion percentage
food_pre_raw4 <- food_pre_raw3[, -c(1:3, 144)]
food_pre_raw4 <- na.omit(food_pre_raw4)

# Z-score standardization
food_pre_raw5 <- food_pre_raw4 %>%
  na.omit() %>%
  mutate_all(list(scale))

# Save and load processed data as CSV
fwrite(food_pre_raw5, file.path(data_dir, "food_pre_raw5.csv"))
food_pre_raw5 <- fread(file.path(data_dir, "food_pre_raw5.csv"))
