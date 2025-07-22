# Exploratory Factor Analysis ----
# KMO Test
kmo_result <- KMO(food_pre_raw5)
if (kmo_result$MSA < 0.70) {
  stop("KMO statistic is below 0.70; sample insufficient for factor analysis")
}

# Perform factor analysis
efa_result <- fa(food_pre_raw5, nfactors = 19, rotate = "oblimin")
# Save and load EFA results as RData
save(efa_result, file = file.path(data_dir, "efa_result.RData"))
load(file.path(data_dir, "efa_result.RData"))

# View factor analysis results
print(efa_result)

ssload <- efa_result[["Vaccounted"]] %>% data.frame()
fload <- efa_result[["loadings"]][1:140, ] %>% cbind(field_df)
fload <- fread(file.path(data_dir, "floadings_with_description.csv"))
# Ensure fload has MR columns and Description column
if (!"Description" %in% names(fload)) {
  fload$Description <- paste0("Item_", 1:nrow(fload))
}

fload_results <- data.frame(MR = character(0), Description = character(0), Condition = character(0))

for (col_idx in 1:19) {
  col_name <- paste0("MR", col_idx)
  if (col_name %in% names(fload)) {
    values <- fload[[col_name]]
    
    descriptions_gt_02 <- fload$Description[values > 0.2]
    descriptions_lt_minus_02 <- fload$Description[values < -0.2]
    
    if (length(descriptions_gt_02) > 0) {
      greater_than_0_2 <- data.frame(
        MR = rep(col_name, length(descriptions_gt_02)),
        Description = descriptions_gt_02,
        Condition = "GreaterThan_0_2"
      )
      fload_results <- rbind(fload_results, greater_than_0_2)
    }
    
    if (length(descriptions_lt_minus_02) > 0) {
      less_than_minus_02 <- data.frame(
        MR = rep(col_name, length(descriptions_lt_minus_02)),
        Description = descriptions_lt_minus_02,
        Condition = "LessThan_minus_0_2"
      )
      fload_results <- rbind(fload_results, less_than_minus_02)
    }
  } else {
    message(paste("Warning: Factor column", col_name, "not found in fload."))
  }
}
