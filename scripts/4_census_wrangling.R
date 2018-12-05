# 4_census_wrangling.R
# Working over census variables to reduce dimensionality

# Load dataset
hexGrid <- readRDS("./data/hexGrid.rds")

# Working over age variables
# First, deleting non adults variables
todrop <- names(hexGrid) %in% c("d0a7", "d7a17") 
hexGridNew <- hexGrid[!todrop]
# Creating young adults variable
hexGridNew$young_adults <- hexGridNew$d18a29 + hexGridNew$d30a39
# Creating rest of adults variable
hexGridNew$rest_adults <- hexGridNew$d40a49 + hexGridNew$d50a59 + hexGridNew$d60a69 + hexGridNew$d70a79 + hexGridNew$d80a89 + hexGridNew$d90a_
# Deleting original age variables
todrop2 <- names(hexGridNew) %in% c("d18a29", "d30a39", "d40a49", "d50a59", "d60a69", "d70a79", "d80a89", "d90a_") 
hexGridNew <- hexGridNew[!todrop2]

# Participation: create a variable showing difference between 2nd and 1st round
hexGridNew$diff_part <- hexGridNew$part2 - hexGridNew$part

# Women/Men ratio:
hexGridNew$women_ratio <- hexGridNew$tot_muj / hexGridNew$tot_hom

