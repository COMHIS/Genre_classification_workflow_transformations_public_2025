# Setting the working directory and calling the relevant libraries

# Call relevant libraries
library(tidyverse)

#set the working path
setwd("~/Git_root/Genre_classification_workflow_transformations_2025")

##################################################

# For documentation: call sessionInfo() and save data about the latest run
writeLines(capture.output(sessionInfo()), paste0("data-work/sessionInfo","_of_the_post_evaluation_data_creation_run_of_",as.character(Sys.Date()),".txt"))

# Download a table to map main_categories to super categories
super_category_table <- read.csv("data/data-work/super_category_conversion_tables.csv",stringsAsFactors = FALSE)

# Download the genre category data
category_data <- read.csv("data/data-output/estc_categories.csv",stringsAsFactors = FALSE) 

# Polish the genre category data and update it with the supercategories.
category_data_updated <- left_join(category_data,super_category_table) %>% mutate(.,source=ifelse(is.na(source),"hand_annotated",source)) %>% mutate(.,sub_category=ifelse(is.na(sub_category),"",sub_category)) %>% distinct(estc_id,super_category,main_category,sub_category,source)

# Save the updated genre data.
write.csv(category_data_updated,"data/data-output/category_data_post_evaluation_fixed.tsv",row.names = FALSE)