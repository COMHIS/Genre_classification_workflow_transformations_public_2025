# NOTE: This script does not try to exactly replicate how the final sets of keywords or ngrams were obtained, but to demonstrate
# how most of them were acquired by analysing the concentration of these elements to specific main categories
# in pre-existing (e.g. manually annotated) data sets. In practice, these analyses were performed many times and iteratively as the 
# amount of available data to study correlations between main categories and ngrams/keywords grew.

# Set working directory to folder containing text files
setwd("~/Git_root/Genre_classification_workflow_transformations_2025")

# Load the necessary package
library(tidyverse)
library(eccor)
library(estcr)
library(magrittr)

# Create a sessioninfo text filw
writeLines(capture.output(sessionInfo()), paste0("data/data-work/sessionInfo","_of_the_exploratory_tools_run_of_",as.character(Sys.Date()),".txt"))

##########################################################

# The following lines demonstrate how the approach described in the Appendix of the article was used to explore for keywords
# and ngrams that could be useful in the article. The resulting demo table is in the data-output folder.

# Download manually annotated documents.

manually_annotated_documents <- read.csv("data/data-work/all_manually_annotated_documents_final_expanded.csv",stringsAsFactors = FALSE)

# Download a data set of ESTC keywords.

estc_topics_og <- read.csv("data/data-work//estc_prominent_topics.csv",stringsAsFactors = FALSE)

# Compute the number of times different main categories occur in the manually annotated data

manually_annotated_data_main_category_counts <- manually_annotated_documents %>% count(main_category,name="n_category_total") %>% filter(main_category!="unknown" & main_category!="") %>% mutate(.,n_total_all_categories=sum(n_category_total)) %>% mutate(.,prop_of_category=n_category_total/n_total_all_categories)

# Aggregate counts of the keywords in the entire ESTC.

estc_topics_og_count <- estc_topics_og %>% count(topic,name="n_topic") %>% arrange(-n_topic)

# Aggregate counts of the keywords in the set of manually annotated documents.

estc_topics_og_count_in_manually_annotated <- estc_topics_og %>% inner_join((manually_annotated_documents %>% distinct(estc_id))) %>% count(topic,name="n_topic") %>% arrange(-n_topic)

# Also create a data set of keywords in the manually annotated documents that is not aggregated.

estc_topics_og_count_manually_annotated_unaggregated <- estc_topics_og %>% inner_join((manually_annotated_documents %>% distinct(estc_id))) 

# Now we can compute how disproportionately different keywords are concentrated to specific main categories in the set of manually annotated documents. 
# The filtering steps at the last piping steps only leave us such keywords that are quite frequent and highly concentrated to specific main categories
# in the manually annotated data.

keywords_per_main_category <- manually_annotated_documents %>% distinct(estc_id,main_category) %>% left_join(estc_topics_og_count_manually_annotated_unaggregated) %>% filter(main_category!="" & main_category!="unknown" & main_category!="ephemera" & main_category!="letter") %>% distinct(estc_id,topic,main_category) %>% group_by(main_category,topic) %>% summarise(.,n_topic_per_category=length(estc_id)) %>% left_join(estc_topics_og_count_in_manually_annotated) %>% left_join(manually_annotated_data_main_category_counts) %>% mutate(.,prop_of_topic_in_category=n_topic_per_category/n_topic) %>% mutate(.,topic_category_ratio=prop_of_topic_in_category/prop_of_category) %>% filter(prop_of_topic_in_category>=0.5 & n_topic_per_category>=10)  %>% arrange(-n_topic_per_category)

# Create a subset keywords that belong the set of the 'highly concentrated' keywords but only includes the most common ones.

keywords_per_main_category_subset_of_prominent_ones <- keywords_per_main_category %>% filter(n_topic>=300) %>% arrange(-n_topic)
write.csv(keywords_per_main_category_subset_of_prominent_ones,"data/data-output/category_correlated_keyword_demo_data_set.csv",row.names = FALSE)

# Although the example was implemented with keywords, there is no conceptual difference to how similar analysis was conducted with
# ngrams.



