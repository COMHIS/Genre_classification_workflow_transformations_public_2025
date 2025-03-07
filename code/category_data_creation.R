# For replication purposes, the state of R when the script was run the last time has been pasted to the beginning of this script.

# Info about last time was run with sessionInfo()
# R version 4.4.1 (2024-06-14)
# Platform: x86_64-pc-linux-gnu
# Running under: Ubuntu 20.04.6 LTS

# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
# LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3;  LAPACK version 3.9.0

# locale:
#  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8   
# [6] LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

# time zone: Europe/Helsinki
# tzcode source: system (glibc)

# attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#  [1] eccor_1.0.1     tidytext_0.4.2  httr_1.4.7      RCurl_1.98-1.14 estcr_1.5.3     jsonlite_1.8.8  lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1  
# [10] dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0 quanteda_4.0.2 

#loaded via a namespace (and not attached):
#  [1] janeaustenr_1.0.0 utf8_1.2.4        generics_0.1.3    bitops_1.0-7      stringi_1.8.3     lattice_0.22-5    hms_1.1.3         magrittr_2.0.3   
#[9] grid_4.4.1        timechange_0.3.0  pkgload_1.3.4     Matrix_1.6-5      stopwords_2.3     fansi_1.0.6       scales_1.3.0      cli_3.6.2        
#[17] crayon_1.5.2      rlang_1.1.3       tokenizers_0.3.0  bit64_4.0.5       munsell_0.5.1     withr_3.0.0       tools_4.4.1       tzdb_0.4.0       
#[25] colorspace_2.1-0  fastmatch_1.1-4   assertthat_0.2.1  vctrs_0.6.5       R6_2.5.1          lifecycle_1.0.4   bit_4.0.5         arrow_15.0.1     
#[33] pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.5      glue_1.7.0        Rcpp_1.0.12       tidyselect_1.2.1  rstudioapi_0.16.0 farver_2.1.1     
#[41] SnowballC_0.7.1   labeling_0.4.3    compiler_4.4.1 

##################################################################

# Set the working directory, load relevant libraries and define script-specific functions

# Set working directory to the right folder
setwd("~/Git_root/Genre_classification_workflow_transformations_2025")

# Load the necessary package
library(tidyverse)
library(eccor)
library(estcr)
library(tidytext)
library(magrittr)

# Define function to name data a data frame X with a vector
name_data_frame <- function(X,name_vec){
  
  colnames(X) <- name_vec
  return(X)
}


# Define a function creates ngrams (where the exact n desired is the input for the function) for all ESTC titles
title_ngram_to_category_mapping <- function(n=2){
  
  frequent_ngrams_by_record <- estc_title %>% unnest_ngrams(.,n=n,input=full_title,output=ngram) %>% distinct(estc_id,ngram) 

  return(frequent_ngrams_by_record)
}

# Define a function takes as its input a vector of subcategories and returns a polished (e.g. duplicates removed) string in which the values 
# have been compressed to a single cell and are separated by ;.
clean_sub_categories <- function(sub_category){
  
  sub_category <- sub_category %>% str_split(.,";") %>% unlist(.)
  sub_category_cleaned <- sub_category[sub_category!="" & !is.na(sub_category)] %>% unique(.)
  sub_category_cleaned_compressed <- paste(sub_category_cleaned,collapse = ";")
  
  return(sub_category_cleaned_compressed)
  
}

##################################################################

# Classification workflow starts from here

# Download manually annotated documents and periodicals (steps 1 and 2).

manually_annotated_documents <- read.csv("data/data-work/manually_annotated_documents_and_periodicals.csv",stringsAsFactors = FALSE)

# Download a data set of ESTC keywords used in step 3.
estc_topics_og <- read.csv("data/data-work/estc_prominent_topics.csv",stringsAsFactors = FALSE)

# Download title data used in step 4.
estc_title <- read.csv("data/data-work/estc_full_title.csv",stringsAsFactors = FALSE)

# Aggregate counts of keywords and save the result into a table. 

estc_topics_og_count <- estc_topics_og %>% count(topic,name="n_topic") %>% arrange(-n_topic)

# Download a table of keywords that have been manually elected.

manually_selected_topics <- read.csv("data/data-work/manually_selected_topics.csv",stringsAsFactors = FALSE)

# Create a table with the keyword information from the general table but only for the manually selected keywords

estc_topics_og_count_all <- estc_topics_og %>% inner_join(manually_selected_topics)

# Calculate the prominence score for all keywords

estc_topics_og_count_topic_scores <- estc_topics_og_count_all %>% count(topic,name="topic_count") %>% mutate(,topic_score=log(topic_count))

# Use the prominence scores of the manually selected keywords (the name of the file estc_topics_og_count_all is a bit misleading, as this is only
# the manually selected keywords) to select main categories and subcategories ESTC records (step 3 of the workflow)

estc_topics_og_count_all_to_category <- estc_topics_og_count_all %>% left_join(estc_topics_og_count_topic_scores) %>% group_by(estc_id,main_category) %>% summarise(,topic_score=sum(topic_score),sub_category=clean_sub_categories(sub_category)) %>% as.data.frame(.) %>% group_by(estc_id) %>% summarise(.,main_category=main_category[which.max(topic_score)],sub_category=clean_sub_categories(sub_category),topic_score=max(topic_score)) %>% left_join(.,(load_estc_core() %>% distinct(estc_id,work_id))) %>% group_by(work_id,main_category) %>% summarise(,topic_score=sum(topic_score),sub_category=clean_sub_categories(sub_category)) %>% as.data.frame(.) %>% group_by(work_id) %>% summarise(.,main_category=main_category[which.max(topic_score)],sub_category=clean_sub_categories(sub_category),topic_score=max(topic_score)) %>% as.data.frame(.) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% distinct(estc_id,main_category,sub_category) %>% mutate(.,source="estc_topics")

# Combine the labels obtained with keywords to the results of steps 1 and 2 (so 1,2, and 3 are now together)

all_annotated_with_hand_and_topic <- manually_annotated_documents %>% distinct(estc_id,main_category,sub_category) %>% full_join((estc_topics_og_count_all_to_category %>% anti_join((manually_annotated_documents %>% distinct(estc_id)))))

# Download the manually selected ngrams of step 4.

manually_selected_ngrams_1 <- read.csv("data/data-work/category_related_ngrams_1.csv",stringsAsFactors = FALSE) %>% filter(keep=="1") %>% distinct(ngram,main_category,sub_category)
manually_selected_ngrams_2 <- read.csv("data/data-work/category_related_ngrams_2.csv",stringsAsFactors = FALSE) %>% filter(main_category!="") %>% distinct(ngram,main_category,sub_category)
manually_selected_ngrams_3 <- read.csv("data/data-work/category_related_ngrams_3.csv",stringsAsFactors = FALSE) %>% filter(main_category!="") %>% distinct(ngram,main_category,sub_category)

# Bring the manually selected ngrams to a single table.

manually_selected_ngrams <- manually_selected_ngrams_1 %>% full_join((manually_selected_ngrams_2 %>% anti_join((manually_selected_ngrams_1 %>% distinct(ngram)))))
manually_selected_ngrams <- manually_selected_ngrams %>% full_join((manually_selected_ngrams_3 %>% anti_join((manually_selected_ngrams %>% distinct(ngram)))))

# Create data sets of frequently appearing ngrams (monograms to pentagrams) in the titles of ESTC records
# Each row is of the format ESTC-id - title ngram.

title_monograms <-  title_ngram_to_category_mapping(n=1) %>% inner_join(manually_selected_ngrams)
title_bigrams <-  title_ngram_to_category_mapping(n=2) %>% inner_join(manually_selected_ngrams)
title_trigrams <- title_ngram_to_category_mapping(n=3) %>% inner_join(manually_selected_ngrams)
title_quartograms <-title_ngram_to_category_mapping(n=4) %>% inner_join(manually_selected_ngrams)
title_pentagrams <- title_ngram_to_category_mapping(n=5) %>% inner_join(manually_selected_ngrams)

# Bring ngrams of varying lengths to a single table (only manually selected ones)

title_ngrams <- rbind.data.frame(title_monograms,title_bigrams,title_trigrams,title_quartograms,title_pentagrams)

# Calculate prominence scores for the manually selected ngrams

title_ngrams_ngram_counts <- title_ngrams %>% count(ngram,name="ngram_count") %>% mutate(,ngram_score=log(ngram_count))

# Use the ngrams and the prominence score to label ESTC records (step 4).

estc_ngram_count_all_to_category <- title_ngrams %>% left_join(title_ngrams_ngram_counts) %>% group_by(estc_id,main_category) %>% summarise(,ngram_score=sum(ngram_score),sub_category=clean_sub_categories(sub_category)) %>% as.data.frame(.) %>% group_by(estc_id) %>% summarise(.,main_category=main_category[which.max(ngram_score)],sub_category=clean_sub_categories(sub_category),ngram_score=max(ngram_score)) %>% left_join(.,(load_estc_core() %>% distinct(estc_id,work_id))) %>% group_by(work_id,main_category) %>% summarise(,ngram_score=sum(ngram_score),sub_category=clean_sub_categories(sub_category)) %>% as.data.frame(.) %>% group_by(work_id) %>% summarise(.,main_category=main_category[which.max(ngram_score)],sub_category=clean_sub_categories(sub_category),topic_score=max(ngram_score)) %>% as.data.frame(.) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% distinct(estc_id,main_category,sub_category) %>% mutate(.,source="estc_titles")

# Add the results of the ngram-based labeling to the data obtained from previous step (so now steps 1, 2, 3 and 4 are in a single table)

all_annotated_with_hand_and_topic_and_title <- estc_ngram_count_all_to_category %>% anti_join((all_annotated_with_hand_and_topic %>% distinct(estc_id))) %>% rbind.data.frame(.,all_annotated_with_hand_and_topic) %>% mutate(.,main_category=ifelse(main_category=="poliitcs","politics",main_category)) %>% mutate(.,main_category=ifelse(main_category=="art","arts",main_category)) %>% mutate(.,main_category=ifelse(main_category=="sciene","science",main_category)) %>% mutate(.,main_category=ifelse(main_category=="literature"|main_category=="arts","literature_and_arts",main_category))

# Do a second round of ngram- and keyword-based labeling. Start by downloading manually selected keywords and ngrams related to these iterations-

estc_2nd_iteration_keywords <- read.csv("data/data-work/2nd_iteration_keywords.csv",stringsAsFactors = FALSE)
estc_2nd_iteration_topics <- read.csv("data/data-work/2nd_iteration_topics.csv",stringsAsFactors = FALSE) %>% filter(main_category!="")

# Remove those keywords from this round of labeling that were already used previously.

keywords_in_not_annotated <- estc_title %>% anti_join((all_annotated_with_hand_and_topic_and_title %>% distinct(estc_id))) %>% unnest_ngrams(.,input=full_title,output=ngram,n=1) %>% left_join(estc_2nd_iteration_keywords) %>% count(ngram)
estc_2nd_iteration_keywords <- estc_2nd_iteration_keywords %>% left_join(keywords_in_not_annotated)

# Keywords of step 6 used here to label records. These have no subcategories, so the process is somewhat more straightforward.

estc_2nd_iteration_topic_records <- estc_topics_og %>% anti_join((all_annotated_with_hand_and_topic_and_title %>% distinct(estc_id))) %>% left_join(estc_2nd_iteration_topics) %>% filter(!is.na(main_category)) %>% group_by(estc_id,main_category) %>% summarise(.,category_score=sum(log(n))) %>% as.data.frame(.) %>% group_by(estc_id) %>% summarise(.,main_category=main_category[which.max(category_score)],category_score=max(category_score)) %>% as.data.frame(.) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% group_by(work_id) %>% summarise(.,main_category=main_category[which.max(category_score)]) %>% as.data.frame(.) %>% distinct(work_id,main_category) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% as.data.frame(.) %>% filter(!is.na(estc_id)) %>% distinct(estc_id,main_category) %>% mutate(.,sub_category=NA,source="second_iteration_estc_topic") 

# Ngrams of step 5 used here to label records. These have no subcategories, so the process is somewhat more straightforward.

estc_2nd_iteration_title_records <- estc_title %>% anti_join((all_annotated_with_hand_and_topic_and_title %>% distinct(estc_id))) %>% unnest_ngrams(.,input=full_title,output=ngram,n=1) %>% left_join(estc_2nd_iteration_keywords) %>% filter(!is.na(main_category)) %>% group_by(estc_id,main_category) %>% summarise(.,category_score=sum(log(n))) %>% as.data.frame(.) %>% group_by(estc_id) %>% summarise(.,main_category=main_category[which.max(category_score)],category_score=max(category_score)) %>% as.data.frame(.) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% group_by(work_id) %>% summarise(.,main_category=main_category[which.max(category_score)]) %>% as.data.frame(.) %>% distinct(work_id,main_category) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% as.data.frame(.) %>% filter(!is.na(estc_id)) %>% distinct(estc_id,main_category) %>% mutate(.,sub_category=NA,source="second_iteration_estc_ngram")

# Combine labelled records from steps 5 and 6. 

estc_2nd_iteration_records <- estc_2nd_iteration_topic_records %>% anti_join((estc_2nd_iteration_title_records %>% distinct(estc_id))) %>% rbind.data.frame(.,estc_2nd_iteration_title_records)

# Merge steps 5 and 6 to the data labelled in preceding steps to create a rough version of the final data.

all_annotated_with_hand_and_topic_and_title <- all_annotated_with_hand_and_topic_and_title %>% rbind.data.frame(.,estc_2nd_iteration_records) %>% filter(!is.na(estc_id))

##################################################################

# Polishing of the final output genre data set.

# Download a table used to harmonise subcategory data

sub_category_harmonised <- read.csv("data/data-work/sub_category_conversions_harmonised.csv",stringsAsFactors = FALSE)

# Go through all subcategories for all genre data and replace the existing values with harmonised values.

for(i in 1:nrow(sub_category_harmonised)){
  all_annotated_with_hand_and_topic_and_title$sub_category <- str_replace_all(all_annotated_with_hand_and_topic_and_title$sub_category,pattern = sub_category_harmonised$sub_category[i],replacement = sub_category_harmonised$sub_category_harmonised[i])
}

# Polish subcategory information for each edition in the genre data.

for(i in 1:nrow(all_annotated_with_hand_and_topic_and_title)){
  all_annotated_with_hand_and_topic_and_title$sub_category[i] <- clean_sub_categories(all_annotated_with_hand_and_topic_and_title$sub_category[i])
}

# Produce an overview of sub categories in a table (basis for Appendix table 1 in the article).

sub_category_table <- clean_sub_categories(all_annotated_with_hand_and_topic_and_title$sub_category) %>% str_split(.,";") %>% as.data.frame(.) %>% name_data_frame("sub_category")

# Create a table about the physical size of ESTC records. 

estc_document_size <- load_estc_core() %>% distinct(estc_id,document_type,pagecount.orig) %>% mutate(.,document_type=ifelse(is.na(pagecount.orig)|is.na(document_type),"unknown",document_type)) %>% distinct(estc_id,document_type)

# Using info about physical size, do final merges and conversions of categories and add information about the physical size of the ESTC records. Remove duplicated records.

all_annotated_with_hand_and_topic_and_title_with_category_conversions <- all_annotated_with_hand_and_topic_and_title %>% mutate(.,main_category=ifelse(sub_category=="psalm"|sub_category=="hymn","religion",main_category)) %>% left_join(estc_document_size,.) %>% mutate(,main_category=ifelse(document_type=="Pamphlet" & (main_category=="literature_and_arts"|main_category=="history"|main_category=="philosophy"),"ephemera-entertainment",main_category)) %>% mutate(,main_category=ifelse(document_type=="Pamphlet" & (main_category=="science"|main_category=="education"),"ephemera-practical",main_category)) %>% mutate(main_category=ifelse(is.na(main_category),"unknown",main_category))
not_duplicated_rows <- all_annotated_with_hand_and_topic_and_title_with_category_conversions %>% count(estc_id) %>% filter(n==1) %>% distinct(estc_id)
all_annotated_with_hand_and_topic_and_title_with_category_conversions <- all_annotated_with_hand_and_topic_and_title_with_category_conversions %>% inner_join(not_duplicated_rows) %>% filter(!is.na(estc_id))

##################################################################

# Save final category data

write.csv(all_annotated_with_hand_and_topic_and_title_with_category_conversions,"data/data-output/estc_categories.csv",row.names = FALSE)

# Take samples for evaluation

# Create a sample of documents for which the workflow produced a label.

set.seed(101)
sample_of_labelled <- all_annotated_with_hand_and_topic_and_title_with_category_conversions %>% filter(main_category!="unknown") %>% sample_n(.,size=1000) %>% left_join((estc_title) %>% distinct(estc_id,full_title))

# Create a sample of documents for which the workflow did not produce a label.

set.seed(102)
sample_of_not_labelled <- all_annotated_with_hand_and_topic_and_title_with_category_conversions %>% filter(main_category=="unknown") %>% sample_n(.,size=500) %>% left_join((estc_title) %>% distinct(estc_id,full_title))

# Save the samples

write.csv(sample_of_labelled,"data/data-work/category_evaluation_sample.csv",row.names = FALSE)
write.csv(sample_of_not_labelled,"data/data-work/category_evaluation_sample_missing.csv",row.names = FALSE)
