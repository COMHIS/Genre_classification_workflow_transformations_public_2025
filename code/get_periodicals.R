# The script starts with description of the conditions on which it was run previously. This can ease replication in some instances:

######################################################

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

######################################################

# Set working directory and call relevant packages
setwd("~/Git_root/Genre_classification_workflow_transformations_2025")

# Load the necessary package
library(tidyverse)
library(magrittr)
library(estcr)
library(estcr)

##############################################

# Download manually annotated documents

manually_annotated_documents <- read.csv("data/data-work/all_manually_annotated_documents_final_expanded.csv",stringsAsFactors = FALSE)

# Download a data set of all estc keywords
estc_topics_og <- read.csv("data/data-work/estc_prominent_topics.csv",stringsAsFactors = FALSE)

# Download ESTC full title data
estc_title <- read.csv("data/data-work/estc_full_title.csv",stringsAsFactors = FALSE)

# Agregate counts of the raw topics

estc_topics_og_count <- estc_topics_og %>% count(topic,name="n_topic") %>% arrange(-n_topic)

# Download data about microfilm collections and bibiliographies and subset values possibly related to periodicals
estc_microfilms_and_citation <- read.csv("estc-data-verified/estc-csv-raw-filtered/estc_raw_sane.csv",stringsAsFactors = FALSE,sep="\t") %>% filter((Field_code=="510"&Subfield_code=="a")|(Field_code=="533"&Subfield_code=="f")) 
estc_microfilms_and_citation_per_record <- estc_microfilms_and_citation %>% mutate(.,Value_harmonised=gsub(x=Value,pattern=";.+",replacement="")) %>% mutate(.,Value_harmonised=gsub(x=Value_harmonised,pattern="\\:+",replacement="")) %>% mutate(.,Value_harmonised=tolower(Value_harmonised)) 
estc_microfilms_and_citation_unique_values <- estc_microfilms_and_citation %>% mutate(.,Value_harmonised=gsub(x=Value,pattern=";.+",replacement="")) %>% mutate(.,Value_harmonised=gsub(x=Value_harmonised,pattern="\\:+",replacement="")) %>% mutate(.,Value_harmonised=tolower(Value_harmonised)) %>% count(Value_harmonised)
estc_microfilms_and_citation_unique_values_potential_periodicals <- estc_microfilms_and_citation_unique_values %>% mutate(.,potential_periodical=grepl(x=Value_harmonised,pattern="(periodical)|(periodicals)|(newspaper)|(newspapers)|(newsbook)|(newsbooks)|(coranto)|(corantos)")) %>% filter(potential_periodical==TRUE)

# Save data for manual scrutiny
write.csv(estc_microfilms_and_citation_unique_values_potential_periodicals,"data/data-work/potential_periodical_collections.csv",row.names = FALSE)

# Download selected (manually checked and selected) citations and microfilm collections related to periodicals
estc_microfilms_and_citation_unique_values_potential_periodicals_harmonised <- read.csv("data/data-work/potential_periodical_collections_harmonised.csv",stringsAsFactors = FALSE) %>% filter(is.na(remove))

# Download selected genre and topic information related to periodicals
estc_topics_potential_periodicals_harmonised <- read.csv("data/data-work/periodical_topics_harmonised.csv",stringsAsFactors = FALSE) 

# Seek periodicals from the different sources
periodicals_from_microfilm_and_citation <- estc_microfilms_and_citation_per_record %>% inner_join(estc_microfilms_and_citation_unique_values_potential_periodicals_harmonised) %>% distinct(estc_id)
periodicals_from_topic <- estc_topics_og %>% inner_join(estc_topics_potential_periodicals_harmonised) %>% distinct(estc_id)
all_periodicals <- rbind.data.frame(periodicals_from_microfilm_and_citation,periodicals_from_topic) %>% distinct(estc_id)
all_periodicals_over_work_id <- all_periodicals %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% distinct(work_id) %>% left_join((load_estc_core() %>% distinct(estc_id,work_id))) %>% distinct(estc_id)
all_periodicals_over_work_id_with_categories <- all_periodicals_over_work_id %>% mutate(.,main_category="literature",sub_category="periodical",source="periodical_metadata")

##############################################

# Steps 1 and 2 of the workflow

# Take the manually annotated documents. Remove any possible periodicals, as those are reserved for the other step.
manually_annotated_documents_without_periodicals <- manually_annotated_documents %>% filter(sub_category!="periodical") %>% mutate(.,source="hand_annotated")

# Combine step 1 and step 2.
manually_annotated_documents_with_new_periodicals <- all_periodicals_over_work_id_with_categories %>% anti_join((manually_annotated_documents_without_periodicals %>% distinct(estc_id))) %>% rbind.data.frame(.,manually_annotated_documents_without_periodicals)

# Save the table that has both the new information about periodicals (step 1) and the manually annotated documents (step 2).
write.csv(manually_annotated_documents_with_new_periodicals,"data/data-work/manually_annotated_documents_and_periodicals.csv",row.names = FALSE)
