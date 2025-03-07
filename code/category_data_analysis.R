##############################################
# Set the working directory, call relevant libraries and automatically produce information relevant for replication purposes

# Call relevant libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(estcr)
library(gghsci)
library(eccor)
library(psych)

##############################################
# Define article specific function to compute a confidence interval for a binary variable based on the central limit theorem
# and to minimise unnecessary dependencies

binary_prop_and_confidence_interval <- function(n_s,n_f,q=0.95){
  q_tail <- (1-q)/2 
  n <- n_s+n_f
  point_estimate <- n_s/n
  std_deviation <- sqrt(point_estimate*(1-point_estimate)/n)
  quantile_mapping <- qnorm(p=1-q_tail)
  upper_bound <- point_estimate+quantile_mapping*std_deviation
  lower_bound <- point_estimate-quantile_mapping*std_deviation
  return(c(lower_bound,point_estimate,upper_bound))
  
}


#set the working path
setwd("~/Git_root/Genre_classification_workflow_transformations_2025")

# For documentation: call sessionInfo() and save data about the latest run
writeLines(capture.output(sessionInfo()), paste0("data/data-work/sessionInfo","_of_the_run_of_",as.character(Sys.Date()),".txt"))

##############################################
# Proper analysis of the genre data starts from here

# Download the genre data for the entire ESTC.

type_label_data <- read.csv("data/data-output/estc_categories.csv",stringsAsFactors = FALSE)

# Set the known proportion of data without labels

prop_of_missing <- (type_label_data %>% filter(main_category=="unknown") %>% nrow(.))/(type_label_data %>% nrow(.))

# Despite its length, this is only a set of basic data wrangling: standardising inconsistencies in typing of labels of the annotators and creating three
# different binary scores derivated from the (0,0.5,1) scores assigned to individual labels
data_with_labels <- read.csv("data/data-output/category_evaluation_sample_evaluated_final_version.csv",stringsAsFactors = FALSE,header = TRUE) %>% mutate(.,aggregate_score=rowMeans(cbind(main_category_iiro,main_category_kira), na.rm=TRUE)) %>% 
  mutate(.,plausible_category=ifelse(aggregate_score>=0.5,1,0)) %>% 
  mutate(.,good_category=ifelse(aggregate_score>=0.75,1,0)) %>% 
  mutate(.,best_category=ifelse(aggregate_score>=1,1,0)) %>% 
  mutate(real_category_iiro=ifelse(real_category_iiro=="","unknown",real_category_iiro)) %>% mutate(real_category_iiro=ifelse(real_category_iiro=="literatur and arts","literature and arts",real_category_iiro)) %>%
  mutate(real_category_iiro=ifelse(real_category_iiro=="","unknown",real_category_iiro)) %>% 
  mutate(real_category_iiro=ifelse(real_category_iiro=="literatur and arts","literature and arts",real_category_iiro)) %>%
  mutate(real_category_iiro=ifelse(real_category_iiro=="literature and arts","literature_and_arts",real_category_iiro)) %>%
   mutate(real_category_iiro=ifelse(real_category_iiro=="philosophy?","philosophy",real_category_iiro)) %>%
   mutate(real_category_iiro=ifelse(real_category_iiro=="science or education"|real_category_iiro=="politics or law","unknown",real_category_iiro)) %>% 
   mutate(real_category_iiro=ifelse(real_category_iiro=="politics?","politics",real_category_iiro)) %>% 
  mutate(real_category_iiro=ifelse(real_category_iiro=="law?","law",real_category_iiro))  %>%
  mutate(real_category_kira=ifelse(real_category_kira=="","unknown",real_category_kira)) %>% mutate(real_category_kira=ifelse(real_category_kira=="literatur and arts","literature and arts",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="","unknown",real_category_kira)) %>% 
  mutate(real_category_kira=ifelse(real_category_kira=="literatur and arts","literature and arts",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="literature and arts","literature_and_arts",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="philosophy?","philosophy",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="science or education"|real_category_kira=="politics or law","unknown",real_category_kira)) %>% 
  mutate(real_category_kira=ifelse(real_category_kira=="politics?","politics",real_category_kira)) %>% 
  mutate(real_category_kira=ifelse(real_category_kira=="law?","law",real_category_kira))  %>%
  mutate(real_category_kira=ifelse(real_category_kira=="practical","ephemera-practical",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="entertainment","ephemera-entertainment",real_category_kira)) %>%
  mutate(real_category_kira=ifelse(real_category_kira=="geography","science",real_category_kira)) %>%
  mutate(real_category_iiro=ifelse(real_category_iiro=="practical","ephemera-practical",real_category_iiro)) %>%
  mutate(real_category_iiro=ifelse(real_category_iiro=="entertainment","ephemera-entertainment",real_category_iiro)) %>%
mutate(real_category_iiro=ifelse(document_type=="Pamphlet" & (real_category_iiro=="philosophy" | real_category_iiro=="literature_and_arts" | real_category_iiro=="history"),"ephemera-entertainment",real_category_iiro)) %>%
mutate(real_category_iiro=ifelse(document_type=="Pamphlet" & (real_category_iiro=="science" | real_category_iiro=="education"),"ephemera-practical",real_category_iiro)) %>%
mutate(real_category_kira=ifelse(document_type=="Pamphlet" & (real_category_kira=="philosophy" | real_category_kira=="literature_and_arts" | real_category_kira=="history"),"ephemera-entertainment",real_category_kira)) %>%
mutate(real_category_kira=ifelse(document_type=="Pamphlet" & (real_category_kira=="science" | real_category_kira=="education"),"ephemera-practical",real_category_kira))
  
# Download a table to standardise some inconsistencies of typing labels. 
category_corrections_table <- read.csv("data/data-work/category_corrections.csv",stringsAsFactors = FALSE) 

# Download the sample of records without labels that was manually annotated. Ensure that the categories for certain small publication types
# are used consistently.
data_with_missing_labels <- read.csv("data/data-output/category_evaluation_sample_missing_evaluated_final_version.csv",stringsAsFactors = FALSE) %>%
  mutate(main_category_iiro=ifelse(document_type=="Pamphlet" & (main_category_iiro=="philosophy" | main_category_iiro=="literature_and_arts" | main_category_iiro=="history" | main_category_iiro=="literature and arts"),"ephemera-entertainment",main_category_iiro)) %>%
  mutate(main_category_iiro=ifelse(document_type=="Pamphlet" & (main_category_iiro=="science" | main_category_iiro=="education"),"ephemera-practical",main_category_iiro)) %>%
  mutate(main_category_kira=ifelse(document_type=="Pamphlet" & (main_category_kira=="philosophy" | main_category_kira=="literature_and_arts" | main_category_kira=="history" | main_category_kira=="literature and arts"),"ephemera-entertainment",main_category_kira)) %>%
  mutate(main_category_kira=ifelse(document_type=="Pamphlet" & (main_category_kira=="science" | main_category_kira=="education"),"ephemera-practical",main_category_kira)) 


# Produce an annotator agreeance summary table

data_with_labels_summarised <- data_with_labels %>% count(main_category_iiro,main_category_kira)
n_of_plausible_labels <- data_with_labels %>% filter(aggregate_score>=0.5) %>% nrow(.)

# Calculate confidence intervals for the plausible, good, and best ratings. First the counts
n_data_with_labels_non_missing <- data_with_labels %>% filter(!is.na(main_category_iiro) & !is.na(main_category_kira)) %>% nrow(.)
n_of_best_labels <- data_with_labels %>% filter(aggregate_score>=1) %>% nrow(.)
n_of_good_labels <- data_with_labels %>% filter(aggregate_score>=0.75) %>% nrow(.)
n_of_plausible_labels <- data_with_labels %>% filter(aggregate_score>=0.5) %>% nrow(.)

# Calculate confidence intervals for plausible, good and best ratings. Using the counts, calculate the confidence intervals.

best_labels_confidence_interval <- binary_prop_and_confidence_interval(n_of_best_labels,n_data_with_labels_non_missing-n_of_best_labels)
best_labels_confidence_interval

good_labels_confidence_interval <- binary_prop_and_confidence_interval(n_of_good_labels,n_data_with_labels_non_missing-n_of_good_labels)
good_labels_confidence_interval


plausible_labels_confidence_interval <- binary_prop_and_confidence_interval(n_of_plausible_labels,n_data_with_labels_non_missing-n_of_plausible_labels)
plausible_labels_confidence_interval


# Also, calculate Cohen's Kappa for the annotator agreement on the verified data

data_with_labels_cohensk <- cohen.kappa(x=cbind(data_with_labels$main_category_iiro,data_with_labels$main_category_kira))
data_with_labels_cohensk

# Calculate Cohen's Kappa for the manually annotated data without labels
data_with_missing_labels_cohensk <- cohen.kappa(x=cbind(data_with_missing_labels$main_category_iiro,data_with_missing_labels$main_category_kira))
data_with_missing_labels_cohensk

# How often the authors' quality assements of labels agree
data_with_labels_summarised_ageement_categories <- data_with_labels_summarised %>% filter((main_category_iiro==1 & main_category_kira==1)|(main_category_kira==0.5 & main_category_iiro==0.5)|(main_category_iiro==0 & main_category_kira==0)) 
n_agreeance <- sum(data_with_labels_summarised_ageement_categories$n)
n_of_non_misising_labels <- data_with_labels %>% filter(!is.na(main_category_iiro) & !is.na(main_category_kira)) %>% nrow(.)
prop_of_agreance_and_confidence_intervals_evaluated_labels <- binary_prop_and_confidence_interval(n_agreeance,n_of_non_misising_labels-n_agreeance,0.95)


# how often the manually assigned labels are the same
n_of_agreement_manually_assigned_labels_to_missing_data <- data_with_missing_labels %>% filter(main_category_iiro==main_category_kira) %>% nrow(.)
prop_of_agreance_missing_data <- n_of_agreement_manually_assigned_labels_to_missing_data/nrow(data_with_missing_labels)
prop_of_agreance_and_confidence_intervals_missing_data <- binary_prop_and_confidence_interval(n_of_agreement_manually_assigned_labels_to_missing_data,nrow(data_with_missing_labels)-n_of_agreement_manually_assigned_labels_to_missing_data,q=0.95)

# The results indicate at least substantial (lower bound) agreement between the annotators.

# Create a table of all possible proposed label/true label -combinations (assigned_and_real_hypothetical_combinations)
assigned_category <- cbind.data.frame(assigned_category=unique(data_with_labels$main_category),key=1)
real_category <- cbind.data.frame(real_category=unique(c(data_with_labels$real_category_iiro,data_with_labels$real_category_kira)),key=1)
assigned_and_real_hypothetical_combinations <- full_join(assigned_category,real_category) %>% distinct(assigned_category,real_category)

# Calculate how precise the different categories are measured with different metrics and how many records there are ber category in the sample.
categories_by_precission <- data_with_labels %>% group_by(main_category) %>% summarise(.,aggregate_precission_score=mean(aggregate_score,na.rm=TRUE),plausible_category_precission=mean(plausible_category,na.rm = TRUE),good_category_precission=mean(good_category,na.rm=TRUE),best_category_precission=mean(best_category,na.rm =TRUE),n_classified_to_category=length(plausible_category))

# Calculate the number of not-good (as defined in the article) labels in the manually evaluated data. This is first done for each annotator separately.
n_of_weak_labels_per_category_true_labels_iiro <- data_with_labels %>% filter(main_category_iiro<1 & aggregate_score<0.75) %>% as.data.frame(.) %>% mutate(real_category=real_category_iiro) %>% count(real_category,name="n_not_detected_iiro") 
n_of_weak_labels_per_category_true_labels_kira <- data_with_labels %>% filter(main_category_kira<1 & aggregate_score<0.75) %>% as.data.frame(.) %>% mutate(real_category=real_category_kira) %>% count(real_category,name="n_not_detected_kira") 

# Set weights for the two annotators. These are used in computations that require averaging over the two annotators. As they have now both annotated all data sets, parameters are set so that they have an equal weight.
n_labelled_by_iiro <- 1000
n_labelled_by_kira <-1000
w_kira <- min(1,n_labelled_by_kira/n_labelled_by_iiro)
w_kira_missing_data <- 1 

# Combine the annotations of Kira and Iiro for records wrongly annotated by the semi-automatic process. 
# The end result is a table that gives the averaged (weighted over the annotators) number of wrongly classified records
# for each category
aggregated_weak_label_true_labels <- n_of_weak_labels_per_category_true_labels_iiro %>% left_join(n_of_weak_labels_per_category_true_labels_kira) %>% mutate(.,n_not_detected_kira=ifelse(is.na(n_not_detected_kira),0,n_not_detected_kira)) %>% mutate(w_iiro=1) %>% mutate(.,w_kira=w_kira) %>% mutate(.,weighted_sum=w_iiro/(w_iiro+w_kira)*n_not_detected_iiro+w_kira/(w_iiro+w_kira)*n_not_detected_kira) %>% mutate(.,proportion_in_wrongly_classified=weighted_sum/sum(weighted_sum)) %>% mutate(.,averaged_n_in_wrongly_classied=weighted_sum)

# Slightly polished and simplified version of the table above
aggregated_weak_label_true_labels_simplified <- aggregated_weak_label_true_labels %>% mutate(main_category=real_category) %>% distinct(main_category,averaged_n_in_wrongly_classied,proportion_in_wrongly_classified) %>% mutate(.,main_category=ifelse(main_category=="literature and arts","literature_and_arts",main_category)) 

# Calculate the number of records belonging to each category from the sample of records without labels that was
# manually annotated by Kira and Iiro. These numbers are used to estimate the proportion of categories among those records
# that do not have a label.
n_of_missing_labels_per_category_labelled_iiro <- data_with_missing_labels %>% count(main_category_iiro,name="n_in_records_missing_label_iiro") %>% mutate(.,main_category=main_category_iiro) %>% distinct(main_category,n_in_records_missing_label_iiro)
n_of_missing_labels_per_category_labelled_kira <- data_with_missing_labels %>% count(main_category_kira,name="n_in_records_missing_label_kira") %>% mutate(.,main_category=main_category_kira) %>% distinct(main_category,n_in_records_missing_label_kira)

# Aggregate (over the annotators) the number of records belonging to each category from the sample of records without labels that was. Start by bringing the data
# from the two annotators to the same table

aggregated_table_of_missing_labels_per_category_labelled <- left_join(n_of_missing_labels_per_category_labelled_iiro,n_of_missing_labels_per_category_labelled_kira) 

# Average over the annotators, with their weights set equally to 1 since both have annotated all data. 
aggregated_table_of_missing_labels_per_category_labelled <- aggregated_table_of_missing_labels_per_category_labelled %>% left_join(category_corrections_table) %>% group_by(main_category) %>% summarise(.,n_in_records_missing_label_iiro=sum(n_in_records_missing_label_iiro),n_in_records_missing_label_kira=sum(n_in_records_missing_label_kira)) %>% as.data.frame(.) %>% mutate(w_iiro=1,w_kira=w_kira_missing_data) %>% mutate(.,averaged_n_among_missing_labels=w_iiro/(w_iiro+w_kira)*n_in_records_missing_label_iiro+w_kira/(w_kira+w_iiro)*n_in_records_missing_label_kira) %>% mutate(.,proportion_of_category_among_records_without_label=averaged_n_among_missing_labels/sum(averaged_n_among_missing_labels,na.rm=TRUE)) 
aggregated_table_of_missing_labels_per_category_labelled_simplified <- aggregated_table_of_missing_labels_per_category_labelled %>% distinct(main_category,averaged_n_among_missing_labels,proportion_of_category_among_records_without_label) %>% mutate(.,main_category=ifelse(main_category=="literature and arts","literature_and_arts",main_category))                                                                                                                                                                                                                                                                                                                                                                                                                                                 

# Calculate the number of 'unknowns' (could not determine the right label) in the set of falsely classified records. This is used to adjust some calculations 
n_unknown_in_falsely_classified <- aggregated_weak_label_true_labels_simplified %>% filter(main_category=="unknown")
n_unknown_in_falsely_classified <- n_unknown_in_falsely_classified$averaged_n_in_wrongly_classied
n_falsely_classified <- data_with_labels %>% filter(good_category==0) %>% nrow(.)
  
#  Calculate the number of 'unknowns' (could not determine the right label) in the sample of records without labels. This is used to adjust some calculations 
n_unknown_in_missing <- aggregated_table_of_missing_labels_per_category_labelled %>% filter(main_category=="unknown")
n_unknown_in_missing <- n_unknown_in_missing$averaged_n_among_missing_labels
n_missing <- data_with_missing_labels %>% nrow(.)

# Create the final table of summary statistics. Especially, the aim is to estimate not only the precision of each category
# but also its coverage of the labels it is supposed to cover. This is done by calculating the estimated number of 'good' classifications
# in any main category compared to the estimated 'total', that also includes falsely classified records that actually belong to the main category
# but have been labelled as being something else, and the estimated number of records that belong to the category that have not been classified at all.
# Our samples enable us to estimate all of these.

joint_simplified_table_of_summary_statistics <- left_join(categories_by_precission,aggregated_weak_label_true_labels_simplified) %>% left_join(aggregated_table_of_missing_labels_per_category_labelled_simplified) %>% mutate(.,coverage_of_good_labels=n_classified_to_category/sum(n_classified_to_category)*good_category_precission*(1-prop_of_missing)*(sum(n_classified_to_category)/nrow(data_with_labels))) %>% mutate(.,coverage_of_false_labels=(1-prop_of_missing)*(1-sum(n_classified_to_category*good_category_precission)/nrow(data_with_labels))*averaged_n_in_wrongly_classied/sum(averaged_n_in_wrongly_classied)*((n_falsely_classified-n_unknown_in_falsely_classified)/(n_falsely_classified)),coverage_of_missing_labels=prop_of_missing*averaged_n_among_missing_labels/sum(averaged_n_among_missing_labels)*(n_missing-n_unknown_in_missing)/(n_missing)) %>% mutate(.,coverage_of_category=coverage_of_good_labels/(coverage_of_good_labels+coverage_of_false_labels+coverage_of_missing_labels))

# Create a table that estimates the number of the 'good' labels in the manually annotated data set (this can be marginally different than the raw count due to the few data points that we could not annotate).
good_classifications_per_category <- joint_simplified_table_of_summary_statistics %>% mutate(.,n_good_classifications=n_classified_to_category*good_category_precission) %>% distinct(main_category,n_good_classifications)

# Based on the table just above, create separate tables than can be used to incorporate the size of the real and assigned category separately to other tables.

good_classifications_per_category_real <- good_classifications_per_category %>% rename(real_category=main_category) %>% rename(n_good_classifications_in_real_category=n_good_classifications)
good_classifications_per_category_assigned <- good_classifications_per_category %>% rename(assigned_category=main_category) %>% rename(n_good_classifications_in_assigned_category=n_good_classifications)

# Create a table of the pairs of wrong and real main category for falsely classified records identified in the manually evaluated sample of labels. Do this first for each annotator
# separately-

iiro_label_mislabelled_pairs <- data_with_labels %>% filter(good_category==0) %>% filter(main_category_iiro<1) %>% count(main_category,real_category_iiro,name="n_of_records_iiro") %>% mutate(assigned_category=main_category,real_category=real_category_iiro) %>% distinct(assigned_category,real_category,n_of_records_iiro)
kira_label_mislabelled_pairs <- data_with_labels %>% filter(good_category==0) %>% filter(main_category_kira<1) %>% count(main_category,real_category_kira,name="n_of_records_kira") %>% mutate(assigned_category=main_category) %>% mutate(real_category=real_category_kira) %>% distinct(assigned_category,real_category,n_of_records_kira)

# Following previous lines, create a weighted average of the two annotators for the final analysis. As this is the final version of the data
# that has all of the annotations from both annotators, the weights are equal.

aggregated_label_misslabelled_pairs <- left_join(assigned_and_real_hypothetical_combinations,iiro_label_mislabelled_pairs) %>% left_join(kira_label_mislabelled_pairs) %>% mutate(.,n_of_records_iiro=ifelse(is.na(n_of_records_iiro),0,n_of_records_iiro)) %>% mutate(.,n_of_records_kira=ifelse(is.na(n_of_records_kira),0,n_of_records_kira)) %>% mutate(.,n_averaged_mislabelled_pairs=1/(1+w_kira)*n_of_records_iiro+(w_kira)/(1+w_kira)*n_of_records_kira) %>% left_join(good_classifications_per_category_real) %>% left_join(good_classifications_per_category_assigned) %>% filter(real_category!="unknown")

# Create a more convenient summary table of missclassifications
aggregated_label_misslabelled_pairs_summary_table <- aggregated_label_misslabelled_pairs %>% mutate(.,absolute_error=n_averaged_mislabelled_pairs) %>% mutate(.,error_relative_to_assigned_category=absolute_error/n_good_classifications_in_assigned_category,error_relative_to_real_category=absolute_error/n_good_classifications_in_real_category)

# Create a version of the summary table with trunctated variable names for less messy plotting.
aggregated_label_misslabelled_pairs_summary_table_plot_version <- aggregated_label_misslabelled_pairs_summary_table %>% mutate(.,assigned_category=ifelse(assigned_category=="ephemera-practical","practical",assigned_category)) %>% mutate(.,assigned_category=ifelse(assigned_category=="ephemera-entertainment","entertainment",assigned_category)) %>% mutate(.,assigned_category=ifelse(assigned_category=="literature_and_arts","lit./arts",assigned_category)) %>% mutate(.,real_category=ifelse(real_category=="literature_and_arts","lit./arts",real_category)) %>% mutate(.,real_category=ifelse(real_category=="ephemera-practical","practical",real_category)) %>% mutate(.,real_category=ifelse(real_category=="ephemera-entertainment","entertainment",real_category))  %>% mutate(.,real_category=ifelse(real_category=="education","edu.",real_category)) %>% mutate(.,assigned_category=ifelse(assigned_category=="education","edu.",assigned_category)) 

# Plot the absolute errors between categories
absolute_errors_plot <- ggplot(aggregated_label_misslabelled_pairs_summary_table_plot_version, aes(x = assigned_category, y = real_category, fill = absolute_error)) +
  geom_tile()+xlab("Assigned category")+ylab("Real category")+ggtitle(label="Absolute misassignments between categories")+guides(fill=guide_legend(title="Absolute error (N)"))+gghsci::theme_hsci_continuous(base_size = 11)
absolute_errors_plot

# Plot the relative (to assigned category) errors between categories
relative_to_assigned_category_errors_plot <- ggplot(aggregated_label_misslabelled_pairs_summary_table_plot_version, aes(x = assigned_category, y = real_category, fill = error_relative_to_assigned_category*100)) +
  geom_tile()+xlab("Assigned category")+ylab("Real category")+ggtitle(label="Relative (to assigned category) misassignments between categories")+guides(fill=guide_legend(title="Error relative to assigned category (%)"))+gghsci::theme_hsci_continuous(base_size = 11)
relative_to_assigned_category_errors_plot

# Plot the relative (to real category) errors between categories
relative_to_real_category_errors_plot <- ggplot(aggregated_label_misslabelled_pairs_summary_table_plot_version, aes(x = assigned_category, y = real_category, fill = error_relative_to_real_category*100)) +
  geom_tile()+xlab("Assigned category")+ylab("Real category")+ggtitle(label="Relative (to real category) misassignments between categories")+guides(fill=guide_legend(title="Error relative to real category (%)"))+gghsci::theme_hsci_continuous(base_size = 11)
relative_to_real_category_errors_plot

# Create a plot that brings the relative error plots together

relative_errors_pair_plot <- gridExtra::grid.arrange(relative_to_assigned_category_errors_plot,relative_to_real_category_errors_plot,ncol=1)

# Save the relevant output figures.

# Absolute size of different errors.

ggsave(plot=absolute_errors_plot,"figures/absolute_error_plot.png",dpi=600)
ggsave(plot=absolute_errors_plot,"figures/absolute_error_plot.pdf")
embedFonts("figures/absolute_error_plot.pdf")

# Save figure 3 of the article.

ggsave(plot=relative_to_assigned_category_errors_plot,"figures/relative_to_assigned_category_error_plot.png",dpi=600)
ggsave(plot=relative_to_assigned_category_errors_plot,"figures/relative_to_assigned_category_error_plot.pdf")
embedFonts("figures/relative_to_assigned_category_error_plot.pdf")

# Save figure 2 of the article.

ggsave(plot=relative_to_real_category_errors_plot,"figures/relative_to_real_category_error_plot.png",dpi=600)
ggsave(plot=relative_to_real_category_errors_plot,"figures/relative_to_real_category_error_plot.pdf")
embedFonts("figures/relative_to_real_category_error_plot.pdf")

# Joint figure of relative errors (figures 2 and 3).

ggsave(plot=relative_errors_pair_plot,"figures/relative_errors_pair_plot.pdf")
embedFonts("figures/relative_errors_pair_plot.pdf")


#Save the output tables regarding preiccion, coverage and pairwise errors in labels.

write.csv(aggregated_label_misslabelled_pairs_summary_table_plot_version,"data/data-output/summary_table_of_mislabelled_records.csv",row.names = FALSE)
write.csv(joint_simplified_table_of_summary_statistics,"data/data-output/summary_table_of_precission_and_coverage_of_main_categories.csv",row.names = FALSE)
write.csv(aggregated_label_misslabelled_pairs_summary_table,"data/data-output/summary_table_of_pairwise_labeling_errors.csv",row.names = FALSE)

# Download the final category data for additional plotting

estc_category_data_final <- read.csv("data/data-output/category_data_post_evaluation_fixed.tsv",stringsAsFactors = FALSE) %>% mutate(.,main_category=ifelse(main_category=="literature_and_arts","literature and arts",main_category))

# Plot the proportion of different categories between 1500 and 1800. 

estc_category_data_final_plot <- estc_category_data_final %>% left_join(load_estc_core() %>% distinct(estc_id,publication_decade)) %>% filter(publication_decade>=1500 & publication_decade<=1800) %>% mutate(decade=publication_decade) %>% ggplot(data=.,aes(x=decade,fill=main_category))+geom_bar(position="fill")+scale_colour_grey()+xlab("Decade")+ylab("%")+gghsci::theme_hsci_discrete(base_size = 15)+guides(fill=guide_legend(title="Main Category"))+ggtitle("Proportion of Editions in the ESTC by Category and Decade")
estc_category_data_final_plot 

# Save the temporal plots in two different formats. Figure 4 of the article.

ggsave(plot=estc_category_data_final_plot,"figures/category_temporal_development.pdf")
embedFonts("figures/category_temporal_development.pdf")
ggsave(plot=estc_category_data_final_plot,"figures/category_temporal_development.png",dpi=600)

# Compare the relative size of categories in 18th century ECCO to ESTC

# Compute the genre distribution for ECCO

ecco_genre_distribution <- load_ecco_core() %>% distinct(estc_id) %>% left_join(estc_category_data_final) %>% filter(!is.na(main_category)) %>% count(main_category,name = "n_ecco") %>% mutate(,prop_ecco=n_ecco/sum(n_ecco))

# Compute the genre distribution for the eighteenth-century ESTC

estc_18th_c_genre_distribution <- load_estc_core() %>% distinct(estc_id,publication_year) %>% left_join(estc_category_data_final) %>% filter(!is.na(main_category) & publication_year>=1701 & publication_year<=1800) %>% count(main_category,name = "n_estc_18th") %>% mutate(,prop_estc_18th=n_estc_18th/sum(n_estc_18th))

# Plot the differences between the relative sizes of categories between the ESTC and ECCO

estc_18th_and_ecco_distributions <- left_join(estc_18th_c_genre_distribution,ecco_genre_distribution) %>% mutate(,ecco_to_estc_ratio=prop_ecco/prop_estc_18th) %>% mutate(,relative_difference_in_ecco_to_expected_ratio=ecco_to_estc_ratio-1)
estc_18th_and_ecco_distributions_plot <- estc_18th_and_ecco_distributions %>% ggplot(data=.)+geom_col(aes(x=main_category,y=relative_difference_in_ecco_to_expected_ratio*100))+coord_flip()+gghsci::theme_hsci_continuous(base_size = 15)+xlab("Main Category")+ylab("Difference to 18th-century ESTC (%)")+ggtitle("Relative Size of Categories in ECCO Compared to 18th-Century ESTC")
estc_18th_and_ecco_distributions_plot 

# Save the comparative plot between genres in ESTC and ECCO in two different formats. Figure 5 of the article.

ggsave(plot=estc_18th_and_ecco_distributions_plot,"figures/ecco_to_18th_century_ESTC_proportion_of_different_genres.pdf")
embedFonts("figures/ecco_to_18th_century_ESTC_proportion_of_different_genres.pdf")
ggsave(plot=estc_18th_and_ecco_distributions_plot,"figures/ecco_to_18th_century_ESTC_proportion_of_different_genres.png",dpi = 600)

#####################################

# Additional misc. calculations and tables: 

# Make a table of subcategories and clean the list a bit. The end result is the basis for Appendix table 1.
sub_categories_listed <- type_label_data$sub_category %>% str_split(.,pattern=";") %>% unlist(.) %>% unique(.) %>% as.data.frame(.) 
colnames(sub_categories_listed)[1] <- "sub_category"
sub_categories_listed_cleaned <- sub_categories_listed %>% filter(sub_category!="" & !is.na(sub_category))

# Create a table that lists appearances of main categories and subcategories pairwise. Used in the article for the anecdotal
# examples regarding the relative prominence of certain subcategories in specific main categories.
sub_categorier_per_main_category <- type_label_data %>% count(main_category,sub_category) %>% filter(sub_category!="")

# calculate how many records in the manually annotated sample without categories from the workflow have the value 'unknown' for at least
# one annotator
n_of_unknown <- data_with_missing_labels %>% filter(main_category_iiro=="unknown" & main_category_kira=="unknown") %>% nrow(.)