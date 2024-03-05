
###########################################
#
#     loading datasets from simulation 
#
###########################################
source("./scripts/data_wrangling/merge_data_replication_projects.R")


#load("../data/df_combined.RData")
setwd("./datasets")

temp = list.files(getwd(), all.files = T)
temp <- temp[-c(1,2)]

vec_m_error <- grep("m_error", temp)
vec_s_error <- grep("s_error", temp)
vec_null <- grep("null", temp)

list_m_error <- list()
list_s_error <- list()
list_null <- list()



loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

for (i in 1:length(vec_m_error)){
  list_m_error[[i]] <-loadRData(temp[vec_m_error[i]])
}

for (i in 1:length(vec_null)){
    list_null[[i]] <-loadRData(temp[vec_null[i]])
}

for (i in 1:length(vec_s_error)){
  list_s_error[[i]] <-loadRData(temp[vec_s_error[i]])
}

variables_for_analysis <- c( "sample_size_approach", "project", "scenario",  
                             "study_id", "orig_d" , "es_true", "orig_ss", "rep_sample_size", "conducted",
                             "n_success", "N", "pct_success") 

###########################
#
#     data wrangling 
#
###########################



for (i in 1:length(list_m_error)) {
  
  list_m_error[[i]] <- 
    list_m_error[[i]] %>%
    select(all_of(variables_for_analysis))%>%
    mutate(scenario = "m_error")
  
}

for (i in 1:length(list_null)) {
  
  list_null[[i]] <- 
    list_null[[i]] %>%
    select(all_of(variables_for_analysis))%>%
    mutate(scenario = "null_effect")
  
}


for (i in 1:length(list_s_error)) {
  
  list_s_error[[i]] <- 
    list_s_error[[i]] %>%
    select(all_of(variables_for_analysis))%>%
    mutate(scenario = "s_error")
  
}


######################################
#
# combining lists into data.frames 
#
######################################


df_m_error <- ldply(list_m_error, data.frame)
df_null <- ldply(list_null, data.frame)
df_s_error <- ldply(list_s_error, data.frame)

names_approaches <- c(rep("a_80pct", nrow(df_combined)), 
                      rep("a_95pct", nrow(df_combined)), 
                      rep("b_0.5", nrow(df_combined)), 
                      rep("b_1", nrow(df_combined)), 
                      rep("c", nrow(df_combined)), 
                      rep("d", nrow(df_combined))) 

df_m_error$sample_size_approach <- names_approaches
df_null$sample_size_approach <- names_approaches
df_s_error$sample_size_approach <- names_approaches

res_summary <- rbind(df_m_error, df_null, df_s_error)

save(res_summary, file = "./res_summary.RData")




#############################
#
#
#   Dataset preparation 
#   for visualization 
#
#############################


t4 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))


t5 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "s_error") %>%
  group_by(sample_size_approach) %>%
  mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))


ci_range <- df_combined$orig_ci_high - df_combined$orig_ci_low

df_se  <- ci2se(lower = df_combined$orig_ci_low, 
                upper = df_combined$orig_ci_high, 
                conf.level =0.95)
t2 <-
  res_summary%>%
  filter(conducted == "yes") %>%
  group_by(sample_size_approach, scenario)%>%
  dplyr::summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)),
                   mean_pct_success = round(mean(pct_success), 2),
                   sum_rep_ss = sum(rep_sample_size))
t2


# extracting orig_d from dataset 

vec_orig_d <- 
  unlist(
    as.vector(
      res_summary %>%
        filter(scenario == "m_error", 
               sample_size_approach == "a_80pct") %>%
        select(orig_d)
    )
  )

# creating breaks for the quartile bins of orig_d 
vec_breaks <- 
  c(min(vec_orig_d),
    as.numeric(quantile(vec_orig_d, prob = c(.25, .5, .75))), 
    round(max(vec_orig_d)))

# assigning bins to orig_d 
vec_orig_d_bin <- findInterval(vec_orig_d, vec_breaks)

# adding orig_d_bin to the res_summary dataset 
d3 <- 
  res_summary %>%
  mutate(orig_d_bin = rep(vec_orig_d_bin, 18))

# filter d3 for conducted experiments 
d3f <- 
  d3 %>%
  filter(conducted == "yes")


rep_success_orig_d_bins <- 
  d3f %>%
  group_by(sample_size_approach, scenario, orig_d_bin) %>%
  dplyr::summarize(mean_success = round(mean(pct_success),1))



sum_orig_ss <- sum(df_combined$orig_ss)

per_orig_ss <- round(sum(df_combined$orig_ss)/nrow(df_combined))

median_orig_ss <- median(df_combined$orig_ss)

median_rep_ss <- median(df_combined$rep_ss)

df_fig5 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  dplyr::summarize(
    n_conducted = sum(conducted == "yes"),
    sum_rep_ss = sum(rep_sample_size, na.rm = T), 
    ss_per_experiment = round(sum_rep_ss/n_conducted), 
    median_rep_ss = median(rep_sample_size, na.rm = T)) 

df_fig5

