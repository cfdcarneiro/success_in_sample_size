###############################################
#
#   D: Replication with pSceptical 
#
###############################################



# source additional functions
source("./scripts/simulation/functions_for_simulation.R")
# source("./scripts/simulation/functions_for_simulation_d.R")
source("./scripts/data_wrangling/load_packages.R")


##################################
### Simulate replication study ###
### SCENARIO 1: ##################
##################################

# preparing data set df_combined 

load("./datasets/df_combined.RData")

# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:nrow(df_combined)) %>%
  select(study_id, everything())

# calculate sample size for replication

# Approach D:
# Replication study powered for reverse Bayesian approach (skeptical p-value)
# with an effect size shrinkage estimate of 25%  

rep_sample_size_d <- NULL
max_sample_size_total <- 280

for (i in 1:nrow(df_combined)) {
  rep_sample_size_d[i] <- sample_size_d(data = df_combined[i,])
}

rep_sample_size_d[rep_sample_size_d == "NaN"] <- NA
#rep_sample_size_d[rep_sample_size_d > max_sample_size_total] <- max_sample_size_total

df_combined$rep_sample_size_d <- rep_sample_size_d



# We calculated sample sizes for pSceptical. This approach let's us decide which studies will 
# be replicated. Some are already sorted out at the stage of sample size calculation, either bc 
# a replication doesnt seem necessary given the original evidence, or an replication seems unfeasible bc 
# either sample sizes in the replication study would be far to high or because it would not be possible 
# to declare replication success for some of the studies 

df_combined$conducted <- vector(length = nrow(df_combined))

vec_not_nec <- ifelse(df_combined$rep_sample_size_d < 4 & is.na(df_combined$rep_sample_size_d) == FALSE, TRUE, FALSE)
vec_unfeasible <- ifelse(df_combined$rep_sample_size_d >= 280 | is.na(df_combined$rep_sample_size_d) == TRUE, TRUE, FALSE)
vec_go <- ifelse(vec_unfeasible == FALSE & vec_not_nec == FALSE, TRUE, FALSE)


vec_conducted <- vector(length = nrow(df_combined))
vec_conducted[which(vec_go == TRUE)] <- "yes"
vec_conducted[which(vec_unfeasible == TRUE)] <- "unfeasible"
vec_conducted[which(vec_not_nec == TRUE)] <- "not_necessary"

df_combined$conducted <- vec_conducted



decision1_d <- 
  df_combined %>%
  group_by(conducted) %>%
  dplyr::summarize(n = n(),
                   p_crit = sum(orig_p_2sided>=0.05)) %>%
  mutate(approach = "pSkeptical")
decision1_d

df_d_non_sig <- df_combined %>% filter(orig_p_2sided >= 0.05)

crp[which(crp$orig_d %in% c(df_d_non_sig %>% filter(conducted == "yes") %>% pull(orig_d)) == TRUE),]

crp[which(crp$orig_d %in% c(df_d_non_sig %>% filter(conducted != "yes") %>% pull(orig_d)) == TRUE),]

df_d_not_conducted <- df_combined %>% filter(conducted != "yes")
df_d_conducted <- df_combined %>% filter(conducted == "yes")

df_d_not_conducted %>% 
  filter(orig_p_2sided < 0.05)

df_d_conducted %>%
  filter(orig_p_2sided > 0.05)

####################################
#
#
#     Simulate replication studies for 
#     chosen experiments 
#
#####################################



# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 1000

study_id_vector <- which(df_combined$conducted == "yes")

# now use replication sample size 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id]/2,
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id]/2, 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}

# extracting results from the list to form data frames with the results, averaged
# over the n_exp experiments 

row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))


res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = df_combined$orig_d[study_id_vector]/ 2 ,
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "m_error")



res_summary_d <- left_join(df_combined, res_summary_d) 


res_summary_d_m_error <- res_summary_d

save(res_summary_d_m_error, file = "./data/res_summary_d_m_error.RData")

##################################
### Simulate replication study ###
### SCENARIO 2: ##################
##################################

# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 1000

study_id_vector <- which(df_combined$conducted == "yes")


# now use replication sample size to simulate new experiments 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = 0, 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}


row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", 
               "rep_z", "rep_se_z", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = 0,
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "null_effect")

  
res_summary_d <- left_join(df_combined, res_summary_d) 

res_summary_d_null <- res_summary_d

save(res_summary_d_null, file = "./data/res_summary_d_null.RData")

##################################
### Simulate replication study ###
### SCENARIO 3: ##################
##################################


# set seed to reproduce results
set.seed(824565)

# number of experiments we run for each true underlying effect size
n_exp <- 1000

study_id_vector <- which(df_combined$conducted == "yes")


# now use replication sample size to simulate new experiments 

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]), 
                       sample_size = df_combined$rep_sample_size_d[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]), 
               zo = df_combined$zo[study_id], 
               orig_ss = df_combined$orig_ss[study_id], 
               rep_ss = df_combined$rep_sample_size_d[study_id])
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep_pSceptical)
  
}


row_names <- NULL
col_names <- c("study_id", "effect", 
               "ci_low", "ci_high", "p_value", 
               "rep_z", "rep_se_z", "zr", "zo", 
               "orig_ss", "rep_ss", "pScep", "success")

res_summary_rep_d <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))


res_summary_d <- 
  res_summary_rep_d %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(success == 1),
            N = n(),
            pct_success = n_success/N * 100)%>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_d[study_id_vector],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "d",
         project = df_combined$project[study_id_vector],
         scenario = "s_error")


res_summary_d <- left_join(df_combined, res_summary_d) 

res_summary_d_s_error <- res_summary_d

save(res_summary_d_s_error, file = "./data/res_summary_d_s_error.RData")
