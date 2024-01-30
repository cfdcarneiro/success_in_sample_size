###########################################################
#
#   two-trial significance, 95%: 
#   Replication with powered experiments at 95% level  
#   for the original effect estimate 
#
###########################################################




# source additional functions
source("./scripts/simulation/functions_for_simulation.R")
source("./scripts/data_wrangling/load_packages.R")
# source("load_packages.R")

# load combined data of all three replication projects
load("./datasets/df_combined.RData")

# calculate sample size for replication

# Approach A:
# Replication study powered for the effect size obtained 
# in the original study at 95% and 95% respectively
# take only significant studies

# how many of the original studies yielded a significant result?
sum(df_combined$orig_p_2sided < 0.05)


# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:nrow(df_combined)) %>% 
  select(study_id, everything())


# power level = 95%
rep_sample_size_a_95 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_a_95[i] <-
    ceiling(sample_size_a(data = df_combined[i, ],
                          power = 0.95))
  
}

df_combined$rep_sample_size_a_95 <- rep_sample_size_a_95 * 2



################################
#
#   Decision to replicate 
#
################################

vec_not_nec <- ifelse(df_combined$rep_sample_size_a_95 < 4 & df_combined$orig_p_2sided < 0.05, TRUE, FALSE)
vec_unfeasible <- ifelse(df_combined$rep_sample_size_a_95 > 280 | df_combined$orig_p_2sided >= 0.05, TRUE, FALSE)
vec_go <- ifelse(vec_not_nec == FALSE & vec_unfeasible == FALSE, TRUE, FALSE)

vec_conducted <- vector(length = nrow(df_combined))
vec_conducted[which(vec_go == TRUE)] <- "yes"
vec_conducted[which(vec_unfeasible == TRUE)] <- "unfeasible"
vec_conducted[which(vec_not_nec == TRUE)] <- "not_necessary"

df_combined$conducted <- vec_conducted

decision1_a_95 <- df_combined%>%group_by(conducted)%>%summarize(n = n())%>%mutate(approach = "standard_95")
decision1_a_95


####################################
#
#
#     Simulate replication studies for 
#     chosen experiments 
#
#####################################


##################################
### Simulate replication study ###
### SCENARIO 1: M-error ##########
##################################

# set seed to reproduce results
set.seed(84335)

# number of experiments we run for each true underlying effect size
n_exp <- 1

# first use replication sample size with 95% power

# for simulation only take significant results
study_id_vector <- which(df_combined$conducted == "yes")


list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id] / 2)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}


row_names <- NULL
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_a_95[study_id_vector],
         es_true = df_combined$orig_d[study_id_vector] / 2,
         sample_size_approach = "a_95pct",
         project = df_combined$project[study_id_vector],
         scenario = "m_error")


res_summary_a_95 <- left_join(df_combined, res_summary_a_95) 

res_summary_a_95 <- 
  res_summary_a_95 %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))


res_summary_a_95_m_error <- res_summary_a_95

save(res_summary_a_95_m_error, file = "./datasets/res_summary_a_95_m_error.RData")


##################################
### Simulate replication study ###
### SCENARIO 2: Null-effect ######
##################################

# set seed to reproduce results
set.seed(84335)

# number of experiments we run for each true underlying effect size
n_exp <- 10

# first use replication sample size with 95% power

# for simulation only take significant results
study_id_vector <- which(df_combined$conducted == "yes")


list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(0,
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = 0)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}


row_names <- NULL
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_a_95[study_id_vector],
         es_true = 0,
         sample_size_approach = "a_95pct",
         project = df_combined$project[study_id_vector],
         scenario = "null_effect")


res_summary_a_95 <- left_join(df_combined, res_summary_a_95) 

res_summary_a_95 <- 
  res_summary_a_95 %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))


res_summary_a_95_null <- res_summary_a_95

save(res_summary_a_95_null, file = "./datasets/res_summary_a_95_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: S-error ######
##################################

# set seed to reproduce results
set.seed(84335)

# number of experiments we run for each true underlying effect size
n_exp <- 10

# first use replication sample size with 95% power

# for simulation only take significant results
study_id_vector <- which(df_combined$conducted == "yes")


list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_a_95[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]))
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}


row_names <- NULL
col_names <- c("study_id", "p_value", "effect")


res_summary_rep_a_95 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_a_95 <- 
  res_summary_rep_a_95 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_a_95[study_id_vector],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "a_95pct",
         project = df_combined$project[study_id_vector],
         scenario = "s_error")


res_summary_a_95 <- left_join(df_combined, res_summary_a_95) 

res_summary_a_95 <- 
  res_summary_a_95 %>%
  select(c("study_id", "n_success", "N", "pct_success", "orig_ss", 
           "rep_sample_size", "es_true", "sample_size_approach", 
           "project", "scenario", "conducted"))


res_summary_a_95_s_error <- res_summary_a_95

save(res_summary_a_95_s_error, file = "./datasets/res_summary_a_95_s_error.RData")


