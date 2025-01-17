###########################################################
#
#   Replication powered for a SESOI = 0.5 
#
###########################################################



# source packages and additional functions
source("./scripts/simulation/functions_for_simulation.R")
source("./scripts/data_wrangling/load_packages.R")

# load combined data of all three replication projects
load("./datasets/df_combined.RData")

# calculate sample size for replication

# Approach B:
# Replication study powered at 50% for the smallest effect size of interest (SESOI)
# SESOI 1 = 0.5

# set SESOI to 0.5

# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:nrow(df_combined)) %>% 
  select(study_id, everything())

SESOI <- 0.5 

rep_sample_size_b_0.5 <- NULL

for (i in 1:nrow(df_combined)) {
  
  rep_sample_size_b_0.5[i] <-
    ceiling(sample_size_b(data = df_combined[i, ],
                          SESOI = SESOI,
                          power = 0.5))
  
}

df_combined$rep_sample_size_b_0.5 <- rep_sample_size_b_0.5 * 2



################################
#
#   Decision to replicate 
#
################################

vec_not_nec <- ifelse(df_combined$rep_sample_size_b_0.5 < 4, TRUE, FALSE)
vec_unfeasible <- ifelse(df_combined$rep_sample_size_b_0.5 > 280 | df_combined$orig_ci_high < SESOI, TRUE, FALSE)
vec_go <- ifelse(vec_not_nec == FALSE & vec_unfeasible == FALSE, TRUE, FALSE)

vec_conducted <- vector(length = nrow(df_combined))
vec_conducted[which(vec_go == TRUE)] <- "yes"
vec_conducted[which(vec_unfeasible == TRUE)] <- "unfeasible"
vec_conducted[which(vec_not_nec == TRUE)] <- "not_necessary"

df_combined$conducted <- vec_conducted

decision1_b_0.5 <- df_combined%>%group_by(conducted)%>%summarize(n = n())%>%mutate(approach = "SESOI_0.5")
decision1_b_0.5


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
n_exp <- 1000

# first use replication sample size with 95% power

# for simulation only take significant results
study_id_vector <- which(df_combined$conducted == "yes")

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] / 2)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "p_value", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= SESOI),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = df_combined$orig_d / 2,
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "m_error")

res_summary_b_0.5 <- left_join(df_combined, res_summary_b_0.5)


res_summary_b_0.5_m_error <- res_summary_b_0.5

save(res_summary_b_0.5_m_error, file = "./datasets/res_summary_b_0.5_m_error.RData")


##################################
### Simulate replication study ###
### SCENARIO 2: Null-effect ######
##################################



set.seed(84335)

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = 0,
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = 0)
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}

# rep_data_summary[[1]]

row_names <- NULL
col_names <- c("study_id", "p_value", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= SESOI),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = 0,
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "null_effect")


res_summary_b_0.5 <- left_join(df_combined, res_summary_b_0.5)


res_summary_b_0.5_null <- res_summary_b_0.5

save(res_summary_b_0.5_null, file = "./datasets/res_summary_b_0.5_null.RData")




#################################
### Simulate replication study ###
### SCENARIO 3: S - error   ######
##################################

set.seed(84335)

# first use replication sample size with SESOI = 0.5
list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_b_0.5[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = study_id_vector[study_id],
               ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]))
      
    }
    
    list_rep_data <- rep_data
    
  }

rep_data_summary <- list()

plan(multisession, workers = 10)
for (i in 1:length(study_id_vector)) {
  
  rep_data_summary[[i]] <- 
    future_map(list_rep_data[[i]], get_summary_study_rep)
  
}


row_names <- NULL
col_names <- c("study_id", "p_value", "effect")

res_summary_rep_b_0.5 <- 
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_b_0.5 <- 
  res_summary_rep_b_0.5 %>% 
  group_by(study_id) %>% 
  summarize(n_success = sum(p_value <= 0.05 & effect >= 0.5),
            N = n(),
            pct_success = n_success/N * 100) %>% 
  mutate(orig_ss = df_combined$orig_ss,
         rep_sample_size = df_combined$rep_sample_size_b_0.5,
         es_true = df_combined$orig_d - (1.25 * df_combined$orig_d),
         sample_size_approach = "b_0.5",
         project = df_combined$project,
         scenario = "s_error")


res_summary_b_0.5 <- left_join(df_combined, res_summary_b_0.5)


res_summary_b_0.5_s_error <- res_summary_b_0.5

save(res_summary_b_0.5_s_error, file = "./datasets/res_summary_b_0.5_s_error.RData")

res_summary_b_0.5 <- 
  bind_rows(res_summary_b_0.5_m_error, 
            res_summary_b_0.5_null, 
            res_summary_b_0.5_s_err)


