###########################################################
#
#   safeguard power analysis: 
#   Replication powered lower 80% confidence interval 
# 
#
###########################################################



# source packages and additional functions
source("../scripts/simulation/functions_for_simulation.R")
source("../scripts/data_wrangling/load_packages.R")


# load combined data of all three replication projects
load("./datasets/df_combined.RData")
# calculate sample size for replication


# add column study_id to loop over later
df_combined <-
  df_combined %>% 
  mutate(study_id = 1:nrow(df_combined)) %>% 
  select(study_id, everything())


# Approach C:
# Replication study powered at 80% for the lower 80% confidence bound 
# obtained from the original study
# if lower CI bound < 0, the rep_sample_size_c column will display NA

# use des() function from compute.es package to compute CI around orig_d
# ci_80_low is the lower bound of the 80% CI

df_combined$ci_80_low <- NULL

for (i in 1:nrow(df_combined)) {
  
  df_combined$ci_80_low[i] <-
    
    des(d =df_combined$ orig_d[i], 
        n.1 = df_combined$orig_ss[i] / 2, 
        n.2 = df_combined$orig_ss[i] / 2,
        level = 80)$l.d
  
}

summary(df_combined$ci_80_low)

# now compute replication sample size with the lower 80% confidence bound
rep_sample_size_c <- NULL

for (i in 1:nrow(df_combined)) {
  
  if (df_combined$ci_80_low[i] > 0) {
    
    rep_sample_size_c[i] <-
      ceiling(sample_size_c(data = df_combined[i, ],
                            power = 0.8))
    
  } else {
    
    rep_sample_size_c[i] <- NA
    
  }
  
}

df_combined$rep_sample_size_c <- rep_sample_size_c * 2


################################
#
#   Decision to replicate 
#
################################

vec_not_nec <- ifelse(df_combined$rep_sample_size_c < 4, TRUE, FALSE)
vec_unfeasible <- ifelse(df_combined$rep_sample_size_c >= 280 | df_combined$ci_80_low < 0, TRUE, FALSE)
vec_go <- ifelse(vec_not_nec == FALSE & vec_unfeasible == FALSE, TRUE, FALSE)

vec_conducted <- vector(length = nrow(df_combined))
vec_conducted[which(vec_go == TRUE)] <- "yes"
vec_conducted[which(vec_unfeasible == TRUE)] <- "unfeasible"
vec_conducted[which(vec_not_nec == TRUE)] <- "not_necessary"

df_combined$conducted <- vec_conducted

decision1_c <- 
  df_combined%>%
  group_by(conducted)%>%
  dplyr::summarize(n = n())%>%
  mutate(approach = "safeguard")
decision1_c



##################################
### Simulate replication study ###
### SCENARIO 1: M - error ########
##################################



# set seed to reproduce results
set.seed(84335)

# number of experiments we run for each true underlying effect size
n_exp <- 1000

study_id_vector <- which(df_combined$conducted == "yes")

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(df_combined$orig_d[study_id] / 2,
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
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

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_c[study_id_vector],
         es_true = df_combined$orig_d[study_id_vector] / 2,
         sample_size_approach = "c",
         project = df_combined$project[study_id_vector],
         scenario = "m_error")

res_summary_c <- left_join(df_combined, res_summary_c)



save(res_summary_c, file = "./datasets/res_summary_c_m_error.RData")


##################################
### Simulate replication study ###
### SCENARIO 2: null effect ######
##################################

set.seed(84335)

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(0,
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
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

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_c[study_id_vector],
         es_true = 0,
         sample_size_approach = "c",
         project = df_combined$project[study_id_vector],
         scenario = "null_effect")

res_summary_c <- left_join(df_combined, res_summary_c)


save(res_summary_c, file = "./datasets/res_summary_c_null.RData")


##################################
### Simulate replication study ###
### SCENARIO 3: s- error #########
##################################

set.seed(84335)

list_rep_data <- 
  
  foreach(study_id = study_id_vector) %do% {
    
    rep_data <- list()
    
    for(i in 1:n_exp) {
      
      rep_data[[i]] <- 
        generate_study(ES_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
                       sample_size = df_combined$rep_sample_size_c[study_id])
      
      rep_data[[i]] <-
        rep_data[[i]] %>% 
        mutate(study_id = df_combined$study_id[study_id],
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

res_summary_rep_c <-
  as_tibble(matrix(unlist(rep_data_summary),
                   nrow = n_exp * length(study_id_vector), byrow = TRUE,
                   dimnames = list(c(row_names),
                                   c(col_names))))

res_summary_c <-
  res_summary_rep_c %>%
  group_by(study_id) %>%
  summarize(n_success = sum(p_value <= 0.05),
            N = n(),
            pct_success = n_success/N * 100) %>%
  mutate(orig_ss = df_combined$orig_ss[study_id_vector],
         rep_sample_size = df_combined$rep_sample_size_c[study_id_vector],
         es_true = df_combined$orig_d[study_id] - (1.25 * df_combined$orig_d[study_id]),
         sample_size_approach = "c",
         project = df_combined$project[study_id_vector],
         scenario = "s_error")

res_summary_c <- left_join(df_combined, res_summary_c)


save(res_summary_c, file = "./datasets/res_summary_c_s_error.RData")



