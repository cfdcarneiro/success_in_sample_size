
###########################################
#
#     loading datasets from simulation 
#
###########################################


#load("../data/df_combined.RData")
setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/success_in_sample_size/datasets")

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
