# Confirmatory Preclinical Studies 
##################################

#setwd("./success_in_sample_size")

source("./scripts/data_wrangling/load_packages.R")
cps<-read.csv(file = "./scripts/data_wrangling/cps.csv", sep = ";", header = TRUE)

cps <- select(cps, -c("experiment_id"))

sapply(1:5, function(i) {
  cps[, i] <<- as.numeric(gsub(",", ".", cps[, i]))
})

# generating dataset for descriptive analysis 
cps$project<-"CPS"

cps_d <- cps

write.csv(cps_d, "./scripts/data_wrangling/cps_d.R")

#############################################


cps <- 
  cps %>% 
  filter(effect_size_type == "Cohen's d")

for (i in 1:nrow(cps)) {
  cps$orig_se[i] <-
    ci2se(lower = cps$orig_ci_low[i], 
          upper = cps$orig_ci_high[i])
  
  cps$orig_p_2sided[i] <- 
    ci2p(lower = cps$orig_ci_low[i], 
         upper = cps$orig_ci_high[i],
         conf.level = 0.95, 
         alternative = "two.sided")
   
  cps$zo[i] <-
    cps$orig_d[i]/cps$orig_se[i]
}







