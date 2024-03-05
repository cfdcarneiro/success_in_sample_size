# Reproducibility Project: Cancer Biology 
#########################################


# preparing crp (Reproducibility Project Cancer Biology) data set 
# available under: https://osf.io/39s7j

#setwd("./success_in_sample_size")
source("~/success_in_sample_size/scripts/data_wrangling/load_packages.R")

crp<-read.csv(file = "~/success_in_sample_size/scripts/data_wrangling/crp.csv", header = TRUE, sep = ",")

crp$Original.sample.size<-as.numeric(crp$Original.sample.size)

#crp <- mutate(crp, index = 1:nrow(crp))

vec_not_NA <- !is.na(crp$Original.sample.size)

crp <- crp[vec_not_NA,]


#selecting relevant columns 

sel <- colnames(crp) %in% c("Original.sample.size","Original.p.value",
                            "Original.effect.size", "Replication.effect.size",
                            "Original.lower.CI","Original.upper.CI", 
                            "Replication.sample.size", "Effect.size.type", "Replication.p.value") 
crp <- crp[,sel]

#assigning standardized names to the tibble
names<-c("orig_ss", "rep_ss", "orig_p_2sided", "rep_p_2sided", "effect_size_type", "orig_d", "orig_ci_low", "orig_ci_high", "rep_d")
colnames(crp)<-names

crp$project<-"CRP"
#############################################
# generating dataset for descriptive analysis 
crp_d <- crp

write.csv(crp_d, "~/success_in_sample_size/scripts/data_wrangling/crp_d.R")
#############################################


crp <- crp %>%
  filter(effect_size_type == "Cohen's d" | effect_size_type == "Glass' delta") %>%
  filter(!is.na(orig_d) == TRUE)


for (i in 1:nrow(crp)) {
  crp$orig_se[i] <-
    ci2se(lower = crp$orig_ci_low[i], 
          upper = crp$orig_ci_high[i])
  
  
  crp$zo[i] <-
    crp$orig_d[i]/crp$orig_se[i]
}






