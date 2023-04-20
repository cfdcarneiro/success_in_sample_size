# Reproducibility Project: Cancer Biology 
#########################################


#preparing crp (Reproducibility Project Cancer Biology) data set 
# https://osf.io/39s7j

source("./scripts/data_wrangling/load_packages.R")
crp<-read.csv(file = "./scripts/data_wrangling/crp.csv", header = TRUE, sep = ",")

crp$Original.sample.size<-as.numeric(crp$Original.sample.size)

#crp <- mutate(crp, index = 1:nrow(crp))

vec_not_NA <- !is.na(crp$Original.effect.size)

crp <- crp[vec_not_NA,]




#############################################
# generating dataset for descriptive analysis 
crp_d <- crp

write.csv(crp_d, "./scripts/data_wrangling/crp_d.R")
#############################################


crp <- crp %>%
  filter(Effect.size.type == "Cohen's d" | Effect.size.type == "Glass' delta")



#selecting relevant columns 

sel <- colnames(crp) %in% c("Original.sample.size","Original.p.value",
                          "Original.effect.size",
                          "Original.lower.CI","Original.upper.CI") 
crp <- crp[,sel]

crp <- 
  filter(crp, is.na(crp$Original.effect.size) == FALSE)

#assigning standardized names to the tibble
names<-c("orig_ss", "orig_p_2sided", "orig_d", "orig_ci_low", "orig_ci_high")
colnames(crp)<-names

crp$orig_z <- numeric(length = nrow(crp))
crp$orig_ci_low_z<-numeric(length = nrow(crp))
crp$orig_ci_high_z<-numeric(length = nrow(crp))
crp$orig_se_z<-numeric(length = nrow(crp))

for (i in 1:nrow(crp)) {
  crp$orig_z[i] <- FisherZ(d_to_r(crp$orig_d[i]))
  crp$orig_ci_high_z[i]<-FisherZ(d_to_r(crp$orig_ci_high[i]))
  crp$orig_ci_low_z[i]<-FisherZ(d_to_r(crp$orig_ci_low[i]))
  crp$orig_se_z[i]<-ci2se(lower = crp$orig_ci_low_z[i], upper = crp$orig_ci_high_z[i])
}


crp$project<-"CRP"

#orig_se is on the z-scale 





