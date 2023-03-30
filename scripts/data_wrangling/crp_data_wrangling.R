# Reproducibility Project: Cancer Biology 
#########################################


#preparing crp (Reproducibility Project Cancer Biology) data set 
# https://osf.io/39s7j

source("./scripts/data_wrangling/load_packages.R")
crp<-read.csv(file = "./scripts/data_wrangling/crp.csv", header = TRUE, sep = ",")

crp$Original.sample.size<-as.numeric(crp$Original.sample.size)

#flowchart and filtering experiments: 
crp_flowchart<-tibble(stages = c("raw data", "no Cohen's d", "ss > 100", "ss = NA", "included data", "excluded data"), number = NA)
crp_flowchart[1,2]<-nrow(crp)
crp_flowchart[2,2] <- nrow(filter(crp, Effect.size.type != "Cohen's d"))
#crp_flowchart[3,2] <- nrow(filter(crp, Original.sample.size > 100))
crp_flowchart[4,2] <- sum(is.na(crp$Original.sample.size))
crp <- crp %>%
  filter(Original.sample.size > 0)

crp <- crp %>%
  filter(Effect.size.type == "Cohen's d")

# crp <- crp %>%                          # all studies with n > 100 are excluded because of Effect.size.type != Cohen's d
#  filter(Original.sample.size < 100) 

crp_flowchart[5,2] <- nrow(crp)
crp_flowchart[6,2] <- crp_flowchart[1,2] - crp_flowchart[5,2]




#selecting relevant columns 

sel<-colnames(crp) %in% c("Original.sample.size","Original.p.value",
                          "Original.effect.size",
                          "Original.lower.CI","Original.upper.CI") 
crp<-crp[,sel]

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


# exclusion criteria 

# crp_flowchart%>%kbl(caption = "CRP Flowchart") %>%kable_classic(full_width = F)
# exclusion_crp<-tibble(exclusion_criteria = c("Effect.size.type == Cohen's d", 
#                                              "Original.sample.size< 100",
#                                              "Original.sample.size != 0"))
# exclusion_crp%>%kbl(caption = "exclusion criteria CRP")%>%kable_classic(full_width = F)

