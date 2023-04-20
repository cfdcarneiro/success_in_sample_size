# Confirmatory Preclinical Studies 
##################################

# setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/success_in_sample_size")

source("./scripts/data_wrangling/load_packages.R")
cps<-read.csv(file = "./scripts/data_wrangling/cps.csv", sep = ";", header = TRUE)

sapply(2:6, function(i) {
  cps[, i] <<- as.numeric(gsub(",", ".", cps[, i]))
})

# generating dataset for descriptive analysis 

cps_d <- cps

write.csv(cps_d, "./scripts/data_wrangling/cps_d.R")

#############################################

cps_flowchart <- tibble(stages = c("raw data", "no Cohen's d calculation", "included data", "excluded data"), number = NA)

cps_flowchart[1,2] <- nrow(cps)
cps_flowchart[2,2] <- nrow(filter(cps, effect_size_type != "Cohen's d" | is.na(effect_size_type)))
cps_flowchart[4,2] <- nrow(filter(cps, effect_size_type != "Cohen's d" | is.na(effect_size_type)))

cps <- filter(cps, effect_size_type == "Cohen's d")
cps_flowchart[3,2] <- nrow(cps)

cps_flowchart%>%
  kbl(caption = "CPS Flowchart") %>%
  kable_classic(full_width = F)


#
# value transformation 
# 

cps$orig_z <- numeric(length(nrow(cps)))
cps$orig_ci_low_z<-numeric(length(nrow(cps)))
cps$orig_ci_high_z<-numeric(length(nrow(cps)))
cps$orig_se_z<-numeric(length(nrow(cps)))
cps$orig_p_2sided<-numeric(length = nrow(cps))
cps$project<-"CPS"

for (i in 1:nrow(cps)) {
  cps$orig_z[i] <- FisherZ(d_to_r(cps$orig_d[i]))
  cps$orig_ci_high_z[i]<-FisherZ(d_to_r(cps$orig_ci_high[i]))
  cps$orig_ci_low_z[i]<-FisherZ(d_to_r(cps$orig_ci_low[i]))
  cps$orig_se_z[i]<-ci2se(lower = cps$orig_ci_low_z[i], upper  = cps$orig_ci_high_z[i])
  cps$orig_p_2sided[i]<-ci2p(lower = cps$orig_ci_low_z[i], 
                             upper = cps$orig_ci_high_z[i],
                             alternative = "two.sided")
}




