######################################
# Brasilian reproducibility initiative
######################################


#setwd("./success_in_sample_size")
source("./scripts/data_wrangling/load_packages.R")
bri <- read.csv(file = "./scripts/data_wrangling/bri.csv", sep = ";", header = TRUE)

#formating the number of rows 
bri <- bri[,1:23]



# filtering out missing values 
bri <- bri %>% 
  filter(Reported.Control.Sample.Size != "NI", 
                  Reported.Treated.Sample.Size != "NI") 



# selecting variable columns for further analysis
num<-c(7,8, 10:13, 15:20, 22, 23)
bri<-bri[,num] 



#transforming all values to numeric 
sapply(1:ncol(bri), function(i) {
  bri[, i] <<- as.numeric(bri[, i])
})


bri$zo <- numeric(length = nrow(bri))
bri$orig_ci_low<-numeric(length = nrow(bri))
bri$orig_ci_high<-numeric(length = nrow(bri))
bri$orig_d<-numeric(length = nrow(bri))
bri$orig_p_2sided <- numeric(length = nrow(bri))


for (i in 1:nrow(bri)) {
        re<-esc_mean_sd(
            grp2m = bri$Control.Mean[i],
            grp2n = bri$Assumed.Control.Sample.Size[i],
            grp2sd =  bri$Control.SD[i],
            grp1m = bri$Treated.Mean[i],
            grp1n = bri$Assumed.Treated.Sample.Size[i],
            grp1sd = bri$Treated.SD[i],
            es.type = "d")
        bri$orig_d[i]<-re$es
        bri$orig_ci_high[i]<-re$ci.hi
        bri$orig_ci_low[i]<-re$ci.lo
        bri$zo[i] <- abs(re$es/re$se)
        bri$orig_p_2sided[i] <- z2p(bri$zo[i], alternative = "two.sided") # original two-sided p-value
}     



vec_neg <- which(bri$orig_d < 0)


bri$orig_d[vec_neg] <- abs(bri$orig_d[vec_neg])

bri$orig_ci_low2[vec_neg] <- bri$orig_ci_high[vec_neg]
bri$orig_ci_high2[vec_neg] <- bri$orig_ci_low[vec_neg]
bri$orig_ci_low[vec_neg] <- bri$orig_ci_low2[vec_neg]
bri$orig_ci_high[vec_neg] <- bri$orig_ci_high2[vec_neg]



bri$orig_ci_high <- abs(bri$orig_ci_high) 

for (i in 1:length(bri$orig_ci_low[vec_neg])) {
  if (bri$orig_ci_low[vec_neg][i] < 0 ) {
    bri$orig_ci_low[vec_neg][i] <- abs(bri$orig_ci_low[vec_neg][i]) 
  } else {bri$orig_ci_low[vec_neg][i] <- -1* bri$orig_ci_low[vec_neg][i] }
}

bri$orig_ci_high2 <- NULL
bri$orig_ci_low2 <- NULL


bri$orig_ss <- 
  bri$Assumed.Control.Sample.Size + bri$Assumed.Treated.Sample.Size

bri$rep_ss <-
  bri$Sample.Size.for.Replication * 2 


bri$project <- "BRI"

bri$effect_size_type <- "Cohen's d"


sel <- colnames(bri) %in% c("orig_ss", "rep_ss", "orig_p_2sided", "effect_size_type", "orig_d", 
                     "orig_ci_low", "orig_ci_high", "zo", "project")

bri <- bri[,sel]

#############################
# generating data set for descriptive analysis 

bri_d <- bri

write.csv(bri_d, "./scripts/data_wrangling/bri_d.R")

#############################################


