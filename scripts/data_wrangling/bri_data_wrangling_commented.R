######################################
# Brasilian reproducibility initiative
######################################

#setwd("~/Desktop/samplesize_for_decisionmaking")

#setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/BRI")
source("./scripts/data_wrangling/load_packages.R")
bri_flowchart <- tibble(stages = c("raw data", "ss = NA", "no Cohen's d calculation", "included data", "excluded data"), number = NA)
#bri <- read.csv(file = "./scripts/data_wrangling/bri.csv", sep = ";", header = TRUE)
bri <- read_delim("bri.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
bri_flowchart[1,2] <- nrow(bri)

# ps
# not sure why, but the above code for importing the dataset is not working directly
# then, when I import myself (either through read_delim or read_csv) the col names still include spaces, so all the code below needed adjustments to remove dots

#formating the number of rows 
bri <- bri[,1:23]

# filtering out missing values 
bri2 <- bri %>% 
  filter(`Reported Control Sample Size` != "NI", 
         `Reported Treated Sample Size` != "NI") 
bri_flowchart[2,2] <- nrow(bri2)

# ps 
# this filter excludes papers in which sample size was given as a range, without information on exact sample sizes for all groups
# (not really that sample size information was completely missing)
# in the flowchart, the ss=NA should actually be nrow(bri) - nrow(bri2)


bri3 <- bri %>% 
  filter(Delta != "NI", 
          `Pooled SD` !="NI")
bri_flowchart[3,2] <- nrow(bri3)

# ps 
# same as before, in the flowchart, the "no Cohen's d calculation" should actually be the nrow(bri) - nrow(bri3)

# selecting variable columns for further analysis
#num<-c(7,8, 10:13, 15:20, 22, 23)
# selecting rows without typos or implausible values 

 #rown<-c(1:28, 30:38,40)
  # bri<-bri[rown,num] #typo in row 29? row 39: Control.SD == 0.00 

# ps
# I didn't understand the exclusions
# first, the row numbers did not seem to match 
# also, control sd = 0 can be expected (or rather should be NA) because of normalization (paired designs, with one-sample t test)

bri_flowchart[2,2]<-nrow(bri)

bri_flowchart%>%
  kbl(caption = "BRI Flowchart") %>%
  kable_classic(full_width = F)

exclusion_bri<-tibble(exclusion_criteria = c("typo, row 29/study 6, PCR 147",
                                             "Control.SD = 0.00, row 39/study 18, PCR75"))


exclusion_bri%>%
  kbl(caption = "exclusion criteria BRI")%>%
  kable_classic(full_width = F)


#transforming all values to numeric 
sapply(1:ncol(bri), function(i) {
  bri[, i] <<- as.numeric(bri[, i])
})



bri$orig_z <- numeric(length = nrow(bri))
bri$orig_ci_low<-numeric(length = nrow(bri))
bri$orig_ci_high<-numeric(length = nrow(bri))
bri$orig_d<-numeric(length = nrow(bri))


for (i in 1:nrow(bri)) {
        re<-esc_mean_sd(
            grp2m = bri$Control.Mean[i],
            grp2sd = bri$Control.SD[i],
            grp2n = bri$Reported.Control.Sample.Size[i],
            grp1m = bri$Treated.Mean[i],
            grp1sd = bri$Treated.SD[i], 
            grp1n = bri$Reported.Treated.Sample.Size[i],
            es.type = "d")
        bri$orig_d[i]<-re$es
        bri$orig_ci_high[i]<-re$ci.hi
        bri$orig_ci_low[i]<-re$ci.lo
}     

# ps
# for comparable results, should use assumed sample sizes?

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


bri$orig_ci_low_z<-numeric(length = nrow(bri))
bri$orig_ci_high_z<-numeric(length = nrow(bri))
bri$orig_se_z<-numeric(length = nrow(bri))

for (i in 1:nrow(bri)) {
  bri$orig_z[i] <- FisherZ(d_to_r(bri$orig_d[i]))
  bri$orig_ci_low_z[i]<-FisherZ(rho = d_to_r(bri$orig_ci_low[i])) 
  bri$orig_ci_high_z[i]<-FisherZ(rho = d_to_r(bri$orig_ci_high[i]))
  bri$orig_se_z[i]<-ci2se(lower = bri$orig_ci_low_z[i], upper = bri$orig_ci_high_z[i])
}



bri$orig_p_2sided<-numeric(length = nrow(bri))

for (i in 1:nrow(bri)) {
  bri$orig_p_2sided[i]<-ci2p(lower = bri$orig_ci_low_z[i], 
                             upper = bri$orig_ci_high_z[i],
                             alternative = "two.sided")
}

bri$orig_ss <- 
  bri$Assumed.Control.Sample.Size + bri$Assumed.Treated.Sample.Size


bri$project<-"BRI"

bri4 = bri %>% mutate(total.original = as.numeric(`Assumed Treated Sample Size`)+as.numeric(`Assumed Control Sample Size`))
bri4 = bri4  %>% mutate(total.replication = `Sample Size for Replication`*2)
bri4 = bri4  %>% mutate(ratio.repl.orig = total.replication/total.original)
