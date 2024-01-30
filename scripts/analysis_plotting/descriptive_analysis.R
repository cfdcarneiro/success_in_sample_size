# Descriptive data analysis

setwd("./success_in_sample_size")
source("./scripts/data_wrangling/merge_data_replication_projects.R")
source("./scripts/data_wrangling/merge_data_descriptive.R")

#######################
#
#     Settings 
#
#######################
cols <- RColorBrewer::brewer.pal(8, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(size = 22),
                plot.subtitle = element_text( size = 20),
                axis.title.x = element_text(size = 22), 
                axis.title.y = element_text(size = 22, vjust = 2), 
                axis.text = element_text(size = 20), 
                legend.text = element_text(size = 20), 
                legend.title = element_text(size = 20), 
                plot.margin = margin(1,1,1.2,1, "cm"))

#####################################################
#                                                   #
#    preparation for analysis &  visualization      #
#                                                   #
#####################################################

t1_des <-   
  df_descriptive %>% 
  group_by(project) %>% 
  dplyr::summarize(median_rep_ss = median(rep_ss, na.rm = T), 
                   median_orig_ss = median(orig_ss, na.rm = T), 
                   median_d = median(orig_d, na.rm = T), 
                   rep_ss_quan25 = quantile(rep_ss, probs = 0.25, na.rm = T), 
                   rep_ss_quan75 = quantile(rep_ss, probs = 0.75, na.rm = T), 
                   orig_ss_quan25 = quantile(orig_ss, probs = 0.25, na.rm = T), 
                   orig_ss_quan75 = quantile(orig_ss, probs = 0.75, na.rm = T), 
                   orig_d_quan25 = quantile(orig_d, probs = 0.25, na.rm = T), 
                   orig_d_quan75 = quantile(orig_d, probs = 0.75, na.rm = T))



t2_des <- 
  df_descriptive %>%
  dplyr::summarize(median_rep_ss = median(rep_ss, na.rm = T), 
                   median_orig_ss = median(orig_ss, na.rm  = T), 
                   median_d = median(orig_d, na.rm = T), 
                   rep_ss_quan25 = quantile(rep_ss, probs = 0.25, na.rm = T), 
                   rep_ss_quan75 = quantile(rep_ss, probs = 0.75, na.rm = T), 
                   orig_ss_quan25 = quantile(orig_ss, probs = 0.25, na.rm = T), 
                   orig_ss_quan75 = quantile(orig_ss, probs = 0.75, na.rm = T), 
                   orig_d_quan25 = quantile(orig_d, probs = 0.25, na.rm = T), 
                   orig_d_quan75 = quantile(orig_d, probs = 0.75, na.rm = T))



#########################
#                       #
#       BRI             #
#                       #
#########################

qu25 <- quantile(bri_des$orig_d, probs = 0.25, na.rm = T)
qu50 <- quantile(bri_des$orig_d, probs = 0.5, na.rm = T) 
qu75 <- quantile(bri_des$orig_d, probs = 0.75, na.rm = T) 

bri_d$es <- NULL

bri_des$es <- ifelse(bri_des$orig_d > qu75, "> 4.3", 0)
bri_des$es <- ifelse(bri_des$orig_d > qu50 & bri_des$orig_d <= qu75, "2.4 - 4.3", bri_des$es)
bri_des$es <- ifelse(bri_des$orig_d > qu25 & bri_des$orig_d <=qu50, "1.5 - 2.4", bri_des$es)
bri_des$es <- ifelse(bri_des$orig_d <= qu25, "< 1.5", bri_des$es)

bri_des$es <- factor(bri_des$es, levels = c("< 1.5", "1.5 - 2.4", "2.4 - 4.3", "> 4.3"))




#######################
#                     #
#     CRP             #
#                     #
#######################

sum(crp_des$orig_ss>40 | crp_des$rep_ss> 100)
# restriction on y and x values applied --> 13/135 data points excluded to improve visualization 

# creating bins for distribution of original effect sizes 
qu25 <- quantile(crp_des$orig_d, probs = 0.25, na.rm = T)
qu50 <- quantile(crp_des$orig_d, probs = 0.5, na.rm = T) 
qu75 <- quantile(crp_des$orig_d, probs = 0.75, na.rm = T) 

crp_d$es <- NULL

crp_d$es <- ifelse(crp_d$orig_d > qu75, "> 5.5", 0)
crp_d$es <- ifelse(crp_d$orig_d > qu50 & crp_d$orig_d <= qu75, "2.3 - 5.5", crp_d$es)
crp_d$es <- ifelse(crp_d$orig_d > qu25 & crp_d$orig_d <=qu50, "0.8 - 2.3", crp_d$es)
crp_d$es <- ifelse(crp_d$orig_d <= qu25, "< 0.8", crp_d$es)

crp_d$es <- factor(crp_d$es, labels = c("< 0.8", "0.8 - 2.3", "2.3 - 5.5", "> 5.5"))


# sample size choices in the replication vs original experiment 
crp_d$reduced <- crp_d$orig_ss > crp_d$rep_ss
crp_d$equal <- crp_d$orig_ss == crp_d$rep_ss
crp_d$increased <- crp_d$orig_ss < crp_d$rep_ss

sum(crp_d$reduced)


# creating binary variable for conventional statistical significance 
crp_d$rep_sig <- crp_d$rep_p_2sided< 0.05

rep_sig_NA <- which(!is.na(crp_d$rep_sig))

crp_success <- crp_d[rep_sig_NA,]

# calculating relative effect size 
crp_d$relative_d <- crp_d$rep_d/crp_d$orig_d

crp_d$col_scheme <- NULL 


###################################
#                                 #
#         Analysis                #
#                                 #
###################################

###############################################################
#                                                             #
# reduced, equal, increased sample sizes in the replication   #
#                                                             #
###############################################################
 

sum(df_descriptive$orig_ss > df_descriptive$rep_ss, na.rm = T)/ nrow(df_descriptive)

df_descriptive$reduced <- 
  df_descriptive$orig_ss > df_descriptive$rep_ss
sum(df_descriptive$reduced == TRUE)


df_descriptive$same <- 
  df_descriptive$orig_ss == df_descriptive$rep_ss
sum(df_descriptive$same == TRUE)/nrow(df_descriptive)

df_descriptive$increased <- 
  df_descriptive$orig_ss < df_descriptive$rep_ss

df_descriptive %>%
  group_by(project)%>%
  dplyr::summarize(n = n(),
                   n_reduced = sum(reduced), 
                   perc_reduced = n_reduced/n, 
                   n_same = sum(same), 
                   perc_same = n_same/n, 
                   n_increased = sum(increased), 
                   perc_increased = n_increased/n)


#############################
#
# Reduced sample sizes in CRP 
# 
##############################


crp_d$reduced <- 
  crp_d$orig_ss > crp_d$rep_ss

crp_d$equal <- 
  crp_d$orig_ss == crp_d$rep_ss

crp_d$increased <- 
  crp_d$orig_ss < crp_d$rep_ss

sum(crp_d$reduced)

crp_d$rep_sig <- crp_d$rep_p_2sided< 0.05

rep_sig_NA <- which(!is.na(crp_d$rep_sig))

crp_success <- crp_d[rep_sig_NA,]

crp_success%>%
  group_by(reduced)%>%
  dplyr::summarize(n = n(), 
                   rep_success = sum(rep_sig == TRUE), 
                   perc_success = rep_success/n)



summary(crp_d$orig_ss)

# number of significant original studies 
sum(crp_d$orig_p_2sided < 0.05, na.rm = T)
sum(is.na(crp_d$orig_p_2sided))


#########
#   C
#########
# Shrinkage in CRP effect sizes 

# number of significant original studies 
sum(crp_d$orig_p_2sided < 0.05, na.rm = T)

# calculating the relative effect size 
crp_d$rel_es <- crp_d$rep_d/crp_d$orig_d

# relative increase in effect size 
sum(crp_d$rel_es >= 1, na.rm = T)/nrow(crp_d)

# magnitude error 
sum(crp_d$rel_es < 1 & crp_d$rel_es >= 0, na.rm = T)/nrow(crp_d)

# sign error 
sum(crp_d$rel_es < 0, na.rm = T)/nrow(crp_d)


# shrinkage 
summary(crp_d$rel_es)

summary(crp_d$rel_es[crp_d$rel_es > 0 & crp_d$rel_es < 1])

summary(crp_d$rel_es[crp_d$rel_es < 0])






