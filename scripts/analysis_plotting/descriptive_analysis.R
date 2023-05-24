# Descriptive data visualization & analysis

source("./scripts/data_wrangling/load_packages.R")
source("./scripts/data_wrangling/bri_data_wrangling.R")
source("./scripts/data_wrangling/crp_data_wrangling.R")
source("./scripts/data_wrangling/cps_data_wrangling.R")
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

########################
#
#     visualization
#
########################

df_descriptive <- read.csv("./scripts/data_wrangling/df_descriptive.R")

########################
# 
#       BRI
#
#######################



fig_bri.sample.size<-
  ggplot(bri_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              alpha = 0.5, width = 0.5, size = 7, color = cols[4])+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  labs(x = "total original sample size", 
       y = "total replication sample size")+
  ggtitle("Brazilian Reproducibility Initiative")+ 
  letter+
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[4]))


fig_bri.sample.size

qu25 <- quantile(bri_des$orig_d, probs = 0.25, na.rm = T)
qu50 <- quantile(bri_des$orig_d, probs = 0.5, na.rm = T) 
qu75 <- quantile(bri_des$orig_d, probs = 0.75, na.rm = T) 

bri_d$es <- NULL

bri_des$es <- ifelse(bri_des$orig_d > qu75, "> 4.3", 0)
bri_des$es <- ifelse(bri_des$orig_d > qu50 & bri_des$orig_d <= qu75, "2.4 - 4.3", bri_des$es)
bri_des$es <- ifelse(bri_des$orig_d > qu25 & bri_des$orig_d <=qu50, "1.5 - 2.4", bri_des$es)
bri_des$es <- ifelse(bri_des$orig_d <= qu25, "< 1.5", bri_des$es)

bri_des$es <- factor(bri_des$es, levels = c("< 1.5", "1.5 - 2.4", "2.4 - 4.3", "> 4.3"))




######################
#
#     CRP
#
######################

fig_crp.sample.size<-
  ggplot(crp_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              color = cols[1], alpha = 0.5, width = 0.5, size = 7)+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  ylab("total replication sample size")+
  xlab("total original sample size")+
  ggtitle("Reproducibility Project: Cancer Biology")+
  coord_cartesian(xlim = c(0,40), ylim= c(0,100))+
  letter+
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[1]))


sum(crp_des$orig_ss>40 | crp_des$rep_ss> 100)
# restriction on y and x values applied --> 13/135 data points excluded to improve visualization 

fig_crp.sample.size


qu25 <- quantile(crp_des$orig_d, probs = 0.25, na.rm = T)
qu50 <- quantile(crp_des$orig_d, probs = 0.5, na.rm = T) 
qu75 <- quantile(crp_des$orig_d, probs = 0.75, na.rm = T) 

crp_d$es <- NULL

crp_d$es <- ifelse(crp_d$orig_d > qu75, "> 5.5", 0)
crp_d$es <- ifelse(crp_d$orig_d > qu50 & crp_d$orig_d <= qu75, "2.3 - 5.5", crp_d$es)
crp_d$es <- ifelse(crp_d$orig_d > qu25 & crp_d$orig_d <=qu50, "0.8 - 2.3", crp_d$es)
crp_d$es <- ifelse(crp_d$orig_d <= qu25, "< 0.8", crp_d$es)

crp_d$es <- factor(crp_d$es, labels = c("< 0.8", "0.8 - 2.3", "2.3 - 5.5", "> 5.5"))

crp_d$reduced <- crp_d$orig_ss > crp_d$rep_ss
crp_d$equal <- crp_d$orig_ss == crp_d$rep_ss
crp_d$increased <- crp_d$orig_ss < crp_d$rep_ss

sum(crp_d$reduced)

crp_d$rep_sig <- crp_d$rep_p_2sided< 0.05


rep_sig_NA <- which(!is.na(crp_d$rep_sig))

crp_success <- crp_d[rep_sig_NA,]




######################
#
#     CPS
# 
######################


fig_cps.sample.size <-
  ggplot(cps_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              color = cols[7], alpha = 0.5, width = 0.5, size = 7)+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  ylab("total replication sample size")+
  xlab("total original sample size")+
  ggtitle("Confirmatory Preclinical Studies")+
  letter+ 
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[7]))


fig_cps.sample.size



######################
#
#     Fig 2 
#
######################



# p = list(fig_crp.sample.size,fig_bri.sample.size,fig_cps.sample.size) %>% map(~.x + labs(x=NULL, y=NULL))
# 
# fig2 <- grid.arrange(
#   grobs = p,
#   nrow = 1, ncol = 3,
# #  top = textGrob("Sample size choices in preclinical replication projects", x = 0, hjust = 0, vjust = 0.3, gp = gpar(fontface = "bold", cex = 2)),
#   left = textGrob("total replication sample size", rot = 90, gp = gpar(cex = 1.5)),
#   bottom = textGrob("total original sample size", gp = gpar(cex = 1.5)),
#   vp=viewport(width=0.95, height=0.95)
# )
# fig2

fig1_a <- ggarrange(fig_crp.sample.size, fig_bri.sample.size, fig_cps.sample.size, 
                    nrow = 1, 
                    labels = c("a)"),
                    font.label = list(face = "bold", size = 24))

fig1_a
#####################
#
#     Fig 3 
#
#####################

# fig3a<-  ggplot(crp_d) + 
#   geom_jitter(aes(x = orig_ss, y = rep_ss, color = es, shape = es),
#               alpha = 0.5, width = 0.3, size = 3)+
#   geom_abline(intercept = 0, slope = 1, lty = "dashed")+
#   coord_cartesian(xlim = c(0,40), ylim= c(0,100))+
#   labs(x = "total original sample size", 
#        y = "total replication sample size", 
#        color = "original effect size", 
#        shape = "")+
# #  ggtitle("Inverse relation between original effect size and replication sample size", 
# #          subtitle = "Sample size choices in Reproducibility Project: Cancer Biology")+
#   letter
# 
# fig3a



fig3 <-  
  ggplot(bri_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss, color = es, shape = es),
              alpha = 0.8, width = 0.5, size = 7)+
  scale_shape(guide = "none")+
  scale_color_manual(values = c("#E7298A", "#AD1FD1FF", "#E592E8FF", "#E82AE8FF"))+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  labs(x = "total original sample size", 
       y = "total replication sample size", 
       color = "original effect size", 
       shape = "")+
  #  ggtitle("Inverse relation between original effect size and replication sample size", 
  #          subtitle = "Sample size choices in Brasilian Reproducibility Initiative")+
  letter

fig3


###################
#
#     Fig 4 
#
###################


fig4 <- 
  ggplot(crp_success) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss, color = rep_sig, shape = rep_sig),
              alpha = 0.8, width = 0.5, size = 7)+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  coord_cartesian(xlim = c(0,40), ylim= c(0,100))+
  scale_color_manual(labels = c("no", "yes"), 
                     values = c("#009E73", "#63EDC8FF"))+
  scale_shape(guide = "none")+
  labs(x = "total original sample size", 
       y = "total replication sample size", 
       color = "p < 0.05")+
  letter+ 
  theme(legend.position = "right")

fig4


fig1_bc <- 
  ggarrange(fig3, fig4, 
            nrow = 1,
            labels = c("b)", "c)"),
            font.label = list(face = "bold", 
                              size = 24))
fig1_bc



#####################
#
#     Fig 5 
#
#####################
# 
# vec_na_orig_d <- !is.na(crp_d$orig_d) & !is.na(crp_d$rep_d)
# sum(vec_na_orig_d)
# 
# 
# fig5 <- ggplot(crp_d[vec_na_orig_d,]) + 
#   geom_jitter(aes(x = orig_d, y = rep_d), 
#               alpha = 0.5, width = 0.3, size = 4, color = cols[4])+ 
#   geom_abline(intercept = 0, slope = 1, lty = "dashed")+
#   coord_cartesian(xlim = c(0,10), ylim= c(0,10))+
#   labs(x = "original effect size", 
#        y = "replication effect size")+ 
# #  ggtitle("Effect size reduction in Reproducibility Project: Cancer Biology")+
#   letter
# 
# fig5
# 
# 
# sum(crp_d$orig_d> 10 | crp_d$rep_d>10, na.rm = T)
# # 15 data points removed 
# 
# 
# p = list(fig3,fig4)
# 
# 
# 
# fig_combine_34 <- grid.arrange(
#   grobs = p, 
#   nrow = 1, ncol = 2,
# )


##################
#
# combined figure
#
##################

# p = list(fig2, fig_combine_345)
# 
# fig_combine <- grid.arrange(
#   grobs = p, 
#   nrow = 2, ncol = 1
# )

fig1 <- 
  ggarrange(fig1_a, fig1_bc, 
            nrow = 2)

fig1

##################################
#
#         Analysis 
# 
##################################

#########
#   A 
#########
# reduced, equal, increased sample sizes in the replication 


sum(df_descriptive$orig_ss > df_descriptive$rep_ss, na.rm = T)/ nrow(df_descriptive)



df_descriptive$reduced <- df_descriptive$orig_ss > df_descriptive$rep_ss
df_descriptive$same <- df_descriptive$orig_ss == df_descriptive$rep_ss
df_descriptive$increased <- df_descriptive$orig_ss < df_descriptive$rep_ss

df_descriptive %>%
  group_by(project)%>%
  summarize(n = n(),
            n_reduced = sum(reduced), 
            perc_reduced = n_reduced/n, 
            n_same = sum(same), 
            perc_same = n_same/n, 
            n_increased = sum(increased), 
            perc_increased = n_increased/n)


##########
#   B
##########
# Reduced sample sizes in CRP 

crp_d$reduced <- crp_d$orig_ss > crp_d$rep_ss
crp_d$equal <- crp_d$orig_ss == crp_d$rep_ss
crp_d$increased <- crp_d$orig_ss < crp_d$rep_ss

sum(crp_d$reduced)

crp_d$rep_sig <- crp_d$rep_p_2sided< 0.05

rep_sig_NA <- which(!is.na(crp_d$rep_sig))

crp_success <- crp_d[rep_sig_NA,]

crp_success%>%
  group_by(reduced)%>%
  summarize(n = n(), 
            rep_success = sum(rep_sig == TRUE), 
            perc_success = rep_success/n)


crp_success%>%
  group_by(equal)%>%
  summarize(n = n(), 
            rep_success = sum(rep_sig == TRUE), 
            perc_success = rep_success/n)


crp_success%>%
  group_by(increased)%>%
  summarize(n = n(), 
            rep_success = sum(rep_sig == TRUE), 
            perc_success = rep_success/n)

#########
#   C
#########
# Shrinkage in CRP effect sizes 

# number of significant original studies 
sum(crp_d$orig_p_2sided < 0.05)

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












