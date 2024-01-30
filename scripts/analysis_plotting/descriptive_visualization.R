# descriptive data visualization 

setwd("./success_in_sample_size")
source("./scripts/analysis_plotting/descriptive_analysis")

###############################################################################
#                                                                             #
#   Fig 3: Comparison of sample size choices in original vs replication       #
#   experiments within three large preclinical replication study projects     #
#                                                                             #
###############################################################################


fig_bri.sample.size<-
  ggplot(bri_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              alpha = 0.3, width = 0.5, size = 3, color = cols[4])+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  labs(x = "total original sample size", 
       y = "total replication sample size")+
  ggtitle("Brazilian Reproducibility\nInitiative")+ 
  letter+
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[4]))

plot(fig_bri.sample.size)

fig_crp.sample.size<-
  ggplot(crp_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              color = cols[1], alpha = 0.3, width = 0.5, size = 3)+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  ylab("total replication sample size")+
  xlab("total original sample size")+
  ggtitle("Reproducibility Project:\nCancer Biology")+
  coord_cartesian(xlim = c(0,40), ylim= c(0,100))+
  letter+
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[1]))

plot(fig_crp.sample.size)

fig_cps.sample.size <-
  ggplot(cps_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss),
              color = cols[7], alpha = 0.3, width = 0.5, size = 3)+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  ylab("total replication sample size")+
  xlab("total original sample size")+
  ggtitle("Confirmatory Preclinical\nStudies")+
  letter+ 
  theme(plot.title = element_text(size = 22, face = "bold", color = cols[7]))

plot(fig_cps.sample.size)

fig3 <- ggarrange(fig_crp.sample.size, fig_bri.sample.size, fig_cps.sample.size, 
                  nrow = 1, 
                  #labels = c("a)"),
                  font.label = list(face = "bold", size = 22))

fig3

###################################################################################
#                                                                                 #
#   Fig.4:                                                                        #
#   a) Inverse relationship of original effect size estimate                      #
#   and replication sample size in the Brazilian Reproducibility Initiative       #
#   b) relative effect sizes in the Reproducibility Project: Cancer Biology       #
#                                                                                 #
###################################################################################


fig_4a <- 
  ggplot(crp_d, aes(x = relative_d)) + 
  geom_histogram(
    position="identity",
    color = "#000000", 
    fill  = cols[1],
    alpha = 0.5,
    bins = 1000) + 
  coord_cartesian(xlim = c(-1.2,1.2)) + 

  geom_vline(xintercept = c(-0.25, 0, 0.5),
             linetype = "dashed", 
             color = c("gray45","gray45","gray45"), 
             size = 1)+ 
  labs(x = expression(paste ("relative effect size = effect size"[replication],  "/ effect size"[original])), 
       y = "count")+ 
  scale_y_continuous(breaks = c(1,5,8))+
  letter + 
  theme(axis.title.x = element_text(size = 20, vjust = -2), 
        axis.title.y = element_text(size = 20, vjust = 2), 
        legend.title = element_blank(), 
        legend.position = "right", 
        legend.text = element_text(size = 11))


fig_4a <- 
  fig_4a +   
  annotate("text", 
           x = c(-0.15, 0.15, 0.6), 
           y = c(9, 9,9), 
           label = c("c)", "b)", "a)"),
           size = 8)

plot(fig_4a)

fig_4b <-  
  ggplot(bri_des) + 
  geom_jitter(aes(x = orig_ss, y = rep_ss, fill = es, shape = es),
              alpha = 0.6, width = 0.5, size = 5)+
  scale_shape_manual(guide = "none", values = c(22, 21, 23,24))+
  scale_fill_manual(values = c("#E7298A", "#AD1FD1FF", "#E592E8FF", "#E82AE8FF"))+
  guides(fill = guide_legend(override.aes=list(shape=c(22, 21, 23,24))))+
  geom_abline(intercept = 0, slope = 1, lty = "dashed")+
  labs(x = "total original sample size", 
       y = "total replication sample size", 
       fill = "original effect sizes", 
       shape = "")+
  letter + 
  theme(legend.position = c(0.85, 0.15),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.title.x = element_text(size = 20, vjust = -2), 
        axis.title.y = element_text(size = 20, vjust = 2))


plot(fig_4b)


fig4_ab <- 
  ggarrange(fig_4a, fig_4b, 
            nrow = 1,
            labels = c("a)", "b)"),
            font.label = list(face = "bold", 
                              size = 24))
fig4_ab


##################################
#
#
#
# Supplement 
#
#
##################################


table_S1<- rbind(t1_des[1:4], t2_des %>% mutate(project ="across") %>% select(project, 1:3))

table_S1<-mutate(table_S1, median_d = round(median_d, 1), 
                 sample_size_calculation_approach = c("original effect estimate \n95% power", 
                                                      "anticipated effect size shrinkage", 
                                                      "original effect estimate 80% power", 
                                                      ""))

kable(table_S1, col.names = c("project", 
                              "median replication \nsample size", 
                              "median original\nsample size", 
                              "median SMD", 
                              "sample size approach"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F)






