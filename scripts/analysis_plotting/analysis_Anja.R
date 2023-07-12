#################################
#
#     set up 
#
#################################


setwd("..")
source("./scripts/data_wrangling/load_packages.R")
source("./scripts/analysis_plotting/prepare_data_analysis.R")



##############################
# descriptive data 
# data check 
#############################

t1 <- 
  res_summary %>%
  group_by(sample_size_approach, scenario)%>%
  dplyr::summarize( n = n())

kable(t1)%>%kable_classic()


##############################
# 
#       data analysis 
# 
##############################

##############################
#
#     Replication sample size 
#
##############################

analysis_rep_ss<- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  dplyr::summarize(
    n_conducted = sum(conducted == "yes"),
    sum_rep_ss = sum(rep_sample_size, na.rm = T), 
    ss_per_experiment = round(sum_rep_ss/n_conducted), 
    median_rep_ss = median(rep_sample_size, na.rm = T), 
    quan25 = quantile(rep_sample_size, probs = 0.25, na.rm = T), 
    quan75 = quantile(rep_sample_size, probs = 0.75, na.rm = T),
    max_rep_ss = max(rep_sample_size, na.rm = T),
    sd_rep_ss = sd(rep_sample_size, na.rm = T), 
    high_invest = sum(rep_sample_size[rep_sample_size >= quan75], na.rm = T)/sum_rep_ss) 


analysis_rep_ss


kable(analysis_rep_ss, 
      caption = "Analysis of replication sample sizes within each approach")%>%
  kable_classic()

######################
#
#     M-error 
#     scenario
#
######################


summary_m_error <- 
  res_summary%>%
  filter(scenario == "m_error")%>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d")) %>%
  group_by(sample_size_approach)%>%
  dplyr::summarize(
            n = n(),
            n_conducted = sum(conducted == "yes"),
            pct_conducted = n_conducted/n,
            median_rep_ss = round(median(rep_sample_size, na.rm = TRUE)), 
            median_pct_success = round(median(pct_success, na.rm = T)),
            sd_pct_success = round(sd(pct_success, na.rm = T)),
            max_pct_success = round(max(pct_success, na.rm = T)), 
            min_pct_success = round(min(pct_success, na.rm = T)),
            quan25 = round(quantile(pct_success, probs = 0.25, na.rm = T)),
            quan75 = round(quantile(pct_success, probs = 0.75, na.rm = T)),
            sum_rep_ss = sum(rep_sample_size, na.rm = T))

summary_m_error

kable(summary_m_error, 
      caption = "M-error: Summary measures comparing sample size calculation approaches")%>%
  kable_classic()


######################
#
#     Null effect 
#     scenario
#
######################


summary_null_effect <- 
  res_summary%>%
  filter(scenario == "null_effect")%>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d")) %>%
  group_by(sample_size_approach)%>%
  dplyr::summarize(
    n = n(),
    n_conducted = sum(conducted == "yes"),
    pct_conducted = n_conducted/n,
    median_rep_ss = round(median(rep_sample_size, na.rm = TRUE)), 
    median_pct_success = round(median(pct_success, na.rm = T)),
    sd_pct_success = round(sd(pct_success, na.rm = T)),
    max_pct_success = round(max(pct_success, na.rm = T)), 
    min_pct_success = round(min(pct_success, na.rm = T)),
    quan25 = round(quantile(pct_success, probs = 0.25, na.rm = T)),
    quan75 = round(quantile(pct_success, probs = 0.75, na.rm = T)),
    sum_rep_ss = sum(rep_sample_size, na.rm = T))

summary_null_effect

kable(summary_null_effect, 
      caption = "Null-effect: Summary measures comparing sample size calculation approaches")%>%
  kable_classic()




#############################
#
#
#   Dataset preparation 
#   for visualization 
#
#############################

# 
# t4 <- 
#   res_summary %>%
#   filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
#          scenario == "m_error") %>%
#   group_by(sample_size_approach) %>%
#   mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))
# 
# 
# t5 <- 
#   res_summary %>%
#   filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
#          scenario == "s_error") %>%
#   group_by(sample_size_approach) %>%
#   mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))
# 
# 
# ci_range <- df_combined$orig_ci_high - df_combined$orig_ci_low
# 
# df_se  <- ci2se(lower = df_combined$orig_ci_low, 
#                 upper = df_combined$orig_ci_high, 
#                 conf.level =0.95)
# t2 <-
#   res_summary%>%
#   filter(conducted == "yes") %>%
#   group_by(sample_size_approach, scenario)%>%
#   dplyr::summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)),
#                    mean_pct_success = round(mean(pct_success), 2),
#                    sum_rep_ss = sum(rep_sample_size))
# t2
# 
# 
# # extracting orig_d from dataset 
# 
# vec_orig_d <- 
#   unlist(
#     as.vector(
#       res_summary %>%
#         filter(scenario == "m_error", 
#                sample_size_approach == "a_80pct") %>%
#         select(orig_d)
#     )
#   )
# 
# # creating breaks for the quartile bins of orig_d 
# vec_breaks <- 
#   c(min(vec_orig_d),
#     as.numeric(quantile(vec_orig_d, prob = c(.25, .5, .75))), 
#     round(max(vec_orig_d)))
# 
# # assigning bins to orig_d 
# vec_orig_d_bin <- findInterval(vec_orig_d, vec_breaks)
# 
# # adding orig_d_bin to the res_summary dataset 
# d3 <- 
#   res_summary %>%
#   mutate(orig_d_bin = rep(vec_orig_d_bin, 18))
# 
# # filter d3 for conducted experiments 
# d3f <- 
#   d3 %>%
#   filter(conducted == "yes")
# 
# 
# rep_success_orig_d_bins <- 
#   d3f %>%
#   group_by(sample_size_approach, scenario, orig_d_bin) %>%
#   dplyr::summarize(mean_success = round(mean(pct_success),1))
# 
# 
# 
# sum_orig_ss <- sum(df_combined$orig_ss)
# 
# per_orig_ss <- round(sum(df_combined$orig_ss)/nrow(df_combined))
# 
# median_orig_ss <- median(df_combined$orig_ss)
# 
# df_fig6 <- 
#   res_summary %>%
#   filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
#          scenario == "m_error") %>%
#   group_by(sample_size_approach) %>%
#   dplyr::summarize(
#     n_conducted = sum(conducted == "yes"),
#     sum_rep_ss = sum(rep_sample_size, na.rm = T), 
#     ss_per_experiment = round(sum_rep_ss/n_conducted), 
#     median_rep_ss = median(rep_sample_size, na.rm = T)) 
# 
# df_fig6



##############################
#  
#     Visualization
#
##############################

cols <- RColorBrewer::brewer.pal(8, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(size = 22),
                plot.subtitle = element_text( size = 20),
                axis.title.x = element_text(size = 18, face = "bold"), 
                axis.title.y = element_text(size = 18, face = "bold"), 
                axis.text = element_text(size = 18), 
                legend.text = element_text(size = 14), 
                legend.title = element_text(size = 20), 
                legend.key.size = unit(3, "points"),
                legend.key.width = unit(0.5,"cm"),
                plot.margin = margin(1,1,1.2,1, "cm"))

facet_names <- c("a_80pct" = "two-trial\nsignificance", 
                 "a_95pct" = "original effect size \n95 % power",
                 "b_0.5" = "SESOI (d = 0.5)",
                 "b_1" = "SESOI (d = 1)", 
                 "c" = "safeguard\npower analysis",
                 "d" = "sceptical \np-value")




################################################
#
#     Fig 5 
#     
#     visualizing the number of experiments 
#     chosen for replication experiments 
#
################################################



df_fig5 <-
  res_summary %>% 
  filter(scenario == "m_error", 
         sample_size_approach %in% c("a_80pct", "b_1", "c", "d")) %>% 
  group_by(sample_size_approach, conducted) %>% 
  dplyr::summarize(N = n(),
                   pct = N / nrow(df_combined) * 100)

df_fig5$conducted <- as.factor(df_fig5$conducted)

plot_fig5 <- 
  ggplot(data = df_fig5,
         aes(x = sample_size_approach,
             y = N, 
             fill = conducted, 
             color = sample_size_approach)) +
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.3, 
           size = 2) +
  geom_text(aes(label=N),
            color = "snow2",
            fontface = "bold",
            position="stack",
            size = 5,
            hjust=1.5) +
  coord_flip()+
  labs(x = "",
       y = "number of original experiments",
       fill = "Replication conducted") +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard",
                              "sceptical \np-value")) +
  scale_fill_manual(limits = c("yes",
                               "unfeasible"),
                    labels = c("selected for replication",
                               "not selected for replication"),
                    values = c("gray46", "grey21"))+
  scale_color_manual(values = c(cols[7], cols[5], cols[6], cols[3]), 
                     guide = "none") + 
  letter + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.5, 0.05), 
        legend.background = element_rect(color = "snow3"),
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig5)

#################################################
#
#       Sample size increase in the replication
# 
#       Figure 6 
#
#################################################


plot_fig6a <- 
  ggplot(df_fig6, aes(x = sample_size_approach, 
                      y = sum_rep_ss, 
                      group = sample_size_approach, 
                      fill = sample_size_approach)) + 
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.4) +
  geom_hline(yintercept = sum_orig_ss, 
             color = cols[4], 
             linetype  = "longdash",
             linewidth = 1.5,
             alpha = 0.5)+
  geom_text(aes(label=sum_rep_ss),
            color = "snow2",
            fontface = "bold",
            position="stack",
            hjust= 1.3, 
            size = 5)+ 
  scale_y_continuous(breaks = c(0, sum_orig_ss, 2000, 3000, 4000, 5000)) +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3]))+
  coord_flip() +
  labs(x = "",
       y = "total replication sample size") +
  letter + 
  theme(legend.position = "none", 
        axis.text.x = element_text(color = c("black",
                                             cols[4],
                                             "black",
                                             "black",
                                             "black",
                                             "black")), 
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig6a)


df_fig6d <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error")%>%
  filter(!is.na(rep_sample_size))

plot_fig6d <- 
  ggplot(df_fig6d, aes(x = sample_size_approach, 
                       y = rep_sample_size, 
                       group = sample_size_approach, 
                       fill = sample_size_approach)) + 
  geom_boxplot() +
  geom_hline(yintercept = median_orig_ss, 
             color = cols[4], 
             linetype  = "longdash",
             size = 1.5,
             alpha = 0.5)+
  scale_y_log10(breaks = c(median_orig_ss, 20, 50, 100, 300)) +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3]))+
  coord_flip() +
  labs(x = "",
       y = "total replication sample size") +
  letter + 
  theme(legend.position = "none", 
        axis.text.x = element_text(color = c(cols[4],
                                             "black",
                                             "black",
                                             "black", 
                                             "black" 
        )), 
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig6d)

fig_6ad <- 
  ggarrange(
    plot_fig6a, 
    plot_fig6d, 
    nrow = 1,
    ncol = 2,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

#################################################
#
#   Figure 7 
#
#   M-error: Replication success distribution across
#   sample size calculation approaches 
#
#
#################################################

df_fig7 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario %in% c("m_error", "null_effect", "s_error"))

plot_fig7_m_error <- 
  ggplot(df_fig7 %>% filter(scenario == "m_error")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("two-trial\nsignificance",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard power\nanalysis",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3])) +
  coord_flip() + 
  labs(x = "",
       y = "replication success probability (%)", 
       title = "M-error scenario") +
  letter + 
  theme(legend.position = "none", 
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3]))) 



plot(plot_fig7_m_error)


plot_fig_7b <- 
  ggplot(d3f %>% filter(scenario == "m_error", 
                        sample_size_approach %in% c("a_80pct", "b_1", "c", "d"))) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "original effect size (SMD)", 
                    labels = c("0.6 - 2.0", 
                               "2.0 - 3.3", 
                               "3.3 - 6.1", 
                               "6.1 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "M-error", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

plot(plot_fig_7b)

fig7 <-
  ggarrange(
    plot_fig7_m_error,
    plot_fig_7b,
    nrow = 2,
    ncol = 1,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig7)





####################################
#
#   Figure 8 
#
#   Null effect : Replication success 
#   distribution across sample size calculation 
#   approaches  
#   
#
####################################


plot_fig8_null_effect <- 
  ggplot(df_fig7 %>% filter(scenario == "null_effect")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("two-trial\nsignificance",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard power\nanalysis",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3])) + 
  coord_flip() + 
  labs(x = "",
       y = "replication success probability (%)")+
  #title = "Null effect scenario") +
  letter + 
  theme(legend.position = "none", 
        axis.text.y = element_text(face = "bold",
                                   color = c(cols[7], cols[5], cols[6], cols[3])))

plot(plot_fig8_null_effect)


plot_fig_8b <- 
  ggplot(d3f %>% 
           filter(scenario == "null_effect", 
                  sample_size_approach %in% c("a_80pct", "b_1", "c", "d"))) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "original effect size (SMD)", 
                    labels = c("0.6 - 2.0", 
                               "2.0 - 3.3", 
                               "3.3 - 6.1", 
                               "6.1 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(#title = "Null-effect", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

plot_fig_8b 

fig8 <-
  ggarrange(
    plot_fig8_null_effect,
    plot_fig_8b,
    nrow = 2,
    ncol = 1,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig8)






#########################################
#                                       #
#     analysis replication sample size  #
#                                       #
#########################################

#############################
#
# Safeguard power analysis 
#
#############################

df_c <- 
  res_summary %>%
  filter(sample_size_approach == "c", 
         scenario == "m_error")


hist(df_c$rep_sample_size)
sum(df_c$rep_sample_size, na.rm = T)
summary(df_combined$rep_sample_size_c)
sd(df_c$rep_sample_size, na.rm = T)
quant75 <- as.numeric(quantile(df_c$rep_sample_size, probs = 0.75, na.rm = T))
sum(df_c[df_c$rep_sample_size>=quant75,]$rep_sample_size, na.rm = T)/sum(df_c$rep_sample_size, na.rm = T)


ggplot(data = df_c)+ 
  geom_point(aes(x = ci_range, y = rep_sample_size))



example <- 
  res_summary %>% 
  filter(orig_d == 2.297534) %>%
  filter(scenario == "m_error")
################################
#
#
#
#   Supplement 
#
#
#################################

###############################################
#
# Figure comparing replication sample sizes 
# with replication success probability 
# for different approaches 
#
###############################################

fig3_a<-
  grid.arrange(
    
    ggplot(t2 %>% filter(scenario == "m_error")) +
      geom_point(aes(x = sum_rep_ss,
                     y = mean_pct_success,
                     color = sample_size_approach),
                 size = 7)+
      scale_x_log10()+
      scale_color_manual(name = " ",
                         labels = c("standard significance, 80% power",
                                    "standard significance, 95% power",
                                    "smallest effect size of interest, d = 0.5",
                                    "smallest effect size of interest, d = 1",
                                    "safeguard power analysis, 60% lower CI bound",
                                    "p-skeptical"),
                         values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+
      labs(title = "M-error",
           y = "mean replication success rate",
           x = " sum of replication sample sizes"
      )+
      theme(legend.position = "none")+
      letter,
    
    ggplot(t2 %>% filter(scenario == "null_effect")) +
      geom_point(aes(x = sum_rep_ss,
                     y = mean_pct_success,
                     color = sample_size_approach),
                 size = 7)+
      scale_x_log10()+
      scale_color_manual(name = " ",
                         labels = c("standard significance, 80% power",
                                    "standard significance, 95% power",
                                    "smallest effect size of interest, d = 0.5",
                                    "smallest effect size of interest, d = 1",
                                    "safeguard power analysis, 60% lower CI bound",
                                    "p-skeptical"),
                         values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+
      labs(title = "Null-effect",
           y = "",
           x = " sum of replication sample sizes")+
      theme(legend.position = "none")+
      letter,
    
    ggplot(t2 %>% filter(scenario == "s_error")) +
      geom_point(aes(x = sum_rep_ss,
                     y = mean_pct_success,
                     color = sample_size_approach),
                 size = 7)+
      scale_x_log10()+
      scale_color_manual(name = " ",
                         labels = c("standard significance, 80% power",
                                    "standard significance, 95% power",
                                    "smallest effect size of interest, d = 0.5",
                                    "smallest effect size of interest, d = 1",
                                    "safeguard power analysis, 60% lower CI bound",
                                    "p-skeptical"),
                         values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+
      labs(title = "S-error",
           y = "",
           x = " sum of replication sample sizes")+
      theme(legend.position = "none")+
      letter,
    ncol = 3,
    top = textGrob("Mean replication success rate vs sum of replication sample sizes",gp=gpar(fontsize=22, fontface = "bold"))
  )

fig3_a

plot1_legend <- ggplot(t2 %>% filter(scenario == "null_effect")) +
  geom_point(aes(x = sum_rep_ss,
                 y = mean_pct_success,
                 color = sample_size_approach),
             size = 7)+
  scale_color_manual(name = " ",
                     labels = c("standard significance, 80% power",
                                "standard significance, 95% power",
                                "smallest effect size of interest, d = 0.5",
                                "smallest effect size of interest, d = 1",
                                "safeguard power analysis, 60% lower CI bound",
                                "p-skeptical"),
                     values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+
  theme(legend.position = "bottom")+
  letter


plot1_legend



# function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# extract legend from plot1 using above function
legend <- get_only_legend(plot1_legend)


fig3_a <- grid.arrange(fig3_a, legend, heights = c(10, 1)) 
fig3_a




fig3_a <-
  ggarrange(fig3_a,
            nrow = 1,
            labels = c("a)"),
            font.label = list(face = "bold",
                              size = 24))
fig3_a



#########################################################
#
# Figure displaying relative sample sizes per approach 
#

#filtering the conducted replication studies 
d2 <- 
  res_summary %>%
  filter(conducted == "yes")


# calculating the ratio of replication sample size vs original sample size 
d2$ratio_ss <-
  d2$rep_sample_size/d2$orig_ss



fig3_b <- ggplot(data = d2, aes(y = sample_size_approach, x = ratio_ss, fill = sample_size_approach)) + 
  geom_boxplot() +
  coord_trans( x = "log10")+
  scale_x_continuous(breaks=c(0,0.5,1,2,5,10,25))+
  geom_vline(xintercept = 1, color = "red", linetype = 2, alpha = 0.5)+
  labs(title = "Relative sample sizes under different sample size calculation approaches",
       y = "", 
       x = expression(paste ("relative sample size = N"[confirmatory],  "/ N"[exploratory]))
  ) + 
  scale_fill_manual(name = " ", 
                    labels = c("original effect size, 80% power", 
                               "original effect size, 95% power", 
                               "smallest effect size of interest, d = 0.5", 
                               "smallest effect size of interest, d = 1", 
                               "safeguard power analysis, 60% lower CI bound", 
                               "p-skeptical"), 
                    values = c("black", "#E69F00", "#56B4E9", "#009E73", "grey55", "#0072B2"))+ 
  scale_y_discrete(name = " ", 
                   labels = c("original effect size, 80% power", 
                              "original effect size, 95% power", 
                              "SESOI, d = 0.5", 
                              "SESOI, d = 1", 
                              "safeguard power analysis", 
                              "p-sceptical"))+
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())+ 
  letter

fig3_b <- grid.arrange(fig3_b, legend, heights = c(10, 1))
fig3_b

fig3_b <- 
  ggarrange(fig3_b, 
            nrow = 1, 
            labels = c("b)"), 
            font.label = list(face = "bold", 
                              size = 24))

fig3_b



######################################
#
# Replication success probabilities 
# for S-error scenario 
#
######################################


fig2_e <- 
  ggplot(d3f %>% 
           filter(scenario == "s_error", 
                  sample_size_approach %in% c("a_80pct", "b_1", "c", "d"))) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 2.0", 
                               "2.0 - 3.3", 
                               "3.3 - 6.1", 
                               "6.1 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "S-error: Rate of successful replications stratified by original effect size", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

fig2_e


##############################
# Figure 4 
# rep_sample_size ~ orig_d
#
##############################

d4 <- 
  res_summary %>%
  mutate(orig_d = rep(df_combined$orig_d, 18)) %>%
  filter(scenario == "m_error")

plot(d3$rep_sample_size ~ d3$orig_d)

ggplot(d4) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted))+
  facet_grid(rows = vars(sample_size_approach)) +
  scale_y_log10()+
  scale_x_log10()

d4a <- 
  d4 %>%
  filter(sample_size_approach == c("d", "c", "a_80pct", "b_1"))


ggplot(d4 %>% filter(sample_size_approach == "d")) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted), size = 3)+
  facet_grid(cols = vars(sample_size_approach))+
  scale_y_log10()+
  scale_x_log10()



ggplot(d4 %>% filter(sample_size_approach == "d") %>% filter(scenario == "m_error")) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = pct_success), size = 3)+
  facet_grid(cols = vars(sample_size_approach))+
  scale_color_continuous(type = "viridis")+ 
  coord_cartesian(xlim = c(0,10), ylim = c(0,100))


ggplot(d4 %>% filter(sample_size_approach == c("d", "c", "a_80pct", "b_1"))) + 
  geom_point(aes(x = orig_d, y = rep_sample_size, color = conducted))+
  facet_grid(cols = vars(sample_size_approach))+
  scale_y_log10()+
  scale_x_log10()



plot_fig6b <- 
  ggplot(df_fig6, aes(x = sample_size_approach, 
                      y = median_rep_ss, 
                      group = sample_size_approach, 
                      fill = sample_size_approach)) + 
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.4) +
  geom_hline(yintercept = median_orig_ss, 
             color = cols[4], 
             linetype  = "longdash",
             size = 1.5,
             alpha = 0.5)+
  geom_text(aes(label=median_rep_ss),
            color = "snow2",
            fontface = "bold",
            position="stack",
            hjust= 1.3, 
            size = 5)+ 
  scale_y_continuous(breaks = c(0, median_orig_ss, 10)) +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3]))+
  coord_flip() +
  labs(x = "",
       y = "total replication sample size") +
  letter + 
  theme(legend.position = "none", 
        axis.text.x = element_text(color = c("black",
                                             cols[4],
                                             "black",
                                             "black",
                                             "black")), 
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig6b)


fig_6ab <-
  ggarrange(
    plot_fig6a,
    plot_fig6b,
    nrow = 1,
    ncol = 2,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

fig_6ab




plot_fig7_s_error <- 
  ggplot(df_fig7 %>% filter(scenario == "s_error")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("two-trial\nsignificance",
                              "smallest effect \nsize of interest \n d = 1",
                              "safeguard power\nanalysis",
                              "sceptical \np-value")) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3])) + 
  coord_flip() + 
  labs(x = "",
       y = "replication success probability (%)", 
       title = "S-error scenario") +
  letter + 
  theme(legend.position = "none", 
        axis.text.y = element_text(face = "bold",
                                   color = c(cols[7], cols[5], cols[6], cols[3])))

plot(plot_fig7_s_error)



