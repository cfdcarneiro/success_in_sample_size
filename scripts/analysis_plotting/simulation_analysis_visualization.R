#################################
#
#     set up 
#
#################################


#setwd("./success_in_sample_size")

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
                legend.title = element_text(size = 14), 
                legend.key.size = unit(3, "points"),
                legend.key.width = unit(0.5,"cm"),
                plot.margin = margin(1,1,1.2,1, "cm"))

facet_names <- c("a_80pct" = "two-trial\nsignificance", 
                 "a_95pct" = "two-trial\nsignificance,95 % power",
                 "b_0.5" = "SESOI (d = 0.5)",
                 "b_1" = "SESOI", 
                 "c" = "safeguard\npower analysis",
                 "d" = "sceptical \np-value")

my_labels <- c("two-trial \nsignificance",
                "smallest effect \nsize of interest",
                "safeguard power \nanalysis",
                "sceptical \np-value")




#######################################################
#
#       Figure 5 
#       a)  sum of replication sample sizes by approach 
#       b)  distribution of sample sizes of experiments in 
#           replication by approach 
#
######################################################


plot_fig5a <- 
  ggplot(df_fig5, aes(x = sample_size_approach, 
                      y = sum_rep_ss, 
                      group = sample_size_approach, 
                      fill = sample_size_approach)) + 
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.4) +
  geom_hline(yintercept = sum_orig_ss, 
             color = cols[4],
             alpha = 0.2,
             linetype  = "longdash",
             linewidth = 1.5)+
  geom_text(aes(label=sum_rep_ss),
            color = "snow2",
            fontface = "bold",
            position="stack",
            hjust= 1.3, 
            size = 5)+ 
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = my_labels) + 
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
        axis.text.x = element_text(color = "black"), 
        axis.text.y = element_markdown(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig5a)


df_fig5b <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error")%>%
  filter(!is.na(rep_sample_size))

plot_fig5b <- 
  ggplot(df_fig5b, aes(x = sample_size_approach, 
                       y = rep_sample_size, 
                       group = sample_size_approach, 
                       fill = sample_size_approach)) + 
  geom_boxplot() +
  geom_hline(yintercept = median_orig_ss, 
             color = cols[4], 
             linetype  = "longdash",
             size = 1.5,
             alpha = 0.2)+
  scale_y_log10(breaks = c(median_orig_ss, 20, 50, 100, 300)) +
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = facet_names) +
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3]))+
  coord_flip() +
  labs(x = "",
       y = "replication sample size per experiment") +
  letter + 
  theme(legend.position = "none", 
        axis.text.x = element_text(color = "black"), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.text.y = element_text(face = "bold", 
        #                            color = c(cols[7], cols[5], cols[6], cols[3])), 
        axis.title.x = element_text(size = 18))

plot(plot_fig5b)

fig_5ab <- 
  ggarrange(
    plot_fig5a, 
    plot_fig5b, 
    nrow = 1,
    ncol = 2,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

fig_5ab

#################################################
#
#   Figure 6
#
#   M-error: Replication success distribution across
#   sample size calculation approaches 
#
#
#################################################

df_fig6 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario %in% c("m_error", "null_effect", "s_error"))

plot_fig6_m_error <- 
  ggplot(df_fig6 %>% filter(scenario == "m_error")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = my_labels) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3])) +
  coord_flip() + 
  labs(x = "",
       y = "replication success \nprobability (%)") + 
       #title = "M-error scenario") +
  letter + 
  theme(legend.position = "none", 
        axis.text.y = element_text(face = "bold", 
                                   color = c(cols[7], cols[5], cols[6], cols[3]))) 



plot(plot_fig6_m_error)


plot_fig_6b <- 
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
  labs(#title = "M-error", 
       x = "",
       y = "replication success \nprobability (%)")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

plot(plot_fig_6b)

fig6 <-
  ggarrange(
    plot_fig6_m_error,
    plot_fig_6b,
    nrow = 2,
    ncol = 1,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig6)





####################################
#
#   Figure 7 
#
#   Null effect : Replication success 
#   distribution across sample size calculation 
#   approaches  
#   
#
####################################
df_fig7 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario %in% c("m_error", "null_effect", "s_error"))

plot_fig7_null_effect <- 
  ggplot(df_fig7 %>% filter(scenario == "null_effect")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = my_labels) + 
  scale_fill_manual(limits = c("a_80pct",
                               "b_1",
                               "c",
                               "d"), 
                    values = c(cols[7], cols[5], cols[6], cols[3])) + 
  coord_flip() + 
  labs(x = "",
       y = "replication success\nprobability (%)")+
  #title = "Null effect scenario") +
  letter + 
  theme(legend.position = "none", 
        axis.text.y = element_text(face = "bold",
                                   color = c(cols[7], cols[5], cols[6], cols[3])))

plot(plot_fig7_null_effect)



plot_fig7b <- 
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
       y = "replication success\nprobability")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

plot_fig7b 

fig7 <-
  ggarrange(
    plot_fig7_null_effect,
    plot_fig7b,
    nrow = 2,
    ncol = 1,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig7)





