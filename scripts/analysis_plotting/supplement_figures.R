################################
#
#
#
#   Supplement 
#
#
#################################

#setwd("./success_in_sample_size")

source("./scripts/analysis_plotting/simulation_analysis_visualization.R")
source("./scripts/analysis_plotting/descriptive_visualization.R")



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

#######################################
#
#
#     Supplement 
#
#
#######################################



################################################
#
#     Fig S2 
#     
#     visualizing the number of experiments 
#     chosen for replication experiments 
#
################################################



df_figS2 <-
  res_summary %>% 
  filter(scenario == "m_error", 
         sample_size_approach %in% c("a_80pct", "b_1", "c", "d")) %>% 
  group_by(sample_size_approach, conducted) %>% 
  dplyr::summarize(N = n(),
                   pct = N / nrow(df_combined) * 100)

df_figS2$conducted <- as.factor(df_fig5$conducted)

plot_figS2 <- 
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
                   labels = c("two-trial \nsignificance",
                              "smallest effect \nsize of interest",
                              "safeguard power \nanalysis",
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

plot(plot_figS2)
