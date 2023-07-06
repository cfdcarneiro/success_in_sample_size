##############################
# 
#       data analysis 
# 
# 

##############################
# descriptive data 
# data check 
#############################

t1 <- 
  res_summary %>%
  group_by(sample_size_approach, scenario)%>%
  dplyr::summarize( n = n())

kable(t1)%>%kable_classic()


t2 <- 
  res_summary%>%
  filter(scenario == "m_error")%>%
  #filter(conducted == "yes") %>%
  group_by(sample_size_approach)%>%
  dplyr::summarize(
            n = n(),
            n_conducted = sum(conducted == "yes"),
            pct_conducted = n_conducted/n,
            mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)), 
            mean_pct_success = round(mean(pct_success, na.rm = T)),
            sd_pct_success = round(sd(pct_success, na.rm = T)),
            max_pct_success = round(max(pct_success, na.rm = T)), 
            min_pct_success = round(min(pct_success, na.rm = T)),
            sum_rep_ss = sum(rep_sample_size, na.rm = T))

#summary(is.na(res_summary))

t2

t2 <- 
  res_summary%>%
  filter(conducted == "yes") %>%
  group_by(sample_size_approach, scenario)%>%
  dplyr::summarize(mean_rep_ss = round(mean(rep_sample_size, na.rm = TRUE)), 
            mean_pct_success = round(mean(pct_success), 2), 
            sum_rep_ss = sum(rep_sample_size))#%>%
t2


t3 <-
  t2 %>%
  pivot_wider(names_from = scenario, values_from = mean_pct_success)
  

colnames(t3) <- c("sample_size_approach", 
                  "mean_rep_ss", 
                  "sum_rep_ss", 
                  "success_rate_m_error", 
                  "success_rate_null_effect", 
                  "success_rate_s_error")

t3


kable(t3) %>% 
  kable_classic()



t4 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))


t5 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "s_error") %>%
  group_by(sample_size_approach) %>%
  mutate(sum_rep_ss = sum(rep_sample_size, na.rm = T))


ci_range <- df_combined$orig_ci_high - df_combined$orig_ci_low

df_se  <- ci2se(lower = df_combined$orig_ci_low, 
                upper = df_combined$orig_ci_high, 
                conf.level =0.95)



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
                legend.key.width = unit(0.5,"cm"))#,
                #plot.margin = margin(1,1,1.2,1, "cm"))



fig3_c <- 
  ggplot(t4) +
  geom_boxplot(aes(x = as.factor(sum_rep_ss), 
                  y = pct_success, 
                  color = sample_size_approach)) #+ 
  #scale_x_log10()

plot(fig3_c)

ggplot(t4, aes(x = pct_success, 
               group = sample_size_approach,
               fill = sample_size_approach)) + 
  geom_density(adjust=1.5, position="fill")

ggplot(t4, aes(x = rep_sample_size, 
               group = sample_size_approach,
               fill = sample_size_approach)) + 
  geom_density(adjust=1.5, position="fill")

ggplot(t4, aes(y = pct_success,
               x = rep_sample_size,
               group = sample_size_approach,
               color = sample_size_approach)) + 
  geom_point()



ggplot(t4, aes(y = pct_success, 
               group = sample_size_approach,
               fill = sample_size_approach)) + 
  geom_density(adjust=1.5, position="fill")






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


#fig3_a <- grid.arrange(fig3_a, legend, heights = c(10, 1))
#fig3_a




fig3_a <- 
  ggarrange(fig3_a, 
            nrow = 1,
            labels = c("a)"),
            font.label = list(face = "bold", 
                              size = 24))
fig3_a

# ggsave("../graphs/fig3_a.png", fig3_a, width = 40, height = 30, units = "cm")

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

# fig3 <- 
#   ggarrange(fig3_a, fig3_b, 
#             ncol  = 1) + 
#   theme(plot.margin = margin(1,0,1,0, "cm")) 
# fig3

# ggsave("fig3_results1.png", fig3, width = 50, height = 50, units = "cm")



##############################
# Figure 3 
# success rates in relation to 
# orig_d, bins for quartiles of 
# distribution of orig_d 
#
##############################

# creating bins for orig_d 

d1 <- 
  res_summary %>%
  filter(scenario == "m_error", 
         sample_size_approach == "a_80pct")

vec_orig_d <- d1$orig_d


vec_breaks <- c(min(vec_orig_d),
  as.numeric(quantile(vec_orig_d, prob = c(.25, .5, .75))), 
  round(max(vec_orig_d)))

vec_orig_d_bin <- findInterval(vec_orig_d, vec_breaks)

d3 <- 
  res_summary %>%
  mutate(orig_d_bin = rep(vec_orig_d_bin, 18))

d3f <- 
  d3 %>%
  filter(conducted == "yes")


d3a <- 
  d3f %>%
  group_by(sample_size_approach, scenario, orig_d_bin) %>%
  dplyr::summarize(mean_success = round(mean(pct_success),1))



# p3a <- 
#   ggplot(d3a)+ 
#   geom_col(aes(x = orig_d_bin, y = mean_success, fill = factor(orig_d_bin))) + 
#   facet_grid(rows = vars(scenario), 
#              cols = vars(sample_size_approach))+ 
#   scale_fill_manual(name = "Cohen's d, exploratory study", 
#                     labels = c("0.6 - 2.0", 
#                                "2.0 - 3.3", 
#                                "3.3 - 6.1", 
#                                "6.1 - 85"), 
#                     values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+ 
#   labs(y = "mean replication success rate in synthetic replication studies", 
#        x = ""
#   ) +
#   theme(legend.position = "bottom", 
#         panel.grid.major = element_blank())
# 
# p3a
# 
# p3b <- 
#   ggplot(d3a %>% filter(scenario == "m_error"))+ 
#   geom_col(aes(x = orig_d_bin, y = mean_success, fill = factor(orig_d_bin))) + 
#   facet_grid(cols = vars(sample_size_approach))+ 
#   scale_fill_manual(name = "Cohen's d, exploratory study", 
#                     labels = c("0.6 - 1.7", 
#                                "1.7 - 2.8", 
#                                "2.8 - 6.2", 
#                                "6.2 - 85"), 
#                     values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+ 
#   labs(y = "percentage of replication success", 
#        x = ""
#   ) +
#   theme(legend.position = "bottom", 
#         panel.grid.major = element_blank())
# 
# p3b
#   
#   
# ggplot(d3f) + 
#   geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success))+
#   facet_grid(rows = vars(scenario), 
#              cols = vars(sample_size_approach))

facet_names <- c("a_80pct" = "original effect size \n80 % power", 
                 "a_95pct" = "original effect size \n95 % power",
                 "b_0.5" = "SESOI = 0.5",
                 "b_1" = "SESOI = 1.0", 
                 "c" = "safeguard",
                 "d" = "sceptical \np-value")



fig2_c <- 
  ggplot(d3f %>% filter(scenario == "m_error", 
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
  labs(title = "M-error: Rate of successful replications stratified by original effect size", 
       x = "",
       y = "Percentage success")+
  theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text.x = element_text(size = 15))+
  letter

fig2_c



fig2_d <- 
  ggplot(d3f %>% filter(scenario == "null_effect")) + 
  geom_boxplot(aes(x = factor(orig_d_bin), y = pct_success, fill = factor(orig_d_bin)))+
  facet_grid(cols = vars(sample_size_approach), 
             labeller = labeller(.cols = facet_names))+ 
  scale_fill_manual(name = "Cohen's d, exploratory study", 
                    labels = c("0.6 - 2.0", 
                               "2.0 - 3.3", 
                               "3.3 - 6.1", 
                               "6.1 - 85"), 
                    values = c("#E69F00", "#56B4E9", "#009E73", "grey55"))+
  labs(title = "Null-effect: Rate of successful replications stratified by original effect size", 
       x = "",
       y = "Percentage success")+
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 15))+
  letter

fig2_d


fig2_e <- 
  ggplot(d3f %>% filter(scenario == "s_error")) + 
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



fig3 <- 
  ggarrange(fig3_b,fig2_c, 
            nrow = 1,
            labels = c("b)", "c)"),
            font.label = list(face = "bold", 
                              size = 24))

fig2




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


###################
# Fig 5
# Mex 
#####################

vec <- c(4,6,46,250)

d5 <- 
  res_summary %>%
  mutate(orig_d = rep(df_combined$orig_d, 18)) %>%
  filter(conducted == "yes") %>%
  filter(scenario == "m_error") %>%
  mutate(bins = findInterval(rep_sample_size, vec))

facet_names <- c("a_80pct" = "Significance \n80 % power", 
                 "a_95pct" = "Significance \n95 % power",
                 "b_0.5" = "SESOI = 0.5",
                 "b_1.0" = "SESOI = 1.0", 
                 "c" = "Safeguard",
                 "d" = "Skeptical \np-value")


p5 <- 
  ggplot(d5, 
         aes(x = factor(bins), 
             y = pct_success, 
             fill = factor(bins)))+ 
  geom_boxplot(size = 0.3)+
  facet_wrap(~sample_size_approach, 
             labeller = labeller(.cols = facet_names), 
             ncol = 2)+
  scale_fill_manual(limits = c("1", "2", "3", "4", "5"),
                  labels = c("4-6", "7-14", "15-46","46-250", "> 250"),
                  values = c("#009E73", "#CC79A7", "#56B4E9","#E69F00", "#0072B2"))
p5









res_summary <- rbind(res_summary_a, res_summary_b, res_summary_c, res_summary_d)
table(res_summary$sample_size_approach)

mean_success <- 
  res_summary_d %>%
  group_by(scenario)%>%
  summarize(mean_success_d = mean(pct_success, na.rm = T), 
            quan_25 = quantile(pct_success, prob = 0.25, na.rm = T), 
            quan_75 = quantile(pct_success, prob = 0.75, na.rm = T)) %>%
  mutate(mean_success_orig = round(sum(df_combined$orig_p_2sided < 0.05)/86 *100,2)) 

conducted <- 
  res_summary_d %>%
  group_by(conducted)%>%
  summarize(n = n()/3, 
            pct = n/86)


ratio_animals <- 
  res_summary_d %>%
  filter(conducted == "yes") %>%
  mutate(ss_ratio = rep_sample_size_d/orig_ss)%>%
  summarize(mean_ratio  = mean(ss_ratio)) 

abs_animals <-
  res_summary_d %>%
  summarize(sum_orig_ss = sum(orig_ss), 
            sum_rep_ss = sum(rep_sample_size_d, na.rm = T), 
            sum_total = sum_orig_ss + sum_rep_ss) 



abs_animals <- 
  res_summary_d %>%
  summarize(mean_orig_ss = mean(orig_ss), 
            )


p1 <- 
    ggplot(res_summary_d %>% filter(conducted == "yes")) + 
    geom_histogram(aes(x = rep_sample_size_d))+ 
    facet_wrap(~scenario)


################################################
#
#
#     visualizing the number of experiments 
#     chosen for replication experiments 
#
#
#     fig 3a 
#
################################################



df_fig3a <-
  res_summary %>% 
  filter(scenario == "m_error", 
         sample_size_approach %in% c("a_80pct", "b_1", "c", "d")) %>% 
  group_by(sample_size_approach, conducted) %>% 
  dplyr::summarize(N = n(),
                   pct = N / nrow(df_combined) * 100)

df_fig3a$conducted <- as.factor(df_fig3a$conducted)

plot_fig3a <- 
  ggplot(data = df_fig3a,
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

plot(plot_fig3a)



test <- 
  res_summary %>% 
  filter(scenario == "m_error", 
         sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         conducted == "yes")

list_conducted <- 
  list(
  id_a_80pct = test %>% 
    filter(sample_size_approach == "a_80pct") %>% pull(study_id), 
  id_b_1 = test %>% 
    filter(sample_size_approach == "b_1") %>% pull(study_id),
  id_c = test %>% 
    filter(sample_size_approach == "c") %>% pull(study_id), 
  id_d = test %>% 
    filter(sample_size_approach == "d") %>% pull(study_id)
)

install.packages("ggVennDiagram")
library(ggVennDiagram)

ggVennDiagram(list_conducted, category.names = c("original effect size \n80 % power",
                                                 "smallest effect \nsize of interest \n d = 1",
                                                 "safeguard",
                                                 "sceptical \np-value")) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")

test <- 
  res_summary %>% 
  filter(scenario == "m_error", 
         sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         conducted != "yes")

list_conducted <- 
  list(
    id_a_80pct = test %>% 
      filter(sample_size_approach == "a_80pct") %>% pull(study_id), 
    # id_b_1 = test %>% 
    #   filter(sample_size_approach == "b_1") %>% pull(study_id),
    id_c = test %>% 
      filter(sample_size_approach == "c") %>% pull(study_id), 
    id_d = test %>% 
      filter(sample_size_approach == "d") %>% pull(study_id)
  )

#install.packages("ggVennDiagram")
#library(ggVennDiagram)

ggVennDiagram(list_conducted, category.names = c("original effect size \n80 % power",
                                                 #"smallest effect \nsize of interest \n d = 1",
                                                 "safeguard",
                                                 "sceptical \np-value")) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")



#################################################
#
#       Sample size increase in the replication
#
#################################################


sum_orig_ss <- sum(df_combined$orig_ss)

per_orig_ss <- round(sum(df_combined$orig_ss)/nrow(df_combined))

median_orig_ss <- median(df_combined$orig_ss)

df_fig6 <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  dplyr::summarize(
    n_conducted = sum(conducted == "yes"),
    sum_rep_ss = sum(rep_sample_size, na.rm = T), 
    ss_per_experiment = round(sum_rep_ss/n_conducted), 
    median_rep_ss = median(rep_sample_size, na.rm = T)) 

df_fig6

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


plot_fig6b <- 
  ggplot(df_fig6, aes(x = sample_size_approach, 
                       y = median_rep_ss, 
                       group = sample_size_approach, 
                       fill = sample_size_approach)) + 
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.4) +
  geom_hline(yintercept = per_orig_ss, 
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
  scale_y_continuous(breaks = c(0, per_orig_ss, 20, 30, 40)) +
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

plot_fig6c <- 
  ggplot(df_fig6, aes(x = sample_size_approach, 
                      y = ss_per_experiment, 
                      group = sample_size_approach, 
                      fill = sample_size_approach)) + 
  geom_bar(stat = "identity",
           position = "stack", 
           width = 0.4) +
  geom_hline(yintercept = per_orig_ss, 
             color = cols[4], 
             linetype  = "longdash",
             size = 1.5,
             alpha = 0.5)+
  geom_text(aes(label=ss_per_experiment),
            color = "snow2",
            fontface = "bold",
            position="stack",
            hjust= 1.3, 
            size = 5)+ 
  scale_y_continuous(breaks = c(0, per_orig_ss, 20, 30, 40)) +
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

plot(plot_fig6c)

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

fig_6bc <- 
  ggarrange(
    plot_fig6b, 
    plot_fig6c, 
    nrow = 1,
    ncol = 2,
    labels = c("median", "mean"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig_6bc)


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
  # geom_text(aes(label=ss_per_experiment),
  #           color = "snow2",
  #           fontface = "bold",
  #           position="stack",
  #           hjust= 1.3, 
  #           size = 5)+ 
  scale_y_log10()+#breaks = c(0, per_orig_ss, 20, 30, 40)) +
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
                                             "black")), 
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

fig_6ad

#################################################
#
#   Replication success distribution across
#   sample size calculation approaches 
#
#   fig3c
#
#################################################

df_fig3c <- 
  res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario %in% c("m_error", "s_error"))

plot_fig3c_m_error <- 
  ggplot(df_fig3c %>% filter(scenario == "m_error")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 0.5",
                              "safeguard",
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

  

plot(plot_fig3c_m_error)
  
plot_fig3c_s_error <- 
  ggplot(df_fig3c %>% filter(scenario == "s_error")) + 
  geom_boxplot(aes(y = pct_success, 
                   x = sample_size_approach, 
                   fill = sample_size_approach)) + 
  scale_x_discrete(limits = c("a_80pct",
                              "b_1",
                              "c",
                              "d"),
                   labels = c("original effect size \n80 % power",
                              "smallest effect \nsize of interest \n d = 0.5",
                              "safeguard",
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

plot(plot_fig3c_s_error)


fig3ab <-
  ggarrange(
    plot_fig3a,
    plot_fig3b,
    nrow = 1,
    ncol = 2,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig3ab)

fig3cd <-
  ggarrange(
    plot_fig3c_m_error,
    plot_fig3c_s_error,
    nrow = 1,
    ncol = 2,
    labels = c("a)", "b)"),
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig3cd)

fig3 <-
  ggarrange(
    fig3ab,
    fig3cd,
    nrow = 2,
    ncol = 1,
    font.label = list(face = "bold",
                      size = 24)
  )

plot(fig3)


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
sum(df_c[df_c$rep_sample_size>100,]$rep_sample_size, na.rm = T)

ggplot(data = df_c)+ 
  geom_point(aes(x = ci_range, y = rep_sample_size))


res_summary %>%
  filter(sample_size_approach %in% c("a_80pct", "b_1", "c", "d"), 
         scenario == "m_error") %>%
  group_by(sample_size_approach) %>%
  dplyr::summarize(
    n_conducted = sum(conducted == "yes"),
    sum_rep_ss = sum(rep_sample_size, na.rm = T), 
    ss_per_experiment = round(sum_rep_ss/n_conducted), 
    median_rep_ss = median(rep_sample_size, na.rm = T)) 

