# setwd("~/Desktop/samplesize_for_decisionmaking")


setwd("C:/Users/collazoa/OneDrive - Charité - Universitätsmedizin Berlin/Dokumente/GitHub/success_in_sample_size")

source("./scripts/data_wrangling/load_packages.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/bri_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/crp_data_wrangling.R")
# setwd("C:/Users/collazoa/OneDrive - Charit? - Universit?tsmedizin Berlin/Dokumente/GitHub/code_replication_sample_size")
source("./scripts/data_wrangling/cps_data_wrangling.R")


sel <- c("orig_ss", "rep_ss", "orig_p_2sided", "effect_size_type", "orig_d", 
                            "orig_ci_low", "orig_ci_high", "zo", "project")

df_combined <- bind_rows(
  cps %>% select(all_of(sel)), 
  bri %>% select(all_of(sel)), 
  crp %>% select(all_of(sel))
  )

#save(df_combined, file = "./data/df_combined.RData")
