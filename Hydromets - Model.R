# ===============================================================
# Load packages
# ===============================================================
install.packages(c("afex","car","performance","emmeans","irr"))
library(afex)
library(car)
library(performance)
library(emmeans)
library(dplyr)
library(tidyr)
library(ggplot2)
library(irr)

# ===============================================================
# Load & prepare data
# ===============================================================
expert <- read.csv("C:/Users/Nhung/Downloads/Hydromets.xlsx - Sheet0.csv", header = TRUE, sep = ",")
summary(expert)

expert <- expert %>%
  rename(org_likelihood = org_likelihood_1,
         org_severity = org_severe_1,
         org_confidence = org_confident_1)

expert$work_experience <- factor(expert$work_experience,
                                 levels = c("Less than 1 year",
                                            "1 - 3 years",
                                            "5 - 10 years",
                                            "More than 10 years"))

expert <- expert %>%
  mutate(mapping_3 = ifelse(mapping_3 == "Other", "32", mapping_3))

# ===============================================================
# Long-format datasets
# ===============================================================
expert_format_long <- expert %>%
  pivot_longer(cols = matches("^(org|alt_\\d+)_(likelihood|severity|confidence)$"),
               names_to = c("format", "rating_type"),
               names_pattern = "^(org|alt_\\d+)_(likelihood|severity|confidence)$",
               values_to = "score", values_drop_na = TRUE) %>%
  select(ResponseId, work_experience, Group, format, rating_type, score)

expert_compare_long <- expert %>%
  pivot_longer(cols = matches("risk_compare_\\d+$"),
               names_to = "compare_var", values_to = "choice",
               values_drop_na = TRUE) %>%
  select(ResponseId, work_experience, Group, compare_var, choice)

expert_rank_long <- expert %>%
  pivot_longer(cols = matches("^R\\d+$"),
               names_to = "rank_var", values_to = "rank_value",
               values_drop_na = TRUE) %>%
  select(ResponseId, work_experience, Group, rank_var, rank_value)

expert_mapping_long <- expert %>%
  mutate(across(starts_with("mapping_"), as.character)) %>%
  pivot_longer(cols = matches("^mapping_\\d+$"),
               names_to = "mapping_var", values_to = "recorded",
               values_drop_na = TRUE) %>%
  mutate(mapping_value = as.numeric(recorded)) %>%
  select(ResponseId, work_experience, Group, mapping_var, recorded)

# ===============================================================
# Recode variables for format_long
# ===============================================================
expert_format_long$Group <- ifelse(expert_format_long$Group == "1A2B", 1,
                                   ifelse(expert_format_long$Group == "1B2A", 2, NA))

expert_format_long <- expert_format_long %>%
  mutate(
    cell_location = case_when(
      format == "org" ~ "Sig impact - Low likelihood",
      Group %in% c("1", "2") & format == "alt_1" ~ "Sig impact - Low likelihood",
      Group %in% c("1", "2") & format == "alt_2" ~ "Minor impact - Medium likelihood"),
    likelihood_level = case_when(
      format == "org" ~ "2",
      Group %in% c("1", "2") & format == "alt_1" ~ "2",
      Group %in% c("1", "2") & format == "alt_2" ~ "3"),
    impact_level = case_when(
      format == "org" ~ "3",
      Group %in% c("1", "2") & format == "alt_1" ~ "3",
      Group %in% c("1", "2") & format == "alt_2" ~ "2"),
    format_new = case_when(
      format == "org" ~ "org",
      Group == "1" & format == "alt_1" ~ "size",
      Group == "1" & format == "alt_2" ~ "continuous",
      Group == "2" & format == "alt_1" ~ "continuous",
      Group == "2" & format == "alt_2" ~ "size"),
    has_number = ifelse(format_new == "continuous", 1, 0),
    has_sizechange = ifelse(format_new == "size", 1, 0),
    color = "yellow",
    format_new = factor(format_new, levels = c("org","size","continuous")),
    cell_location = factor(cell_location,
                           levels = c("Sig impact - Low likelihood",
                                      "Minor impact - Medium likelihood")),
    color = factor(color, levels = c("yellow"))
  )

write.csv(expert_format_long,
          file = "C:/Users/Nhung/Downloads/expert_rating_long.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ===============================================================
# Likelihood models
# ===============================================================
ex_likelihood <- subset(expert_format_long, rating_type == "likelihood")

main <- lmer(score ~ format_new * cell_location + work_experience +
               (1|Group) + (1|ResponseId), data = ex_likelihood)
anova(main, type=3); ranova(main); check_collinearity(main)
emmeans(main, pairwise ~ format_new|cell_location, adjust="tukey")

main1 <- lmer(score ~ format_new + cell_location + work_experience +
                (1|ResponseId), data = ex_likelihood)
anova(main1, type=3); check_collinearity(main1)
emmeans(main1, pairwise ~ format_new, adjust="tukey")

m_lmm <- lmer(score ~ format_new + work_experience + likelihood_level +
                (1|ResponseId), data = ex_likelihood)
anova(m_lmm, type=3); check_collinearity(m_lmm)
emmeans(m_lmm, pairwise ~ format_new|likelihood_level, adjust="tukey")

ex_org <- subset(ex_likelihood, format_new=="org" & cell_location=="Sig impact - Low likelihood")
t.test(score ~ Group, data=ex_org)

# ===============================================================
# Severity models
# ===============================================================
ex_severity <- subset(expert_format_long, rating_type == "severity")

main <- lmer(score ~ format_new * cell_location + work_experience +
               (1|Group) + (1|ResponseId), data = ex_severity)
anova(main, type=3); ranova(main); check_collinearity(main)
emmeans(main, pairwise ~ format_new|cell_location, adjust="tukey")

main1 <- lmer(score ~ format_new + cell_location + work_experience +
                (1|ResponseId), data = ex_severity)
anova(main1, type=3); check_collinearity(main1)
emmeans(main1, pairwise ~ format_new|cell_location, adjust="tukey")

m_lmm <- lmer(score ~ format_new + work_experience + impact_level +
                (1|ResponseId), data = ex_severity)
anova(m_lmm, type=3); check_collinearity(m_lmm)
emmeans(m_lmm, pairwise ~ format_new|impact_level, adjust="tukey")

ex_org <- subset(ex_severity, format_new=="org" & cell_location=="Sig impact - Low likelihood")
t.test(score ~ Group, data=ex_org)

# ===============================================================
# Confidence models
# ===============================================================
ex_conf <- subset(expert_format_long, rating_type == "confidence")

ex_org <- subset(ex_conf, format_new=="org" & cell_location=="Sig impact - Low likelihood")
t.test(score ~ Group, data=ex_org)

main <- lmer(score ~ format_new * cell_location + work_experience +
               (1|Group) + (1|ResponseId), data = ex_conf)
anova(main, type=3); check_collinearity(main)
emmeans(main, pairwise ~ format_new|cell_location, adjust="tukey")

main1 <- lmer(score ~ format_new + cell_location + work_experience +
                (1|ResponseId), data = ex_conf)
anova(main1, type=3); check_collinearity(main1)
emmeans(main1, pairwise ~ format_new|cell_location, adjust="tukey")

m_lmm <- lmer(score ~ format_new + work_experience + impact_level + likelihood_level +
                (1|ResponseId), data = ex_conf)
anova(m_lmm, type=3); check_collinearity(m_lmm)
emmeans(m_lmm, pairwise ~ format_new|impact_level, adjust="tukey")

ex_conf_sub <- subset(ex_conf, cell_location != "Minor impact - Medium likelihood")
main_sub <- lmer(score ~ format_new + work_experience + Group + (1|ResponseId), data = ex_conf_sub)
anova(main_sub, type=3); check_collinearity(main_sub)
emmeans(main_sub, pairwise ~ format_new|work_experience, adjust="tukey")

# ===============================================================
# Ranking
# ===============================================================
df_rank <- expert_rank_long %>%
  mutate(format = case_when(rank_value==1~"Org",
                            rank_value==2~"Size",
                            rank_value==3~"Axis",
                            rank_value==4~"Discrete",
                            rank_value==5~"Continuous"),
         rank_order = as.numeric(str_remove(rank_var,"R"))) %>%
  select(ResponseId, format, rank_order, work_experience)

friedman_test(rank_order ~ format | ResponseId, data=df_rank)
friedman_effsize(rank_order ~ format | ResponseId, data=df_rank)
pairwise_wilcox_test(rank_order ~ format, paired=TRUE,
                     p.adjust.method="bonferroni", data=df_rank)

# ===============================================================
# Compare
# ===============================================================
mapping_compare <- tribble(
  ~compare_var,~scenario,~A_location,~B_location,~A_color,~B_color,~A_size,~B_size,~A_number,~B_number,~x_axis,
  "risk_compare_1","org","L3I2","L2I3","yellow","yellow","Same","Same",NA,NA,"Impact",
  "risk_compare_2","size","L4I2","L1I3","yellow","yellow","Bigger","Smaller",NA,NA,"Impact",
  "risk_compare_3","continuous","L3I2","L1I3","yellow","yellow","Same","Same",7,9,"Impact"
)

df_compare <- expert_compare_long %>%
  left_join(mapping_compare, by="compare_var") %>%
  mutate(choice_bin=ifelse(choice=="Risk B",1,0),
         diff_size=ifelse(A_size!=B_size,1,0),
         diff_number=ifelse(is.na(A_number)|is.na(B_number),0,
                            ifelse(A_number!=B_number,1,0)))

tab_choice <- df_compare %>%
  count(scenario,choice) %>%
  group_by(scenario) %>%
  mutate(percent=round(n/sum(n)*100,1))

tab_matrix <- table(df_compare$scenario, df_compare$choice)
fisher.test(tab_matrix)

# ===============================================================
# Mapping alignment
# ===============================================================
expert_mapping_long <- expert_mapping_long %>%
  mutate(expected_value = case_when(mapping_var=="mapping_1"~23,
                                    mapping_var=="mapping_2"~22,
                                    mapping_var=="mapping_3"~41),
         recorded_color = case_when(recorded %in% c(23,32,42)~"yellow",
                                    recorded %in% c(22,21,41,31)~"green",
                                    recorded %in% c(24,33)~"amber"),
         expected_color = case_when(expected_value %in% c(23,32,42)~"yellow",
                                    expected_value %in% c(22,21,41,31)~"green",
                                    expected_value %in% c(24,33)~"amber"),
         lik_lvl=as.integer(substr(expected_value,1,1)),
         impact_lvl=as.integer(substr(expected_value,2,2)),
         rec_lik_lvl=as.integer(substr(recorded,1,1)),
         rec_im_lvl=as.integer(substr(recorded,2,2)),
         rec_risk_score=rec_lik_lvl*rec_im_lvl,
         ex_risk_score=lik_lvl*impact_lvl,
         alignment_risk_score=ex_risk_score-rec_risk_score,
         alignment_label=case_when(alignment_risk_score==0~"Exactly",
                                   alignment_risk_score<0~"Overwarning",
                                   alignment_risk_score>0~"Underwarning"),
         align_lik_score=lik_lvl-rec_lik_lvl,
         align_impact_score=impact_lvl-rec_im_lvl,
         format=case_when(mapping_var=="mapping_1"~"org",
                          mapping_var=="mapping_2"~"size",
                          mapping_var=="mapping_3"~"continuous"))

write.csv(expert_mapping_long,
          "C:/Users/Nhung/Downloads/expert_mapping_long.csv",
          row.names=FALSE,fileEncoding="UTF-8")

tab <- table(expert_mapping_long$format, expert_mapping_long$alignment_label)
fisher.test(tab)

kruskal.test(alignment_risk_score ~ work_experience, data=expert_mapping_long)

df_wide <- reshape2::dcast(expert_mapping_long, ResponseId ~ mapping_var, value.var="recorded")
kappam.fleiss(df_wide[,2:4])
