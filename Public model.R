# ===============================================================
# Load packages
# ===============================================================
install.packages(c("tidyverse","psych","car","ggpubr","lme4","emmeans","sjPlot",
                   "likert","readr","rstatix","ez","haven","lmerTest","mediation","writexl"))

library(tidyverse)
library(psych)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(ez)
library(haven)
library(likert)
library(writexl)
library(flextable)
library(performance)
library(data.table)
library(stringr)
library(forcats)

# ===============================================================
# Load & clean data
# ===============================================================
data <- read.csv("C:/Users/Nhung/Downloads/df_full.csv", header = TRUE, sep = ",")
names(data)[names(data) == "Duration..in.seconds."] <- "Duration"

cols_to_drop <- c("Duration","rank_explain","expected_changes","matrix_context",
                  "num1","num2","num3","num4","graph1","graph2","graph3","graph4","diff")
data <- data[ , !(names(data) %in% cols_to_drop)]

# Reliability
alpha_result1 <- psych::alpha(data[,c("act_Org_1","act_Org_2","act_Org_3","act_Org_4")])
alpha_result2 <- psych::alpha(data[,c("act_Alt_1","act_Alt_2","act_Alt_3","act_Alt_4")])
print(alpha_result1); print(alpha_result2)

# Factors
data$edu <- factor(data$edu)
data$age <- factor(data$age,
                   levels = c(0,7,1,2,4,5,6,3),
                   labels = c("Under 18","18-19","20-29","30-39","40-49","50-59","60+","Prefer not to say"))

# Mean scores
data <- data %>%
  mutate(act_Org_mean = rowMeans(across(act_Org_1:act_Org_4), na.rm = TRUE),
         act_Alt_mean = rowMeans(across(act_Alt_1:act_Alt_4), na.rm = TRUE))

# ===============================================================
# Trust vs Education / Age
# ===============================================================
t.test(trust_agency ~ edu, data = data); wilcox.test(trust_agency ~ edu, data = data)
t.test(trust_statement ~ edu, data = data); wilcox.test(trust_statement ~ edu, data = data)

aov_agency <- aov(trust_agency ~ age, data = data); summary(aov_agency); TukeyHSD(aov_agency)
kruskal.test(trust_agency ~ age, data = data)
aov_statement <- aov(trust_statement ~ age, data = data); summary(aov_statement)
kruskal.test(trust_statement ~ age, data = data)

pairwise.wilcox.test(data$trust_agency, data$age, p.adjust.method = "bonferroni")

# ===============================================================
# Chi-square: Age x Flood Experience
# ===============================================================
chisq.test(table(data$age, data$flood_experience))
data$age_collapsed <- forcats::fct_collapse(data$age,
                                            "<30"=c("Under 18","18-19","20-29"),
                                            "30-39"="30-39","40-49"="40-49",
                                            "50-59"="50-59","60+"="60+")
chisq.test(table(data$age_collapsed, data$flood_experience))
fisher.test(table(data$age_collapsed, data$flood_experience))

# ===============================================================
# Education vs Action Means
# ===============================================================
t.test(act_Org_mean ~ edu, data = data); wilcox.test(act_Org_mean ~ edu, data = data)
t.test(act_Alt_mean ~ edu, data = data); wilcox.test(act_Alt_mean ~ edu, data = data)

# ===============================================================
# Split groups: 1AB vs 2AB
# ===============================================================
df_1AB <- subset(data, group %in% c("1A","1B"))
df_2AB <- subset(data, group %in% c("2A","2B"))

# Trust comparisons
t.test(df_1AB$trust_agency, df_2AB$trust_agency); wilcox.test(df_1AB$trust_agency, df_2AB$trust_agency)
t.test(df_1AB$trust_statement, df_2AB$trust_statement); wilcox.test(df_1AB$trust_statement, df_2AB$trust_statement)

# Correlations
cor.test(df_1AB$trust_agency, df_1AB$act_Org_mean, method="spearman")
cor.test(df_1AB$trust_statement, df_1AB$act_Org_mean, method="spearman")
cor.test(df_1AB$trust_agency, df_1AB$act_Alt_mean, method="spearman")
cor.test(df_1AB$trust_statement, df_1AB$act_Alt_mean, method="spearman")

cor.test(df_2AB$trust_agency, df_2AB$act_Org_mean, method="spearman")
cor.test(df_2AB$trust_statement, df_2AB$act_Org_mean, method="spearman")
cor.test(df_2AB$trust_agency, df_2AB$act_Alt_mean, method="spearman")
cor.test(df_2AB$trust_statement, df_2AB$act_Alt_mean, method="spearman")

# Regression
summary(lm(act_Org_mean ~ trust_agency + trust_statement, data=df_1AB))
summary(lm(act_Alt_mean ~ trust_agency + trust_statement, data=df_1AB))
summary(lm(act_Org_mean ~ trust_agency + trust_statement, data=df_2AB))
summary(lm(act_Alt_mean ~ trust_agency + trust_statement, data=df_2AB))

# Paired Org vs Alt
t.test(df_1AB$act_Org_mean, df_1AB$act_Alt_mean, paired = TRUE)
t.test(df_2AB$act_Org_mean, df_2AB$act_Alt_mean, paired = TRUE)

# Between 1AB vs 2AB
t.test(df_1AB$act_Org_mean, df_2AB$act_Org_mean)
t.test(df_1AB$act_Alt_mean, df_2AB$act_Alt_mean)

# ===============================================================
# Descriptives
# ===============================================================
desc_group <- data %>%
  group_by(group) %>%
  summarise(n=n(),
            mean_trust_agency=mean(trust_agency,na.rm=TRUE),
            sd_trust_agency=sd(trust_agency,na.rm=TRUE),
            mean_trust_statement=mean(trust_statement,na.rm=TRUE),
            sd_trust_statement=sd(trust_statement,na.rm=TRUE),
            mean_org=mean(act_Org_mean,na.rm=TRUE),
            sd_org=sd(act_Org_mean,na.rm=TRUE),
            mean_alt=mean(act_Alt_mean,na.rm=TRUE),
            sd_alt=sd(act_Alt_mean,na.rm=TRUE))
print(desc_group)

desc_age <- data %>%
  group_by(age) %>%
  summarise(n=n(),
            mean_trust_agency=mean(trust_agency,na.rm=TRUE),
            sd_trust_agency=sd(trust_agency,na.rm=TRUE),
            mean_trust_statement=mean(trust_statement,na.rm=TRUE),
            sd_trust_statement=sd(trust_statement,na.rm=TRUE))
print(desc_age)
# ===============================================================
# Long-format data for ANOVA
# ===============================================================
long <- data %>%
  pivot_longer(
    cols = matches("^(org|v1|v2)_(likelihood|severe|confident)$"),
    names_to = c("format","rating_type"),
    names_pattern = "^(org|v1|v2)_(likelihood|severe|confident)$",
    values_to = "score",
    values_drop_na = TRUE
  ) %>%
  mutate(
    rating_type = recode(rating_type,
                         "severe"="severity",
                         "confident"="confidence"),
    rating_type = factor(rating_type, levels=c("likelihood","severity","confidence")),
    format = factor(format, levels=c("org","v1","v2"))
  )

long <- long %>%
  mutate(
    cell_location = case_when(group %in% c("1B","2B") ~ "Sig impact - Very likely",
                              group %in% c("1A","2A") ~ "Sig impact - Somewhat likely"),
    change_type = case_when(group %in% c("1A","1B") ~ "Size, color & axis",
                            group %in% c("2A","2B") ~ "Number range"),
    format = case_when(format=="v1" & group %in% c("1A","1B") ~ "horizontal impact",
                       format=="v2" & group %in% c("1A","1B") ~ "vertical impact",
                       format=="v1" & group %in% c("2A","2B") ~ "discrete number",
                       format=="v2" & group %in% c("2A","2B") ~ "continuous number",
                       TRUE ~ as.character(format)),
    color_level = ifelse(group %in% c("1A","2A"), "Yellow","Amber"),
    color_seen = case_when(group %in% c("1A","2A") ~ "Yellow",
                           format=="org" & group=="1B" ~ "Amber",
                           format %in% c("horizontal impact","vertical impact") & group=="1B" ~ "Red",
                           group=="2B" ~ "Amber"),
    axis_x = case_when(group %in% c("1A","1B") & format=="horizontal impact" ~ "Impact",
                       group %in% c("1A","1B") & format=="vertical impact" ~ "Likelihood",
                       TRUE ~ "Impact")
  )

write_csv(long, "C:/Users/Nhung/Downloads/df_long.csv")

df_rate_long <- long %>%
  select(ResponseId,format,rating_type,score,cell_location,change_type,color_level,
         color_seen,axis_x,num_score,graph_score,matrix_experienced,age,gender,edu,
         flood_experienced,group,act_Org_mean,act_Alt_mean,numeracy_level,graph_literacy,
         trust_agency,trust_statement)

# ===============================================================
# Mediation (example with likelihood → continuous number)
# ===============================================================
df <- df_rate_long %>%
  filter(rating_type=="likelihood",
         format %in% c("org","continuous number"),
         group %in% c("2A","2B")) %>%
  mutate(format_bin = ifelse(format=="continuous number",1,0),
         action_diff = act_Alt_mean - act_Org_mean)

model.m <- lm(score ~ format_bin, data=df)
model.y <- lm(action_diff ~ format_bin + score, data=df)
med.out <- mediate(model.m, model.y, treat="format_bin", mediator="score",
                   boot=TRUE, sims=1000)
summary(med.out)

# ===============================================================
# ANOVA: Likelihood, Severity, Confidence
# ===============================================================
analyze_rating <- function(data, rating){
  cat("\n====================", toupper(rating), "====================\n")
  df <- subset(data, rating_type==rating)
  df$format_new <- factor(df$format,
                          levels=c("org","horizontal impact","vertical impact",
                                   "discrete number","continuous number"),
                          labels=c("original","alt_sizecolor","alt_sizecolor",
                                   "alt_discrete","alt_continuous"))
  
  # Mixed model (full with covariates)
  m_full <- lmer(score ~ format_new*color_level + axis_x + age+gender+edu+flood_experienced+
                   num_score+graph_score+matrix_experienced+trust_agency+trust_statement+
                   act_Org_mean + (1|ResponseId),
                 data=df)
  print(anova(m_full,type=3))
  print(effectsize::eta_squared(m_full, partial=TRUE))
  print(emmeans(m_full, pairwise ~ format_new|color_level))
}

capture.output({
  analyze_rating(df_rate_long,"likelihood")
  analyze_rating(df_rate_long,"severity")
  analyze_rating(df_rate_long,"confidence")
}, file="C:/Users/Nhung/Downloads/anova_full_results.txt")

# ===============================================================
# Ranking analysis
# ===============================================================
df_rank_long <- data %>%
  filter(!is.na(easy)&!is.na(medium)&!is.na(hard)) %>%
  select(ResponseId,group,easy,medium,hard) %>%
  pivot_longer(cols=c(easy,medium,hard),names_to="position",values_to="format") %>%
  mutate(rank_order = recode(position,"easy"=1,"medium"=2,"hard"=3),
         format = case_when(format=="1"~"Org",
                            format=="3"&group%in%c("1A","1B")~"Horizontal impact",
                            format=="2"&group%in%c("1A","1B")~"Vertical impact",
                            format=="2"&group%in%c("2A","2B")~"Discrete number",
                            format=="3"&group%in%c("2A","2B")~"Continuous number",
                            TRUE~format),
         change_type = ifelse(group%in%c("1A","1B"),"Size, color & axis","Number range"))

# Friedman test + post-hoc
friedman_results <- df_rank_long %>%
  group_by(change_type) %>% friedman_test(rank_order~format|ResponseId)
pairwise_results <- df_rank_long %>%
  group_by(change_type) %>%
  pairwise_wilcox_test(rank_order~format, paired=TRUE, p.adjust.method="bonferroni")

writeLines(capture.output({
  print(friedman_results)
  print(pairwise_results)
}), "C:/Users/Nhung/Downloads/ranking_results.txt")

# ===============================================================
# Comparison (choice A/B)
# ===============================================================
com <- data %>% select(ResponseId,group,compare_1,compare_2,compare_3)
df_compare_long <- com %>%
  pivot_longer(cols=starts_with("compare"),names_to="compare_var",values_to="choice") %>%
  filter(!is.na(choice)) %>%
  mutate(format = case_when(
    group%in%c("1A","1B")&compare_var=="compare_1"~"Org",
    group%in%c("1A","1B")&compare_var=="compare_2"~"Horizontal impact",
    group%in%c("1A","1B")&compare_var=="compare_3"~"Vertical impact",
    group%in%c("2A","2B")&compare_var=="compare_1"~"Org",
    group%in%c("2A","2B")&compare_var=="compare_2"~"Discrete number",
    group%in%c("2A","2B")&compare_var=="compare_3"~"Continuous number"),
    choice=factor(choice,levels=c(1,2),labels=c("A","B")))

df_compare_long <- df_compare_long %>%
  mutate(choice_bin=ifelse(choice=="B",1,0))
model_com <- glm(choice_bin ~ format, data=df_compare_long, family=binomial)
summary(model_com)

# ===============================================================
# Plotting Likelihood/Severity/Confidence
# ===============================================================
df_all <- data.frame(
  Outcome=rep(c("Likelihood","Severity","Confidence"),each=8),
  WarningLevel=rep(rep(c("Amber","Yellow"),each=4),3),
  Format=rep(c("Original","Size (H&V)","Discrete","Continuous"),6),
  Mean=c(88.3,88.8,87.8,90.6,44.5,36.7,43.4,49.6,
         72.6,70.4,75.1,79.3,60.1,45.7,58.2,63.6,
         67.9,67.5,71.4,71.1,59.2,57.6,61.7,60.5),
  SE=c(rep(4.8,4),rep(4.9,4),rep(4.7,4),rep(4.8,4),rep(7.3,4),rep(7.4,4))
)

df_all$Format <- factor(df_all$Format,levels=c("Original","Size (H&V)","Discrete","Continuous"))
custom_colors <- c("Original"="#193477","Size (H&V)"="#69b3fb","Discrete"="#debc99","Continuous"="#c47a43")

plot_outcome <- function(data,outcome_name){
  ggplot(data %>% filter(Outcome==outcome_name),
         aes(x=WarningLevel,y=Mean,fill=Format))+
    geom_bar(stat="identity",position=position_dodge(0.9),width=0.8,color="black")+
    geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),
                  position=position_dodge(0.9),width=0.25)+
    scale_fill_manual(values=custom_colors)+
    ylim(0,100)+labs(title=outcome_name,y="Mean ± SE",x="Warning level")+
    theme_minimal(base_size=14)+
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_line(color="grey70"),
          legend.position="bottom",legend.title=element_blank())
}

ggsave("plot_likelihood.png", plot_outcome(df_all,"Likelihood"), width=6, height=5, dpi=300)
ggsave("plot_severity.png", plot_outcome(df_all,"Severity"), width=6, height=5, dpi=300)
ggsave("plot_confidence.png", plot_outcome(df_all,"Confidence"), width=6, height=5, dpi=300)

