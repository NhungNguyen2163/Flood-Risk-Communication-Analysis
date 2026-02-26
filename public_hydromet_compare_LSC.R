# ===============================================================
# Load packages
# ===============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(DescTools)

# ===============================================================
# Load data
# ===============================================================
ex <- read.csv("C:/Users/Nhung/Downloads/hydromets_format_long.csv", header=TRUE, sep=",")
pu <- read.csv("C:/Users/Nhung/Downloads/public_format_long.csv", header=TRUE, sep=",")

# ===============================================================
# Helper: descriptive + tests
# ===============================================================
compare_audience <- function(df, label){
  cat("\n==================", toupper(label), "==================\n")
  
  print(df %>%
          group_by(audience, rating_type) %>%
          summarise(mean=mean(score,na.rm=TRUE),
                    median=median(score,na.rm=TRUE),
                    sd=sd(score,na.rm=TRUE),
                    IQR=IQR(score,na.rm=TRUE),
                    .groups="drop"))
  
  for(rt in unique(df$rating_type)){
    cat("\n---", rt, "(Welch t-test) ---\n")
    print(t.test(score ~ audience,
                 data=subset(df, rating_type==rt),
                 var.equal=FALSE))
  }
  
  m <- lmer(score ~ audience * rating_type + (1|ResponseId), data=df)
  print(anova(m, type=3))
  print(emmeans(m, pairwise ~ audience | rating_type))
  
  for(rt in unique(df$rating_type)){
    cat("\n---", rt, "(Wilcoxon) ---\n")
    print(wilcox.test(score ~ audience,
                      data=subset(df, rating_type==rt)))
  }
  
  print(
    ggplot(df, aes(x=audience, y=score, fill=audience)) +
      geom_boxplot(outlier.shape=16, outlier.color="red") +
      facet_wrap(~rating_type, scales="free_y") +
      theme_minimal()
  )
}

# ===============================================================
# ORG
# ===============================================================
df_expert_org <- ex %>%
  filter(format=="org") %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Expert")

df_public_org <- pu %>%
  filter(format=="org", group %in% c("1A","2A")) %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Public")

org_compare <- bind_rows(df_expert_org, df_public_org)
compare_audience(org_compare, "org")

# Winsorize
winsorize_vec <- function(x, probs=c(0.05,0.95)){
  qs <- quantile(x, probs=probs, na.rm=TRUE)
  x[x<qs[1]] <- qs[1]; x[x>qs[2]] <- qs[2]
  return(x)
}

org_compare <- org_compare %>%
  group_by(rating_type,audience) %>%
  mutate(score_wins=winsorize_vec(score,probs=c(0.05,0.95))) %>%
  ungroup()

for(rt in unique(org_compare$rating_type)){
  cat("\n---", rt, "(Winsorized Welch t-test) ---\n")
  print(t.test(score_wins ~ audience,
               data=subset(org_compare, rating_type==rt),
               var.equal=FALSE))
}

# ===============================================================
# SIZE
# ===============================================================
df_expert_size <- ex %>%
  filter(format=="alt_1", Group=="1A2B") %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Expert")

df_public_size <- pu %>%
  filter(format=="horizontal impact", group %in% c("1A")) %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Public")

size_compare <- bind_rows(df_expert_size, df_public_size) %>%
  mutate(format="size")
compare_audience(size_compare, "size")

# ===============================================================
# CONTINUOUS
# ===============================================================
df_expert_num <- ex %>%
  filter(format=="alt_1", Group=="1B2A") %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Expert")

df_public_num <- pu %>%
  filter(format=="continuous number", group %in% c("2A")) %>%
  select(ResponseId,rating_type,score,format) %>%
  mutate(audience="Public")

continuous_compare <- bind_rows(df_expert_num, df_public_num) %>%
  mutate(format="continuous")
compare_audience(continuous_compare, "continuous")

# ===============================================================
# R version
# ===============================================================
R.version
