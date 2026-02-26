# ===============================================================
# I. Install packages and libraries
# ===============================================================
install.packages(c("tidyverse","psych","car","ggpubr","lme4","emmeans",
                   "sjPlot","likert","readr","ez"))
library(tidyverse); library(psych); library(ggplot2); library(data.table)
library(likert); library(ez); library(stringr)

# ===============================================================
# II. Data Quality checking
# ===============================================================
df <- read.csv("C:/Users/Nhung/Downloads/Public - data (in use).csv", header=TRUE, sep=",")
df <- df[-c(1,2), ]
df <- df[df$Finished != "0", ]
df$group <- as.factor(df$group)
names(df) <- gsub("act_alt","act_Alt",names(df))

group_list <- split(df, df$group)
for(grp in names(group_list)){
  filename <- paste0("group_", grp, ".csv")
  write.csv(group_list[[grp]], file=filename, row.names=FALSE)
}

df_1a <- read.csv("C:/Users/Nhung/Downloads/group_1A.csv")
df_1b <- read.csv("C:/Users/Nhung/Downloads/group_1B.csv")
df_2a <- read.csv("C:/Users/Nhung/Downloads/group_2A.csv")
df_2b <- read.csv("C:/Users/Nhung/Downloads/group_2B.csv")

df_1a <- df_1a[, colSums(is.na(df_1a)) < nrow(df_1a)]
df_1b <- df_1b[, colSums(is.na(df_1b)) < nrow(df_1b)]
df_2a <- df_2a[, colSums(is.na(df_2a)) < nrow(df_2a)]
df_2b <- df_2b[, colSums(is.na(df_2b)) < nrow(df_2b)]

map <- c(
  "Org_likelihood"="org_likelihood","Org_severe"="org_severe","Org_confident"="org_confident",
  "1_likelihood"="v1_likelihood","1_severe"="v1_severe","1_confident"="v1_confident",
  "2_likelihood"="v2_likelihood","2_severe"="v2_severe","2_confident"="v2_confident",
  "compare_1"="compare_1","compare_2"="compare_2","compare_3"="compare_3",
  "rank_1"="easy","rank_2"="medium","rank_3"="hard","rank_explain"="rank_explain",
  "act_Org_1"="act_Org_1","act_Org_2"="act_Org_2","act_Org_3"="act_Org_3","act_Org_4"="act_Org_4",
  "act_Alt_1"="act_Alt_1","act_Alt_2"="act_Alt_2","act_Alt_3"="act_Alt_3","act_Alt_4"="act_Alt_4"
)

rename_by_contains <- function(df, mapping, ignore_case=FALSE){
  nms <- names(df); drop_idx <- integer(0)
  for(pat in names(mapping)){
    hits <- grep(pat,nms,fixed=TRUE,ignore.case=ignore_case)
    if(length(hits)==0) next
    new_name <- unname(mapping[[pat]])
    if(is.na(new_name)||new_name==""){drop_idx <- c(drop_idx,hits)}
    else{nms[hits] <- new_name}
  }
  names(df) <- nms
  if(length(drop_idx)) df <- df[,-unique(drop_idx),drop=FALSE]
  dup <- which(duplicated(names(df)))
  if(length(dup)) df <- df[,-dup,drop=FALSE]
  df
}

df_1a <- rename_by_contains(df_1a,map)
df_1b <- rename_by_contains(df_1b,map)
df_2a <- rename_by_contains(df_2a,map)
df_2b <- rename_by_contains(df_2b,map)

df_full <- bind_rows(df_1a,df_1b,df_2a,df_2b)
write.csv(df_full,"Public_append.csv",row.names=FALSE,fileEncoding="UTF-8")
df_full <- read.csv("C:/Users/Nhung/Downloads/Public_append.csv")

missing_count <- sapply(df_full,function(x) sum(is.na(x)|x==""))
names(df_full) <- gsub("^num4$","num3",names(df_full))
names(df_full) <- gsub("^num6$","num4",names(df_full))

summary(duplicated(df_full$ResponseId))
summary(duplicated(df_full$prolific_ID))
table(df_full$group)

# ===============================================================
# Numeracy & Graph literacy
# ===============================================================
df_full$num1_score <- ifelse(df_full$num1==2,1,0)
df_full$num2_score <- ifelse(df_full$num2==3,1,0)
df_full$num3_score <- ifelse(df_full$num3==1,1,0)
df_full$num4_score <- ifelse(!is.na(df_full$num4)&df_full$num4==4,1,0)
df_full$num_score <- df_full$num1_score+df_full$num2_score+df_full$num3_score+df_full$num4_score
df_full$numeracy_level <- cut(df_full$num_score,breaks=c(-1,2,4),labels=c("low","high"))

df_full$graph1_score <- ifelse(df_full$graph1==3,1,0)
df_full$graph2_score <- ifelse(df_full$graph2==3,1,0)
df_full$graph3_score <- ifelse(df_full$graph3==1,1,0)
df_full$graph4_score <- ifelse(df_full$graph4==4,1,0)
df_full$graph_score <- df_full$graph1_score+df_full$graph2_score+df_full$graph3_score+df_full$graph4_score
df_full$graph_literacy <- cut(df_full$graph_score,breaks=c(-1,2,4),labels=c("low","high"))

# ===============================================================
# Format variables
# ===============================================================
factor_vars <- c("group","gender","age","edu","diff","flood_experienced",
                 "reccent_flood","house_condition","compare_1","compare_2","compare_3",
                 "graph1","graph2","graph3","graph4","num1","num2","num3","num4")
df_full[factor_vars] <- lapply(df_full[factor_vars], as.factor)

ordered_vars <- c("matrix_clarity","matrix_experienced","matrix_intended_act",
                  "trust_1_expertise","trust_1_prepare","trust_1_data",
                  "trust_2_reliable","trust_2_matrix","trust_2_graphics","trust_2_decision",
                  "act_Org_1","act_Org_2","act_Org_3","act_Org_4",
                  "act_Alt_1","act_Alt_2","act_Alt_3","act_Alt_4")
df_full[ordered_vars] <- lapply(df_full[ordered_vars], function(x) factor(x,levels=c("1","2","3","4","5"),ordered=TRUE))

numeric_vars <- grep("likelihood|severe|confident|Duration|Recaptcha|score",names(df_full),value=TRUE)
df_full[numeric_vars] <- lapply(df_full[numeric_vars],as.numeric)

text_vars <- c("expected_changes","matrix_context","rank_explain")
df_full[text_vars] <- lapply(df_full[text_vars],as.character)

# ===============================================================
# Quality checks
# ===============================================================
df_full$open_missing_count <- rowSums(df_full[c("expected_changes","matrix_context","rank_explain")]==""
                                      |is.na(df_full[c("expected_changes","matrix_context","rank_explain")]))
df_full$missing_2plus_open <- df_full$open_missing_count>2
df_full$too_fast <- as.numeric(df_full$Duration..in.seconds.)<677
likert_cols <- c("matrix_clarity","matrix_experienced","matrix_intended_act",
                 "trust_1_expertise","trust_1_prepare","trust_1_data",
                 "trust_2_reliable","trust_2_matrix","trust_2_graphics","trust_2_decision",
                 "act_Org_1","act_Org_2","act_Org_3","act_Org_4",
                 "act_Alt_1","act_Alt_2","act_Alt_3","act_Alt_4")
df_full$uniform_response <- apply(df_full[likert_cols],1,function(row){
  row <- as.character(row); length(unique(row[!is.na(row)&row!=""]))==1
})

df_flagged_2plus <- df_full %>%
  filter((missing_2plus_open+too_fast+uniform_response)>=2 & Q_RecaptchaScore<0.5)

cols_to_remove <- c("StartDate","EndDate","Status","Progress","RecordedDate",
                    "DistributionChannel","UserLanguage","Q_RecaptchaScore",
                    "consent_1","consent_2","consent_3","consent_4",
                    "consent_5","consent_6","Finished","prolific_ID",
                    "open_missing_count","missing_2plus_open","too_fast","uniform_response")
df_full <- df_full[,!(names(df_full)%in%cols_to_remove)]



###=============================================================================
# ------------- III. Descriptive statistics - Overall & Test
# 1. Duration
ggplot(df_full, aes(x = Duration..in.seconds.)) +
  geom_histogram(binwidth = 60, fill = "steelblue", color = "black") +
  labs(
    title = "Time completion distribution",
    x = "In second",
    y = "Number of Participants"
  ) +
  theme_minimal()

ggplot(df_full, aes(x = group, y = Duration..in.seconds.)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(
    title = "Time completion distribution by group",
    x = "Group",
    y = "Time in seconds"
  ) +
  theme_minimal()

# 2. Age

df_full$gender <- factor(df_full$gender,
                         levels = c(0, 1, 2, 3),
                         labels = c("Male", "Female", 
                                    "Non-binary / third gender", 
                                    "Prefer not to say"))

ggplot(df_full, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  labs(
    title = "Gender Distribution in the Sample",
    x = "Gender",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(df_full, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ group) +
  labs(
    title = "Gender distribution by group",
    x = "Gender",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

df_full$age_group <- factor(df_full$age,
                            levels = c(0, 7, 1, 2, 4, 5, 6, 3),
                            labels = c("Under 18", "18–19", "20–29", "30–39",
                                       "40–49", "50–59", "60+", "Prefer not to say"),
                            ordered = TRUE
)

ggplot(df_full, aes(x = age_group)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Age distribution",
       x = "Age group",
       y = "Number of Participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(df_full, aes(x = age_group)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ group) +
  labs(
    title = "Age distribution by Group formats",
    x = "Age group",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 3. Education level
df_full$edu <- factor(df_full$edu,
                      levels = c(0, 1),
                      labels = c("No degree", "Degree or higher"))

ggplot(df_full, aes(x = edu)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +
  labs(
    title = "Education level distribution in the Sample",
    x = "Education level",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggplot(df_full, aes(x = edu)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ group) +
  labs(
    title = "Education level distribution by group formats",
    x = "Education level",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# 4. recent_flood, flood experience và house condition
summary(df_full$reccent_flood)
summary(df_full$flood_experienced)
summary(df_full$house_condition)
summary(df_full$diff)

# 5. Numeracy score
df_long <- df_full %>%
  select(num1, num2, num3, num4) %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "response")

ggplot(df_long, aes(x = response)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ question, scales = "free_x") +
  labs(title = "Distribution of responses for numeracy questions",
       x = "Response", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(df_full, aes(x = numeracy_level)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Numeracy Level",
       x = "Numeracy Level", y = "Number of people") +
  theme_minimal()

ggplot(df_full, aes(x = numeracy_level)) +
  geom_bar(fill = "darkorange") +
  facet_wrap(~ group) +
  labs(title = "Distribution of Numeracy Level by Group",
       x = "Numeracy Level", y = "Number of People") +
  theme_minimal()

# Histogram numeracy_score
ggplot(df_full, aes(x = num_score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Numeracy Score",
    x = "Numeracy Score",
    y = "Count"
  ) +
  theme_minimal()

score_ratios <- df_full %>%
  summarise(
    num1 = mean(num1_score == 1, na.rm = TRUE) * 100,
    num2 = mean(num2_score == 1, na.rm = TRUE) * 100,
    num3 = mean(num3_score == 1, na.rm = TRUE) * 100,
    num4 = mean(num4_score == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "percent")

ggplot(score_ratios, aes(x = item, y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage of right answer in Numeracy test",
    x = "Question",
    y = "Percentage (%) of right answer"
  ) +
  ylim(0, 100) +
  theme_minimal()


score_ratios_by_group <- df_full %>%
  group_by(group) %>%
  summarise(
    num1 = mean(num1_score == 1, na.rm = TRUE) * 100,
    num2 = mean(num2_score == 1, na.rm = TRUE) * 100,
    num3 = mean(num3_score == 1, na.rm = TRUE) * 100,
    num4 = mean(num4_score == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(
    cols = -group,
    names_to = "item",
    values_to = "percent"
  )

ggplot(score_ratios_by_group, aes(x = item, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of right answer in Numeracy test by Group",
    x = "Question",
    y = "Percentage (%) right answer (%)"
  ) +
  ylim(0, 100) +
  theme_minimal()

# 6. Graph literacy score
df_long <- df_full %>%
  select(graph1, graph2, graph3, graph4) %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "response")

ggplot(df_long, aes(x = response)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ question, scales = "free_x") +
  labs(title = "Distribution of responses for graph literacy questions",
       x = "Response", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(df_full, aes(x = graph_literacy)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of graph literacy Level",
       x = "Graph literacy Level", y = "Number of people") +
  theme_minimal()

ggplot(df_full, aes(x = graph_literacy)) +
  geom_bar(fill = "darkorange") +
  facet_wrap(~ group) +
  labs(title = "Distribution of graph literacy Level by Group",
       x = "Graph literacy Level", y = "Number of People") +
  theme_minimal()

ggplot(df_full, aes(x = graph_score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Graph literacy Score",
    x = "Graph Literacy Score",
    y = "Count"
  ) +
  theme_minimal()

score_ratios <- df_full %>%
  summarise(
    graph1 = mean(graph1_score == 1, na.rm = TRUE) * 100,
    graph2 = mean(graph2_score == 1, na.rm = TRUE) * 100,
    graph3 = mean(graph3_score == 1, na.rm = TRUE) * 100,
    graph4 = mean(graph4_score == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "percent")

ggplot(score_ratios, aes(x = item, y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage of right answer in Graph literacy test",
    x = "Question",
    y = "Percentage (%) of right answer"
  ) +
  ylim(0, 100) +
  theme_minimal()


score_ratios_by_group <- df_full %>%
  group_by(group) %>%
  summarise(
    graph1 = mean(graph1_score == 1, na.rm = TRUE) * 100,
    graph2 = mean(graph2_score == 1, na.rm = TRUE) * 100,
    graph3 = mean(graph3_score == 1, na.rm = TRUE) * 100,
    graph4 = mean(graph4_score == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(
    cols = -group,
    names_to = "item",
    values_to = "percent"
  )

ggplot(score_ratios_by_group, aes(x = item, y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of right answer in Graph literacy test by Group",
    x = "Question",
    y = "Percentage (%) right answer (%)"
  ) +
  ylim(0, 100) +
  theme_minimal()

table(df_full$graph_literacy, df_full$numeracy_level)

prop.table(table(df_full$graph_literacy, df_full$numeracy_level)) * 100



###=============================================================================
# ------------- IV. Descriptive statistics - Perceived risk, trust and action

df_1a <- df_full %>%
  filter(group == "1A")
df_1b <- df_full %>%
  filter(group == "1B")
df_2a <- df_full %>%
  filter(group == "2A")
df_2b <- df_full %>%
  filter(group == "2B")
df_1ab <- df_full %>%
  filter(group %in% c("1A", "1B"))
df_2ab <- df_full %>%
  filter(group %in% c("2A", "2B"))

plot_group_boxplot <- function(df, group_label) {
  cols <- c("org_likelihood", "org_severe", "org_confident", 
            "v1_likelihood", "v1_severe", "v1_confident", 
            "v2_likelihood", "v2_severe", "v2_confident"
  )
  
  df_plot <- df[, cols]
  
  df_long <- df_plot %>%
    pivot_longer(cols = everything(), names_to = "Measure", values_to = "Value") %>%
    mutate(
      Type = case_when(
        grepl("likelihood", Measure) ~ "Likelihood",
        grepl("severe", Measure) ~ "Severity",
        grepl("confident", Measure) ~ "Confident"
      ),
      Version = case_when(
        grepl("org", Measure) ~ "Original",
        grepl("v1", Measure) ~ "Version 1",
        grepl("v2", Measure) ~ "Version 2"
      )
    )
  
  df_long$Type <- factor(df_long$Type, levels = c("Likelihood", "Severity", "Confident"))
  
  ggplot(df_long, aes(x = Version, y = Value, fill = Version)) +
    geom_boxplot() +
    facet_wrap(~Type, scales = "free_y") +
    labs(title = paste(group_label, ": Likelihood, Severity, Confident by Version"),
         y = "Percentage", x = "Version") +
    ylim(0, 100) +
    theme_minimal()
}


plot_group_boxplot(df_1a, "Group 1A")
plot_group_boxplot(df_1b, "Group 1B")
plot_group_boxplot(df_2a, "Group 2A")
plot_group_boxplot(df_2b, "Group 2B")

# 2. Comparison
get_compare_freq <- function(df_1, df_2, compare_cols) {
  compare_freq_list <- lapply(compare_cols, function(col) {
    values <- c(as.character(df_1[[col]]), as.character(df_2[[col]]))
    table(values)
  })

  names(compare_freq_list) <- compare_cols
  return(compare_freq_list)
}


compare_cols_1ab <- c("compare_1", "compare_2", "compare_3")
compare_cols_2ab <- c("compare_1", "compare_2", "compare_3")

compare_freq_list_1ab <- get_compare_freq(df_1a, df_1b, compare_cols_1ab)
compare_freq_list_2ab <- get_compare_freq(df_2a, df_2b, compare_cols_2ab)


compare_freq_list_1ab
compare_freq_list_2ab

chisq_test_1ab_1 <- chisq.test(compare_freq_list_1ab$compare_1)
chisq_test_1ab_2 <- chisq.test(compare_freq_list_1ab$compare_2)
chisq_test_1ab_3 <- chisq.test(compare_freq_list_1ab$compare_3)

chisq_test_2ab_1 <- chisq.test(compare_freq_list_2ab$compare_1)
chisq_test_2ab_2 <- chisq.test(compare_freq_list_2ab$compare_2)
chisq_test_2ab_3 <- chisq.test(compare_freq_list_2ab$compare_3)


chisq_test_1ab_1
chisq_test_1ab_2
chisq_test_1ab_3
chisq_test_2ab_1
chisq_test_2ab_2
chisq_test_2ab_3

# 3. Trust
trust_columns <- df_full %>%
  select(contains("trust"))

summary(trust_columns)

describe(trust_columns)

for(col in colnames(trust_columns)) {
  # Đổi nhãn cho các cột cụ thể
  x_label <- col
  if(col == "trust_1_expertise") {
    x_label <- "Trust in forecasters’ expertise"
  } else if(col == "trust_1_prepare") {
    x_label <- "Trust in emergency preparedness"
  } else if(col == "trust_1_data") {
    x_label <- "Trust in meteorological data"
  } else if(col == "trust_2_reliable") {
    x_label <- "Trust in warning reliability"
  } else if(col == "trust_2_matrix") {
    x_label <- "Trust in the colour-coded matrix"
  } else if(col == "trust_2_graphics") {
    x_label <- "Trust in the warning map"
  } else if(col == "trust_2_decision") {
    x_label <- "Trust in timely decision support"
  }

  p <- ggplot(trust_columns, aes_string(x = col)) +
    geom_bar(stat = "count") + 
    labs(title = paste( x_label), x = x_label, y = "Number of Participants") +
    scale_fill_brewer(palette = "Set3") +  
    theme_minimal() +
    theme(legend.position = "none")  
  
  print(p)
}

df_full <- df_full %>%
  mutate(across(starts_with("trust_1"), ~ as.numeric(as.character(.)))) %>%
  mutate(across(starts_with("trust_2"), ~ as.numeric(as.character(.))))


df_full <- df_full %>%
  mutate(trust_agency = round(rowMeans(select(., starts_with("trust_1")), na.rm = TRUE), 2))


df_full <- df_full %>%
  mutate(trust_statement = round(rowMeans(select(., starts_with("trust_2")), na.rm = TRUE), 2))

view(df_full)
describe(df_full$trust_agency)


trust_agency_vars <- df_full %>% select(starts_with("trust_1"))
alpha(trust_agency_vars)

trust_statement_vars <- df_full %>% select(starts_with("trust_2"))
alpha(trust_statement_vars)


# 4. Ranking
summary(df_full$easy)
summary(df_full$medium)
summary(df_full$hard)

na_rank <- subset(df_full, is.na(easy) | is.na(medium) | is.na(hard))
View(na_rank)
print(na_rank$rank_explain)

rank_analysis <- function(df, rank_cols, group_label) {
  df_clean <- df %>%
    select(all_of(rank_cols)) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  df_long <- df_clean %>%
    pivot_longer(cols = everything(), names_to = "Matrix", values_to = "Rank")
  
  df_long$Rank <- factor(df_long$Rank,
                         levels = c("1", "2", "3"),
                         labels = c("Original", "Version 1", "Version 2"))
  
  df_long <- df_long %>%
    mutate(Difficulty = case_when(
      grepl("easy", Matrix) ~ "Easiest",
      grepl("medium", Matrix) ~ "Moderate",
      grepl("hard", Matrix) ~ "Hardest",
      TRUE ~ "Unknown"
    ))
  
  top_rank <- df_long %>%
    group_by(Difficulty, Rank) %>%
    summarise(Freq = n(), .groups = "drop") %>%
    group_by(Difficulty) %>%
    slice_max(order_by = Freq, n = 1, with_ties = FALSE) %>%
    rename(TopRank = Rank)
  
  df_long <- df_long %>%
    left_join(top_rank, by = "Difficulty") %>%
    mutate(Highlight = ifelse(Rank == TopRank, "Top", "Other"))
  
  ggplot(df_long, aes(x = Rank, fill = Highlight)) +
    geom_bar() +
    facet_wrap(~Difficulty) +
    scale_fill_manual(values = c("Top" = "darkblue", "Other" = "lightblue")) +
    labs(title = paste("Rank Frequency of", group_label),
         x = "Version", y = "Number of votes") +
    theme_minimal()
}
rank_cols_1ab <- c("easy", "medium", "hard")
rank_cols_2ab <- c("easy", "medium", "hard")

rank_analysis(df_1a, rank_cols_1ab, "Group 1A")
rank_analysis(df_1b, rank_cols_1ab, "Group 1B")
rank_analysis(df_2a, rank_cols_2ab, "Group 2A")
rank_analysis(df_2b, rank_cols_2ab, "Group 2B")

df_1ab <- dplyr::bind_rows(df_1a, df_1b)
rank_analysis(df_1ab, rank_cols_1ab, "Group 1AB")

df_2ab <- dplyr::bind_rows(df_2a, df_2b)
rank_analysis(df_2ab, rank_cols_2ab, "Group 2AB")


# 5. Protective action - Group 1AB
df_1ab <- df_1ab %>%
  mutate(across(act_Org_1:act_Org_4, as.numeric))
df_1ab <- df_1ab %>%
  mutate(across(act_Alt_1:act_Alt_4, as.numeric))
df_2ab <- df_2ab %>%
  mutate(across(act_Org_1:act_Org_4, as.numeric))
df_2ab <- df_2ab %>%
  mutate(across(act_Alt_1:act_Alt_4, as.numeric))

alpha(df_1ab %>% select(act_Org_1:act_Org_4))
alpha(df_1ab %>% select(act_Alt_1:act_Alt_4))

alpha(df_2ab %>% select(act_Org_1:act_Org_4))
alpha(df_2ab %>% select(act_Alt_1:act_Alt_4))


df_1ab <- df_1ab %>%
  mutate(action_score_Org = rowMeans(select(., act_Org_1:act_Org_4), na.rm = TRUE),
         action_score_Alt = rowMeans(select(., act_Alt_1:act_Alt_4), na.rm = TRUE))


t.test(df_1ab$action_score_Org, df_1ab$action_score_Alt, paired = TRUE)

for(i in 1:4) {
  print(t.test(df_1ab[[paste0("act_Org_", i)]],
               df_1ab[[paste0("act_Alt_", i)]],
               paired = TRUE))
}


for (i in 1:4) {
  cat("\nStatement", i, "\n")
  print(
    wilcox.test(
      df_1ab[[paste0("act_Org_", i)]],
      df_1ab[[paste0("act_Alt_", i)]],
      paired = TRUE,
      exact = FALSE
    )
  )
}

diff_results <- data.frame(
  Statement = 1:4,
  MeanDiff = sapply(1:4, function(i) {
    mean(df_1ab[[paste0("act_Alt_", i)]] -
           df_1ab[[paste0("act_Org_", i)]], na.rm = TRUE)
  }),
  MedianDiff = sapply(1:4, function(i) {
    median(df_1ab[[paste0("act_Alt_", i)]] -
             df_1ab[[paste0("act_Org_", i)]], na.rm = TRUE)
  })
)

diff_results <- diff_results %>%
  arrange(desc(abs(MeanDiff)))

print(diff_results)


# 5. Protective Action - Group 2AB
df_2ab <- df_2ab %>%
  mutate(action_score_Org = rowMeans(select(., act_Org_1:act_Org_4), na.rm = TRUE),
         action_score_Alt = rowMeans(select(., act_Alt_1:act_Alt_4), na.rm = TRUE))

# So sánh paired t-test
t.test(df_2ab$action_score_Org, df_2ab$action_score_Alt, paired = TRUE)

# So sánh từng item
for(i in 1:4) {
  print(t.test(df_2ab[[paste0("act_Org_", i)]],
               df_2ab[[paste0("act_Alt_", i)]],
               paired = TRUE))
}

# Wilcoxon signed-rank test cho từng statement
for (i in 1:4) {
  cat("\nStatement", i, "\n")
  print(
    wilcox.test(
      df_2ab[[paste0("act_Org_", i)]],
      df_2ab[[paste0("act_Alt_", i)]],
      paired = TRUE,
      exact = FALSE
    )
  )
}

diff_results <- data.frame(
  Statement = 1:4,
  MeanDiff = sapply(1:4, function(i) {
    mean(df_2ab[[paste0("act_Alt_", i)]] -
           df_2ab[[paste0("act_Org_", i)]], na.rm = TRUE)
  }),
  MedianDiff = sapply(1:4, function(i) {
    median(df_2ab[[paste0("act_Alt_", i)]] -
             df_2ab[[paste0("act_Org_", i)]], na.rm = TRUE)
  })
)

diff_results <- diff_results %>%
  arrange(desc(abs(MeanDiff)))

print(diff_results)


pa_data <- df_1ab %>%
  select(act_Org_1:act_Org_4, act_Alt_1:act_Alt_4)

pa_data[] <- lapply(pa_data, factor, levels = 1:5)

likert_org <- likert(pa_data[, 1:4])
plot(likert_org)
likert_alt <- likert(pa_data[, 5:8])
plot(likert_alt)

# Chọn cột Org và Alt, đổi thành factor với levels 1–5
pa_data <- df_2ab %>%
  select(act_Org_1:act_Org_4, act_Alt_1:act_Alt_4)

pa_data[] <- lapply(pa_data, factor, levels = 1:5)

# Tạo dữ liệu likert cho Org
likert_org <- likert(pa_data[, 1:4])
plot(likert_org)
likert_alt <- likert(pa_data[, 5:8])
plot(likert_alt)

write.csv(df_full, file = "df_full.csv", row.names = FALSE, fileEncoding = "UTF-8")
data <- read.csv("C:/Users/Nhung/Downloads/df_full.csv", header = TRUE, sep = ",")









