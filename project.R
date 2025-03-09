### Final Project R course
### Created by: Maya Pohes

# Packages and imports ----
source('.//functions.R')
library(dplyr)
library(ggplot2)
library(pROC)
library(ggpubr)
library(broom)
library(tidyr)


# Variables ----
data_folder_path         <- ".//data/"
baseline_df_csv_path     <- ".//data/first.csv"
last_df_csv_control      <- ".//data/control.csv"
last_df_csv_intervention <- ".//data/intervention.csv"

participant_ids <- c(
  "אפ1706", "עמ0701", "רו0701", "יס0910", "ימ1402", "אא2007", "ימ2803", "אד2504", "אז2806", "זב1308",
  "אש1005", "יי0505", "דפ2206", "יא1507", "שכ0611", "עח1005", "תפ2303", "נב1907", "אש2902", "זא2511",
  "של2205", "יר2611", "דמ0106", "אנ2202", "טפ1103", "או0401", "פב0704", "שד1307", "זב2402", "סד0102",
  "גפ1703", "אק2511", "בצ1109", "מפ2211", "רג2503", "רט0506", "נח2404", "יכ1012", "שה0112", "נא2108",
  "נפ2407", "טי0812", "גנ2512", "הק1804", "אב2602", "תא0602", "שב0102", "מג1110", "שר2901", "כי0603",
  "נע1902", "שב1508", "אב2308", "אע1103", "אב0604", "גצ1802", "אס2808", "לב2811", "אל2007", "אנ0408",
  "שא2906", "לב1703", "נל0108", "שז0301", "אש2306", "רג0906", "בג2806", "עג0302", "עה3003", "אב1109",
  "אנ1203", "נמ1007", "עב2702", "שש1210", "תא1211", "גט2909", "פא1005", "לא1001", "רכ1908", "נג1708",
  "אד1004", "נא2908", "אג2508", "עבש1107", "סש0202", "ממ2711", "לל2611", "לש0308", "הש1004", "שג0406",
  "דט2308", "רע2302", "יע0805", "עב1005", "ניק1302", "מחש1211"
)

NM_cols      <- c("ParticipantId", "Progress", "night.count", "nightmare.count", "NM.intensity")

DASS_cols    <- c("DASS_1", "DASS_2", "DASS_3", "DASS_4", "DASS_5", "DASS_6", 
                  "DASS_7", "DASS_8", "DASS_9", "DASS_10", "DASS_11", "DASS_12", 
                  "DASS_13", "DASS_14", "DASS_15", "DASS_16", "DASS_17", "DASS_18", 
                  "DASS_19", "DASS_20", "DASS_21")

SDQ_cols     <- c("SDQ_1", "SDQ_2", "SDQ_3", "SDQ_4", "SDQ_5", "SDQ_6", "SDQ_7", 
                  "SDQ_8", "SDQ_9", "SDQ_10", "SDQ_11", "SDQ_12", "SDQ_13", "SDQ_14", 
                  "SDQ_15", "SDQ_16", "SDQ_17", "SDQ_18", "SDQ_19", "SDQ_20", 
                  "SDQ_21", "SDQ_22", "SDQ_23", "SDQ_24", "SDQ_25")

CSHQ_cols    <- c("cshqSleepTime", "cshqBlock1_1", "cshqBlock1_2", "cshqBlock1_3", 
                  "cshqBlock1_4", "cshqBlock1_5", "cshqBlock1_6", "cshqBlock1_7", 
                  "cshqBlock1_8", "cshqBlock2_1", "cshqBlock2_2", "cshqBlock2_3", 
                  "cshqBlock2_4", "cshqBlock2_5", "cshqBlock2_6", "cshqBlock2_7", 
                  "cshqBlock2_8", "cshqBlock2_9", "cshqBlock2_10", "cshqBlock2_11", 
                  "cshqBlock2_12", "cshqBlock2_13", "cshqBlock2_14", 
                  "cshqBlock2_15", "cshqBlock3_1", "cshqBlock3_2", "cshqBlock4_1", 
                  "cshqBlock4_2", "cshqBlock4_3", "cshqBlock4_4", "cshqBlock4_5", 
                  "cshqBlock5_1", "cshqBlock6_1", "cshqBlock6_2")

COMPLIANCE_cols <- c("VideoWatch", "ConnectP", "EncourageUse", "ConnectC", "TakeToBed", 
                     "DCPractice", "Cooperate", "AltEnding", "IRTDaysPractice", "MinutesPractice")

NM_cols_1      <- c("night.count.1", "nightmare.count.1", "NM.intensity.1")

SDQ_cols_1     <- c("SDQ_1.1", "SDQ_2.1", "SDQ_3.1", "SDQ_4.1", "SDQ_5.1", "SDQ_6.1", "SDQ_7.1", 
                    "SDQ_8.1", "SDQ_9.1", "SDQ_10.1", "SDQ_11.1", "SDQ_12.1", "SDQ_13.1", "SDQ_14.1", 
                    "SDQ_15.1", "SDQ_16.1", "SDQ_17.1", "SDQ_18.1", "SDQ_19.1", "SDQ_20.1", 
                    "SDQ_21.1", "SDQ_22.1", "SDQ_23.1", "SDQ_24.1", "SDQ_25.1")

CSHQ_cols_1    <- c("cshqSleepTime.1", "cshqBlock1_1.1", "cshqBlock1_2.1", "cshqBlock1_3.1", 
                    "cshqBlock1_4.1", "cshqBlock1_5.1", "cshqBlock1_6.1", "cshqBlock1_7.1", 
                    "cshqBlock1_8.1", "cshqBlock2_1.1", "cshqBlock2_2.1", "cshqBlock2_3.1", 
                    "cshqBlock2_4.1", "cshqBlock2_5.1", "cshqBlock2_6.1", "cshqBlock2_7.1", 
                    "cshqBlock2_8.1", "cshqBlock2_9.1", "cshqBlock2_10.1", "cshqBlock2_11.1")

COMPLIANCE_cols1 <- c("VideoWatch.1", "ConnectP.1", "EncourageUse.1", "ConnectC.1", "TakeToBed.1", 
                      "DCPractice.1", "Cooperate.1", "AltEnding.1", "IRTDaysPractice.1", "MinutesPractice.1")

RELEVANT_COLS_BASELINE    <- c(NM_cols, DASS_cols, SDQ_cols, CSHQ_cols, 
                               NM_cols_1, SDQ_cols_1, CSHQ_cols_1)
RELEVANT_COLS_CONTROL     <- c(NM_cols, NM_cols_1)
RELEVANT_COLS_INTERVENTION <- c(NM_cols, NM_cols_1, COMPLIANCE_cols, COMPLIANCE_cols1)


# Read relevant data ----
baseline_df = read.csv(baseline_df_csv_path, header = TRUE)[-1,]
last_control = read.csv(last_df_csv_control, header = TRUE)[-1,]
last_intervention = read.csv(last_df_csv_intervention, header = TRUE)[-1,]

new_baseline_df = subset(baseline_df, select = RELEVANT_COLS_BASELINE)
control_df = subset(last_control, select = RELEVANT_COLS_CONTROL)
intervention_df = subset(last_intervention, select = RELEVANT_COLS_INTERVENTION)

# leave in ONLY ids that have 2 questionnaires
filtered_data = sort_by_participant_id(new_baseline_df, control_df, intervention_df)
baseline_filtered <- filtered_data$baseline
control_filtered <- filtered_data$control
intervention_filtered <- filtered_data$intervention

# drop rows where "Progress" column is not 100
baseline_filtered <- baseline_filtered[baseline_filtered$Progress == 100, ]
control_filtered <- control_filtered[control_filtered$Progress == 100, ]
intervention_filtered <- intervention_filtered[intervention_filtered$Progress == 100, ]









# שלב ב' - עיבוד מקדים של הנתונים ----
# Merge boys and girls columns ----
# baseline
combined_baseline_df = baseline_filtered
columns_to_merge_baseline <- c(NM_cols, DASS_cols, SDQ_cols, CSHQ_cols)
for (col in columns_to_merge_baseline) {
  combined_baseline_df <- merge_boys_and_girls_columns(combined_baseline_df, col)
}

# control
combined_control_df = control_filtered
columns_to_merge_control <- c(NM_cols)
for (col in columns_to_merge_control) {
  combined_control_df <- merge_boys_and_girls_columns(combined_control_df, col)
}

# intervention
combined_intervention_df = intervention_filtered
columns_to_merge_intervention <- c(NM_cols, COMPLIANCE_cols)
for (col in columns_to_merge_intervention) {
  combined_intervention_df <- merge_boys_and_girls_columns(combined_intervention_df, col)
}


# Create sum values for each metric questions scores in baseline & compliance in intervention only ----
combined_baseline_df = baseline_metrics_calculate(combined_baseline_df, DASS_cols, SDQ_cols, CSHQ_cols)
combined_intervention_df[COMPLIANCE_cols] <- lapply(combined_intervention_df[COMPLIANCE_cols], as.integer)
combined_intervention_df = intervention_metrics_calculate(combined_intervention_df)


# Merge baseline with control/intervention group by ParticipantId column ----
control_filtered <- merge(combined_control_df, 
                          combined_baseline_df[, c("ParticipantId", "night.count", "NM.intensity")], 
                          by = "ParticipantId", suffixes = c("", "_baseline"))
control_filtered$change_night_count <-  
  control_filtered$night.count - control_filtered$night.count_baseline
control_filtered$change_nm_intensity <-  
  control_filtered$NM.intensity - control_filtered$NM.intensity_baseline

intervention_filtered <- merge(combined_intervention_df, 
                               combined_baseline_df[, c("ParticipantId", "night.count", "NM.intensity")], 
                         by = "ParticipantId", suffixes = c("", "_baseline"))
intervention_filtered$change_night_count <- 
  intervention_filtered$night.count - intervention_filtered$night.count_baseline
intervention_filtered$change_nm_intensity <-  
  intervention_filtered$NM.intensity - intervention_filtered$NM.intensity_baseline


# Add group labels & Combine ----
control_filtered$group <- "control"
intervention_filtered$group <- "intervention"
combined_df <- bind_rows(control_filtered, intervention_filtered)

# Merge with baseline predictors
combined_df <- merge(combined_df, combined_baseline_df[, c("ParticipantId", "sum_dass", "sum_sdq", "sum_cshq")], by = "ParticipantId")
combined_df$group <- factor(combined_df$group, levels = c("control", "intervention"))
contrasts(combined_df$group)


# Leave in only rows where night.count or night.count_baseline or NM.intensity or NM.intensity_baseline is not NA ----
combined_df <- combined_df[!is.na(combined_df$night.count) & !is.na(combined_df$night.count_baseline), ]
combined_df <- combined_df[!is.na(combined_df$NM.intensity) & !is.na(combined_df$NM.intensity_baseline), ]


# Drop participants that are not in participants list ----
combined_df$ParticipantId <- trimws(combined_df$ParticipantId)
combined_df <- combined_df[combined_df$ParticipantId %in% participant_ids, ]






# שלב א' - הצגת הנתונים האקספלורטיבית  ----
# Box plots ----
ggplot(combined_df, aes(x = group, y = change_night_count, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Change in Night Count: Control vs. Intervention",
       x = "Group", y = "Change in Night Count")

# Each group has 2 - baseline and last
# Reshape data to long format
long_df <- combined_df %>%
  pivot_longer(cols = c(night.count_baseline, night.count), 
               names_to = "Timepoint", 
               values_to = "Night_Count")

# Rename the timepoints for better readability
long_df$Timepoint <- recode(long_df$Timepoint, 
                            night.count_baseline = "Baseline",
                            night.count = "After 4 Weeks")

ggplot(long_df, aes(x = group, y = Night_Count, fill = Timepoint)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6) +
  theme_minimal() +
  labs(title = "Night Count Comparison: Baseline vs. After 4 Weeks",
       x = "Group", y = "Night Count") +
  scale_fill_manual(values = c("Baseline" = "blue", "After 4 Weeks" = "red")) +
  theme(legend.title = element_blank())


# Intensity

ggplot(combined_df, aes(x = group, y = change_nm_intensity, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Change in NM intensity: Control vs. Intervention",
       x = "Group", y = "Change in NM intensity")

# Each group has 2 - baseline and last
# Reshape data to long format
long_df <- combined_df %>%
  pivot_longer(cols = c(NM.intensity_baseline, NM.intensity), 
               names_to = "Timepoint", 
               values_to = "Intensity")

# Rename the timepoints for better readability
long_df$Timepoint <- recode(long_df$Timepoint, 
                            NM.intensity_baseline = "Baseline",
                            NM.intensity = "After 4 Weeks")

ggplot(long_df, aes(x = group, y = Intensity, fill = Timepoint)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.6) +
  theme_minimal() +
  labs(title = "NM intensity Comparison: Baseline vs. After 4 Weeks",
       x = "Group", y = "NM intensity") +
  scale_fill_manual(values = c("Baseline" = "blue", "After 4 Weeks" = "red")) +
  theme(legend.title = element_blank())


# Violin plots ----
ggplot(combined_df, aes(x = group, y = change_night_count, fill = group)) +
  geom_violin(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Change in Night Count After 4 Weeks: Control vs. Intervention",
       x = "Group", y = "Change in Night Count") +
  scale_fill_manual(values = c("control" = "red", "intervention" = "blue"))

ggplot(combined_df, aes(x = group, y = change_nm_intensity, fill = group)) +
  geom_violin(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Change in Nightmare Intensity After 4 Weeks: Control vs. Intervention",
       x = "Group", y = "Change in Nightmare Intensity") +
  scale_fill_manual(values = c("control" = "red", "intervention" = "blue"))





# שלב ג' - ניתוח הנתונים ----
# Linear Regression ----
model <- lm(change_night_count ~ sum_dass + sum_sdq + sum_cshq + group, data = combined_df)
summary(model)

# plot coefficients
coef_df <- tidy(model) %>% 
  filter(term != "(Intercept)")  

ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Regression Coefficients - Change in Night Count", x = "Predictor", y = "Estimate")


# Logistic Regression ----
intervenetion_df <- combined_df[combined_df$group == "intervention", ]
intervenetion_df$improved <- as.factor(ifelse(intervenetion_df$change_night_count < 0, 1, 0))

logit_model <- glm(improved ~ sum_parent + sum_child, 
                   data = intervenetion_df, family = binomial)
summary(logit_model)  

# plot coefficients
intervenetion_df$predicted_prob <- predict(logit_model, type="response")

coef_df <- tidy(logit_model, conf.int = TRUE) %>% 
  filter(term != "(Intercept)")  
ggplot(coef_df, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Logistic Regression Coefficients",
       x = "Predictor", y = "Estimate (Log Odds)") +
  theme_minimal()

# Additional plots
ggplot(intervenetion_df, aes(x = sum_child, y = predicted_prob)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  theme_minimal() +
  labs(title = "Logistic Regression: Probability of Improvement",
       x = "Sum Child Score", y = "Predicted Probability of Improvement")

ggplot(intervenetion_df, aes(x = sum_parent, y = predicted_prob)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  theme_minimal() +
  labs(title = "Logistic Regression: Probability of Improvement",
       x = "Sum Parent Score", y = "Predicted Probability of Improvement")

# ROC ----

roc_curve <- roc(intervenetion_df$improved, intervenetion_df$predicted_prob)

ggroc(roc_curve) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  ggtitle("ROC Curve for Logistic Regression") +
  theme_minimal()

auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")





# Extra analysis ----
# Linear regression for change in nightmare intensity ----
model <- lm(change_nm_intensity ~ sum_dass + sum_sdq + sum_cshq + group, data = combined_df)
summary(model)

model_interaction <- lm(change_nm_intensity ~ sum_dass * group + sum_sdq * group + sum_cshq * group, data = combined_df)
summary(model_interaction)

# plot coefficients
coef_df <- tidy(model) %>% 
  filter(term != "(Intercept)")  
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Regression Coefficients - Nightmare Intensity", x = "Predictor", y = "Estimate")

# Correlation between compliance and change in night count in intervention group ----
cor_parent_change <- cor(intervention_filtered$sum_parent, intervention_filtered$change_night_count, use = "complete.obs")
cor_child_change <- cor(intervention_filtered$sum_child, intervention_filtered$change_night_count, use = "complete.obs")
cor_parent_child <- cor(intervention_filtered$sum_parent, intervention_filtered$sum_child, use = "complete.obs")

cat("Correlation between sum_parent and change_night_count:", cor_parent_change, "\n")
cat("Correlation between sum_child and change_night_count:", cor_child_change, "\n")
cat("Correlation between sum_child and sum_parent:", cor_parent_child, "\n")

# plots
ggplot(intervention_filtered, aes(x = sum_parent, y = change_night_count)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(title = "Intervention Group: Correlation of Sum Parent & Direction Change in Night Count",
       x = "Sum Parent", y = "Direction Change in Night Count")

ggplot(intervention_filtered, aes(x = sum_child, y = change_night_count)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(title = "Intervention Group: Correlation of Sum Child & Direction Change in Night Count",
       x = "Sum Child", y = "Direction Change in Night Count")

ggplot(intervention_filtered, aes(x = sum_parent, y = sum_child)) +
  geom_point(color = "purple", alpha = 0.7) +  # Use correct color
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(title = "Intervention Group: Correlation of Sum Parent & Sum Child",
       x = "Sum Parent", y = "Sum Child")

