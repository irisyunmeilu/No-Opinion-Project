###########################No Opinion Project###################################
#Appendix H. Impute DK responses and compare it with the original estimate.
# install.packages(c("mice","survey","mitools","dplyr","ggplot2"))
library(mice)
library(survey)
library(mitools)
library(dplyr)
library(ggplot2)
library(srvyr)
library(tidyr)

# ---- VARIABLES YOU NEED TO EDIT ----#
data$sch_c3 = factor(data$sch_c3, levels = c("never missed", "missed 1-2days", "missed 3+days"))

data <- data %>%
  mutate(race3 = case_when(
    race4 == "Black" ~ "Black",
    race4 == "White" ~ "White",
    race4 %in% c("Hispanic", "Other") ~ "Other",
    TRUE ~ NA_character_   # keeps missing values as NA
  ))
data$race3 = factor(data$race3, levels = c("White", "Black", "Other"))
data <- data %>%
  mutate(cohort2=case_when(
    year<1986 ~ "cohort1976-85",
    year>=1986&year<=2008 ~"cohort1986-2008",
    year<2019&year>2008 ~"cohort2009-18",
    TRUE ~ NA_character_
  ))
data$cohort2<-factor(data$cohort2)



yvar <- "pol_p1.n5"                # your perception variable with missing values
predictors <- c("sex1","race3", "rural5", "ideo2","p_edu.n3", "any.p", "region1","happy1.n","care_soc1.n") # put all predictors used for imputation
year_var   <- "year"                         # year identifier (numeric or factor)
wvar       <- "weight"
keep_var   <- "cohort2"



# Keep only variables needed for MI
data$year<-as.numeric(as.character(data$year))
#pol_p1.n5: don't know is coded as missing
#pol_p1.n is coded as 6 and missing is NA, we want to drop real NA, but not dropping DK
df_clear <- data %>% 
  filter(!is.na(pol_p1.n))
df_clear<-subset(df_clear, year<2019)
#sex has missing data: 
df_clear$sex1 <- ifelse(is.na(df_clear$sex1), "Don't know", df_clear$sex1)
df_clear$sex1<- factor(df_clear$sex1)
#df_clear <- df_clear %>% filter(!is.na(sex1))
#rural urban has LOS OF missing data, don't use it as a predictor
#parental education, missing recode as don't know
df_clear$p_edu.n3 <- ifelse(is.na(df_clear$p_edu.n3), "Don't know", df_clear$p_edu.n3)
df_clear$p_edu.n3<- factor(df_clear$p_edu.n3)


dat <- subset(df_clear, select=c(pol_p1.n5,sex1,race3, p_edu.n3, ideo2,any.p, region1, year,weight, cohort2 ))
#dat <- subset(df_clear, select=c(pol_p1.n5,sex1,race3,ideo2,any.p, region1 ))




################################################################################
# ============================================================
# COHORT ONLY: Original vs. Imputed
# ============================================================

# -----------------------------------
# 1. IMPUTED DATA
# -----------------------------------

results_cohort <- lapply(designs, function(des) {
  svyby(
    formula = ~pol_p1.n5,
    by      = ~cohort2,
    design  = des,
    FUN     = svymean,
    vartype = "se",
    na.rm   = TRUE
  )
})

imp_cohort <- bind_rows(
  lapply(seq_along(results_cohort), function(i) {
    results_cohort[[i]] %>%
      mutate(imputation = i)
  })
) %>%
  group_by(cohort2) %>%
  summarize(
    # Pooled mean
    mean_imputed = mean(pol_p1.n5, na.rm = TRUE),
    
    # Rubin's rules
    within_var  = mean(se^2, na.rm = TRUE),
    between_var = var(pol_p1.n5, na.rm = TRUE),
    m           = sum(!is.na(pol_p1.n5)),
    
    total_var = within_var +
      (1 + 1/m) * between_var,
    
    se_imputed = sqrt(total_var),
    
    .groups = "drop"
  )
imp_cohort

# -----------------------------------
# 2. ORIGINAL DATA
# -----------------------------------

orig_cohort <- svyby(
  formula = ~pol_p1.n5,
  by      = ~cohort2,
  design  = design_original,
  FUN     = svymean,
  vartype = "se",
  na.rm   = TRUE
) %>%
  transmute(
    cohort2,
    mean_original = pol_p1.n5,
    se_original   = se
  )


# -----------------------------------
# 3. COMBINE ORIGINAL + IMPUTED
# -----------------------------------

comparison_cohort <- orig_cohort %>%
  full_join(
    imp_cohort %>%
      select(
        cohort2,
        mean_imputed,
        se_imputed
      ),
    by = "cohort2"
  ) %>%
  mutate(
    difference = mean_imputed - mean_original
  )

comparison_cohort
################################################################################
################################################################################
#Cohort*Sex

# ============================================================
# COHORT × SEX: Original vs. Imputed
# ============================================================
#sex1=1=male; 2=female
# 1. IMPUTED DATA
results_sex <- lapply(designs, function(des) {
  svyby(
    formula = ~pol_p1.n5,
    by      = ~cohort2 + sex1,
    design  = des,
    FUN     = svymean,
    vartype = "se",
    na.rm   = TRUE
  )
})

# Pool estimates and SEs using Rubin's rules
imp_sex <- bind_rows(
  lapply(seq_along(results_sex), function(i) {
    results_sex[[i]] %>%
      mutate(imputation = i)
  })
) %>%
  group_by(cohort2, sex1) %>%
  summarize(
    mean_imputed = mean(pol_p1.n5, na.rm = TRUE),
    
    # Within-imputation variance
    within_var = mean(se^2, na.rm = TRUE),
    
    # Between-imputation variance
    between_var = var(pol_p1.n5, na.rm = TRUE),
    
    # Number of imputations
    m = sum(!is.na(pol_p1.n5)),
    
    # Rubin's total variance
    total_var = within_var +
      (1 + 1/m) * between_var,
    
    # Pooled SE
    se_imputed = sqrt(total_var),
    
    .groups = "drop"
  )


# 2. ORIGINAL DATA
orig_sex <- svyby(
  formula = ~pol_p1.n5,
  by      = ~cohort2 + sex1,
  design  = design_original,
  FUN     = svymean,
  vartype = "se",
  na.rm   = TRUE
) %>%
  transmute(
    cohort2,
    sex1,
    mean_original = pol_p1.n5,
    se_original   = se
  )


# 3. COMBINE ORIGINAL AND IMPUTED RESULTS
comparison_sex <- orig_sex %>%
  full_join(
    imp_sex %>%
      select(
        cohort2,
        sex1,
        mean_imputed,
        se_imputed
      ),
    by = c("cohort2", "sex1")
  ) %>%
  mutate(
    difference = mean_imputed - mean_original
  )

comparison_sex

#data visualizatin


# Prepare cohort × sex data for plotting
plot_sex <- comparison_sex %>%
  filter(cohort2 %in% c("cohort1976-85", "cohort2009-18")) %>%
  droplevels() %>%
  select(
    cohort2, sex1,
    mean_original, se_original,
    mean_imputed, se_imputed
  ) %>%
  pivot_longer(
    cols = c(mean_original, mean_imputed),
    names_to = "data_type",
    values_to = "mean"
  ) %>%
  mutate(
    se = ifelse(
      data_type == "mean_original",
      se_original,
      se_imputed
    ),
    data_type = recode(
      data_type,
      "mean_original" = "Original",
      "mean_imputed"  = "Imputed"
    )
  )

p.sex<-ggplot(
  plot_sex,
  aes(
    x = cohort2,
    y = mean,
    fill = data_type
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean - 1.96 * se,
      ymax = mean + 1.96 * se
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~sex1) +
  scale_x_discrete(
    labels = c(
      "cohort1976-85" = "1976–85",
      "cohort2009-18" = "2009–18"
    )
  ) +
  labs(
    x = "Cohort",
    y = "Mean",
    fill = "Data",
    title = "Original vs. Imputed Estimates by Cohort and Sex"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )
p.sex
################################################################################
# ============================================================
# COHORT × RACE: Original vs. Imputed
# ============================================================

# 1. IMPUTED DATA
results_race <- lapply(designs, function(des) {
  svyby(
    formula = ~pol_p1.n5,
    by      = ~cohort2 + race3,
    design  = des,
    FUN     = svymean,
    vartype = "se",
    na.rm   = TRUE
  )
})

# Pool estimates and SEs using Rubin's rules
imp_race <- bind_rows(
  lapply(seq_along(results_race), function(i) {
    results_race[[i]] %>%
      mutate(imputation = i)
  })
) %>%
  group_by(cohort2, race3) %>%
  summarize(
    # Pooled mean
    mean_imputed = mean(pol_p1.n5, na.rm = TRUE),
    
    # Within-imputation variance
    within_var = mean(se^2, na.rm = TRUE),
    
    # Between-imputation variance
    between_var = var(pol_p1.n5, na.rm = TRUE),
    
    # Number of imputations
    m = sum(!is.na(pol_p1.n5)),
    
    # Rubin's total variance
    total_var = within_var +
      (1 + 1/m) * between_var,
    
    # Rubin-pooled SE
    se_imputed = sqrt(total_var),
    
    .groups = "drop"
  )


# 2. ORIGINAL DATA
orig_race <- svyby(
  formula = ~pol_p1.n5,
  by      = ~cohort2 + race3,
  design  = design_original,
  FUN     = svymean,
  vartype = "se",
  na.rm   = TRUE
) %>%
  transmute(
    cohort2,
    race3,
    mean_original = pol_p1.n5,
    se_original   = se
  )


# 3. COMBINE ORIGINAL AND IMPUTED RESULTS
comparison_race <- orig_race %>%
  full_join(
    imp_race %>%
      select(
        cohort2,
        race3,
        mean_imputed,
        se_imputed
      ),
    by = c("cohort2", "race3")
  ) %>%
  mutate(
    difference = mean_imputed - mean_original
  )

comparison_race

#Visualize comparison_race

plot_race <- comparison_race %>%
  filter(cohort2 %in% c("cohort1976-85", "cohort2009-18")) %>%
  droplevels() %>%
  select(
    cohort2, race3,
    mean_original, se_original,
    mean_imputed, se_imputed
  ) %>%
  pivot_longer(
    cols = c(mean_original, mean_imputed),
    names_to = "data_type",
    values_to = "mean"
  ) %>%
  mutate(
    se = ifelse(
      data_type == "mean_original",
      se_original,
      se_imputed
    ),
    data_type = recode(
      data_type,
      "mean_original" = "Original",
      "mean_imputed"  = "Imputed"
    )
  )

fig.race<-ggplot(
  plot_race,
  aes(
    x = cohort2,
    y = mean,
    fill = data_type
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean - 1.96 * se,
      ymax = mean + 1.96 * se
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~race3) +
  scale_x_discrete(
    labels = c(
      "cohort1976-85" = "1976–85",
      "cohort2009-18" = "2009–18"
    )
  ) +
  labs(
    x = "Cohort",
    y = "Mean",
    fill = "Data",
    title = "Original vs. Imputed Estimates by Cohort and Race"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

fig.race
###############################################################################
#save: comparison_race, comparison_cohort, and comparison_sex into one single excel file
# Install once if needed

library(openxlsx)

# Create workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Cohort")
addWorksheet(wb, "Cohort_Sex")
addWorksheet(wb, "Cohort_Race")

# Write data to each worksheet
writeData(wb, "Cohort", comparison_cohort)
writeData(wb, "Cohort_Sex", comparison_sex)
writeData(wb, "Cohort_Race", comparison_race)

# Save Excel file
saveWorkbook(
  wb,
  "output/MI_comparison.xlsx",
  overwrite = TRUE
)
# Save Excel file
saveWorkbook(
  wb,
  "output/MI_comparison_ologit.xlsx",
  overwrite = TRUE
)

