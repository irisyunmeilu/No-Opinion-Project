###################No Opinion proejct###########################################
#regression analysis of feeling safe, using DK responses########
library(dplyr)
library(tidyr)

library(survey)
library(broom)
library(writexl)

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
  mutate(across(c(sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1), as.factor))
data$year<-as.numeric(as.character(data$year))
data1<-data
data1<-subset(data1, year<=2018)
data1$pol_p1.n5f <- as.factor(data1$pol_p1.n5)
data1$pol_p1.nf6 <- as.factor(data1$pol_p1.n)
data1$pol_p1.nof <- as.factor(data1$pol_p1.no)
des <- svydesign(ids = ~1, weights = ~weight, data = data1)


#Table 4a.  full sample:
#police perception 1-6, with don't know as the last category
m1 <- svyglm(us_tosch1.n ~ pol_p1.nf6+sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+year+happy1.n+care_soc1.n, design = des, family = gaussian())
summary(m1)
nobs(m1)
reg_table1 <- tidy(m1, conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),                 # odds ratio
    conf.low.OR = exp(conf.low),        # lower CI for OR
    conf.high.OR = exp(conf.high),      # upper CI for OR
    stars = case_when(                  # significance stars
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    coef_with_stars = paste0(round(estimate, 3), stars),
    OR_with_CI = paste0(
      round(OR, 2), " [",
      round(conf.low.OR, 2), ", ",
      round(conf.high.OR, 2), "]"
    )
  ) %>%
  select(term, estimate, OR, std.error, p.value, stars)

#########################################################################
#Table 4b. Cohort-specific Models
#police perception 1-6, with don't know as the last category
#run by cohort
d1<-subset(des, year>=1992&year<2002)
d2<-subset(des, year<2019&year>2008)
#des1 <- svydesign(ids = ~1, weights = ~weight, data = d1)
#des2 <- svydesign(ids = ~1, weights = ~weight, data = d2)
m1 <- svyglm(us_tosch1.n ~ pol_p1.nf6+sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+year+happy1.n+care_soc1.n, design = d1, family = gaussian())
nobs(m1)
summary(m1)
reg_table2 <- tidy(m1, conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),                 # odds ratio
    conf.low.OR = exp(conf.low),        # lower CI for OR
    conf.high.OR = exp(conf.high),      # upper CI for OR
    stars = case_when(                  # significance stars
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    coef_with_stars = paste0(round(estimate, 3), stars),
    OR_with_CI = paste0(
      round(OR, 2), " [",
      round(conf.low.OR, 2), ", ",
      round(conf.high.OR, 2), "]"
    )
  ) %>%
  select(term, estimate, OR, std.error, p.value, stars)

m2 <- svyglm(us_tosch1.n ~ pol_p1.nf6+sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+year+happy1.n+care_soc1.n, design = d2, family = gaussian())
summary(m2)
nobs(m2)
reg_table3 <- tidy(m2, conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),                 # odds ratio
    conf.low.OR = exp(conf.low),        # lower CI for OR
    conf.high.OR = exp(conf.high),      # upper CI for OR
    stars = case_when(                  # significance stars
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    coef_with_stars = paste0(round(estimate, 3), stars),
    OR_with_CI = paste0(
      round(OR, 2), " [",
      round(conf.low.OR, 2), ", ",
      round(conf.high.OR, 2), "]"
    )
  ) %>%
  select(term, estimate, OR, std.error, p.value, stars)


###Combine Table 4 together

# Define file path (using your dataDir variable)
file_path <- file.path("C:/Users/yunmeilu/Box/MTF police/No opinion project/output","regression_output_safe_RR.xlsx")
library(openxlsx)
# Create a new Excel workbook
wb <- createWorkbook()

addWorksheet(wb, "Model1")
writeData(wb, "Model1", reg_table1)
addWorksheet(wb, "Model2")
writeData(wb, "Model2", reg_table2)
addWorksheet(wb, "Model3")
writeData(wb, "Model3", reg_table3)

# Save the workbook (overwrite if it already exists)
saveWorkbook(wb, file_path, overwrite = TRUE)

# Optional: confirm sheets created
getSheetNames(file_path)

################################################################################
################################################################################
#Appendix G: Interaction with cohort
library(ggeffects)

data1 <- data1 %>%
  mutate(cohort2=case_when(
    year>=1992&year<2002 ~ "cohort 1992-2001",
    year<=2018&year>=2009 ~"cohort 2009-2018",
    TRUE ~ NA_character_
  ))
data1$cohort2<-as.factor(data1$cohort2)
des <- svydesign(ids = ~1, weights = ~weight, data = data1)
m1 <- svyglm(us_tosch1.n ~ pol_p1.nf6*cohort2+sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+happy1.n+care_soc1.n, design = des, family = gaussian())
summary(m1)

#export interaction table as appendix: 
library(survey)
library(broom)
library(dplyr)

reg_table <- tidy(m1) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  select(
    Variable = term,
    Coefficient = estimate,
    SE = std.error,
    Statistic = statistic,
    p.value,
    Significance = sig
  )
setwd("C:/Users/yunmeilu/Box/MTF police/No opinion project")
write.csv(reg_table, "output/appendix_safety.csv", row.names = FALSE)


effects <- ggpredict(m1, terms = c("pol_p1.nf6","cohort2"))
effects1<- as.data.frame(effects)
plot(effects) + theme_minimal()
#######################################################################
# Figure 4. showing predicted scores by group
plot(effects) + theme_minimal()
p <- plot(effects, dodge = 0.3) +
  geom_point(
    data = effects1,
    aes(x = x, y = predicted, color = group, shape = group),
    position = position_dodge(width = 0.3),
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "",
    x = "Law Enforcement Perception",
    y = "Predicted Score of Feeling Unsafe",
    color = "Cohort",
    shape = "Cohort"
  ) +
  scale_shape_manual(values = c(16, 17)) +
  scale_x_discrete(
    labels = c(
      "1" = "1. Very Poor",
      "2" = "2. Poor",
      "3" = "3. Fair",
      "4" = "4. Good",
      "5" = "5. Very Good",
      "6" = "6. No opinion"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

p
ggsave(
  filename = "Figure 4. safe_police perception.png",
  plot = p,
  width = 6, height = 4, units = "in", dpi = 300
)


