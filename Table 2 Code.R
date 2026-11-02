#No opinion project#
######################10.22.2025 Regression analysis by cohort##################
#Step 1. Run regression models to predict who said don't know#
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
  mutate(across(c(year,  sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1), as.factor))
data$year<-as.numeric(as.character(data$year))
#Subset sample to the first cohort 1976-85#
d1<-subset(data, year<1986)
d1 <-d1 %>%
  select(weight, year,pol_p1.no,
         sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1, happy1.n,care_soc1.n)%>%
  drop_na()
nrow(d1)

#Subset sample to the second cohort 2009-2018#
d2<-subset(data, year<2019&year>2008)
d2 <-d2 %>%
  select(weight, year,pol_p1.no,
         sex1,race3,rural5,ideo2,sch_c3,p_edu.n3,any.p,region1, happy1.n,care_soc1.n)%>%
  drop_na()
nrow(d2)

#survey set up#
des1 <- svydesign(ids = ~1, weights = ~weight, data = d1)
des2 <- svydesign(ids = ~1, weights = ~weight, data = d2)


#Regression analysis for 1st Cohort#
m1 <- svyglm(pol_p1.no ~ sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+happy1.n+care_soc1.n, 
             design = des1, family = binomial())
summary(m1)
reg_table1a <- tidy(m1, conf.int = TRUE) %>%
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
reg_table1a

#Regression analysis for the 2nd Cohort#
m1 <- svyglm(pol_p1.no ~ sex1+race3+rural5+ideo2+sch_c3+p_edu.n3+any.p+region1+happy1.n+care_soc1.n, 
             design = des2, family = binomial())
summary(m1)
reg_table1b <- tidy(m1, conf.int = TRUE) %>%
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

#save the output to excel#
# Define file path (using your dataDir variable)
#need to update file path:
file_path <- file.path("C:/Users/yunmeilu/Box/MTF police/No opinion project/output", "regression_output_with_OR_cohort.xlsx")
library(openxlsx)
# Create a new Excel workbook
wb <- createWorkbook()

# Add each regression table as a separate worksheet
addWorksheet(wb, "Model1a")
writeData(wb, "Model1a", reg_table1a)

addWorksheet(wb, "Model2a")
writeData(wb, "Model2a", reg_table1b)

# Save the workbook (overwrite if it already exists)
saveWorkbook(wb, file_path, overwrite = TRUE)

# Optional: confirm sheets created
getSheetNames(file_path)



