#No opinion project#
######################Descriptive Table###########################
library(survey)
library(srvyr)
library(gtsummary)
library(dplyr)
library(writexl)

#update pathway here:
setwd("C:/Users/yunmeilu/Box/MTF police/No opinion project")

#Table 1. Full Sample vs. Two Cohorts
data$sch_c3 = factor(data$sch_c3, levels = c("never missed", "missed 1-2days", "missed 3+days"))

data <- data %>%
  mutate(race3 = case_when(
    race4 == "Black" ~ "Black",
    race4 == "White" ~ "White",
    race4 %in% c("Hispanic", "Other") ~ "Other",
    TRUE ~ NA_character_   # keeps missing values as NA
  ))

#missing schools: sch_c1, need to recode
data$sch_c3 <- dplyr::case_when(
  data$sch_c1 %in% c("None") ~ "never missed",
  data$sch_c1 %in% c("1day", "2days") ~ "missed 1-2days",
  data$sch_c1 %in% c("3days", "4-5days","6-10days", "11ormore") ~ "missed 3+days",
  TRUE ~ NA_character_
)

#parental education: also need to recode
data$p_edu.n3 <- dplyr::case_when(
  data$p_edu.n %in% 1:2 ~ "Lower than high school",
  data$p_edu.n %in% 3:4 ~ "High school",
  data$p_edu.n %in% 5:6 ~ "College and above",
  TRUE ~ NA_character_
)

data$p_edu.f<-factor(data$p_edu.n3)

table(data$sex1, useNA="ifany" ) #m=6558
table(data$race3, useNA="ifany" )
table(data$rural5, useNA="ifany" ) #m=12320
table(data$ideo2, useNA="ifany" )
table(data$sch_c3, useNA="ifany" )
table(data$p_edu.n3, useNA="ifany" )  #m=7817
table(data$any.p, useNA="ifany" ) 
table(data$region1, useNA="ifany" )    
table(data$happy1.n, useNA="ifany" )   #m=876
table(data$care_soc1.n, useNA="ifany" ) #m=2326

data <- data %>%
  mutate(across(c(sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1, happy1,care_soc1), as.factor))

#assumption: drop all missing values 
data1 <- data %>%
  select(weight, year,pol_p1.no,
         sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1, happy1, care_soc1,happy1.n, care_soc1.n)%>%
  drop_na()

data1<-subset(data1, year<=2018)
#1999, 2000, 2001, and 1993 has no "don't know category"
data1<-subset(data1, year!=1993)
data1<-subset(data1, year!=1999)
data1<-subset(data1, year!=2000)
data1<-subset(data1, year!=2001)



data1 <- data1 %>%
  # make categorical variables factors
  mutate(
    across(
      c(pol_p1.no, sex1, race3, rural5, ideo2,
        sch_c3, p_edu.n3, any.p, region1),
      ~ factor(.)
    ),
    # make sure scales are numeric (important!)
    happy1.n    = as.numeric(happy1.n),
    care_soc1.n = as.numeric(care_soc1.n)
  )

data_svy <- data1 %>%
  as_survey_design(id = NULL,
                   weights = weight)

###############################################################################
# Table 1a. Full sample, weighted descriptive
tbl_desc <- tbl_svysummary(
  data = data_svy,
  include = c(
    pol_p1.no, sex1, race3, rural5, ideo2,
    sch_c3, p_edu.n3, any.p, region1,
    happy1.n, care_soc1.n
  ),
  type = list(
    # ensure these are treated as continuous
    happy1.n    ~ "continuous2",
    care_soc1.n ~ "continuous2"
  ),
  statistic = list(
    all_categorical() ~ "{p}%",         # ONLY percentage, 1 decimal place
    all_continuous()  ~ "{mean} ({sd})"   # mean (SD)
  ),
  digits = list(
    all_categorical() ~ 1              # 1 decimal place for %
  )
) |>
  bold_labels()
tbl_desc

###############################################################################
# Appendix D. Full sample, unweighted descriptive
tbl_unw <- tbl_summary(
  data = data1,
  include = c(
    pol_p1.no, sex1, race3, rural5, ideo2,
    sch_c3, p_edu.n3, any.p, region1,
    happy1.n, care_soc1.n
  ),
  type = list(
    # ensure these are treated as continuous
    happy1.n    ~ "continuous2",
    care_soc1.n ~ "continuous2"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",   # weighted N and %
    all_continuous() ~ "{mean} ({sd})"  # weighted mean (SD)
  ),
  digits = list(
    all_categorical() ~ 1              # 1 decimal place for %
  )
) |>
  bold_labels()

tbl_unw


tbl_desc_export <- tbl_desc %>% 
  as_tibble()   # convert gtsummary object to a data frame
write_xlsx(tbl_desc_export, "output/tbl_desc_nomissing.xlsx")

tbl_desc_export <- tbl_unw %>% 
  as_tibble()   # convert gtsummary object to a data frame

write_xlsx(tbl_desc_export, "output/tbl_desc_unw_nomissing.xlsx")



###############################################################################
###############################################################################
#Descriptive statistics with samples from the two main cohorts only#

data1<- data1 %>%
  mutate(
    cohort = case_when(
      year<=1985                                      ~ "C1976-85",
      year<=2018&year>=2009                           ~ "C2009-18",
      TRUE                                            ~ NA_character_ ))
data2=subset(data1, !is.na(cohort)) #drop other years, keep 3 periods only
data2 <- data2 %>%
  mutate(
    across(
      c(pol_p1.no, sex1, race3, rural5, ideo2,
        sch_c3, p_edu.n3, any.p, region1, cohort),
      ~ factor(.)
    ),
    happy1    = as.numeric(as.character(happy1.n)),
    care_soc1 = as.numeric(as.character(care_soc1.n))
  )


data_svy <- data2 %>%
  as_survey_design(id = NULL,
                   weights = weight)

###############################################################################
#Table 1b (2nd and 3rd column). weighted descriptive table for two cohorts only
tbl_cohort <- tbl_svysummary(
  data = data_svy,
  by   = cohort,   # group variable
  include = c(
    pol_p1.no, sex1, race3, rural5, ideo2,
    sch_c3, p_edu.n3, any.p, region1,
    happy1, care_soc1
  ),
  type = list(
    happy1    ~ "continuous2",
    care_soc1 ~ "continuous2"
  ),
  statistic = list(
    all_categorical() ~ "{p}%",     # weighted N (%)
    all_continuous()  ~ "{mean} ({sd})"   # weighted mean (SD)
  ),
  digits = list(
    all_categorical() ~ 1              # 1 decimal place for %
  )
) |>
  add_p() |>          # adds survey-appropriate tests by default
  bold_labels()

tbl_cohort

###############################################################################
#Appendix D (2nd and 3rd column). Unweighted descriptive table for two cohorts only
tbl_cohort_unw <- tbl_summary(
  data = data2,
  by   = cohort,   # group variable
  include = c(
    pol_p1.no, sex1, race3, rural5, ideo2,
    sch_c3, p_edu.n3, any.p, region1,
    happy1, care_soc1
  ),
  type = list(
    happy1    ~ "continuous2",
    care_soc1 ~ "continuous2"
  ),
  statistic = list(
    all_categorical() ~ "{n} ({p}%)",     # weighted N (%)
    all_continuous()  ~ "{mean} ({sd})"   # weighted mean (SD)
  ),
  digits = list(
    all_categorical() ~ 1              # 1 decimal place for %
  )
) |>
  add_p() |>          # adds survey-appropriate tests by default
  bold_labels()

tbl_cohort_unw


tbl_desc_export <- tbl_cohort %>% 
  as_tibble()   # convert gtsummary object to a data frame
write_xlsx(tbl_desc_export, "output/tbl_cohort.xlsx")
tbl_desc_export <- tbl_cohort_unw %>% 
  as_tibble()   # convert gtsummary object to a data frame
write_xlsx(tbl_desc_export, "output/tbl_cohort_unw.xlsx")

