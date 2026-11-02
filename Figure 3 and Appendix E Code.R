
####################No Opinion Project##########################################
######Figure 3, Appendix E#################
#Interaction model: having cohort interact with every single indepedent variables#
library(ggeffects)

data$sch_c3 = factor(data$sch_c3, levels = c("never missed", "missed 1-2days", "missed 3+days"))

data <- data %>%
  mutate(race3 = case_when(
    race4 == "Black" ~ "Black",
    race4 == "White" ~ "White",
    race4 %in% c("Hispanic", "Other") ~ "Other",
    TRUE ~ NA_character_   # keeps missing values as NA
  ))
data$race3 = factor(data$race3, levels = c("White", "Black", "Other"))

data1 <- data %>%
  mutate(cohort2=case_when(
    year<1986 ~ "cohort1976-85",
    year<2019&year>2008 ~"cohort2009-18",
    TRUE ~ NA_character_
  ))
data1 <- data1 %>%
  mutate(across(c(sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1,cohort2), as.factor))

des <- svydesign(ids = ~1, weights = ~weight, data = data1)

################################################################################
#Cohort interact with everything ? 
m1 <- svyglm(pol_p1.no ~ sex1*cohort2+race3*cohort2+rural5*cohort2+ideo2*cohort2+sch_c3*cohort2+p_edu.n3*cohort2+any.p*cohort2+region1*cohort2+happy1.n*cohort2+care_soc1.n*cohort2, 
             design = des, family = binomial())
summary(m1)
reg_table_app <- tidy(m1, conf.int = TRUE) %>%
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

write_xlsx(
  reg_table_app,
  "output/app_interaction.xlsx"
)

################################################################################
#Ploting for Figure 3

library(marginaleffects)
#sex*cohort Plot:1
pred <- avg_predictions(
  m1,
  variables = c("cohort2", "sex1"),
  type = "response"
)
p.sex<-data.frame(pred)

library(ggplot2)

p1 <- ggplot(
  pred,
  aes(
    x = cohort2,
    y = estimate,
    color = sex1,
    group = sex1
  )
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = position_dodge(width = 0.15),
    linewidth = 0.8
  ) +
  geom_point(
    position = position_dodge(width = 0.15),
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  labs(
    title = "a.Sex*Cohort",
    x = "Cohort",
    y = "Predicted Probability",
    color = "Sex"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

p1

###########################################################################
#Race*Cohort,2
pred <- avg_predictions(
  m1,
  variables = c("cohort2", "race3"),
  type = "response"
)
p.race<-data.frame(pred)


p2 <- ggplot(
  pred,
  aes(
    x = cohort2,
    y = estimate,
    color = race3,
    group = race3
  )
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = position_dodge(width = 0.15),
    linewidth = 0.8
  ) +
  geom_point(
    position = position_dodge(width = 0.15),
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  labs(
    title = "b.Race*Cohort",
    x = "Cohort",
    y = "Predicted Probability",
    color = "Race"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

p2

###############################################################################
#Parental edu*Cohort,2
pred <- avg_predictions(
  m1,
  variables = c("cohort2", "p_edu.n3"),
  type = "response"
)
p.edu<-data.frame(pred)


p3 <- ggplot(
  pred,
  aes(
    x = cohort2,
    y = estimate,
    color = p_edu.n3,
    group = p_edu.n3
  )
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = position_dodge(width = 0.15),
    linewidth = 0.8
  ) +
  geom_point(
    position = position_dodge(width = 0.15),
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  labs(
    title = "c.Parental edu*Cohort",
    x = "Cohort",
    y = "Predicted Probability",
    color = "Parental edu"
  ) +
  scale_color_discrete(
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

p3

###############################################################################
#Combine the three plots
library(gridExtra)
g <- gridExtra::arrangeGrob(
  p, p2, p3, 
  ncol = 2
)
g
ggsave(
  filename = "output/Figure3_new.png",
  plot = g,
  width = 9, height = 10, units = "in",
  dpi = 300
)

#also generate a table for predicted probabilities
# Define file path (using your dataDir variable)
file_path <- file.path("C:/Users/yunmeilu/Box/MTF police/No opinion project/output","predicted probability_with_interactions.xlsx")
library(openxlsx)
# Create a new Excel workbook
wb <- createWorkbook()

addWorksheet(wb, "sex")
writeData(wb, "sex", p.sex)
addWorksheet(wb, "race")
writeData(wb, "race", p.race)
addWorksheet(wb, "edu")
writeData(wb, "edu", p.edu)

# Save the workbook (overwrite if it already exists)
saveWorkbook(wb, file_path, overwrite = TRUE)

# Optional: confirm sheets created
getSheetNames(file_path)

###############################################################################
###############################################################################
