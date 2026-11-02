#####No opinion project#####################################################
##Figure 1 and Figure 2 Code
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(extrafont)

font_import()
loadfonts(device = "win")

setwd("C:/Users/yunmeilu/Box/MTF police/No opinion project")

#by demographic subgroups: sex1,race4,region, rural5, ideo1, ideo2.n,
# sex1 and race4 ready
#rural urban 5 groups: rural5
#political ideology (con, moderate, lib, radical, don't know): ideo2 or ideo2.n
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

data <- data %>%
  mutate(race3 = case_when(
    race4 == "Black" ~ "Black",
    race4 == "White" ~ "White",
    race4 %in% c("Hispanic", "Other") ~ "Other",
    TRUE ~ NA_character_   # keeps missing values as NA
  ))

#any victimization: any.p
#region: region1

#data1<-subset(data, year<=2019)
#update: exclude 2019
data1<-subset(data, year<=2018)
#1999, 2000, 2001, and 1993 has no "don't know category"
data1<-subset(data1, year!=1993)
data1<-subset(data1, year!=1999)
data1<-subset(data1, year!=2000)
data1<-subset(data1, year!=2001)

####just one figure###########
#Figure 1: 
#no opinion to police perceptions
figdata1a <-data1 %>%
  select(weight, year,
         pol_p1.no,scourt_p1.no,justice_p1.no) 

df_long <- figdata1a %>%
  pivot_longer(cols = c(pol_p1.no,scourt_p1.no,justice_p1.no),
               names_to = "variable", values_to = "response")

df_summary <- df_long %>%
  group_by(year, variable) %>%
  summarize(
    prop_no_opinion = weighted.mean(response,
                                    w = weight, 
                                    na.rm = TRUE),
    .groups = "drop"
  )

fig1<-df_summary %>%
  filter(year<=2018) %>%
  filter(variable=="pol_p1.no") %>%
  ggplot(aes(x = year, y = prop_no_opinion)) +
  #geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +  # linear trend line
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.3) ) +
  labs(
    #title = "Weighted Proportion of 'No Opinion' Responses for Police Perceptions",
    x = "Year/Cohort",
    y = "Weighted Proportion"
  ) +
  theme_bw()+
  #scale_x_continuous(
  #  limits = c(1976, 2018),
  #  breaks = seq(1976, 2018, by = 10)
  #) +
  #theme_classic(base_size = 13)+
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    legend.position = "",              # 👈 Move legend to bottom
    #legend.title = element_text(face = "bold"),
    #legend.box = "horizontal",               # Arrange items horizontally
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

fig1

ggsave(
  filename = "no_opinion_trend.png",              # choose any filename/format
  plot = fig1,                                       # your ggplot object
  path = "C:/Users/yunmeilu/Box/MTF police/No opinion project",
  width = 6, height = 3.5, units = "in", dpi = 300  # high resolution for slides
)


###############################################################################
###############################################################################
#Figure 2/
#no opinion by population subgroups
figdata1a <-data1 %>%
  select(weight, year,  sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1, happy1, care_soc1,
         pol_p1.no)
colSums(is.na(figdata1a))
figdata1a <- data1 %>%
  select(weight, year, sex1, race3, rural5, ideo2,sch_c3, p_edu.n3, any.p, region1, happy1, care_soc1,
         pol_p1.no) %>%
  drop_na() 

# List of variables you want to loop through
group_vars <- c("sex1", "race3", "rural5", "ideo2", "sch_c3", "p_edu.n3", "any.p", "region1","happy1", "care_soc1")
# Function to summarize weighted proportion of pol_p1.no by year and a grouping var
summarize_weighted <- function(df, group_var) {
  g <- rlang::sym(group_var)
  
  df %>%
    dplyr::group_by(year, !!g) %>%
    dplyr::summarise(
      prop_no_opinion = stats::weighted.mean(pol_p1.no, weight, na.rm = TRUE),
      n = dplyr::n(),
      variable = group_var,
      .groups = "drop"
    ) %>%
    dplyr::rename(group = !!g)
}

summary_all <- purrr::map_dfr(group_vars, ~ summarize_weighted(figdata1a, .x))

# Ensure types are right
summary_all <- summary_all %>%
  mutate(year = as.numeric(year))


#Define a common theme (so they look consistent & compact on A4)
common_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

## 1. Sex
fig_sex <- summary_all %>%
  filter(variable == "sex1") %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Sex",
    shape = "Sex",
    title = "A. Sex"
  ) +
  common_theme


## 2. Race
fig_race <- summary_all %>%
  filter(variable == "race3") %>%
  mutate(group = factor(group, levels = c("White", "Black", "Other"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Race",
    shape = "Race",
    title = "B. Race"
  ) +
  common_theme


## 3. Parental Education
fig_paredu <- summary_all %>%
  filter(variable == "p_edu.n3") %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Parental Edu",
    shape = "Parental Edu",
    title = "C. Parental Education"
  ) +
  #guides(
  #  color = guide_legend(nrow = 1),
  #  shape = guide_legend(nrow = 1)
  #) +
  common_theme


## 4. Ideology
fig_ideo <- summary_all %>%
  filter(variable == "ideo2") %>%
  mutate(group = factor(group,
                        levels = c("cons","moderate","lib","radical","don't know"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Ideology",
    shape = "Ideology",
    title = "D. Ideology"
  ) +
  guides(
    color = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2)
  ) +
  common_theme


## 5. Prior Victimization
fig_vic <- summary_all %>%
  filter(variable == "any.p") %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Prior Victimization",
    shape = "Prior Victimization",
    title = "E. Prior Victimization"
  ) +
  common_theme


## 6. Rural/Urban
fig_rural <- summary_all %>%
  filter(variable == "rural5") %>%
  mutate(group = factor(
    group,
    levels = c("rural","small city","medium city (50,000-100,000)",
               "large city (100,000-500,000)","very large city (over 500,000)")
  )) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Rural/Urban",
    shape = "Rural/Urban",
    title = "F. Rural/Urban"
  ) +
  scale_color_discrete(
    labels = c("Rural", "Small city", "Medium city", "Large city", "Very large city")
  ) +
  scale_shape_discrete(
    labels = c("Rural", "Small city", "Medium city", "Large city", "Very large city")
  ) +
  guides(
    color = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2)
  ) +
  common_theme


fig_all <- grid.arrange(
  fig_sex, fig_race,
  fig_paredu, fig_ideo,
  fig_vic, fig_rural,
  ncol = 2,   # two columns
  nrow = 3    # three rows
)

ggsave(
  filename = "fig2a.png",
  plot = fig_all,
  path = figDir,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 300
)



##########################################
## 1. School absence (sch_c3)
fig_sch <- summary_all %>%
  filter(variable == "sch_c3") %>%
  mutate(group = factor(group, levels = c("never missed","missed 1-2days","missed 3+days"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "School absence",
    shape = "School absence",
    title = "G. School engagement"
  ) +
  scale_color_discrete(
    labels = c("Never missed", "Missed 1–2 days", "Missed 3+ days")
  ) +
  scale_shape_discrete(
    labels = c("Never missed", "Missed 1–2 days", "Missed 3+ days")
  ) +
  #guides(
  #  color = guide_legend(nrow = 3),
  #  shape = guide_legend(nrow = 3)
  #) +
  common_theme


## 2. Region (region1)
fig_region <- summary_all %>%
  filter(variable == "region1") %>%
  mutate(group = factor(group, levels = c("Northcentral", "Northeast","South","West"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Region",
    shape = "Region",
    title = "H. Region"
  ) +
  scale_color_discrete(
    labels = c("Northcentral", "Northeast","South","West")
  ) +
  scale_shape_discrete(
    labels = c("Northcentral", "Northeast","South","West")
  ) +
  guides(
    color = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2)
  ) +
  common_theme


## 3. Happiness (happy1)
fig_happy <- summary_all %>%
  filter(variable == "happy1") %>%
  mutate(group = factor(group, levels = c("Not too happy", "Pretty happy","Very happy"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Happiness",
    shape = "Happiness",
    title = "I. Mental wellbeing"
  ) +
  scale_color_discrete(
    labels = c("Not too happy", "Pretty happy","Very happy")
  ) +
  scale_shape_discrete(
    labels = c("Not too happy", "Pretty happy","Very happy")
  ) +
  guides(
    color = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1)
  ) +
  common_theme


## 4. Caring about social problems (care_soc1)
fig_soc <- summary_all %>%
  filter(variable == "care_soc1") %>%
  mutate(group = factor(group,
                        levels = c("Never", "Seldom", "Sometimes",
                                   "Quite often", "A great deal"))) %>%
  ggplot(aes(x = year, y = prop_no_opinion, color = group, group = group)) +
  geom_point(aes(shape = group), size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.35)) +
  labs(
    x = "Cohort",
    y = "Weighted Proportion",
    color = "Care about social problems",
    shape = "Care about social problems",
    title = "J. Interest in social issues"
  ) +
  scale_color_discrete(
    labels = c("Never", "Seldom", "Sometimes", "Quite often", "A great deal")
  ) +
  scale_shape_discrete(
    labels = c("Never", "Seldom", "Sometimes", "Quite often", "A great deal")
  ) +
  guides(
    color = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2)
  ) +
  common_theme

fig_all2 <- grid.arrange(
  fig_sch, fig_region,
  fig_happy, fig_soc,
  ncol = 2,   # 2 columns
  nrow = 2   # 2 rows
)

# If you just want to see it:
fig_all2

ggsave(
  filename = "fig2b.png",
  plot = fig_all2,
  path = figDir,
  width = 8.27,
  height = 8.69,
  units = "in",
  dpi = 300
)
