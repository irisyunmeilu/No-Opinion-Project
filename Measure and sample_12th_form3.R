#-------------------------------------------------------------------------------
# No Opinion Paper
# Measurement Coding
# Late update: 10/15/2026
# Yunmei Lu 
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
#devtools::install_github("kassambara/r2excel")
#devtools::install_github("gergness/srvyr")

## Load the packages
library("pacman")                  # Load pacman package

pacman::p_load(
  #psych,      # factor analysis
  here,       # relative file paths
  foreign,    # read data
  plyr,       #
  dplyr,      # variable processing
  tidyr,      # reshaping data
  forcats,    # reverse factor variables
  srvyr,      # calc % with survey weights
  purrr,      # to use modify_at
  MESS,       # round prop & preserve sum to 100%
  ltm,        # calculate crobach's alpha
  data.table, #
  gtsummary,  # pretty weighted tables
  ggplot2,    # graphing
  colorspace, # color palettes   
  conflicted) # choose default packages

## Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("desc","dplyr")
conflict_prefer("select","dplyr")
conflict_prefer("rename","dplyr")


## Specify the file paths
projDir <- here::here() 
dataDir <- file.path(projDir,"Data/12th")           # File path to where data will be downloaded
outDir  <- "output"                                         # Name of the sub-folder where we will save results
figDir  <- file.path(outDir, "figs")                        # Name of the sub-folder where we will save generated figures
load(paste0(dataDir, "/mtf_form3_12th.Rda"))


# VARIABLES --------------------------------------------------------------------

## Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c(7927,  7928,  7929,  7930,
                7900,  9013,  9045,  8387,   8388,
                8546,  8701,  9079,  9259,   9397,
                9745,  9871,  6133,  6367,   6517,
                6716,  2268,  2477,  2751,   2939,
                3184,  3425,  3753,  4019,   4264,
                4536, 20022, 22480,  25382, 28401,
                30985, 34409, 34861, 35218, 36263,
                36408, 36798, 37182, 37416, 37841,
                38156, 38503)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018, 2019,
                2020, 2021)

Xwalk <- data.frame(surveyyear, studyid)
#####################################################################################

## Rename Variables
names(mtf_V3)
mtf_V3 <- dplyr::rename(mtf_V3,      
                        wt7611    = V5,      wt1217   = ARCHIVE_WT,   year       = V1,      formid    = V3,
                        RID       = V4,      region   = V13,          msa        = V17,
                        race      = V3151,   sex      = V3150,        birthyr    = V3148,   rural     = V3152,
                        f_edu     = V3163,   m_edu    = V3164,
                        ideo      = V3167,   race_nei = V3242,  
                        fd_mj     = V3478,   pol_al   = V3351,         pol_mj     =V3368,    pol_drug  = V3385,
                        nm_ar     = V3322,   
                        sch_c     = V3176,   class_c   = V3178,    
                        pol_p     = V3265,
                        cop_p     = V3255,   union_p  = V3256,   uni_p=V3257,       publicsch_p=V3258, 
                        church_p  = V3259,   media_p=V3260,      president_p=V3261, congress_p=V3262, 
                        scourt_p  = V3263,   justice_p=V3264,     military_p=V3266, 
                        us_tosch  =V3485,    us_atsch= V3484,
                        happy     = V3208,   care_soc= V3209)

data=mtf_V3
rm(mtf_V3)

#####################################################################################
# data Wrangling: data Transformation

## Year
table(data$year)
data$year=as.character(data$year)
data$year[data$year == "76"] <- "1976"
data$year[data$year == "88"] <- "1988"

sum(is.na(data$year)) #34 observations missing
data$year[is.na(data$year)]  <- "1978" # 30 people in 1978 have a missing year variable
data$year <- as.character(data$year) %>%
  as.integer(data$year)

## Weights
table(data$year, !is.na(data$wt7611))
table(data$year, !is.na(data$wt1217))

data <- data %>%
  mutate(
    weight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1217))


#key dependent variable: police perception#####################################
table(data$pol_p)

data <- data %>%
  mutate(
    pol_p1 = case_when(
      pol_p == "1"  | pol_p == "VRY POOR"    | pol_p == "VRY POOR:(1)"   | pol_p == "Very poor"         ~ "very poor",
      pol_p == "2"  | pol_p == "POOR"        | pol_p == "POOR:(2)"       | pol_p == "Poor"              ~ "poor",
      pol_p == "3"  | pol_p == "FAIR"        | pol_p == "FAIR:(3)"       | pol_p == "Fair"              ~ "fair",
      pol_p == "4"  | pol_p == "GOOD"        | pol_p == "GOOD:(4)"       | pol_p ==  "Good"             ~ "good",
      pol_p == "5"  | pol_p == "VRY GOOD"    | pol_p == "VRY GOOD:(5)"   | pol_p ==  "Very good"        ~ "very good",
      pol_p == "8"  | pol_p == "NO OPIN:(8)" | pol_p == "No opinion"     | pol_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      

data$pol_p1<-factor(data$pol_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$pol_p1.n=as.numeric(data$pol_p1)

#no opinion coded as NA
data$pol_p1.n5=data$pol_p1.n
data$pol_p1.n5[data$pol_p1.n==6]=NA  #no opinion coded as NA 

#proportion of no-opinion
data$pol_p1.no=NA
data$pol_p1.no[data$pol_p1.n==6]=1
data$pol_p1.no[data$pol_p1.n<6&data$pol_p1.n>0]=0
table(data$pol_p1.no, useNA="ifany")
table(data$pol_p1.n, useNA="ifany")
table(data$year, data$pol_p1.n, useNA="ifany")
#####################################################################################
#10/3/2025 coding other perception variables: 


###coperation#####
table(data$cop_p)
data <- data %>%
  mutate(
    cop_p1 = case_when(
      cop_p == "1"  | cop_p == "VRY POOR"    | cop_p == "VRY POOR:(1)"   | cop_p == "Very poor"         ~ "very poor",
      cop_p == "2"  | cop_p == "POOR"        | cop_p == "POOR:(2)"       | cop_p == "Poor"              ~ "poor",
      cop_p == "3"  | cop_p == "FAIR"        | cop_p == "FAIR:(3)"       | cop_p == "Fair"              ~ "fair",
      cop_p == "4"  | cop_p == "GOOD"        | cop_p == "GOOD:(4)"       | cop_p ==  "Good"             ~ "good",
      cop_p == "5"  | cop_p == "VRY GOOD"    | cop_p == "VRY GOOD:(5)"   | cop_p ==  "Very good"        ~ "very good",
      cop_p == "8"  | cop_p == "NO OPIN:(8)" | cop_p == "No opinion"     | cop_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$cop_p1<-factor(data$cop_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$cop_p1.n=as.numeric(data$cop_p1)
#no opinion coded as NA
data$cop_p1.n5=data$cop_p1.n
data$cop_p1.n5[data$cop_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$cop_p1.no=0
data$cop_p1.no[data$cop_p1.n==6]=1
table(data$cop_p1.n, useNA="ifany")
table(data$year, data$cop_p1.n, useNA="ifany")

###union#####
table(data$union_p)
data <- data %>%
  mutate(
    union_p1 = case_when(
      union_p == "1"  | union_p == "VRY POOR"    | union_p == "VRY POOR:(1)"   | union_p == "Very poor"         ~ "very poor",
      union_p == "2"  | union_p == "POOR"        | union_p == "POOR:(2)"       | union_p == "Poor"              ~ "poor",
      union_p == "3"  | union_p == "FAIR"        | union_p == "FAIR:(3)"       | union_p == "Fair"              ~ "fair",
      union_p == "4"  | union_p == "GOOD"        | union_p == "GOOD:(4)"       | union_p ==  "Good"             ~ "good",
      union_p == "5"  | union_p == "VRY GOOD"    | union_p == "VRY GOOD:(5)"   | union_p ==  "Very good"        ~ "very good",
      union_p == "8"  | union_p == "NO OPIN:(8)" | union_p == "No opinion"     | union_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$union_p1<-factor(data$union_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$union_p1.n=as.numeric(data$union_p1)
#no opinion coded as NA
data$union_p1.n5=data$union_p1.n
data$union_p1.n5[data$union_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$union_p1.no=0
data$union_p1.no[data$union_p1.n==6]=1
table(data$union_p1.n, useNA="ifany")
table(data$year, data$union_p1.n, useNA="ifany")



###university#####
table(data$uni_p)
data <- data %>%
  mutate(
    uni_p1 = case_when(
      uni_p == "1"  | uni_p == "VRY POOR"    | uni_p == "VRY POOR:(1)"   | uni_p == "Very poor"         ~ "very poor",
      uni_p == "2"  | uni_p == "POOR"        | uni_p == "POOR:(2)"       | uni_p == "Poor"              ~ "poor",
      uni_p == "3"  | uni_p == "FAIR"        | uni_p == "FAIR:(3)"       | uni_p == "Fair"              ~ "fair",
      uni_p == "4"  | uni_p == "GOOD"        | uni_p == "GOOD:(4)"       | uni_p ==  "Good"             ~ "good",
      uni_p == "5"  | uni_p == "VRY GOOD"    | uni_p == "VRY GOOD:(5)"   | uni_p ==  "Very good"        ~ "very good",
      uni_p == "8"  | uni_p == "NO OPIN:(8)" | uni_p == "No opinion"     | uni_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$uni_p1<-factor(data$uni_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$uni_p1.n=as.numeric(data$uni_p1)
#no opinion coded as NA
data$uni_p1.n5=data$uni_p1.n
data$uni_p1.n5[data$uni_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$uni_p1.no=0
data$uni_p1.no[data$uni_p1.n==6]=1
table(data$uni_p1.n, useNA="ifany")
table(data$year, data$uni_p1.n, useNA="ifany")

###public school#####
table(data$publicsch_p)
data <- data %>%
  mutate(
    publicsch_p1 = case_when(
      publicsch_p == "1"  | publicsch_p == "VRY POOR"    | publicsch_p == "VRY POOR:(1)"   | publicsch_p == "Very poor"         ~ "very poor",
      publicsch_p == "2"  | publicsch_p == "POOR"        | publicsch_p == "POOR:(2)"       | publicsch_p == "Poor"              ~ "poor",
      publicsch_p == "3"  | publicsch_p == "FAIR"        | publicsch_p == "FAIR:(3)"       | publicsch_p == "Fair"              ~ "fair",
      publicsch_p == "4"  | publicsch_p == "GOOD"        | publicsch_p == "GOOD:(4)"       | publicsch_p ==  "Good"             ~ "good",
      publicsch_p == "5"  | publicsch_p == "VRY GOOD"    | publicsch_p == "VRY GOOD:(5)"   | publicsch_p ==  "Very good"        ~ "very good",
      publicsch_p == "8"  | publicsch_p == "NO OPIN:(8)" | publicsch_p == "No opinion"     | publicsch_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$publicsch_p1<-factor(data$publicsch_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$publicsch_p1.n=as.numeric(data$publicsch_p1)
#no opinion coded as NA
data$publicsch_p1.n5=data$publicsch_p1.n
data$publicsch_p1.n5[data$publicsch_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$publicsch_p1.no=0
data$publicsch_p1.no[data$publicsch_p1.n==6]=1
table(data$publicsch_p1.n, useNA="ifany")
table(data$year, data$publicsch_p1.n, useNA="ifany")

###church and religion organization#####
table(data$church_p)
data <- data %>%
  mutate(
    church_p1 = case_when(
      church_p == "1"  | church_p == "VRY POOR"    | church_p == "VRY POOR:(1)"   | church_p == "Very poor"         ~ "very poor",
      church_p == "2"  | church_p == "POOR"        | church_p == "POOR:(2)"       | church_p == "Poor"              ~ "poor",
      church_p == "3"  | church_p == "FAIR"        | church_p == "FAIR:(3)"       | church_p == "Fair"              ~ "fair",
      church_p == "4"  | church_p == "GOOD"        | church_p == "GOOD:(4)"       | church_p ==  "Good"             ~ "good",
      church_p == "5"  | church_p == "VRY GOOD"    | church_p == "VRY GOOD:(5)"   | church_p ==  "Very good"        ~ "very good",
      church_p == "8"  | church_p == "NO OPIN:(8)" | church_p == "No opinion"     | church_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$church_p1<-factor(data$church_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$church_p1.n=as.numeric(data$church_p1)
#no opinion coded as NA
data$church_p1.n5=data$church_p1.n
data$church_p1.n5[data$church_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$church_p1.no=0
data$church_p1.no[data$church_p1.n==6]=1
table(data$church_p1.n, useNA="ifany")
table(data$year, data$church_p1.n, useNA="ifany")

###media#####
table(data$media_p)
data <- data %>%
  mutate(
    media_p1 = case_when(
      media_p == "1"  | media_p == "VRY POOR"    | media_p == "VRY POOR:(1)"   | media_p == "Very poor"         ~ "very poor",
      media_p == "2"  | media_p == "POOR"        | media_p == "POOR:(2)"       | media_p == "Poor"              ~ "poor",
      media_p == "3"  | media_p == "FAIR"        | media_p == "FAIR:(3)"       | media_p == "Fair"              ~ "fair",
      media_p == "4"  | media_p == "GOOD"        | media_p == "GOOD:(4)"       | media_p ==  "Good"             ~ "good",
      media_p == "5"  | media_p == "VRY GOOD"    | media_p == "VRY GOOD:(5)"   | media_p ==  "Very good"        ~ "very good",
      media_p == "8"  | media_p == "NO OPIN:(8)" | media_p == "No opinion"     | media_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$media_p1<-factor(data$media_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$media_p1.n=as.numeric(data$media_p1)
#no opinion coded as NA
data$media_p1.n5=data$media_p1.n
data$media_p1.n5[data$media_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$media_p1.no=0
data$media_p1.no[data$media_p1.n==6]=1
table(data$media_p1.n, useNA="ifany")
table(data$year, data$media_p1.n, useNA="ifany")

###president#####
table(data$president_p)
data <- data %>%
  mutate(
    president_p1 = case_when(
      president_p == "1"  | president_p == "VRY POOR"    | president_p == "VRY POOR:(1)"   | president_p == "Very poor"         ~ "very poor",
      president_p == "2"  | president_p == "POOR"        | president_p == "POOR:(2)"       | president_p == "Poor"              ~ "poor",
      president_p == "3"  | president_p == "FAIR"        | president_p == "FAIR:(3)"       | president_p == "Fair"              ~ "fair",
      president_p == "4"  | president_p == "GOOD"        | president_p == "GOOD:(4)"       | president_p ==  "Good"             ~ "good",
      president_p == "5"  | president_p == "VRY GOOD"    | president_p == "VRY GOOD:(5)"   | president_p ==  "Very good"        ~ "very good",
      president_p == "8"  | president_p == "NO OPIN:(8)" | president_p == "No opinion"     | president_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$president_p1<-factor(data$president_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$president_p1.n=as.numeric(data$president_p1)
#no opinion coded as NA
data$president_p1.n5=data$president_p1.n
data$president_p1.n5[data$president_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$president_p1.no=0
data$president_p1.no[data$president_p1.n==6]=1
table(data$president_p1.n, useNA="ifany")
table(data$year, data$president_p1.n, useNA="ifany")

###congress#####
table(data$congress_p)
data <- data %>%
  mutate(
    congress_p1 = case_when(
      congress_p == "1"  | congress_p == "VRY POOR"    | congress_p == "VRY POOR:(1)"   | congress_p == "Very poor"         ~ "very poor",
      congress_p == "2"  | congress_p == "POOR"        | congress_p == "POOR:(2)"       | congress_p == "Poor"              ~ "poor",
      congress_p == "3"  | congress_p == "FAIR"        | congress_p == "FAIR:(3)"       | congress_p == "Fair"              ~ "fair",
      congress_p == "4"  | congress_p == "GOOD"        | congress_p == "GOOD:(4)"       | congress_p ==  "Good"             ~ "good",
      congress_p == "5"  | congress_p == "VRY GOOD"    | congress_p == "VRY GOOD:(5)"   | congress_p ==  "Very good"        ~ "very good",
      congress_p == "8"  | congress_p == "NO OPIN:(8)" | congress_p == "No opinion"     | congress_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$congress_p1<-factor(data$congress_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$congress_p1.n=as.numeric(data$congress_p1)
#no opinion coded as NA
data$congress_p1.n5=data$congress_p1.n
data$congress_p1.n5[data$congress_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$congress_p1.no=0
data$congress_p1.no[data$congress_p1.n==6]=1
table(data$congress_p1.n, useNA="ifany")
table(data$year, data$congress_p1.n, useNA="ifany")

###supreme court#####
table(data$scourt_p)
data <- data %>%
  mutate(
    scourt_p1 = case_when(
      scourt_p == "1"  | scourt_p == "VRY POOR"    | scourt_p == "VRY POOR:(1)"   | scourt_p == "Very poor"         ~ "very poor",
      scourt_p == "2"  | scourt_p == "POOR"        | scourt_p == "POOR:(2)"       | scourt_p == "Poor"              ~ "poor",
      scourt_p == "3"  | scourt_p == "FAIR"        | scourt_p == "FAIR:(3)"       | scourt_p == "Fair"              ~ "fair",
      scourt_p == "4"  | scourt_p == "GOOD"        | scourt_p == "GOOD:(4)"       | scourt_p ==  "Good"             ~ "good",
      scourt_p == "5"  | scourt_p == "VRY GOOD"    | scourt_p == "VRY GOOD:(5)"   | scourt_p ==  "Very good"        ~ "very good",
      scourt_p == "8"  | scourt_p == "NO OPIN:(8)" | scourt_p == "No opinion"     | scourt_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$scourt_p1<-factor(data$scourt_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$scourt_p1.n=as.numeric(data$scourt_p1)
#no opinion coded as NA
data$scourt_p1.n5=data$scourt_p1.n
data$scourt_p1.n5[data$scourt_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion

data$scourt_p1.no=NA
data$scourt_p1.no[data$scourt_p1.n==6]=1
data$scourt_p1.no[data$scourt_p1.n<6&data$justice_p1.n>0]=0
table(data$scourt_p1.no, useNA="ifany")
table(data$scourt_p1.n, useNA="ifany")
table(data$year, data$scourt_p1.n, useNA="ifany")



###other court and the justice system#####
table(data$justice_p)
data <- data %>%
  mutate(
    justice_p1 = case_when(
      justice_p == "1"  | justice_p == "VRY POOR"    | justice_p == "VRY POOR:(1)"   | justice_p == "Very poor"         ~ "very poor",
      justice_p == "2"  | justice_p == "POOR"        | justice_p == "POOR:(2)"       | justice_p == "Poor"              ~ "poor",
      justice_p == "3"  | justice_p == "FAIR"        | justice_p == "FAIR:(3)"       | justice_p == "Fair"              ~ "fair",
      justice_p == "4"  | justice_p == "GOOD"        | justice_p == "GOOD:(4)"       | justice_p ==  "Good"             ~ "good",
      justice_p == "5"  | justice_p == "VRY GOOD"    | justice_p == "VRY GOOD:(5)"   | justice_p ==  "Very good"        ~ "very good",
      justice_p == "8"  | justice_p == "NO OPIN:(8)" | justice_p == "No opinion"     | justice_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$justice_p1<-factor(data$justice_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$justice_p1.n=as.numeric(data$justice_p1)
#no opinion coded as NA
data$justice_p1.n5=data$justice_p1.n
data$justice_p1.n5[data$justice_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$justice_p1.no=NA
data$justice_p1.no[data$justice_p1.n==6]=1
data$justice_p1.no[data$justice_p1.n<6&data$justice_p1.n>0]=0
table(data$justice_p1.no, useNA="ifany")
table(data$justice_p1.n, useNA="ifany")
table(data$year, data$justice_p1.n, useNA="ifany")
###military#####
table(data$military_p)
data <- data %>%
  mutate(
    military_p1 = case_when(
      military_p == "1"  | military_p == "VRY POOR"    | military_p == "VRY POOR:(1)"   | military_p == "Very poor"         ~ "very poor",
      military_p == "2"  | military_p == "POOR"        | military_p == "POOR:(2)"       | military_p == "Poor"              ~ "poor",
      military_p == "3"  | military_p == "FAIR"        | military_p == "FAIR:(3)"       | military_p == "Fair"              ~ "fair",
      military_p == "4"  | military_p == "GOOD"        | military_p == "GOOD:(4)"       | military_p ==  "Good"             ~ "good",
      military_p == "5"  | military_p == "VRY GOOD"    | military_p == "VRY GOOD:(5)"   | military_p ==  "Very good"        ~ "very good",
      military_p == "8"  | military_p == "NO OPIN:(8)" | military_p == "No opinion"     | military_p ==  "NO OPINION:(8)"   ~ "no opinion",
      TRUE                                                                                                             ~  NA_character_ ))      
data$military_p1<-factor(data$military_p1,levels=c("very poor","poor","fair","good",
                                         "very good","no opinion"), ordered = FALSE)
data$military_p1.n=as.numeric(data$military_p1)
#no opinion coded as NA
data$military_p1.n5=data$military_p1.n
data$military_p1.n5[data$military_p1.n==6]=NA  #no opinion coded as NA 
#proportion of no-opinion
data$military_p1.no=0
data$military_p1.no[data$military_p1.n==6]=1
table(data$military_p1.n, useNA="ifany")
table(data$year, data$military_p1.n, useNA="ifany")
#######################################################################################

#sex
data <- data %>%
  mutate(
    sex1 = case_when(
      sex == "1" | sex == "MALE"   | sex == "MALE:(1)"   | sex == "Male"     ~ "Men",
      sex == "2" | sex == "FEMALE" | sex == "FEMALE:(2)" | sex == "Female"   ~ "Women",
      TRUE                                              ~  NA_character_ ))
table(data$sex1, data$sex, useNA="ifany")
data$sex1    <- factor(data$sex1,    levels = c("Men","Women"), ordered = FALSE) 
table(data$sex1, useNA="ifany")
table(data$year, data$sex1, useNA="ifany")

#SES (father or mother highest edu)
#mom edu
table(data$m_edu)
data <- data %>%
  mutate(
    m_edu1 = case_when(
      m_edu == "1" | m_edu == "GRDE SCH" | m_edu == "GRDE SCH:(1)" | m_edu == "Completed grade school or less"     ~ "COMPLETED GRADE SCHOOL OR LESS",
      m_edu == "2" | m_edu == "SOME HS"  | m_edu == "SOME HS:(2)"  | m_edu == "Some high school"                   ~ "SOME HIGH SCHOOL",
      m_edu == "3" | m_edu == "HS GRAD"  | m_edu == "HS GRAD:(3)"  | m_edu == "Completed high school"              ~ "COMPLETED HIGH SCHOOL",
      m_edu == "4" | m_edu == "SOME CLG" | m_edu == "SOME CLG:(4)" | m_edu ==  "Some college"                      ~ "SOME COLLEGE",
      m_edu == "5" | m_edu == "CLG GRAD" | m_edu == "CLG GRAD:(5)" | m_edu ==  "Completed college"                 ~ "COMPLETED COLLEGE",
      m_edu == "6" | m_edu == "GRAD SCH" | m_edu == "GRAD SCH:(6)" | m_edu ==  "Graduate or professional school"   ~ "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE",
      m_edu == "7" | m_edu == "MISSING"  | m_edu == "DK:(7)"       | m_edu ==  "Don't know, or does not apply"     ~ "DON'T KNOW, OR DOES NOT APPLY", # These don't match but make missing so doesn't matter
      TRUE                                                                                                             ~  NA_character_ ))      

data$m_edu1[data$m_edu1 == "DON'T KNOW, OR DOES NOT APPLY"]                  <- NA
data$m_edu1<-factor(data$m_edu1,levels=c("COMPLETED GRADE SCHOOL OR LESS","SOME HIGH SCHOOL","COMPLETED HIGH SCHOOL","SOME COLLEGE",
                                         "COMPLETED COLLEGE","GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"), ordered = FALSE)
data$m_edu1.n=as.numeric(data$m_edu1)

#father edu
table(data$f_edu)
data <- data %>%
  mutate(
    f_edu1 = case_when(
      f_edu == "1" | f_edu == "GRDE SCH" | f_edu == "GRDE SCH:(1)" | f_edu == "Completed grade school or less"     ~ "COMPLETED GRADE SCHOOL OR LESS",
      f_edu == "2" | f_edu == "SOME HS"  | f_edu == "SOME HS:(2)"  | f_edu == "Some high school"                   ~ "SOME HIGH SCHOOL",
      f_edu == "3" | f_edu == "HS GRAD"  | f_edu == "HS GRAD:(3)"  | f_edu == "Completed high school"              ~ "COMPLETED HIGH SCHOOL",
      f_edu == "4" | f_edu == "SOME CLG" | f_edu == "SOME CLG:(4)" | f_edu ==  "Some college"                      ~ "SOME COLLEGE",
      f_edu == "5" | f_edu == "CLG GRAD" | f_edu == "CLG GRAD:(5)" | f_edu ==  "Completed college"                 ~ "COMPLETED COLLEGE",
      f_edu == "6" | f_edu == "GRAD SCH" | f_edu == "GRAD SCH:(6)" | f_edu ==  "Graduate or professional school"   ~ "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE",
      f_edu == "7" | f_edu == "MISSING"  | f_edu == "DK:(7)"       | f_edu ==  "Don't know, or does not apply"     ~ "DON'T KNOW, OR DOES NOT APPLY", # These don't match but make missing so doesn't matter
      TRUE                                                                                                             ~  NA_character_ ))      

data$f_edu1[data$f_edu1 == "DON'T KNOW, OR DOES NOT APPLY"]                  <- NA
data$f_edu1<-factor(data$f_edu1,levels=c("COMPLETED GRADE SCHOOL OR LESS","SOME HIGH SCHOOL","COMPLETED HIGH SCHOOL","SOME COLLEGE",
                                         "COMPLETED COLLEGE","GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"), ordered = FALSE)
data$f_edu1.n=as.numeric(data$f_edu1)

#parents average edu

data$m_edu1.n[is.na(data$m_edu1.n)]=999
data$f_edu1.n[is.na(data$f_edu1.n)]=999

data<-data %>%
  mutate(p_edu.n=if_else(m_edu1.n==999&f_edu1.n==999,NA_real_,
                         if_else(f_edu1.n==999, m_edu1.n,
                                 if_else(m_edu1.n==999,f_edu1.n,
                                         if_else(m_edu1.n>f_edu1.n, m_edu1.n,f_edu1.n)
                                         )
                                 )
                         )
         )

table(data$p_edu.n, useNA="ifany")
table(data$p_edu.n, data$m_edu1.n, useNA="ifany")
table(data$p_edu.n, data$m_edu1.n, useNA="ifany")
table(data$year, data$p_edu.n, useNA="ifany")
##background and demographic####################################################
## Race
table(data$year, data$race)
data$race <- as.character(data$race)

#black vs. whtie only
data <- data %>%
  mutate(
    race1 = case_when(
      (year <= 1998 & year != 1993)     & race == 0                            ~ "White",
      (year <= 1998 & year != 1993)     & race == 1                            ~ "Black",
      year == 1993                     & race == "White (Caucasian)"          ~ "White",
      year == 1993                     & race == "Black or African-American"  ~ "Black",
      (year >= 1999 & year <= 2004)     & race == "WHITE"                      ~ "White",
      (year >= 1999 & year <= 2004)     & race == "BLACK"                      ~ "Black",
      (year >= 2005 & year <= 2009)     & race == "WHITE: (2)"                 ~ "White",
      (year >= 2005 & year <= 2009)     & race == "BLACK: (1)"                 ~ "Black",
      (year >= 2010 & year <= 2021)     & race == "WHITE:(2)"                  ~ "White",
      (year >= 2010 & year <= 2021)     & race == "BLACK:(1)"                  ~ "Black",      
      TRUE                                                                     ~  NA_character_ ))
table(data$race1, data$race)
data$race.f    <- factor(data$race1,    levels = c("Black","White"), ordered = FALSE) 

#race: black, white and other (missing&hispanics are coded as others)
data <- data %>%
  mutate(
    race4 = case_when(
      (year <= 1998 & year != 1993)     & race == 0                            ~ "White",
      (year <= 1998 & year != 1993)     & race == 1                            ~ "Black",
      year == 1993                     & race == "White (Caucasian)"          ~ "White",
      year == 1993                     & race == "Black or African-American"  ~ "Black",
      (year >= 1999 & year <= 2004)     & race == "WHITE"                      ~ "White",
      (year >= 1999 & year <= 2004)     & race == "BLACK"                      ~ "Black",
      (year >= 2005 & year <= 2009)     & race == "WHITE: (2)"                 ~ "White",
      (year >= 2005 & year <= 2009)     & race == "BLACK: (1)"                 ~ "Black",
      (year >= 2010 & year <= 2021)     & race == "WHITE:(2)"                  ~ "White",
      (year >= 2010 & year <= 2021)     & race == "BLACK:(1)"                  ~ "Black", 
      (year >= 2005 & year <= 2009)     & race == "HISPANIC: (3)"              ~ "Hispanic",
      (year >= 2010)                    & race == "HISPANIC:(3)"               ~ "Hispanic",
      TRUE                                                                     ~ "Other" ))
data$race4.f    <- factor(data$race4,    levels = c("White","Black","Hispanic","Other"), ordered = FALSE) 


#######################################################################################################
#rural urban
table(data$year, data$rural)
data$rural=as.character(data$rural)

data <- data %>%
  mutate(
    rural1 = case_when(
      rural == "1" | rural == "2" | rural == "FARM" | rural == "COUNTRY" | rural == "FARM:(1)" | rural == "COUNTRY:(2)" | 
        rural == "On a farm"| rural == "In the country, not on a farm" | rural == "A FARM:(1)"                              ~ "Rural",
      rural == "3" | rural == "SML TOWN:(3)" | rural == "In a small city or town" | rural == "SM CITY:(3)" |
        rural == "4" | rural == "MED CITY:(4)" | rural == "In a medium-sized city (50,000-100,000)" | rural == "3" | 
        rural == "5" | rural == "SUBURB 4:(5)" | rural == "In a suburb of a medium-sized city" | rural == "SUB MED:(5)" | 
        rural == "6" | rural == "LRG CITY:(6)" | rural == "In a large city (100,000 - 500,000)" | rural == "LGE CITY:(6)" | 
        rural == "7" | rural == "SUBURB 6:(7)" | rural == "In a suburb of a large city" | rural == "SUB LGE:(7)" | 
        rural == "8" | rural == "VRYLG CY:(8)" | rural == "In a very large city (over 500,000)" | rural == "V-LGE CITY:(8)" | 
        rural == "9" | rural == "SUBURB 8:(9)" | rural == "In a suburb of a very large city" | rural == "SUB V-LGE:(9)" | 
        rural == "SML TOWN" | rural == "MED CITY" |rural == "SUBURB 4" |rural == "LRG CITY" |rural == "SUBURB 6" |rural == "VRYLG CY" |rural == "SUBURB 8" |
        rural == "CITYTOWN" | rural == "CITYTOWN:(3)"                                                                      ~ "Urban",
      #TRUE                                              ~  NA_character_,
      TRUE                                              ~  NA_character_))
data$rural1    <- factor(data$rural1,    levels = c("Rural","Urban"), ordered = FALSE) 
table(data$rural1,data$rural, useNA="ifany")


#rural urban 5 groups
table(data$year, data$rural)
data$rural=as.character(data$rural)

data <- data %>%
  mutate(
    rural5 = case_when(
      rural == "1" | rural == "2" | rural == "FARM" | rural == "COUNTRY" | rural == "FARM:(1)" | rural == "COUNTRY:(2)" | 
        rural == "On a farm"| rural == "In the country, not on a farm" | rural == "A FARM:(1)"    ~ "rural",
      
      rural == "3" | rural == "SML TOWN:(3)" | rural == "In a small city or town" | rural == "SM CITY:(3)"| 
        rural == "3"|rural == "SML TOWN"|rural == "CITYTOWN" | rural == "CITYTOWN:(3)"  ~"small city",
      
      rural == "4" | rural == "MED CITY:(4)" | rural == "In a medium-sized city (50,000-100,000)"  | 
        rural == "MED CITY" |rural == "SUBURB 4" |rural == "5" | rural == "SUBURB 4:(5)" | 
        rural == "In a suburb of a medium-sized city" | rural == "SUB MED:(5)" ~ "medium city (50,000-100,000)", 
      
      
      rural == "6" | rural == "LRG CITY:(6)" | rural == "In a large city (100,000 - 500,000)" | rural == "LGE CITY:(6)" | 
        rural == "LRG CITY" |rural == "SUBURB 6" |
        rural == "7" | rural == "SUBURB 6:(7)" | rural == "In a suburb of a large city" | rural == "SUB LGE:(7)"       ~"large city (100,000-500,000)",
      
      rural == "8" | rural == "VRYLG CY:(8)" | rural == "In a very large city (over 500,000)" | rural == "V-LGE CITY:(8)" | 
        rural == "VRYLG CY" |rural == "SUBURB 8" |rural == "9" | rural == "SUBURB 8:(9)" | rural == "In a suburb of a very large city" 
      | rural == "SUB V-LGE:(9)"   ~ "very large city (over 500,000)",
      TRUE                                              ~  NA_character_ ))
data$rural5    <- factor(data$rural5,    levels = c("rural","small city","medium city (50,000-100,000)",
                                                    "large city (100,000-500,000)","very large city (over 500,000)"), ordered = FALSE) 
table(data$rural5,data$rural, useNA="ifany")


#rural urban 3 groups
table(data$year, data$rural)
data$rural=as.character(data$rural)

data <- data %>%
  mutate(
    rural3 = case_when(
      rural == "1" | rural == "2" | rural == "FARM" | rural == "COUNTRY" | rural == "FARM:(1)" | rural == "COUNTRY:(2)" | 
        rural == "On a farm"| rural == "In the country, not on a farm" | rural == "A FARM:(1)"    ~ "rural",
      
      rural == "3" | rural == "SML TOWN:(3)" | rural == "In a small city or town" | rural == "SM CITY:(3)"| 
        rural == "3"|rural == "SML TOWN"|rural == "CITYTOWN" | rural == "CITYTOWN:(3)"  |
        
        rural == "4" | rural == "MED CITY:(4)" | rural == "In a medium-sized city (50,000-100,000)"  | 
        rural == "MED CITY" |rural == "SUBURB 4" |rural == "5" | rural == "SUBURB 4:(5)" | 
        rural == "In a suburb of a medium-sized city" | rural == "SUB MED:(5)" |
        
        
        rural == "6" | rural == "LRG CITY:(6)" | rural == "In a large city (100,000 - 500,000)" | rural == "LGE CITY:(6)" | 
        rural == "LRG CITY" |rural == "SUBURB 6" |
        rural == "7" | rural == "SUBURB 6:(7)" | rural == "In a suburb of a large city" | rural == "SUB LGE:(7)"       ~"small, medium, or large city",
      
      rural == "8" | rural == "VRYLG CY:(8)" | rural == "In a very large city (over 500,000)" | rural == "V-LGE CITY:(8)" | 
        rural == "VRYLG CY" |rural == "SUBURB 8" |rural == "9" | rural == "SUBURB 8:(9)" | rural == "In a suburb of a very large city" 
      | rural == "SUB V-LGE:(9)"                                           ~ "very large city (over 500,000)",
      #TRUE                                              ~  NA_character_ ))
      TRUE                                              ~"small, medium, or large city" ##assume missing is urban 
      ))

data$rural3    <- factor(data$rural3,    levels = c("rural","small, medium, or large city","very large city (over 500,000)"), ordered = FALSE) 
table(data$rural3,data$rural, useNA="ifany")

table(data$year, data$rural3, useNA="ifany")


###############################################################################
### political ideology
#table(data$V3167)
#data <- dplyr::rename(data,  ideo= V3167) 
data <- data %>%
  mutate(
    ideo1 = case_when(
      ideo == "1"  | ideo == "VRY CONS"       | ideo == "VRY CONS:(1)"   | ideo == "Very conservative"         ~ "very cons",
      ideo == "2"  | ideo == "CONSERV"        | ideo == "CONSERV:(2)"       | ideo == "Conservative"              ~ "cons",
      ideo == "3"  | ideo == "MODERATE"       | ideo == "MODERATE:(3)"       | ideo == "Moderate"              ~ "moderate",
      ideo == "4"  | ideo == "LIBERAL"        | ideo == "LIBERAL:(4)"       | ideo ==  "Liberal"             ~ "liberal",
      ideo == "5"  | ideo == "VRY LIB"        | ideo == "VRY LIB:(5)"   | ideo ==  "Very liberal"        ~ "very liberal",
      ideo == "6"  | ideo == "RADICAL"        | ideo == "RADICAL:(6)"     | ideo ==  "Radical"          ~ "radical",
      ideo == "8"  | ideo == "NONE/DK:(8)"    | ideo == "None of the above, or don't know"              ~ "don't know",
      TRUE                                                                                                             ~  NA_character_ ))      

data$ideo1<-factor(data$ideo1,levels=c("very cons","cons","moderate","liberal",
                                         "very liberal","radical", "don't know"), ordered = FALSE)
table(data$ideo, data$ideo1, useNA="ifany")
data$ideo1.n=as.numeric(data$ideo1)

table(data$year, data$ideo1.n, useNA="ifany")


#alternative way of coding
data <- data %>%
  mutate(
    ideo2 = case_when(
      ideo == "1"  | ideo == "VRY CONS"       | ideo == "VRY CONS:(1)"   | ideo == "Very conservative"         ~ "cons",
      ideo == "2"  | ideo == "CONSERV"        | ideo == "CONSERV:(2)"       | ideo == "Conservative"              ~ "cons",
      ideo == "3"  | ideo == "MODERATE"       | ideo == "MODERATE:(3)"       | ideo == "Moderate"              ~ "moderate",
      ideo == "4"  | ideo == "LIBERAL"        | ideo == "LIBERAL:(4)"       | ideo ==  "Liberal"             ~ "lib",
      ideo == "5"  | ideo == "VRY LIB"        | ideo == "VRY LIB:(5)"   | ideo ==  "Very liberal"        ~ "lib",
      ideo == "6"  | ideo == "RADICAL"        | ideo == "RADICAL:(6)"     | ideo ==  "Radical"          ~ "radical",
      ideo == "8"  | ideo == "NONE/DK:(8)"    | ideo == "None of the above, or don't know"              ~ "don't know",
      TRUE                                                                                                             ~  "don't know"))      

data$ideo2<-factor(data$ideo2,levels=c("cons","moderate","lib",
                                       "radical", "don't know"), ordered = FALSE)
table(data$ideo, data$ideo2, useNA="ifany")
data$ideo2.n=as.numeric(data$ideo2)

table(data$year, data$ideo2.n, useNA="ifany")



#fd who smoked majuriana (No informationi between 1976-1989)
table(data$fd_mj)
data$fd_mj <- as.character(data$fd_mj)
data <- data %>%
  mutate(
    fd_mj1 = case_when(
      fd_mj == "1"  | fd_mj == "NONE"    | fd_mj == "NONE:(1)"      | fd_mj == "None"         ~ "None",
      fd_mj == "2"  | fd_mj == "A FEW"  | fd_mj == "A FEW:(2)"      | fd_mj == "A few"              ~ "A few",
      fd_mj == "3"  | fd_mj == "SOME"   | fd_mj == "SOME:(3)"       | fd_mj == "Some"              ~ "Some",
      fd_mj == "4"  | fd_mj == "MOST"   | fd_mj == "MOST:(4)"       | fd_mj ==  "Most"             ~ "Most",
      fd_mj == "5"  | fd_mj == "ALL"    | fd_mj == "ALL:(5)"        | fd_mj ==  "All"        ~ "All",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$fd_mj, data$fd_mj1, useNA="ifany")
data$fd_mj1<-factor(data$fd_mj1,levels=c("None","A few","Some","Most","All"), ordered = FALSE)
data$fd_mj1.n=as.numeric(data$fd_mj1)
table(data$fd_mj1, useNA="ifany")

#commitment to school
table(data$sch_c)
data$sch_c <- as.character(data$sch_c)
data <- data %>%
  mutate(
    sch_c1 = case_when(
      sch_c == "1"  | sch_c == "NONE"    | sch_c == "NONE:(1)"      | sch_c == "None"         ~ "None",
      sch_c == "2"  | sch_c == "1 DAY"  | sch_c == "1 DAY:(2)"      | sch_c == "1 day"              ~ "1day",
      sch_c == "3"  | sch_c == "2 DAYS"   | sch_c == "2 DAYS:(3)"       | sch_c == "2 days"              ~ "2days",
      sch_c == "4"  | sch_c == "3 DAYS"   | sch_c == "3 DAYS:(4)"       | sch_c ==  "3 days"             ~ "3days",
      sch_c == "5"  | sch_c == "4-5 DAYS"    | sch_c == "4-5 DAYS:(5)"        | sch_c ==  "4-5 days"        ~ "4-5days",
      sch_c == "6"  | sch_c == "6-10 DA"    | sch_c == "6-10 DA:(6)"        | sch_c ==  "6-10 days"        ~ "6-10days",
      sch_c == "7"  | sch_c == "11+ DAYS"    | sch_c == "11+ DAYS:(7)"        | sch_c ==  "11 or more"        ~ "11ormore",
      #TRUE                                                                                                             ~  NA_character_ 
      TRUE                                                                                                             ~  "None" #1/31/: missing coded as none 
    ))      
table(data$sch_c, data$sch_c1, useNA="ifany")
table(data$year, data$sch_c1, useNA="ifany")
data$sch_c1<-factor(data$sch_c1,levels=c("None","1day","2days","3days","4-5days","6-10days","11ormore"), ordered = FALSE)
data$sch_c1.n=as.numeric(data$sch_c1)
table(data$sch_c1, useNA="ifany")


#commitment to class
table(data$class_c)
data$class_c <- as.character(data$class_c)
data <- data %>%
  mutate(
    class_c1 = case_when(
      class_c == "1"  | class_c == "NONE"    | class_c == "NONE:(1)"      | class_c == "Not at all"         ~ "None",
      class_c == "2"  | class_c == "1-2"  | class_c == "1-2:(2)"      | class_c == "1 or 2 times"              ~ "1-2times",
      class_c == "3"  | class_c == "3-5"   | class_c == "3-5:(3)"       | class_c == "3-5 times"              ~ "3-5times",
      class_c == "4"  | class_c == "6-10"   | class_c == "6-10:(4)"       | class_c ==  "6-10 times"             ~ "6-10times",
      class_c == "5"  | class_c == "11-20"    | class_c == "11-20:(5)"        | class_c ==  "11-20 times"        ~ "11-20times",
      class_c == "6"  | class_c == "21+"    | class_c == "21+:(6)"        | class_c ==  "More than 20 times"        ~ "21+",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$class_c, data$class_c1, useNA="ifany")
table(data$year, data$class_c1, useNA="ifany")
data$class_c1<-factor(data$class_c1,levels=c("None","1-2times","3-5times","6-10times","11-20times","21+"), ordered = FALSE)
data$class_c1.n=as.numeric(data$class_c1)
table(data$class_c1, useNA="ifany")

#alcohol use and police experience:pol_al (2016-2021 missing, )
#this variable is V3347 instead of V3351 in 1976
table(data$pol_al)
data$pol_al <- as.character(data$pol_al)
data <- data %>%
  mutate(
    pol_al1 = case_when(
      pol_al == "0"  | pol_al == "NT MRKED"    | pol_al == "NT MRKED:(0)"      | pol_al == "No"| pol_al == "NT MARKD:(0)"         ~ "No",
      pol_al == "1"  | pol_al == "MARKED"  | pol_al == "MARKED:(1)"      | pol_al == "Yes"              ~ "Yes",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$pol_al, data$pol_al1, useNA="ifany")
table(data$year, data$pol_al1, useNA="ifany")
data$pol_al1<-factor(data$pol_al1,levels=c("No","Yes"), ordered = FALSE)
data$pol_al1.n=as.numeric(data$pol_al1)
table(data$pol_al1, useNA="ifany")


#marijuana use and police experience:pol_al (2016-2021 missing, )
#this variable is V3362 instead of V3351 in 1976
table(data$pol_mj)
data$pol_mj <- as.character(data$pol_mj)
data <- data %>%
  mutate(
    pol_mj1 = case_when(
      pol_mj == "0"  | pol_mj == "NT MRKED"    | pol_mj == "NT MRKED:(0)"      | pol_mj == "No"| pol_mj == "NT MARKD:(0)"         ~ "No",
      pol_mj == "1"  | pol_mj == "MARKED"  | pol_mj == "MARKED:(1)"      | pol_mj == "Yes"              ~ "Yes",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$pol_mj, data$pol_mj1, useNA="ifany")
table(data$year, data$pol_mj1, useNA="ifany")
data$pol_mj1<-factor(data$pol_mj1,levels=c("No","Yes"), ordered = FALSE)
data$pol_mj1.n=as.numeric(data$pol_mj1)
table(data$pol_mj1, useNA="ifany")

#alcohol use and police experience:pol_al (2016-2021 missing, )
#this variable is V3347 instead of V3351 in 1976
table(data$pol_drug)
data$pol_drug <- as.character(data$pol_drug)
data <- data %>%
  mutate(
    pol_drug1 = case_when(
      pol_drug == "0"  | pol_drug == "NT MRKED"    | pol_drug == "NT MRKED:(0)"      | pol_drug == "No"| pol_drug == "NT MARKD:(0)"         ~ "No",
      pol_drug == "1"  | pol_drug == "MARKED"  | pol_drug == "MARKED:(1)"      | pol_drug == "Yes"              ~ "Yes",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$pol_drug, data$pol_drug1, useNA="ifany")
table(data$year, data$pol_drug1, useNA="ifany")
data$pol_drug1<-factor(data$pol_drug1,levels=c("No","Yes"), ordered = FALSE)
data$pol_drug1.n=as.numeric(data$pol_drug1)
table(data$pol_drug1, useNA="ifany")

#mj use and police experience
#stop using mj because of concerned about getting arrested
table(data$nm_ar)
data$nm_ar <- as.character(data$nm_ar)
data <- data %>%
  mutate(
    nm_ar1 = case_when(
      nm_ar == "0"  | nm_ar == "NT MRKED"    | nm_ar == "NT MRKED:(0)"      | nm_ar == "No"| nm_ar == "NT MARKD:(0)"|
        nm_ar == "NT MRKD" | nm_ar== "NT MRKD:(0)"     ~ "No",
      nm_ar == "1"  | nm_ar == "MARKED"  | nm_ar == "MARKED:(1)"      | nm_ar == "Yes"              ~ "Yes",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$nm_ar, data$nm_ar1, useNA="ifany")
table(data$year, data$nm_ar1, useNA="ifany")
data$nm_ar1<-factor(data$nm_ar1,levels=c("No","Yes"), ordered = FALSE)
data$nm_ar1.n=as.numeric(data$nm_ar1)
table(data$nm_ar1, useNA="ifany")

#mj use caused regrated problems
#no data after 2016
data$nm_re=data$V3354
table(data$nm_re)
data$nm_re <- as.character(data$nm_re)
data <- data %>%
  mutate(
    nm_re1 = case_when(
      nm_re == "0"  | nm_re == "NT MRKED"    | nm_re == "NT MRKED:(0)"      | nm_re == "No"| nm_re == "NT MARKD:(0)"|
        nm_re == "NT MRKD" | nm_re== "NT MRKD:(0)"     ~ "No",
      nm_re == "1"  | nm_re == "MARKED"  | nm_re == "MARKED:(1)"      | nm_re == "Yes"              ~ "Yes",
      TRUE                                                                                                             ~  NA_character_ ))      
table(data$nm_re, data$nm_re1, useNA="ifany")
table(data$year, data$nm_re1, useNA="ifany")
data$nm_re1<-factor(data$nm_re1,levels=c("No","Yes"), ordered = FALSE)
data$nm_re1.n=as.numeric(data$nm_re1)
table(data$nm_re1, useNA="ifany")


#victimization experiences
#robbed more than $50
data$vic_rob50l=data$V3407
table(data$year, data$vic_rob50l)
table(data$vic_rob50l)
data <- data %>%
  mutate(
    vic_rob50l1 = case_when(
      vic_rob50l == "1" | vic_rob50l == "Not at all"      | vic_rob50l == "NOT @ALL" | vic_rob50l == "NOT @ALL:(1)" | vic_rob50l == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_rob50l == "2" | vic_rob50l == "Once"            | vic_rob50l == "ONCE"     | vic_rob50l == "ONCE:(2)"                                       ~ "Once",
      vic_rob50l == "3" | vic_rob50l == "Twice"           | vic_rob50l == "TWICE"    | vic_rob50l == "TWICE:(3)"                                      ~ "Twice",
      vic_rob50l == "4" | vic_rob50l == "3 or 4 times"    | vic_rob50l == "3-4TIMES" | vic_rob50l == "3-4TIMES:(4)" | vic_rob50l == "3-4X:(4)"| vic_rob50l == "3-4X"                                 ~ "3-4times",
      vic_rob50l == "5" | vic_rob50l == "5 or more times" | vic_rob50l == "5+ TIMES" | vic_rob50l == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_rob50l1, data$vic_rob50l, useNA="ifany")
table(data$year, data$vic_rob50l1, useNA="ifany")
data$vic_rob50l1<-
  factor(data$vic_rob50l1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_rob50l1.n=as.numeric(data$vic_rob50l1)
table(data$vic_rob50l1.n, useNA="ifany")

#robbed more than $50
data$vic_rob50m=data$V3408
table(data$year, data$vic_rob50m)
table(data$vic_rob50m)
data <- data %>%
  mutate(
    vic_rob50m1 = case_when(
      vic_rob50m == "1" | vic_rob50m == "Not at all"      | vic_rob50m == "NOT @ALL" | vic_rob50m == "NOT @ALL:(1)" | vic_rob50m == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_rob50m == "2" | vic_rob50m == "Once"            | vic_rob50m == "ONCE"     | vic_rob50m == "ONCE:(2)"                                       ~ "Once",
      vic_rob50m == "3" | vic_rob50m == "Twice"           | vic_rob50m == "TWICE"    | vic_rob50m == "TWICE:(3)"                                      ~ "Twice",
      vic_rob50m == "4" | vic_rob50m == "3 or 4 times"    | vic_rob50m == "3-4TIMES" | vic_rob50m == "3-4TIMES:(4)" | vic_rob50m == "3-4X:(4)"| vic_rob50m == "3-4X"                                 ~ "3-4times",
      vic_rob50m == "5" | vic_rob50m == "5 or more times" | vic_rob50m == "5+ TIMES" | vic_rob50m == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_rob50m1, data$vic_rob50m, useNA="ifany")
table(data$year, data$vic_rob50m1, useNA="ifany")
data$vic_rob50m1<-
  factor(data$vic_rob50m1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_rob50m1.n=as.numeric(data$vic_rob50m1)
table(data$vic_rob50m1.n, useNA="ifany")

#property damage
data$vic_pd=data$V3409
table(data$year, data$vic_pd)
table(data$vic_pd)
data <- data %>%
  mutate(
    vic_pd1 = case_when(
      vic_pd == "1" | vic_pd == "Not at all"      | vic_pd == "NOT @ALL" | vic_pd == "NOT @ALL:(1)" | vic_pd == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_pd == "2" | vic_pd == "Once"            | vic_pd == "ONCE"     | vic_pd == "ONCE:(2)"                                       ~ "Once",
      vic_pd == "3" | vic_pd == "Twice"           | vic_pd == "TWICE"    | vic_pd == "TWICE:(3)"                                      ~ "Twice",
      vic_pd == "4" | vic_pd == "3 or 4 times"    | vic_pd == "3-4TIMES" | vic_pd == "3-4TIMES:(4)" | vic_pd == "3-4X:(4)"| vic_pd == "3-4X"                                 ~ "3-4times",
      vic_pd == "5" | vic_pd == "5 or more times" | vic_pd == "5+ TIMES" | vic_pd == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_pd1, data$vic_pd, useNA="ifany")
table(data$year, data$vic_pd1, useNA="ifany")
data$vic_pd1<-
  factor(data$vic_pd1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_pd1.n=as.numeric(data$vic_pd1)
table(data$vic_pd1.n, useNA="ifany")


#Victimized with a weapon
data$vic_iw=data$V3410
#1979-1981--> only "0" now coded as NA
data <- data %>%
  mutate(
    vic_iw1 = case_when(
      vic_iw == "1" | vic_iw == "Not at all"      | vic_iw == "NOT @ALL" | vic_iw == "NOT @ALL:(1)" | vic_iw == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_iw == "2" | vic_iw == "Once"            | vic_iw == "ONCE"     | vic_iw == "ONCE:(2)"                                       ~ "Once",
      vic_iw == "3" | vic_iw == "Twice"           | vic_iw == "TWICE"    | vic_iw == "TWICE:(3)"                                      ~ "Twice",
      vic_iw == "4" | vic_iw == "3 or 4 times"    | vic_iw == "3-4TIMES" | vic_iw == "3-4TIMES:(4)" | vic_iw == "3-4X:(4)"| vic_iw == "3-4X"                                 ~ "3-4times",
      vic_iw == "5" | vic_iw == "5 or more times" | vic_iw == "5+ TIMES" | vic_iw == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
)
table(data$vic_iw1, data$vic_iw, useNA="ifany")
table(data$year, data$vic_iw1, useNA="ifany")
data$vic_iw1<-
  factor(data$vic_iw1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_iw1.n=as.numeric(data$vic_iw1)
table(data$vic_iw1.n, useNA="ifany")

#Threaten with a weapon
data$vic_tw=data$V3411
#1979-1981--> only "0" now coded as NA
data <- data %>%
  mutate(
    vic_tw1 = case_when(
      vic_tw == "1" | vic_tw == "Not at all"      | vic_tw == "NOT @ALL" | vic_tw == "NOT @ALL:(1)" | vic_tw == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_tw == "2" | vic_tw == "Once"            | vic_tw == "ONCE"     | vic_tw == "ONCE:(2)"                                       ~ "Once",
      vic_tw == "3" | vic_tw == "Twice"           | vic_tw == "TWICE"    | vic_tw == "TWICE:(3)"                                      ~ "Twice",
      vic_tw == "4" | vic_tw == "3 or 4 times"    | vic_tw == "3-4TIMES" | vic_tw == "3-4TIMES:(4)" | vic_tw == "3-4X:(4)"| vic_tw == "3-4X"                                 ~ "3-4times",
      vic_tw == "5" | vic_tw == "5 or more times" | vic_tw == "5+ TIMES" | vic_tw == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_tw1, data$vic_tw, useNA="ifany")
table(data$year, data$vic_tw1, useNA="ifany")
data$vic_tw1<-
  factor(data$vic_tw1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_tw1.n=as.numeric(data$vic_tw1)
table(data$vic_tw1.n, useNA="ifany")


#injured without a weapon
data$vic_in=data$V3412   
#1979-1981--> only "0" now coded as NA
data <- data %>%
  mutate(
    vic_in1 = case_when(
      vic_in == "1" | vic_in == "Not at all"      | vic_in == "NOT @ALL" | vic_in == "NOT @ALL:(1)" | vic_in == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_in == "2" | vic_in == "Once"            | vic_in == "ONCE"     | vic_in == "ONCE:(2)"                                       ~ "Once",
      vic_in == "3" | vic_in == "Twice"           | vic_in == "TWICE"    | vic_in == "TWICE:(3)"                                      ~ "Twice",
      vic_in == "4" | vic_in == "3 or 4 times"    | vic_in == "3-4TIMES" | vic_in == "3-4TIMES:(4)" | vic_in == "3-4X:(4)"| vic_in == "3-4X"                                 ~ "3-4times",
      vic_in == "5" | vic_in == "5 or more times" | vic_in == "5+ TIMES" | vic_in == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_in1, data$vic_in, useNA="ifany")
table(data$year, data$vic_in1, useNA="ifany")
data$vic_in1<-
  factor(data$vic_in1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_in1.n=as.numeric(data$vic_in1)
table(data$vic_in1.n, useNA="ifany")

#threaten without a weapon
data$vic_two=data$V3413   
#1979-1981--> only "0" now coded as NA
data <- data %>%
  mutate(
    vic_two1 = case_when(
      vic_two == "1" | vic_two == "Not at all"      | vic_two == "NOT @ALL" | vic_two == "NOT @ALL:(1)" | vic_two == "NOT @ ALL:(1)"      ~ "Not at all",
      vic_two == "2" | vic_two == "Once"            | vic_two == "ONCE"     | vic_two == "ONCE:(2)"                                       ~ "Once",
      vic_two == "3" | vic_two == "Twice"           | vic_two == "TWICE"    | vic_two == "TWICE:(3)"                                      ~ "Twice",
      vic_two == "4" | vic_two == "3 or 4 times"    | vic_two == "3-4TIMES" | vic_two == "3-4TIMES:(4)" | vic_two == "3-4X:(4)"| vic_two == "3-4X"                                 ~ "3-4times",
      vic_two == "5" | vic_two == "5 or more times" | vic_two == "5+ TIMES" | vic_two == "5+ TIMES:(5)"                                   ~ "5+times",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$vic_two1, data$vic_two, useNA="ifany")
table(data$year, data$vic_two1, useNA="ifany")
data$vic_two1<-
  factor(data$vic_two1,levels=c("Not at all","Once","Twice","3-4times","5+times"), ordered = FALSE)
data$vic_two1.n=as.numeric(data$vic_two1)
table(data$vic_two1.n, useNA="ifany")

#victimization prevalence: any victimization
data <- data %>%
  mutate(
    any.p = case_when(
      is.na(vic_rob50l1) & is.na(vic_rob50m1) & is.na(vic_pd1) & is.na(vic_iw1) & is.na(vic_tw1) & is.na(vic_in1) & is.na(vic_two1)           ~ "No", #1/31: missing coded as NO
      vic_rob50l1 == "Not at all"& vic_rob50m1 == "Not at all"& vic_pd1 == "Not at all" & vic_iw1 == "Not at all"&
        vic_tw1 == "Not at all"&vic_in1 == "Not at all"& vic_two1 == "Not at all"                                                               ~"No",
      TRUE                                                                                                                                      ~"Yes"), #any victimization
    pro.p = case_when(
      is.na(vic_rob50l1) & is.na(vic_rob50m1) & is.na(vic_pd1)                                                                                 ~ NA_character_,
      vic_rob50l1 == "Not at all"& vic_rob50m1 == "Not at all"& vic_pd1 == "Not at all"                                                        ~"No",
      TRUE                                                                                                                                      ~"Yes"), #pro victimization
    vio.p = case_when(
      is.na(vic_iw1) & is.na(vic_tw1) & is.na(vic_in1) & is.na(vic_two1)                                                                        ~ NA_character_,
      vic_iw1 == "Not at all"& vic_tw1 == "Not at all"&vic_in1 == "Not at all"& vic_two1 == "Not at all"                                        ~"No",
      TRUE                                                                                                                                      ~"Yes"), #vio victimization
  )


#recode region#
table(data$region)
data <- data %>%
  mutate(
    region1 = case_when(
      region == "1" | region == "NE" | region == "NE:(1)"      | region == "NORTHEAST"       | region == "NORTHEAST:(1)"      ~ "Northeast",
      region == "2" | region == "NC" | region == "NC:(2)"      | region == "NORTH CENTRAL"   | region == "NORTH CENTRL:(2)"| region == "NORTH CENTRAL:(2)" |   region == "MIDWEST:(2)"    ~ "Northcentral",
      region == "3" | region == "SOUTH" | region == "S:(3)"    | region == "S"      | region == "SOUTH:(3)"      ~ "South",
      region == "4" | region == "WEST" | region == "W:(4)"     | region == "W"      | region == "WEST:(4)"        ~ "West",
      TRUE                                                                                                                                ~  NA_character_ ),
  )

#recode feeling safe at school & going to school
table(data$us_tosch, data$year, useNA="ifany")
data <- data %>%
  mutate(
    us_tosch1 = case_when(
      us_tosch == "1" | us_tosch == "NEVER:(1)" | us_tosch == "NEVER"      | us_tosch == "Never"           ~ "Never",
      us_tosch == "2" | us_tosch == "RARELY:(2)" | us_tosch == "RARELY"      | us_tosch == "Rarely"       ~ "Rarely",
      us_tosch == "3" | us_tosch == "SOMEDAYS:(3)" | us_tosch == "SOMEDAYS" | us_tosch == "Some days"          ~ "SOMEDAYS",
      us_tosch == "4" | us_tosch == "MOSTDAYS:(4)" | us_tosch == "MOSTDAYS"  | us_tosch == "Most days"        ~ "MOSTDAYS",
      us_tosch == "5" | us_tosch == "EVERYDAY:(5)" | us_tosch == "EVERYDAY"  | us_tosch == "Every day"        ~ "EVERYDAY",
      TRUE                                                                                                                                ~  NA_character_ ),
  )

table(data$us_tosch, data$us_tosch1, useNA="ifany")
table(data$year, data$us_tosch1, useNA="ifany")
data$us_tosch1<-
  factor(data$us_tosch1,levels=c("Never","Rarely","SOMEDAYS","MOSTDAYS","EVERYDAY"), ordered = FALSE)
data$us_tosch1.n=as.numeric(data$us_tosch1)
table(data$us_tosch1.n, useNA="ifany")

#us_tosch  =V3485,    us_atsch= V3484

#R's happiness: V3208, happy
table(data$happy, data$year, useNA="ifany")
data <- data %>%
  mutate(
    happy1 = case_when(
      happy == "1" | happy == "NT HAPPY:(1)" | happy == "NT HAPPY"      | happy == "Not too happy"           ~ "Not too happy",
      happy == "2" | happy == "PRTY HPY:(2)" | happy == "PRTY HPY"      | happy == "Pretty happy"       ~ "Pretty happy",
      happy == "3" | happy == "VRY HPY:(3)"  | happy == "VRY HPY"       | happy == "Very happy"          ~ "Very happy",
      TRUE                                                                                                                                ~  NA_character_ ),
  )
table(data$happy, data$happy1, useNA="ifany")
table(data$year, data$happy1, useNA="ifany")
data$happy1<-
  factor(data$happy1,levels=c("Not too happy","Pretty happy","Very happy"), ordered = FALSE)
data$happy1.n=as.numeric(data$happy1)
table(data$happy1.n, useNA="ifany")


#R's care about social problems: V3209, care_soc
table(data$care_soc, data$year, useNA="ifany")
data <- data %>%
  mutate(
    care_soc1 = case_when(
      care_soc == "1" | care_soc == "NEVER:(1)" | care_soc == "NEVER"      | care_soc == "Never"           ~ "Never",
      care_soc == "2" | care_soc == "SELDOM:(2)" | care_soc == "SELDOM"      | care_soc == "Seldom"       ~ "Seldom",
      care_soc == "3" | care_soc == "SOMETIME:(3)" | care_soc == "SOMETIME" | care_soc == "Sometimes"          ~ "Sometimes",
      care_soc == "4" | care_soc == "OFTEN:(4)" | care_soc == "OFTEN"  | care_soc == "Quite often"  | care_soc == "QUITE OFTN:(4)"      ~ "Quite often",
      care_soc == "5" | care_soc == "GRT DEAL:(5)" | care_soc == "GRT DEAL"  | care_soc == "A great deal"| care_soc == "GREAT DEAL:(5)"        ~ "A great deal",
      TRUE                                                                                                                                ~  NA_character_ ),
  )

table(data$care_soc, data$care_soc1, useNA="ifany")
table(data$year, data$care_soc1, useNA="ifany")
data$care_soc1<-
  factor(data$care_soc1,levels=c("Never","Seldom","Sometimes","Quite often","A great deal"), ordered = FALSE)
data$care_soc1.n=as.numeric(data$care_soc1)
table(data$care_soc1.n, useNA="ifany")



write.dta(data, file=file.path(dataDir, "mtf_form3_12th_all.dta"))


