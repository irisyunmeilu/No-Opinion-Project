#-------------------------------------------------------------------------------
# No Opinion Project
# Yunmei Lu 
#-------------------------------------------------------------------------------

# PACKAGES ---------------------------------------------------------------------

## WARNING: Remove the leading # to install packages below the first time. 
## Change filepaths below once

# if (!require(remotes)) install.packages("remotes") 
# if (!require(devtools)) install.packages("devtools") 
# remotes::install_github("fsolt/icpsrdata")                      # download ICPSR data
# devtools::install_git("https://git.rud.is/hrbrmstr/waffle.git") # install waffle package not on CRAN
# install.packages("pacman")                                      # Install pacman package if not installed

# Installs and loads packages automatically
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  here,       # relative file paths
  foreign,    # read data
  plyr,       #
  dplyr,      # variable processing
  tidyr,      # reshaping data
  forcats,    # reverse factor variables
  srvyr,      # calc % with survey weights
  purrr,      # to use modify_at
  MESS,       # round prop & preserve sum to 100%
  data.table, #
  gtsummary,  # pretty weighted tables
  ggplot2,    # graphing
  colorspace, # color palettes   
  conflicted) # choose default packages


# https://cran.r-project.org/web/packages/icpsrdata/vignettes/icpsrdata-vignette.html
library("icpsrdata")

# Address any conflicts in the packages
conflict_scout() # identify the conflicts
conflict_prefer("here", "here")
conflict_prefer("mutate", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("arrange", "dplyr")

# Set-up the Directories -------------------------------------------------------

## Set the project directory to the current working directory.
projDir <- here::here()  # File path to this project's directory
dataDir1<-file.path(projDir,"Data/12th")    # File path to where data will be downloaded
outDir  <- "output"                                         # Name of the sub-folder where we will save results
figDir  <- file.path(outDir, "figs")                        # Name of the sub-folder where we will save generated figures


## This will create sub-directory folders in the master project directory if doesn't exist
if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

if (!dir.exists(here::here(figDir))){
  dir.create(figDir)
} else {
  print("Figure directory already exists!")
}

# Download the data ------------------------------------------------------------

#download 12th grade data
icpsr_download(file_id = c(  7927,  7928,  7929,  7930,
                             7900,  9013,  9045,  8387,  8388, 
                             8546,  8701,  9079,  9259,  9397, 
                             9745,  9871,  6133,  6367,  6517,
                             6716,  2268,  2477,  2751,  2939,
                             3184,  3425,  3753,  4019,  4264,
                             4536, 20022, 22480, 25382, 28401,
                             30985, 34409, 34861, 35218, 36263,
                             36408, 36798, 37182, 37416, 37841,
                             38156, 38503),
               download_dir = dataDir1)

# To download one survey year at a time (for yearly updates):
# icpsr_download(file_id = 37416)


# Loading the (Stata) data files -----------------------------------------------
#12th Form 3: 1976-1988 0003 files, 1989-2021, 0004 files
form2dta <- list.files(path=dataDir1, pattern = ".-0002-Data.dta$|.-0003-Data.dta$", recursive = TRUE) # create a list of Form 3 data files -- different folders based on the year
form3dta <- list.files(path=dataDir1, pattern = ".-0003-Data.dta$|.-0004-Data.dta$", recursive = TRUE) # create a list of Form 3 data files -- different folders based on the year

mtf_F3_list <- lapply(file.path(dataDir1, form3dta), read.dta) # turn the list into a list of dataframes

mtfF3 <- rbindlist(mtf_F3_list, use.names=TRUE, fill=TRUE) # Convert the list of data frames into one data frame

## Keep only Form 2 variables

mtf_V3 <- select(mtfF3, CASEID, V1,V3, V4, V5, V13, V16,V17,ARCHIVE_WT, starts_with("V3"))

## Keep only Form 2 survey respondents
mtf_V3 <- subset(mtf_V3, !is.na(V3151))


## Save the dataframe for easy open in the future
### Note: This data is NOT harmonized. Make frequent and judicious referral to the codebooks.
save(mtf_V3, file=file.path(dataDir1, "mtf_form3_12th.Rda"))

message("End of SC_00_setup and packages") # Marks end of R Script
