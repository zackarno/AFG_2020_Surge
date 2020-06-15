
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)

# read_data ---------------------------------------------------------------
df <- read.csv("inputs/clean_dataset/tool1/cleaned_data.csv",na.strings = c(""," ",NA),stringsAsFactors = F)
