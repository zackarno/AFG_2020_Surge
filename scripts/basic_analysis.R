rm(list = ls())
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(stringr)
library(srvyr)
library(survey)
source("scripts/functions.R")

dis_ag_lvl <- c("region","modality","received_aid","displacement")[1]


# read_data ---------------------------------------------------------------

df <- read.csv("inputs/clean_dataset/tool1/cleaned_data.csv",na.strings = c(""," ",NA),stringsAsFactors = F)
region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% dplyr::select(c("Name","Region..name."))
df_with_regions <- df %>% left_join(region,by =c ("consent_response.province"="Name"))
population <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F)

# analysis_by_region ------------------------------------------------------
if (dis_ag_lvl=="region"){
  
displacement_type <- c("host","idp")
df_region <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type | 
                                                  consent_response.received_aid_mark_2 == "no")

population_group <-population %>% dplyr::filter(type == "population_group")
weights_pop_group <- survey_weight(df = df_region,df_strata = "list_displacement",
                         sf_strata = "strata",sf_pop = "population",pop = population_group)
weights_pop_group$weights_pop_group <- weights_pop_group$survey_weight 
weights_pop_group <- weights_pop_group %>% dplyr::select(c("list_displacement","weights_pop_group"))



pop_unconditional_cash <-population %>% dplyr::filter(type == "unconditional_cash")
weights_pop_unconditional_cash <- survey_weight2(df = df_region,df_strata = "Region..name.",
                                   sf_strata = "strata",sf_pop = "population",pop = pop_unconditional_cash)
weights_pop_unconditional_cash$weights_pop_unconditional_cash <- weights_pop_unconditional_cash$survey_weight 
weights_pop_unconditional_cash <- weights_pop_unconditional_cash %>% dplyr::select(c("Region..name.","weights_pop_unconditional_cash"))



pop_conditional_cash <-population %>% dplyr::filter(type == "conditional_cash")
weights_pop_conditional_cash <- survey_weight2(df = df_region,df_strata = "Region..name.",
                                                 sf_strata = "strata",sf_pop = "population",pop = pop_conditional_cash)
weights_pop_conditional_cash$weights_pop_conditional_cash <- weights_pop_conditional_cash$survey_weight 
weights_pop_conditional_cash <- weights_pop_conditional_cash %>% dplyr::select(c("Region..name.","weights_pop_conditional_cash"))



pop_non_beneficiary <-population %>% dplyr::filter(type == "non_beneficiary")
weights_pop_non_beneficiary<- survey_weight2(df = df_region,df_strata = "Region..name.",
                                               sf_strata = "strata",sf_pop = "population",pop = pop_non_beneficiary) 
weights_pop_non_beneficiary$weights_pop_non_beneficiary <- weights_pop_non_beneficiary$survey_weight 
weights_pop_non_beneficiary <- weights_pop_non_beneficiary %>% dplyr::select(c("Region..name.","weights_pop_non_beneficiary"))

weights <- c("weights_pop_group","weights_pop_unconditional_cash","weights_pop_conditional_cash","weights_pop_non_beneficiary")


df_region <- df_region %>% left_join(weights_pop_group) 
df_region <- df_region %>% left_join(weights_pop_unconditional_cash) 
df_region <- df_region %>% left_join(weights_pop_conditional_cash) 
df_region <- df_region %>% left_join(weights_pop_non_beneficiary) 
  
}




