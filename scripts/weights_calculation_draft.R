rm(list = ls())
# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(stringr)
library(srvyr)
library(survey)

type_of_analysis <- c("region_and_beneficiaries","region_and_modality","displacement")[2]

# read_data ---------------------------------------------------------------

df <- read.csv("inputs/clean_dataset/tool1/cleaned_data.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::filter(!is.na(consent_response.received_aid_mark_2)) %>% dplyr::filter(consent == "yes") #filter data

region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% 
  dplyr::select(c("Name","Region..name.")) #region data 

df_with_regions <- df %>% left_join(region,by =c ("consent_response.province"="Name"))

population <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F) #population data full

# region and beneficiaries ----------------------------------------------

if ( type_of_analysis == "region_and_beneficiaries") {

pop <- population %>% dplyr::filter(type == "non_beneficiary" | type== "beneficiary" )

pop <- pop%>% 
  mutate(
    pop_global=sum(population),
    strata_and_bene_non_bene  = paste0(pop$strata,"_",pop$type))

sf_pop<- "population"
displacement_type <- c("host","idp")

df_for_grop_analysis <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type |
                         consent_response.received_aid_mark_2 == "no") %>% mutate(
                         benefeciaries_non_benefeciaries = if_else(consent_response.received_aid_mark_2 == "yes","beneficiary",
                                                                   "non_beneficiary")) %>% mutate(
                         strata_and_bene_non_bene  = paste0(Region..name.,"_",benefeciaries_non_benefeciaries)
                         )


sf_with_weights<- df_for_grop_analysis %>% 
    group_by(Region..name.,benefeciaries_non_benefeciaries) %>% 
    summarise(sample_strata_num=n()) %>% as.data.frame() %>% mutate(
      strata_and_bene_non_bene  = paste0(Region..name.,"_",benefeciaries_non_benefeciaries)
    ) %>% 
    right_join(pop,by = c("strata_and_bene_non_bene"))%>% mutate(
      sample_global = sum(sample_strata_num),
      survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    ) %>% select(c("strata","benefeciaries_non_benefeciaries","strata_and_bene_non_bene",	
    "sample_strata_num","population",	"pop_global",	"sample_global",	"survey_weight")) %>% 
  select(c("strata_and_bene_non_bene","survey_weight"))

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights)

}

# region and modality ----------------------------------------------------------------

if ( type_of_analysis == "region_and_modality") {

pop <- population %>% dplyr::filter(type == "unconditional_cash" | type== "conditional_cash" |
                                      type == "voucher" | type == "in_kind")

pop <- pop%>% 
  mutate(
    pop_global=sum(population),
    strata_condi_un_codi  = paste0(pop$strata,"_",pop$type))

sf_pop<- "population"
displacement_type <- c("host","idp")

df_for_grop_analysis <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type) %>% 
         mutate( strata_condi_un_codi  = paste0(Region..name.,"_",received_aid_type))


df_for_grop_analysis <- df_for_grop_analysis %>% filter(!is.na(received_aid_type))

sf_with_weights<- df_for_grop_analysis %>% 
  group_by(Region..name.,received_aid_type) %>% 
  summarise(sample_strata_num=n()) %>% as.data.frame() %>% mutate(
    strata_condi_un_codi = paste0(Region..name.,"_",received_aid_type)
  ) %>% 
  right_join(pop,by="strata_condi_un_codi") %>% mutate(
    sample_global = sum(sample_strata_num),
    survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
  ) %>% select(c("strata","strata_condi_un_codi","received_aid_type",	
                 "sample_strata_num","population",	"pop_global",	"sample_global",	"survey_weight")) %>% select(c("strata_condi_un_codi",
                                                                                                                 "survey_weight"))
data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights) %>% filter(!is.na(survey_weight))

}


# displacement  -----------------------------------------------------------

if ( type_of_analysis == "displacement") {
  
pop <- population %>% dplyr::filter(type == "population_group")

pop <- pop%>% 
  mutate(
    pop_global=sum(population))

sf_pop<- "population"

df_for_grop_analysis <-  df_with_regions

sf_with_weights<- df_for_grop_analysis %>% 
  group_by(list_displacement) %>% 
  summarise(sample_strata_num=n()) %>% 
  right_join(pop, by=c("list_displacement" = "strata")) %>% as.data.frame() %>% mutate(
    sample_global = sum(sample_strata_num),
    survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)) %>% 
      select(c("list_displacement","population",	"pop_global",	"sample_global","survey_weight")) %>% 
    select(c("list_displacement","survey_weight"))

data_for_analysis <- df_for_grop_analysis %>% left_join(sf_with_weights) %>% filter(!is.na(survey_weight))

}
