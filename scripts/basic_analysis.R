# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(stringr)
library(srvyr)
library(survey)

dis_ag_lvl <- c("region","modality","received_aid","displacement")[1]


# read_data ---------------------------------------------------------------

df <- read.csv("inputs/clean_dataset/tool1/cleaned_data.csv",na.strings = c(""," ",NA),stringsAsFactors = F)
region <-read.csv("dap/unhcr_hh/Region.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% dplyr::select(c("Name","Region..name."))
df_with_regions <- df %>% left_join(region,by =c ("consent_response.province"="Name"))
population <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F)

# analysis_by_region ------------------------------------------------------
if (dis_ag_lvl=="region"){
  
displacement_type <- c("host","idp")

population_group <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% dplyr::filter(
  type == "population_group"
)

df_region <-  df_with_regions %>% dplyr::filter(list_displacement %in% displacement_type)

weights_pop_group <- survey_weight(df = df_region,df_strata = "list_displacement",
                         sf_strata = "strata",sf_pop = "population",pop = population_group)


pop_unconditional_cash <-read.csv("dap/unhcr_hh/population.csv",na.strings = c(""," ",NA),stringsAsFactors = F) %>% dplyr::filter(
  type == "unconditional_cash"
)

weights_pop_unconditional_cash <- survey_weight2(df = df_region,df_strata = "Region..name.",
                                   sf_strata = "strata",sf_pop = "population",pop = pop_unconditional_cash)

}



