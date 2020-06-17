
survey_weight <- function(df_strata,sf_strata,sf_pop,pop){

sf_with_weights<-df_region %>% 
  group_by(!!sym(df_strata)) %>% 
  summarise(sample_strata_num=n()) %>% 
  left_join(pop, by=c("list_displacement"= "strata"))%>% mutate(
    sample_global = sum(sample_strata_num),
    pop_global=sum(!!sym(sf_pop)),
    survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
  )
}


# a <- survey_weight("list_displacement","strata","population",pop = pop2)
