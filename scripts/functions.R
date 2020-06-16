survey_weight <- function(df,pop,df_strata,sf_strata,sf_pop){
  sf_with_weights<- df %>% 
    group_by(!!sym(df_strata)) %>% 
    summarise(sample_strata_num=n()) %>% 
    inner_join(pop, by=c("list_displacement"= "strata"))%>% mutate(
      sample_global = sum(sample_strata_num),
      pop_global=sum(!!sym(sf_pop)),
      survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    )
}


survey_weight2 <- function(df,pop,df_strata,sf_strata,sf_pop){
  sf_with_weights<- df %>% 
    group_by(!!sym(df_strata)) %>% 
    summarise(sample_strata_num=n()) %>% 
    inner_join(pop, by=c("Region..name."= "strata"))%>% mutate(
      sample_global = sum(sample_strata_num),
      pop_global=sum(!!sym(sf_pop)),
      survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
    )
}

